-- |
-- Module:      SocksServer
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2020 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions; POSIX.
--
-- TODO: Module description.
module SocksServer
    ( socksServer
    )
  where

import Prelude (error)

import Control.Applicative (pure)
import Control.Exception (bracket, catch, onException)
import Control.Monad ((>>=), unless)
import Data.Either (Either(Left, Right), either)
import Data.Eq ((==))
import Data.Function (($), (.), const)
import Data.Functor ((<$), (<$>))
import Data.Int (Int)
import Data.List (elem, head)
import Data.Maybe (Maybe(Just, Nothing))
import Foreign.C.Error (Errno(Errno), eCONNREFUSED, eHOSTUNREACH, eNETUNREACH)
import GHC.IO.Exception (IOException(IOError, ioe_errno))
import System.IO (IO)
import Text.Show (show)

import Control.Concurrent.Async (async, waitEitherCancel)
import qualified Data.ByteString as ByteString (null)
import qualified Data.ByteString.Char8 as ByteString (unpack)
import Data.HostAndPort (pattern ListenFor)
import qualified Data.HostAndPort (listenHost, listenPort)
import Data.Serialize (Serialize, Result(..), encode, get, runGetPartial)
import Data.Streaming.Network
    ( AppData
    , HasReadWrite
    , ServerSettings
    , appRead
    , appWrite
    , getReadBufferSize
    , runTCPServer
    , safeRecv
    , serverSettingsTCP
    )
import qualified Data.Streaming.Network.Internal as Streaming.Internal
    ( AppData(..)
    )
import Network.Socket
    ( pattern AF_INET
    , pattern AF_INET6
    , pattern Stream
    , pattern NoDelay
    , AddrInfo
        ( addrAddress
        , addrFamily
        , addrFlags
        , addrProtocol
        , addrSocketType
        , addrSocketType
        )
    , AddrInfoFlag(AI_PASSIVE)
    , Family
    , HostName
    , PortNumber
    , ProtocolNumber
    , ServiceName
    , SockAddr
        ( SockAddrInet
        , SockAddrInet6
        , SockAddrUnix
        )
    , Socket
    , SocketType
    , close
    , connect
    , defaultHints
    , defaultProtocol
    , getAddrInfo
    , setSocketOption
    , socket
    )
import Network.Socket.ByteString (sendAll)
import Network.Socks5.Lowlevel
    ( SocksHello(..)
    , SocksHelloResponse(..)
    , SocksRequest(..)
    , SocksResponse(..)
    )
import Network.Socks5.Types
    ( SocksCommand(..)
    , SocksError(..)
    , SocksHostAddress(..)
    , SocksMethod(..)
    , SocksReply(..)
    )

import Configuration
    ( Configuration(Configuration, listen, verbosity)
    , Listen(Listen)
    )


socksServer :: Configuration -> IO ()
socksServer configuration@Configuration{verbosity} = do
    let serverSettings = mkServerSettings configuration
    runTCPServer serverSettings \appData -> do
        SocksHello methods <- readDecode appData
        writeEncoded appData $ SocksHelloResponse
            -- We don't support authentication.
            if SocksMethodNone `elem` methods
                then SocksMethodNone
                else SocksMethodNotAcceptable

        SocksRequest{..} <- readDecode appData
        case requestCommand of
            SocksCommandConnect -> do
                let -- Use the same buffer size for both ends of the
                    -- connection.
                    bufferSize = getReadBufferSize serverSettings

                runConnection appData bufferSize requestDstAddr requestDstPort

            SocksCommandBind ->
                writeCommandNotSupported requestDstPort requestDstAddr appData

            SocksCommandUdpAssociate ->
                writeCommandNotSupported requestDstPort requestDstAddr appData

            SocksCommandOther _ ->
                writeCommandNotSupported requestDstPort requestDstAddr appData

mkServerSettings :: Configuration -> ServerSettings
mkServerSettings Configuration{listen = Listen ListenFor{..}} =
    serverSettingsTCP listenPort listenHost

writeCommandNotSupported
    :: HasReadWrite appData
    => PortNumber
    -> SocksHostAddress
    -> appData
    -> IO ()
writeCommandNotSupported responseBindPort responseBindAddr appData =
    writeEncoded appData SocksResponse
        { responseReply = SocksReplyError SocksErrorCommandNotSupported
        , responseBindAddr
        , responseBindPort
        }

runConnection
    :: AppData
    -- ^ Downstream connection
    -> Int
    -- ^ Buffer size to use for upstream
    -> SocksHostAddress
    -> PortNumber
    -> IO ()
runConnection downstream bufferSize host port = withUpstream \upstream -> do
    upwards <- async do
        let loop = do
                d <- appRead downstream
                unless (ByteString.null d) do
                    appWrite upstream d
                    loop
        loop

    downwards <- async do
        let loop = do
                d <- appRead upstream
                unless (ByteString.null d) do
                    appWrite downstream d
                    loop
        loop

    () <$ waitEitherCancel upwards downwards
  where
    -- This function hadnles SOCKS response to the client and leaves the
    -- shoveling of the data between upstream and downstream to the function
    -- passed as an argument.
    withUpstream :: (AppData -> IO ()) -> IO ()
    withUpstream f = bracket connectToUpstream closeSocket \case
        Right (sock, address) ->
            case sockAddrToSocksHostAddress address of
                Nothing ->
                    -- This should never happen as we shouldn't suddenly be
                    -- using UNIX sockets.
                    writeEncoded downstream SocksResponse
                        { responseReply =
                            SocksReplyError SocksErrorGeneralServerFailure
                        , responseBindAddr = host
                        , responseBindPort = port
                        }

                Just (responseBindAddr, responseBindPort) -> do
                    writeEncoded downstream SocksResponse
                        { responseReply = SocksReplySuccess
                        , responseBindAddr
                        , responseBindPort
                        }

                    f Streaming.Internal.AppData
                        { appRead' = safeRecv sock bufferSize
                        , appWrite' = sendAll sock
                        , appSockAddr' = address
                        , appLocalAddr' = Nothing
                        , appCloseConnection' = close sock
                        , appRawSocket' = Just sock
                        }
        Left err ->
            writeEncoded downstream SocksResponse
                { responseReply = SocksReplyError err
                , responseBindAddr = host
                , responseBindPort = port
                }

    connectToUpstream :: IO (Either SocksError (Socket, SockAddr))
    connectToUpstream = do
        r@(sock, address) <- case host of
            SocksAddrIPV4 ipv4 ->
                (, SockAddrInet port ipv4)
                    <$> mkSocket AF_INET Stream defaultProtocol

            SocksAddrIPV6 ipv6 -> do
                (, SockAddrInet6 port 0 ipv6 0)
                    <$> mkSocket AF_INET6 Stream defaultProtocol

            SocksAddrDomainName fqdn -> do
                addrInfo <- resolve (ByteString.unpack fqdn) (show port)
                (, addrAddress addrInfo)
                    <$> mkSocket (addrFamily addrInfo) (addrSocketType addrInfo)
                            (addrProtocol addrInfo)

        (Right r <$ connect sock address `onException` close sock)
            `catch` \IOError{ioe_errno} ->
                pure $ Left case ioe_errno of
                    Just errno
                      | Errno errno == eHOSTUNREACH ->
                            SocksErrorHostUnreachable
                      | Errno errno == eCONNREFUSED ->
                            SocksErrorConnectionRefused
                      | Errno errno == eNETUNREACH ->
                            SocksErrorNetworkUnreachable

                    _ ->
                        SocksErrorGeneralServerFailure

    closeSocket :: Either SocksError (Socket, SockAddr) -> IO ()
    closeSocket = either (const $ pure ()) (\(s, _) -> close s)

resolve :: HostName -> ServiceName -> IO AddrInfo
resolve hostName serviceName = do
    let hints = defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }

    -- 'getAddrInfo' is guaranteed to return non-empty list. Internally it
    -- checks that if the list is empty and throws 'IOException' if it is
    -- :facepalm:
    head <$> getAddrInfo (Just hints) (Just hostName) (Just serviceName)

mkSocket :: Family -> SocketType -> ProtocolNumber -> IO Socket
mkSocket family socketType protocol = do
    sock <- socket family socketType protocol
    sock <$ setSocketOption sock NoDelay 1 `onException` close sock

sockAddrToSocksHostAddress :: SockAddr -> Maybe (SocksHostAddress, PortNumber)
sockAddrToSocksHostAddress = \case
    SockAddrInet port ipv4 ->
        Just (SocksAddrIPV4 ipv4, port)
    SockAddrInet6 port _flowInfo ipv6 _scopeId ->
        Just (SocksAddrIPV6 ipv6, port)
    SockAddrUnix _ ->
        Nothing

writeEncoded
    :: (Serialize a, HasReadWrite appData)
    => appData
    -> a
    -> IO ()
writeEncoded appData = appWrite appData . encode

readDecode
    :: (Serialize a, HasReadWrite appData)
    => appData
    -> IO a
readDecode appData = do
    -- Should we check that the connection was closed when reading data?
    firstDataChunk <- appRead appData
    (a, remainder) <- loop (runGetPartial get firstDataChunk)
    a <$ unless (ByteString.null remainder) do
        -- TODO: Better approach DecodingException?
        error "TODO"
  where
    loop = \case
        Fail s _ ->
            error s -- TODO: Better approach DecodingException?

        Partial continue ->
            appRead appData >>= loop . continue

        Done a remainder ->
            pure (a, remainder)
