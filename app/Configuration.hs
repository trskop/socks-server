-- |
-- Module:      Configuration
-- Description: Configuration data type and Dhall decoding for it
-- Copyright:   (c) 2020-2021 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Configuration data type and Dhall decoding for it.
--
-- Based on:
-- <https://trskop.github.io/articles/2020-10-11-configure-services-with-dhall.html>
module Configuration
    ( Configuration(..)
    , Port
    , Listen(..)
    , getConfiguration
    )
  where

import Prelude (fromIntegral)

import Control.Applicative ((<**>), (<*>), (<|>), optional, pure)
import Control.Exception (throwIO)
import Control.Monad ((>>=))
import Data.Bool ((&&), otherwise)
import Data.Function ((.))
import Data.Functor ((<$>), (<$>))
import Data.Int (Int)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Ord ((<=), (>))
import Data.Semigroup ((<>))
import Data.String (fromString)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import System.Environment (lookupEnv)
import System.Exit (die, exitSuccess)
import System.IO (IO)

import qualified Data.Either.Validation as Validation
    ( Validation(Failure, Success)
    )
import Data.HostAndPort (type ListenFor, hostAndPort)
import Data.Text (Text)
import Dhall (FromDhall)
import qualified Dhall
    ( Decoder(Decoder, expected, extract)
    , Extractor
    , FromDhall(autoWith)
    , InputNormalizer
    , extractError
    , input
    , natural
    , string
    )
import Data.Streaming.Network (HostPreference)
import Data.Verbosity (Verbosity)
import qualified Options.Applicative as Options
    ( InfoMod
    , Parser
    , execParser
    , flag
    , flag'
    , help
    , helper
    , info
    , long
    , metavar
    , strOption
    )
import qualified Prettyprinter (line, pretty)
import qualified Prettyprinter.Render.Text as Prettyprinter (putDoc)


data Configuration = Configuration
    { listen :: Listen
    , verbosity :: Verbosity
    }
  deriving stock (Generic)
  deriving anyclass (FromDhall)

type Port = Int

newtype Listen = Listen (ListenFor "socks" HostPreference Port)
  deriving stock (Generic)

instance FromDhall Listen where
    autoWith :: Dhall.InputNormalizer -> Dhall.Decoder Listen
    autoWith _ = decodeListen

decodeListen :: Dhall.Decoder Listen
decodeListen =
    Listen <$> hostAndPort hostPreference port
  where
    hostPreference = fromString <$> Dhall.string

    port =
        let Dhall.Decoder{expected, extract} = Dhall.natural
        in  Dhall.Decoder
                { expected
                , extract = port' . extract
                }

    port'
        :: Dhall.Extractor src embed Natural
        -> Dhall.Extractor src embed Port
    port' = \case
        Validation.Success n
          | n > 0 && n <= 65535 ->
                pure (fromIntegral n)
          | otherwise ->
                Dhall.extractError "Port number must be between zero\
                    \ (excluding) and 65535 (including)"
        Validation.Failure e ->
            Validation.Failure e

-- ----------------------------------------------------------------------------

data Mode
    = Execute (Maybe Text)
    -- ^ We want to execute the application with the given configuration. If
    -- the configuration is 'Nothing' we need to read the environment variable.
    | Typecheck (Maybe Text)
    -- ^ We want to do a dry-run during which we only typecheck the
    -- configuration. If the configuration is 'Nothing' we need to read the
    -- environment variable.
    | PrintType
    -- ^ Instead of doing anything just print the type of the configuration the
    -- application expects to be given.

getConfiguration
    :: forall config
    .  Dhall.Decoder config
    -- ^ Dhall 'Dhall.Decoder' consists of parser and expected type. Dhall
    -- library provides one special 'Dhall.Decoder':
    --
    -- @
    -- 'Dhall.auto' :: 'Dhall.FromDhall' a => 'Dhall.Decoder' a
    -- @
    --
    -- Which allows us to use type class mechanism for deriving and combining
    -- parsers and is a good default in many cases.
    -> (forall a. Options.InfoMod a)
    -- ^ Allows options parser configuration including help message information
    -- and general parser behaviour. Reason for rank-2 type is that we don't
    -- want to expose the unnecessary internals of the parser we are using.
    -> IO config
getConfiguration decoder@Dhall.Decoder{expected} infoMod =
    parseOptions >>= \case
        Execute config ->
            parseConfig config

        Typecheck config -> do
            _ <- parseConfig config
            exitSuccess

        PrintType -> do
            printType
            exitSuccess
  where
    parseConfig :: Maybe Text -> IO config
    parseConfig possiblyExpr = do
        expr <- case possiblyExpr of
            Just expr ->
                pure expr

            Nothing ->
                lookupEnv "CONFIG" >>= maybe dieNoConfig (pure . fromString)

        Dhall.input decoder expr

    printType :: IO ()
    printType = case expected of
        Validation.Success expr ->
            Prettyprinter.putDoc
                ( Prettyprinter.pretty expr
                <> Prettyprinter.line
                )

        Validation.Failure err ->
            -- This indicates a bug in the Decoder.
            throwIO err

    dieNoConfig :: IO a
    dieNoConfig =
        die "Either `CONFIG=EXPR' environment variable or `--config=EXPR'\
            \ command-line option must be specified"

    parseOptions :: IO Mode
    parseOptions =
        Options.execParser (Options.info (options <**> Options.helper) infoMod)

    options :: Options.Parser Mode
    options = printTypeFlag <|> (typecheckFlag <*> optional configOption)

    configOption :: Options.Parser Text
    configOption = Options.strOption
        ( Options.long "config"
        <> Options.metavar "EXPR"
        <> Options.help "Set configuration to EXPR, where EXPR is a Dhall\
            \ expression; if application fails to parse or typecheck the EXPR\
            \ then it terminates with exit code 1"
        )

    typecheckFlag :: Options.Parser (Maybe Text -> Mode)
    typecheckFlag = Options.flag Execute Typecheck
        ( Options.long "config-typecheck"
        <> Options.help "Typecheck the configuration and exit; exit code 0 is\
            \ used on success and exit code 1 on failure to typecheck"
        )

    printTypeFlag :: Options.Parser Mode
    printTypeFlag = Options.flag' PrintType
        ( Options.long "config-print-type"
        <> Options.help "Print Dhall type of configuration accepted by the\
            \ application"
        )
