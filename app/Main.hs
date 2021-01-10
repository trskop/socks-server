-- |
-- Module:      Main
-- Description: SOCKS Server
-- Copyright:   (c) 2021 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- SOCKS Server.
module Main
    ( main
    )
  where

import Data.Semigroup ((<>))
import System.IO (IO)

import qualified Dhall (auto)
import qualified Options.Applicative as Options (fullDesc, header)

import Configuration (getConfiguration)
import SocksServer (socksServer)


main :: IO ()
main = do
    config <- getConfiguration Dhall.auto
        ( Options.fullDesc
        <> Options.header "Very useful info those that call the '--help'."
        )

    socksServer config
