{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{- |
   Module      : Main
   Copyright   : Copyright (C) 2006-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Parses command-line options and calls the appropriate readers and
writers.
-}
module Main where
import Control.Monad ((<=<))
import qualified Control.Exception as E
import HsLua.CLI (Settings (..), runStandalone)
import Text.Pandoc.App (convertWithOpts, defaultOpts, options, parseOptions)
import Text.Pandoc.Class (runIOorExplode)
import Text.Pandoc.Error (handleError)
import Text.Pandoc.Lua (runLua)
import Text.Pandoc.Shared (pandocVersion)
import System.Environment (getProgName)
#ifdef _SERVER
import qualified Network.Wai.Handler.CGI as CGI
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Timeout (timeout)
import Text.Pandoc.Server (ServerOpts(..), parseServerOpts, app)
import Safe (readDef)
import System.Environment (lookupEnv)
#else
import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode(ExitFailure))
#endif

main :: IO ()
main = E.handle (handleError . Left) $ do
  prg <- maybe getProgName pure =<< lookupEnv "PANDOC_PROGRAM_NAME"
  case prg of
    "pandoc-server.cgi" -> do
#ifdef _SERVER
      cgiTimeout <- maybe 2 (readDef 2) <$> lookupEnv "PANDOC_SERVER_TIMEOUT"
      CGI.run (timeout cgiTimeout app)
#else
      serverUnsupported
#endif
    "pandoc-server" -> do
#ifdef _SERVER
      sopts <- parseServerOpts
      Warp.run (serverPort sopts) (timeout (serverTimeout sopts) app)
#else
      serverUnsupported
#endif
    "pandoc-lua" -> do
      let settings = Settings
            { settingsVersionInfo = "\nEmbedded in pandoc " <> pandocVersion
            , settingsRunner = handleError <=< runIOorExplode . runLua
            }
      runStandalone settings
    _ -> parseOptions options defaultOpts >>= convertWithOpts

#ifndef _SERVER
serverUnsupported :: IO ()
serverUnsupported = do
  hPutStrLn stderr $ "Server mode unsupported.\n" <>
                     "Pandoc was not compiled with the 'server' flag."
  exitWith $ ExitFailure 4
#endif
