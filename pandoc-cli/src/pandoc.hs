{-# LANGUAGE OverloadedStrings #-}
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
import System.Environment (getArgs, getProgName)
import Text.Pandoc.App ( convertWithOpts, defaultOpts, options
                       , parseOptionsFromArgs)
import Text.Pandoc.Class (runIOorExplode)
import Text.Pandoc.Error (handleError)
import Text.Pandoc.Lua (runLua)
import Text.Pandoc.Shared (pandocVersion)
import qualified Text.Pandoc.UTF8 as UTF8
import PandocCLI.Server

main :: IO ()
main = E.handle (handleError . Left) $ do
  prg <- getProgName
  rawArgs <- map UTF8.decodeArg <$> getArgs
  case prg of
    "pandoc-server.cgi" -> runCGI
    "pandoc-server"     -> runServer
    "pandoc-lua"        -> runLuaInterpreter prg rawArgs
    _ -> parseOptionsFromArgs options defaultOpts prg rawArgs
         >>= convertWithOpts

-- | Runs pandoc as a Lua interpreter that is (mostly) compatible with
-- the default @lua@ program shipping with Lua.
runLuaInterpreter :: String -> [String] -> IO ()
runLuaInterpreter _progName _args = do
  let settings = Settings
        { settingsVersionInfo = "\nEmbedded in pandoc " <> pandocVersion
        , settingsRunner = handleError <=< runIOorExplode . runLua
        }
  runStandalone settings
