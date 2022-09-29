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
import HsLua.CLI (EnvBehavior (..), Settings (..), runStandalone)
import System.Environment (getArgs, getProgName)
import Text.Pandoc.App ( convertWithOpts, defaultOpts, options
                       , parseOptionsFromArgs)
import Text.Pandoc.Class (runIOorExplode)
import Text.Pandoc.Error (handleError)
import Text.Pandoc.Lua (getEngine, runLua, runLuaNoEnv)
import Text.Pandoc.Shared (pandocVersionText)
import qualified Text.Pandoc.UTF8 as UTF8
import PandocCLI.Server

main :: IO ()
main = E.handle (handleError . Left) $ do
  prg <- getProgName
  rawArgs <- map UTF8.decodeArg <$> getArgs
  case prg of
    "pandoc-server.cgi" -> runCGI
    "pandoc-server"     -> runServer rawArgs
    "pandoc-lua"        -> runLuaInterpreter prg rawArgs
    _ ->
      case rawArgs of
        "lua" : args   -> runLuaInterpreter "pandoc lua" args
        "serve" : args -> runServer args
        _              -> do
          engine <- getEngine
          let cliOpts = options engine
          opts <- parseOptionsFromArgs cliOpts defaultOpts prg rawArgs
          convertWithOpts engine opts

-- | Runs pandoc as a Lua interpreter that is (mostly) compatible with
-- the default @lua@ program shipping with Lua.
runLuaInterpreter :: String -> [String] -> IO ()
runLuaInterpreter progName args = do
  let settings = Settings
        { settingsVersionInfo = "\nEmbedded in pandoc " <> pandocVersionText
        , settingsRunner = runner
        }
  runStandalone settings progName args
  where
    runner envBehavior =
      let runLua' = case envBehavior of
                      IgnoreEnvVars  -> runLuaNoEnv
                      ConsultEnvVars -> runLua
      in handleError <=< runIOorExplode . runLua'
