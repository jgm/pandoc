{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : PandocCLI.Lua
   Copyright   : © 2022-2023 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>

Functions to run the pandoc Lua scripting engine.
-}
module PandocCLI.Lua (runLuaInterpreter, getEngine) where

import Control.Monad ((<=<))
import HsLua.CLI (EnvBehavior (..), Settings (..), runStandalone)
import System.Environment (lookupEnv)
import System.IO.Temp (withSystemTempFile)
import System.IO (hClose)
import Text.Pandoc.Class (runIOorExplode)
import Text.Pandoc.Error (handleError)
import Text.Pandoc.Lua (runLua, runLuaNoEnv, getEngine)
import Text.Pandoc.Version (pandocVersionText)

-- | Runs pandoc as a Lua interpreter that is (mostly) compatible with
-- the default @lua@ program shipping with Lua.
--
-- The filename for the history of the REPL is taken from the
-- @PANDOC_REPL_HISTORY@ environment variable if possible. Otherwise a
-- new temporary file is used; it is removed after the REPL finishes.
runLuaInterpreter :: String    -- ^ Program name
                  -> [String]  -- ^ Command line arguments
                  -> IO ()
runLuaInterpreter progName args = do
  -- We need some kind of temp
  mbhistfile <- lookupEnv "PANDOC_REPL_HISTORY"
  case mbhistfile of
    Just histfile -> runStandaloneWithHistory histfile
    Nothing -> withSystemTempFile "pandoc-hist" $ \fp handle -> do
      -- We cannot pass a handle to the repl; the file will be re-opened
      -- there.
      hClose handle
      runStandaloneWithHistory fp
  where
    runStandaloneWithHistory histfile =  do
      let settings = Settings
            { settingsVersionInfo = "\nEmbedded in pandoc " <>
                                    pandocVersionText
            , settingsRunner = runner
            , settingsHistory = Just histfile
            }
      runStandalone settings progName args
    runner envBehavior =
      let runLua' = case envBehavior of
                      IgnoreEnvVars  -> runLuaNoEnv
                      ConsultEnvVars -> runLua
      in handleError <=< runIOorExplode . runLua'
