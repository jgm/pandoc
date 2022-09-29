{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : PandocCLI.Lua
   Copyright   : © 2022 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>

Functions to run the pandoc Lua scripting engine.
-}
module PandocCLI.Lua (runLuaInterpreter, getEngine) where

import Control.Monad ((<=<))
import HsLua.CLI (EnvBehavior (..), Settings (..), runStandalone)
import Text.Pandoc.Class (runIOorExplode)
import Text.Pandoc.Error (handleError)
import Text.Pandoc.Lua (runLua, runLuaNoEnv, getEngine)
import Text.Pandoc.Shared (pandocVersionText)

-- | Runs pandoc as a Lua interpreter that is (mostly) compatible with
-- the default @lua@ program shipping with Lua.
runLuaInterpreter :: String    -- ^ Program name
                  -> [String]  -- ^ Command line arguments
                  -> IO ()
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
