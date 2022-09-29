{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : PandocCLI.Lua
   Copyright   : © 2022 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>

Placeholder values to be used when pandoc is compiled without support
for the Lua scripting engine.
-}
module PandocCLI.Lua (runLuaInterpreter, getEngine) where

import Control.Monad.IO.Class (MonadIO)
import Text.Pandoc.Error (PandocError (PandocNoScriptingEngine), handleError)
import Text.Pandoc.Scripting (ScriptingEngine, noEngine)

-- | Raises an error, reporting that the scripting engine is unavailable.
runLuaInterpreter :: String    -- ^ Program name
                  -> [String]  -- ^ Command line arguments
                  -> IO ()
runLuaInterpreter _progName _args = do
  handleError (Left PandocNoScriptingEngine)

-- | Placeholder scripting engine.
getEngine :: MonadIO m => m ScriptingEngine
getEngine = pure noEngine
