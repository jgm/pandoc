{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{- |
   Module      : Text.Pandoc.Lua.Init
   Copyright   : © 2017-2024 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>

Functions to initialize the Lua interpreter.
-}
module Text.Pandoc.Lua.Init
  ( initLua
  , userInit
  ) where

import Control.Monad (when)
import Control.Monad.Catch (throwM)
import HsLua as Lua hiding (status)
import Text.Pandoc.Class (report)
import Text.Pandoc.Data (readDataFile)
import Text.Pandoc.Error (PandocError (PandocLuaError))
import Text.Pandoc.Logging (LogMessage (ScriptingWarning))
import Text.Pandoc.Lua.Module (initModules)
import Text.Pandoc.Lua.PandocLua (PandocLua (..), liftPandocLua)
import Text.Pandoc.Lua.SourcePos (luaSourcePos)
import qualified Data.Text as T
import qualified Text.Pandoc.UTF8 as UTF8

-- | Initialize Lua with all default and pandoc-specific libraries and default
-- globals.
initLua :: PandocLua ()
initLua = do
  liftPandocLua Lua.openlibs
  setWarnFunction
  initModules
  liftPandocLua userInit

-- | User-controlled initialization, e.g., running the user's init script.
userInit :: LuaE PandocError ()
userInit = runInitScript

-- | Run the @init.lua@ data file as a Lua script.
runInitScript :: LuaE PandocError ()
runInitScript = runDataFileScript "init.lua"

-- | Get a data file and run it as a Lua script.
runDataFileScript :: FilePath -> LuaE PandocError ()
runDataFileScript scriptFile = do
  script <- unPandocLua $ readDataFile scriptFile
  status <- Lua.dostring script
  when (status /= Lua.OK) $ do
    err <- popException
    let prefix = "Couldn't load '" <> T.pack scriptFile <> "':\n"
    throwM . PandocLuaError . (prefix <>) $ case err of
      PandocLuaError msg -> msg
      _                  -> T.pack $ show err

setWarnFunction :: PandocLua ()
setWarnFunction = liftPandocLua . setwarnf' $ \msg -> do
  -- reporting levels:
  -- 0: this hook,
  -- 1: userdata wrapper function for the hook,
  -- 2: warn,
  -- 3: function calling warn.
  pos <- luaSourcePos 3
  unPandocLua . report $ ScriptingWarning (UTF8.toText msg) pos
