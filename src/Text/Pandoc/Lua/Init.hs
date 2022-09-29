{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Lua
   Copyright   : Copyright © 2017-2022 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Functions to initialize the Lua interpreter.
-}
module Text.Pandoc.Lua.Init
  ( runLua
  , runLuaNoEnv
  ) where

import Control.Monad (forM, forM_, when)
import Control.Monad.Catch (throwM, try)
import Control.Monad.Trans (MonadIO (..))
import Data.Maybe (catMaybes)
import HsLua as Lua hiding (status, try)
import Text.Pandoc.Class (PandocMonad, readDataFile)
import Text.Pandoc.Error (PandocError (PandocLuaError))
import Text.Pandoc.Lua.Marshal.List (newListMetatable, pushListModule)
import Text.Pandoc.Lua.PandocLua (PandocLua, liftPandocLua, runPandocLua)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as T
import qualified Lua.LPeg as LPeg
import qualified HsLua.Aeson
import qualified HsLua.Module.DocLayout as Module.Layout
import qualified HsLua.Module.Path as Module.Path
import qualified HsLua.Module.Text as Module.Text
import qualified Text.Pandoc.Lua.Module.Pandoc as Module.Pandoc
import qualified Text.Pandoc.Lua.Module.MediaBag as Pandoc.MediaBag
import qualified Text.Pandoc.Lua.Module.System as Pandoc.System
import qualified Text.Pandoc.Lua.Module.Template as Pandoc.Template
import qualified Text.Pandoc.Lua.Module.Types as Pandoc.Types
import qualified Text.Pandoc.Lua.Module.Utils as Pandoc.Utils

-- | Run the lua interpreter, using pandoc's default way of environment
-- initialization.
runLua :: (PandocMonad m, MonadIO m)
       => LuaE PandocError a -> m (Either PandocError a)
runLua action =
  runPandocLua . try $ do
    initLuaState
    liftPandocLua action

-- | Like 'runLua', but ignores all environment variables like @LUA_PATH@.
runLuaNoEnv :: (PandocMonad m, MonadIO m)
            => LuaE PandocError a -> m (Either PandocError a)
runLuaNoEnv action =
  runPandocLua . try $ do
    liftPandocLua $ do
      -- This is undocumented, but works -- the code is adapted from the
      -- `lua.c` sources for the default interpreter.
      Lua.pushboolean True
      Lua.setfield Lua.registryindex "LUA_NOENV"
    initLuaState
    liftPandocLua action

-- | Modules that are loaded at startup and assigned to fields in the
-- pandoc module.
--
-- Note that @pandoc.List@ is not included here for technical reasons;
-- it must be handled separately.
loadedModules :: [Module PandocError]
loadedModules =
  [ Pandoc.MediaBag.documentedModule
  , Pandoc.System.documentedModule
  , Pandoc.Template.documentedModule
  , Pandoc.Types.documentedModule
  , Pandoc.Utils.documentedModule
  , Module.Layout.documentedModule { moduleName = "pandoc.layout" }
  , Module.Path.documentedModule { moduleName = "pandoc.path" }
  , Module.Text.documentedModule
  ]

-- | Initialize the lua state with all required values
initLuaState :: PandocLua ()
initLuaState = do
  liftPandocLua Lua.openlibs
  initJsonMetatable
  initPandocModule
  installLpegSearcher
  setGlobalModules
  loadInitScript "init.lua"
 where
  initPandocModule :: PandocLua ()
  initPandocModule = liftPandocLua $ do
    -- Push module table
    registerModule Module.Pandoc.documentedModule
    -- load modules and add them to the `pandoc` module table.
    forM_ loadedModules $ \mdl -> do
      registerModule mdl
      let fieldname (Name mdlname) = Name .
            maybe mdlname snd . Char8.uncons . snd $
            Char8.break (== '.') mdlname
      Lua.setfield (nth 2) (fieldname $ moduleName mdl)
    -- pandoc.List is low-level and must be opened differently.
    requirehs "pandoc.List" (const pushListModule)
    setfield (nth 2) "List"
    -- assign module to global variable
    Lua.setglobal "pandoc"

  loadInitScript :: FilePath -> PandocLua ()
  loadInitScript scriptFile = do
    script <- readDataFile scriptFile
    status <- liftPandocLua $ Lua.dostring script
    when (status /= Lua.OK) . liftPandocLua $ do
      err <- popException
      let prefix = "Couldn't load '" <> T.pack scriptFile <> "':\n"
      throwM . PandocLuaError . (prefix <>) $ case err of
        PandocLuaError msg -> msg
        _                  -> T.pack $ show err

  setGlobalModules :: PandocLua ()
  setGlobalModules = liftPandocLua $ do
    let globalModules =
          [ ("lpeg", LPeg.luaopen_lpeg_ptr)  -- must be loaded first
          , ("re", LPeg.luaopen_re_ptr)      -- re depends on lpeg
          ]
    loadedBuiltInModules <- fmap catMaybes . forM globalModules $
      \(pkgname, luaopen) -> do
        Lua.pushcfunction luaopen
        usedBuiltIn <- Lua.pcall 0 1 Nothing >>= \case
          OK -> do               -- all good, loading succeeded
            -- register as loaded module so later modules can rely on this
            Lua.getfield Lua.registryindex Lua.loaded
            Lua.pushvalue (Lua.nth 2)
            Lua.setfield (Lua.nth 2) pkgname
            Lua.pop 1  -- pop _LOADED
            return True
          _  -> do               -- built-in library failed, load system lib
            Lua.pop 1  -- ignore error message
            -- Try loading via the normal package loading mechanism.
            Lua.getglobal "require"
            Lua.pushName pkgname
            Lua.call 1 1  -- Throws an exception if loading failed again!
            return False

        -- Module on top of stack. Register as global
        Lua.setglobal pkgname
        return $ if usedBuiltIn then Just pkgname else Nothing

    -- Remove module entry from _LOADED table in registry if we used a
    -- built-in library. This ensures that later calls to @require@ will
    -- prefer the shared library, if any.
    forM_ loadedBuiltInModules $ \pkgname -> do
      Lua.getfield Lua.registryindex Lua.loaded
      Lua.pushnil
      Lua.setfield (Lua.nth 2) pkgname
      Lua.pop 1  -- registry

  installLpegSearcher :: PandocLua ()
  installLpegSearcher = liftPandocLua $ do
    Lua.getglobal' "package.searchers"
    Lua.pushHaskellFunction $ Lua.state >>= liftIO . LPeg.lpeg_searcher
    Lua.rawseti (Lua.nth 2) . (+1) . fromIntegral =<< Lua.rawlen (Lua.nth 2)
    Lua.pop 1  -- remove 'package.searchers' from stack

-- | Setup the metatable that's assigned to Lua tables that were created
-- from/via JSON arrays.
initJsonMetatable :: PandocLua ()
initJsonMetatable = liftPandocLua $ do
  newListMetatable HsLua.Aeson.jsonarray (pure ())
