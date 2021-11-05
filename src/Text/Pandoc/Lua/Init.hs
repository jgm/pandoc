{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Lua
   Copyright   : Copyright © 2017-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Functions to initialize the Lua interpreter.
-}
module Text.Pandoc.Lua.Init
  ( runLua
  ) where

import Control.Monad (when)
import Control.Monad.Catch (throwM, try)
import Control.Monad.Trans (MonadIO (..))
import Data.Data (Data, dataTypeConstrs, dataTypeOf, showConstr)
import HsLua as Lua hiding (status, try)
import GHC.IO.Encoding (getForeignEncoding, setForeignEncoding, utf8)
import Text.Pandoc.Class.PandocMonad (PandocMonad, readDataFile)
import Text.Pandoc.Error (PandocError (PandocLuaError))
import Text.Pandoc.Lua.Packages (installPandocPackageSearcher)
import Text.Pandoc.Lua.PandocLua (PandocLua, liftPandocLua, runPandocLua)
import qualified Data.Text as T
import qualified Lua.LPeg as LPeg
import qualified Text.Pandoc.Definition as Pandoc
import qualified Text.Pandoc.Lua.Module.Pandoc as ModulePandoc

-- | Run the lua interpreter, using pandoc's default way of environment
-- initialization.
runLua :: (PandocMonad m, MonadIO m)
       => LuaE PandocError a -> m (Either PandocError a)
runLua luaOp = do
  enc <- liftIO $ getForeignEncoding <* setForeignEncoding utf8
  res <- runPandocLua . try $ do
    initLuaState
    liftPandocLua luaOp
  liftIO $ setForeignEncoding enc
  return res

-- | Initialize the lua state with all required values
initLuaState :: PandocLua ()
initLuaState = do
  liftPandocLua Lua.openlibs
  installPandocPackageSearcher
  initPandocModule
  requireGlobalModules
  loadInitScript "init.lua"
 where
  initPandocModule :: PandocLua ()
  initPandocModule = do
    -- Push module table
    ModulePandoc.pushModule
    -- register as loaded module
    liftPandocLua $ do
      Lua.getfield Lua.registryindex Lua.loaded
      Lua.pushvalue (Lua.nth 2)
      Lua.setfield (Lua.nth 2) "pandoc"
      Lua.pop 1
    -- copy constructors into registry
    putConstructorsInRegistry
    -- assign module to global variable
    liftPandocLua $ Lua.setglobal "pandoc"

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

  requireGlobalModules :: PandocLua ()
  requireGlobalModules = liftPandocLua $ do
    Lua.pushcfunction LPeg.luaopen_lpeg_ptr
    Lua.pcall 0 1 Nothing >>= \case
      Lua.OK -> do
        -- Success. Register as a loaded module.
        -- Get table "_LOADED" from registry, add entry.
        _ <- Lua.getfield Lua.registryindex Lua.loaded
        Lua.pushvalue (Lua.nth 2)
        Lua.setfield (Lua.nth 2) "lpeg"
        Lua.pop 1  -- pop _LOADED

      _ -> do
        -- Maybe LPeg was not compiled into the program. Try loading via
        -- the normal package loading mechanism.
        pop 1 -- ignore error message
        Lua.getglobal "require"
        Lua.pushName "lpeg"
        Lua.call 1 1   -- throws an exception if the module is not found

    -- Module on top of stack. Register as global
    Lua.setglobal "lpeg"

-- | AST elements are marshaled via normal constructor functions in the
-- @pandoc@ module. However, accessing Lua globals from Haskell is
-- expensive (due to error handling). Accessing the Lua registry is much
-- cheaper, which is why the constructor functions are copied into the
-- Lua registry and called from there.
--
-- This function expects the @pandoc@ module to be at the top of the
-- stack.
putConstructorsInRegistry :: PandocLua ()
putConstructorsInRegistry = liftPandocLua $ do
  constrsToReg $ Pandoc.Meta mempty
  constrsToReg $ Pandoc.MetaList mempty
  putInReg "List"  -- pandoc.List
  putInReg "SimpleTable"  -- helper for backward-compatible table handling
 where
  constrsToReg :: Data a => a -> LuaE PandocError ()
  constrsToReg = mapM_ (putInReg . showConstr) . dataTypeConstrs . dataTypeOf

  putInReg :: String -> LuaE PandocError ()
  putInReg name = do
    Lua.push ("pandoc." ++ name) -- name in registry
    Lua.push name -- in pandoc module
    Lua.rawget (Lua.nth 3)
    Lua.rawset Lua.registryindex
