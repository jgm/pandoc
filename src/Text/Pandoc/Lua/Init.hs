{- |
   Module      : Text.Pandoc.Lua
   Copyright   : Copyright © 2017-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Functions to initialize the Lua interpreter.
-}
module Text.Pandoc.Lua.Init
  ( runLua
  ) where

import Control.Monad.Catch (try)
import Control.Monad.Trans (MonadIO (..))
import Data.Data (Data, dataTypeConstrs, dataTypeOf, showConstr)
import Foreign.Lua (Lua)
import GHC.IO.Encoding (getForeignEncoding, setForeignEncoding, utf8)
import Text.Pandoc.Class.PandocIO (PandocIO)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.Packages (installPandocPackageSearcher)
import Text.Pandoc.Lua.PandocLua (PandocLua, liftPandocLua,
                                  loadScriptFromDataDir, runPandocLua)

import qualified Foreign.Lua as Lua
import qualified Text.Pandoc.Definition as Pandoc
import qualified Text.Pandoc.Lua.Module.Pandoc as ModulePandoc

-- | Run the lua interpreter, using pandoc's default way of environment
-- initialization.
runLua :: Lua a -> PandocIO (Either PandocError a)
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
  loadScriptFromDataDir "init.lua"
 where
  initPandocModule :: PandocLua ()
  initPandocModule = do
    -- Push module table
    ModulePandoc.pushModule
    -- register as loaded module
    liftPandocLua $ do
      Lua.pushvalue Lua.stackTop
      Lua.getfield Lua.registryindex Lua.loadedTableRegistryField
      Lua.setfield (Lua.nthFromTop 2) "pandoc"
      Lua.pop 1
    -- copy constructors into registry
    putConstructorsInRegistry
    -- assign module to global variable
    liftPandocLua $ Lua.setglobal "pandoc"

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
  constrsToReg $ Pandoc.Pandoc mempty mempty
  constrsToReg $ Pandoc.Str mempty
  constrsToReg $ Pandoc.Para mempty
  constrsToReg $ Pandoc.Meta mempty
  constrsToReg $ Pandoc.MetaList mempty
  constrsToReg $ Pandoc.Citation mempty mempty mempty Pandoc.AuthorInText 0 0
  putInReg "Attr"  -- used for Attr type alias
  putInReg "ListAttributes"  -- used for ListAttributes type alias
  putInReg "List"  -- pandoc.List
 where
  constrsToReg :: Data a => a -> Lua ()
  constrsToReg = mapM_ (putInReg . showConstr) . dataTypeConstrs . dataTypeOf

  putInReg :: String -> Lua ()
  putInReg name = do
    Lua.push ("pandoc." ++ name) -- name in registry
    Lua.push name -- in pandoc module
    Lua.rawget (Lua.nthFromTop 3)
    Lua.rawset Lua.registryindex
