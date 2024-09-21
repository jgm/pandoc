{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Lua.Module
   Copyright   : © 2017-2024 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>

Setting up and initializing Lua modules.
-}

module Text.Pandoc.Lua.Module
  ( initModules
  ) where

import Control.Monad (forM_, when)
import Data.Version (makeVersion)
import HsLua as Lua
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.Marshal.List (pushPandocList, pushListModule)
import Text.Pandoc.Lua.PandocLua (PandocLua (..), liftPandocLua)
import qualified Data.ByteString.Char8 as Char8
import qualified Lua.LPeg as LPeg
import qualified HsLua.Aeson
import qualified HsLua.Module.DocLayout as Module.Layout
import qualified HsLua.Module.Path as Module.Path
import qualified HsLua.Module.Zip as Module.Zip
import qualified Text.Pandoc.Lua.Module.CLI as Pandoc.CLI
import qualified Text.Pandoc.Lua.Module.Format as Pandoc.Format
import qualified Text.Pandoc.Lua.Module.Image as Pandoc.Image
import qualified Text.Pandoc.Lua.Module.JSON as Pandoc.JSON
import qualified Text.Pandoc.Lua.Module.Log as Pandoc.Log
import qualified Text.Pandoc.Lua.Module.MediaBag as Pandoc.MediaBag
import qualified Text.Pandoc.Lua.Module.Pandoc as Module.Pandoc
import qualified Text.Pandoc.Lua.Module.Scaffolding as Pandoc.Scaffolding
import qualified Text.Pandoc.Lua.Module.Structure as Pandoc.Structure
import qualified Text.Pandoc.Lua.Module.System as Pandoc.System
import qualified Text.Pandoc.Lua.Module.Template as Pandoc.Template
import qualified Text.Pandoc.Lua.Module.Text as Pandoc.Text
import qualified Text.Pandoc.Lua.Module.Types as Pandoc.Types
import qualified Text.Pandoc.Lua.Module.Utils as Pandoc.Utils

initModules :: PandocLua ()
initModules = do
  initPandocModule
  initJsonMetatable
  installLpegSearcher
  setGlobalModules

initPandocModule :: PandocLua ()
initPandocModule = liftPandocLua $ do
  -- Push module table
  registerModule Module.Pandoc.documentedModule
  -- load modules and add them to the `pandoc` module table.
  forM_ submodules $ \mdl -> do
    registerModule mdl
    -- pandoc.text must be require-able as 'text' for backwards compat.
    when (moduleName mdl == "pandoc.text") $ do
      getfield registryindex loaded
      pushvalue (nth  2)
      setfield (nth 2) "text"
      pop 1 -- _LOADED
    -- Shorten name, drop everything before the first dot (if any).
    let fieldname (Name mdlname) = Name .
          maybe mdlname snd . Char8.uncons . snd $
          Char8.break (== '.') mdlname
    Lua.setfield (nth 2) (fieldname $ moduleName mdl)
  -- pandoc.List is low-level and must be opened differently.
  requirehs "pandoc.List" (const pushListModule)
  setfield (nth 2) "List"
  -- assign module to global variable
  Lua.setglobal "pandoc"

-- | Modules that are loaded at startup and assigned to fields in the
-- pandoc module.
--
-- Note that @pandoc.List@ is not included here for technical reasons;
-- it must be handled separately.
submodules :: [Module PandocError]
submodules =
  [ Pandoc.CLI.documentedModule
  , Pandoc.Format.documentedModule
  , Pandoc.Image.documentedModule
  , Pandoc.JSON.documentedModule
  , Pandoc.Log.documentedModule
  , Pandoc.MediaBag.documentedModule
  , Pandoc.Scaffolding.documentedModule
  , Pandoc.Structure.documentedModule
  , Pandoc.System.documentedModule
  , Pandoc.Template.documentedModule
  , Pandoc.Text.documentedModule
  , Pandoc.Types.documentedModule
  , Pandoc.Utils.documentedModule
  , ((Module.Layout.documentedModule { moduleName = "pandoc.layout" }
      `allSince` [2,18])
     `functionsSince` ["bold", "italic", "underlined", "strikeout", "fg", "bg"])
    [3, 4, 1]
  , Module.Path.documentedModule { moduleName = "pandoc.path" }
    `allSince` [2,12]
  , Module.Zip.documentedModule { moduleName = "pandoc.zip" }
    `allSince` [3,0]
  ]
 where
  allSince mdl version = mdl
    { moduleFunctions = map (`since` makeVersion version) $ moduleFunctions mdl
    }
  functionsSince mdl fns version = mdl
    { moduleFunctions = map (\fn ->
                               if (functionName fn) `elem` fns
                               then fn `since` makeVersion version
                               else fn) $ moduleFunctions mdl
    }

-- | Load all global modules and set them to their global variables.
setGlobalModules :: PandocLua ()
setGlobalModules = liftPandocLua $ do
  let globalModules =
        [ ("lpeg", LPeg.luaopen_lpeg_ptr)  -- must be loaded first
        , ("re", LPeg.luaopen_re_ptr)      -- re depends on lpeg
        ]
  forM_ globalModules $
    \(pkgname, luaopen) -> do
      Lua.pushcfunction luaopen
      Lua.pcall 0 1 Nothing >>= \case
        OK -> do               -- all good, loading succeeded
          -- register as loaded module so later modules can rely on this
          Lua.getfield Lua.registryindex Lua.loaded
          Lua.pushvalue (Lua.nth 2)
          Lua.setfield (Lua.nth 2) pkgname
          Lua.pop 1  -- pop _LOADED
        _  -> do               -- built-in library failed, load system lib
          Lua.pop 1  -- ignore error message
          -- Try loading via the normal package loading mechanism.
          Lua.getglobal "require"
          Lua.pushName pkgname
          Lua.call 1 1  -- Throws an exception if loading failed again!

      -- Module on top of stack. Register as global
      Lua.setglobal pkgname

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
  pushPandocList (const pushnil) []
  getmetatable top
  setfield registryindex HsLua.Aeson.jsonarray
  Lua.pop 1
