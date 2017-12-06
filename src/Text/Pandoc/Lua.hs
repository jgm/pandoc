{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-
Copyright © 2017 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}
{- |
   Module      : Text.Pandoc.Lua
   Copyright   : Copyright © 2017 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc lua utils.
-}
module Text.Pandoc.Lua
  ( LuaException (..)
  , LuaPackageParams (..)
  , pushPandocModule
  , runLuaFilter
  , initLuaState
  , luaPackageParams
  ) where

import Control.Monad (when, (>=>))
import Control.Monad.Identity (Identity)
import Control.Monad.Trans (MonadIO (..))
import Data.IORef (newIORef, readIORef)
import Foreign.Lua (FromLuaStack (peek), Lua, LuaException (..),
                    Status (OK), ToLuaStack (push))
import Text.Pandoc.Class (PandocIO, getCommonState, getMediaBag, setMediaBag)
import Text.Pandoc.Definition
import Text.Pandoc.Lua.Filter (LuaFilter, walkMWithLuaFilter)
import Text.Pandoc.Lua.Packages (LuaPackageParams (..),
                                 installPandocPackageSearcher)
import Text.Pandoc.Lua.PandocModule (pushPandocModule) -- TODO: remove
import Text.Pandoc.Lua.Util (loadScriptFromDataDir)
import qualified Foreign.Lua as Lua
import qualified Foreign.Lua.Module.Text as Lua

runLuaFilter :: Maybe FilePath -> FilePath -> String
             -> Pandoc -> PandocIO (Either LuaException Pandoc)
runLuaFilter datadir filterPath format pd = do
  luaPkgParams <- luaPackageParams datadir
  res <- liftIO . Lua.runLuaEither $
         runLuaFilter' luaPkgParams filterPath format pd
  newMediaBag <- liftIO (readIORef (luaPkgMediaBag luaPkgParams))
  setMediaBag newMediaBag
  return res

runLuaFilter' :: LuaPackageParams
              -> FilePath -> String
              -> Pandoc -> Lua Pandoc
runLuaFilter' luaPkgOpts filterPath format pd = do
  initLuaState luaPkgOpts
  -- store module in global "pandoc"
  registerFormat
  top <- Lua.gettop
  stat <- Lua.dofile filterPath
  if stat /= OK
    then do
      luaErrMsg <- peek (-1) <* Lua.pop 1
      Lua.throwLuaError luaErrMsg
    else do
      newtop <- Lua.gettop
      -- Use the implicitly defined global filter if nothing was returned
      when (newtop - top < 1) pushGlobalFilter
      luaFilters <- peek (-1)
      runAll luaFilters pd
 where
  registerFormat = do
    push format
    Lua.setglobal "FORMAT"

luaPackageParams :: Maybe FilePath -> PandocIO LuaPackageParams
luaPackageParams datadir = do
  commonState <- getCommonState
  mbRef <- liftIO . newIORef =<< getMediaBag
  return LuaPackageParams
    { luaPkgCommonState = commonState
    , luaPkgDataDir = datadir
    , luaPkgMediaBag = mbRef
    }

-- Initialize the lua state with all required values
initLuaState :: LuaPackageParams -> Lua ()
initLuaState luaPkgParams = do
  Lua.openlibs
  Lua.preloadTextModule "text"
  installPandocPackageSearcher luaPkgParams
  loadScriptFromDataDir (luaPkgDataDir luaPkgParams) "init.lua"

pushGlobalFilter :: Lua ()
pushGlobalFilter = do
  Lua.newtable
  Lua.getglobal' "pandoc.global_filter"
  Lua.call 0 1
  Lua.rawseti (-2) 1

runAll :: [LuaFilter] -> Pandoc -> Lua Pandoc
runAll = foldr ((>=>) . walkMWithLuaFilter) return

instance (FromLuaStack a) => FromLuaStack (Identity a) where
  peek = fmap return . peek
