{-
Copyright © 2017-2018 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.Lua.Packages
   Copyright   : Copyright © 2017-2018 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc module for lua.
-}
module Text.Pandoc.Lua.Packages
  ( LuaPackageParams (..)
  , installPandocPackageSearcher
  ) where

import Prelude
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.IORef (IORef)
import Foreign.Lua (Lua, NumResults, liftIO)
import Text.Pandoc.Class (CommonState, readDataFile, runIO, setUserDataDir)
import Text.Pandoc.MediaBag (MediaBag)

import qualified Foreign.Lua as Lua
import Text.Pandoc.Lua.Module.Pandoc as Pandoc
import Text.Pandoc.Lua.Module.MediaBag as MediaBag
import Text.Pandoc.Lua.Module.Utils as Utils

-- | Parameters used to create lua packages/modules.
data LuaPackageParams = LuaPackageParams
  { luaPkgCommonState :: CommonState
  , luaPkgDataDir :: Maybe FilePath
  , luaPkgMediaBag :: IORef MediaBag
  }

-- | Insert pandoc's package loader as the first loader, making it the default.
installPandocPackageSearcher :: LuaPackageParams -> Lua ()
installPandocPackageSearcher luaPkgParams = do
  Lua.getglobal' "package.searchers"
  shiftArray
  Lua.pushHaskellFunction (pandocPackageSearcher luaPkgParams)
  Lua.rawseti (Lua.nthFromTop 2) 1
  Lua.pop 1           -- remove 'package.searchers' from stack
 where
  shiftArray = forM_ [4, 3, 2, 1] $ \i -> do
    Lua.rawgeti (-1) i
    Lua.rawseti (-2) (i + 1)

-- | Load a pandoc module.
pandocPackageSearcher :: LuaPackageParams -> String -> Lua NumResults
pandocPackageSearcher luaPkgParams pkgName =
  case pkgName of
    "pandoc"          -> let datadir = luaPkgDataDir luaPkgParams
                         in pushWrappedHsFun (Pandoc.pushModule datadir)
    "pandoc.mediabag" -> let st    = luaPkgCommonState luaPkgParams
                             mbRef = luaPkgMediaBag luaPkgParams
                         in pushWrappedHsFun (MediaBag.pushModule st mbRef)
    "pandoc.utils"    -> let datadirMb = luaPkgDataDir luaPkgParams
                         in pushWrappedHsFun (Utils.pushModule datadirMb)
    _ -> searchPureLuaLoader
 where
  pushWrappedHsFun f = do
    Lua.pushHaskellFunction f
    return 1
  searchPureLuaLoader = do
    let filename = pkgName ++ ".lua"
    modScript <- liftIO (dataDirScript (luaPkgDataDir luaPkgParams) filename)
    case modScript of
      Just script -> pushWrappedHsFun (loadStringAsPackage pkgName script)
      Nothing -> do
        Lua.push ("no file '" ++ filename ++ "' in pandoc's datadir")
        return 1

loadStringAsPackage :: String -> ByteString -> Lua NumResults
loadStringAsPackage pkgName script = do
  status <- Lua.dostring script
  if status == Lua.OK
    then return (1 :: NumResults)
    else do
      msg <- Lua.popValue
      Lua.raiseError ("Error while loading `" <> pkgName <> "`.\n" <> msg)

-- | Get the ByteString representation of the pandoc module.
dataDirScript :: Maybe FilePath -> FilePath -> IO (Maybe ByteString)
dataDirScript datadir moduleFile = do
  res <- runIO $ setUserDataDir datadir >> readDataFile moduleFile
  return $ case res of
    Left _ -> Nothing
    Right s -> Just s
