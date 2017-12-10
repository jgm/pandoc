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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{- |
   Module      : Text.Pandoc.Lua.Packages
   Copyright   : Copyright © 2017 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc module for lua.
-}
module Text.Pandoc.Lua.Packages
  ( LuaPackageParams (..)
  , installPandocPackageSearcher
  ) where

import Control.Monad (forM_)
import Data.ByteString.Char8 (unpack)
import Data.IORef (IORef)
import Foreign.Lua (Lua, NumResults, liftIO)
import Text.Pandoc.Class (CommonState, readDataFile, runIO, setUserDataDir)
import Text.Pandoc.MediaBag (MediaBag)
import Text.Pandoc.Lua.PandocModule (pushPandocModule, pushMediaBagModule)
import Text.Pandoc.Lua.Util (dostring')

import qualified Foreign.Lua as Lua

-- | Parameters used to create lua packages/modules.
data LuaPackageParams = LuaPackageParams
  { luaPkgCommonState :: CommonState
  , luaPkgDataDir :: Maybe FilePath
  , luaPkgMediaBag :: IORef MediaBag
  }

-- | Insert pandoc's package loader as the first loader, making it the default.
installPandocPackageSearcher :: LuaPackageParams -> Lua ()
installPandocPackageSearcher luaPkgParams = do
  luaVersion <- Lua.getglobal "_VERSION" *> Lua.peek (-1)
  if luaVersion == "Lua 5.1"
    then Lua.getglobal' "package.loaders"
    else Lua.getglobal' "package.searchers"
  shiftArray
  Lua.pushHaskellFunction (pandocPackageSearcher luaPkgParams)
  Lua.wrapHaskellFunction
  Lua.rawseti (-2) 1
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
                         in pushWrappedHsFun (pushPandocModule datadir)
    "pandoc.mediabag" -> let st    = luaPkgCommonState luaPkgParams
                             mbRef = luaPkgMediaBag luaPkgParams
                         in pushWrappedHsFun (pushMediaBagModule st mbRef)
    _ -> searchPureLuaLoader
 where
  pushWrappedHsFun f = do
    Lua.pushHaskellFunction f
    Lua.wrapHaskellFunction
    return 1
  searchPureLuaLoader = do
    let filename = pkgName ++ ".lua"
    modScript <- liftIO (dataDirScript (luaPkgDataDir luaPkgParams) filename)
    case modScript of
      Just script -> pushWrappedHsFun (loadStringAsPackage pkgName script)
      Nothing -> do
        Lua.push ("no file '" ++ filename ++ "' in pandoc's datadir")
        return 1

loadStringAsPackage :: String -> String -> Lua NumResults
loadStringAsPackage pkgName script = do
  status <- dostring' script
  if status == Lua.OK
    then return (1 :: NumResults)
    else do
      msg <- Lua.peek (-1) <* Lua.pop 1
      Lua.push ("Error while loading ``" ++ pkgName ++ "`.\n" ++ msg)
      Lua.lerror
      return (2 :: NumResults)

-- | Get the string representation of the pandoc module
dataDirScript :: Maybe FilePath -> FilePath -> IO (Maybe String)
dataDirScript datadir moduleFile = do
  res <- runIO $ setUserDataDir datadir >> readDataFile moduleFile
  return $ case res of
    Left _ -> Nothing
    Right s -> Just (unpack s)

