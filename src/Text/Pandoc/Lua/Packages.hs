{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.Lua.Packages
   Copyright   : Copyright © 2017-2020 Albert Krewinkel
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
import Data.ByteString (ByteString)
import Foreign.Lua (Lua, NumResults, liftIO)
import Text.Pandoc.Class.PandocIO (runIO)
import Text.Pandoc.Class.PandocMonad (readDataFile, setUserDataDir)
import Text.Pandoc.Data (initializeDataFiles)
import qualified Foreign.Lua as Lua
import Text.Pandoc.Lua.Module.Pandoc as Pandoc
import Text.Pandoc.Lua.Module.MediaBag as MediaBag
import Text.Pandoc.Lua.Module.System as System
import Text.Pandoc.Lua.Module.Types as Types
import Text.Pandoc.Lua.Module.Utils as Utils

-- | Parameters used to create lua packages/modules.
data LuaPackageParams = LuaPackageParams
  { luaPkgDataDir :: Maybe FilePath
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
pandocPackageSearcher pkgParams pkgName =
  case pkgName of
    "pandoc"          -> let datadir = luaPkgDataDir pkgParams
                         in pushWrappedHsFun (Pandoc.pushModule datadir)
    "pandoc.mediabag" -> pushWrappedHsFun MediaBag.pushModule
    "pandoc.system"   -> pushWrappedHsFun System.pushModule
    "pandoc.types"    -> pushWrappedHsFun Types.pushModule
    "pandoc.utils"    -> let datadir = luaPkgDataDir pkgParams
                         in pushWrappedHsFun (Utils.pushModule datadir)
    _ -> searchPureLuaLoader
 where
  pushWrappedHsFun f = do
    Lua.pushHaskellFunction f
    return 1
  searchPureLuaLoader = do
    let filename = pkgName ++ ".lua"
    modScript <- liftIO (dataDirScript (luaPkgDataDir pkgParams) filename)
    case modScript of
      Just script -> pushWrappedHsFun (loadStringAsPackage pkgName script)
      Nothing -> do
        Lua.push ("\n\tno file '" ++ filename ++ "' in pandoc's datadir")
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
  res <- runIO $ do
    initializeDataFiles
    setUserDataDir datadir
    readDataFile moduleFile
  return $ case res of
    Left _ -> Nothing
    Right s -> Just s
