{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.Lua.Packages
   Copyright   : Copyright © 2017-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc module for Lua.
-}
module Text.Pandoc.Lua.Packages
  ( installPandocPackageSearcher
  ) where

import Control.Monad.Catch (try)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Foreign.Lua (Lua, NumResults)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Class.PandocMonad (readDataFile)
import Text.Pandoc.Lua.PandocLua (PandocLua, liftPandocLua)

import qualified Foreign.Lua as Lua
import qualified Foreign.Lua.Module.Text as Text
import qualified Text.Pandoc.Lua.Module.Pandoc as Pandoc
import qualified Text.Pandoc.Lua.Module.MediaBag as MediaBag
import qualified Text.Pandoc.Lua.Module.System as System
import qualified Text.Pandoc.Lua.Module.Types as Types
import qualified Text.Pandoc.Lua.Module.Utils as Utils

-- | Insert pandoc's package loader as the first loader, making it the default.
installPandocPackageSearcher :: PandocLua ()
installPandocPackageSearcher = liftPandocLua $ do
  Lua.getglobal' "package.searchers"
  shiftArray
  Lua.pushHaskellFunction pandocPackageSearcher
  Lua.rawseti (Lua.nthFromTop 2) 1
  Lua.pop 1           -- remove 'package.searchers' from stack
 where
  shiftArray = forM_ [4, 3, 2, 1] $ \i -> do
    Lua.rawgeti (-1) i
    Lua.rawseti (-2) (i + 1)

-- | Load a pandoc module.
pandocPackageSearcher :: String -> PandocLua NumResults
pandocPackageSearcher pkgName =
  case pkgName of
    "pandoc"          -> pushWrappedHsFun Pandoc.pushModule
    "pandoc.mediabag" -> pushWrappedHsFun MediaBag.pushModule
    "pandoc.system"   -> pushWrappedHsFun System.pushModule
    "pandoc.types"    -> pushWrappedHsFun Types.pushModule
    "pandoc.utils"    -> pushWrappedHsFun Utils.pushModule
    "text"            -> pushWrappedHsFun Text.pushModule
    _                 -> searchPureLuaLoader
 where
  pushWrappedHsFun f = liftPandocLua $ do
    Lua.pushHaskellFunction f
    return 1
  searchPureLuaLoader = do
    let filename = pkgName ++ ".lua"
    try (readDataFile filename) >>= \case
      Right script -> pushWrappedHsFun (loadStringAsPackage pkgName script)
      Left (_ :: PandocError) -> liftPandocLua $ do
        Lua.push ("\n\tno file '" ++ filename ++ "' in pandoc's datadir")
        return (1 :: NumResults)

loadStringAsPackage :: String -> ByteString -> Lua NumResults
loadStringAsPackage pkgName script = do
  status <- Lua.dostring script
  if status == Lua.OK
    then return (1 :: NumResults)
    else do
      msg <- Lua.popValue
      Lua.raiseError ("Error while loading `" <> pkgName <> "`.\n" <> msg)
