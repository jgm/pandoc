{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{- |
   Module      : Text.Pandoc.Lua.Packages
   Copyright   : Copyright © 2017-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc module for Lua.
-}
module Text.Pandoc.Lua.Packages
  ( installPandocPackageSearcher
  ) where

import Control.Monad (forM_)
import HsLua (NumResults)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.PandocLua (PandocLua, liftPandocLua, loadDefaultModule)

import qualified HsLua as Lua
import qualified HsLua.Module.Path as Path
import qualified HsLua.Module.Text as Text
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
  Lua.pushHaskellFunction $ Lua.toHaskellFunction pandocPackageSearcher
  Lua.rawseti (Lua.nth 2) 1
  Lua.pop 1           -- remove 'package.searchers' from stack
 where
  shiftArray = forM_ [4, 3, 2, 1] $ \i -> do
    Lua.rawgeti (-1) i
    Lua.rawseti (-2) (i + 1)

-- | Load a pandoc module.
pandocPackageSearcher :: String -> PandocLua NumResults
pandocPackageSearcher pkgName =
  case pkgName of
    "pandoc"          -> pushWrappedHsFun $ Lua.toHaskellFunction @PandocError Pandoc.pushModule
    "pandoc.mediabag" -> pushWrappedHsFun $ Lua.toHaskellFunction @PandocError MediaBag.pushModule
    "pandoc.path"     -> pushWrappedHsFun
      (Lua.NumResults 1 <$ Lua.pushModule @PandocError Path.documentedModule)
    "pandoc.system"   -> pushWrappedHsFun $ Lua.toHaskellFunction System.pushModule
    "pandoc.types"    -> pushWrappedHsFun $ Lua.toHaskellFunction @PandocError Types.pushModule
    "pandoc.utils"    -> pushWrappedHsFun $ Lua.toHaskellFunction @PandocError Utils.pushModule
    "text"            -> pushWrappedHsFun
      (Lua.NumResults 1 <$ Lua.pushModule @PandocError Text.documentedModule)
    "pandoc.List"     -> pushWrappedHsFun $ Lua.toHaskellFunction @PandocError (loadDefaultModule pkgName)
    _                 -> reportPandocSearcherFailure
 where
  pushWrappedHsFun f = liftPandocLua $ do
    Lua.pushHaskellFunction f
    return 1
  reportPandocSearcherFailure = liftPandocLua $ do
    Lua.push ("\n\t" <> pkgName <> "is not one of pandoc's default packages")
    return (1 :: NumResults)
