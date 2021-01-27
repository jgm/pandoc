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
import Foreign.Lua (NumResults)
import Text.Pandoc.Lua.PandocLua (PandocLua, liftPandocLua, loadDefaultModule)

import qualified Foreign.Lua as Lua
import qualified Foreign.Lua.Module.Path as Path
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
    "pandoc.path"     -> pushWrappedHsFun Path.pushModule
    "pandoc.system"   -> pushWrappedHsFun System.pushModule
    "pandoc.types"    -> pushWrappedHsFun Types.pushModule
    "pandoc.utils"    -> pushWrappedHsFun Utils.pushModule
    "text"            -> pushWrappedHsFun Text.pushModule
    "pandoc.List"     -> pushWrappedHsFun (loadDefaultModule pkgName)
    _                 -> reportPandocSearcherFailure
 where
  pushWrappedHsFun f = liftPandocLua $ do
    Lua.pushHaskellFunction f
    return 1
  reportPandocSearcherFailure = liftPandocLua $ do
    Lua.push ("\n\t" <> pkgName <> "is not one of pandoc's default packages")
    return (1 :: NumResults)
