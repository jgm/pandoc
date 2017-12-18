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

Functions to initialize the Lua interpreter.
-}
module Text.Pandoc.Lua.Init
  ( LuaException (..)
  , LuaPackageParams (..)
  , runPandocLua
  , initLuaState
  , luaPackageParams
  ) where

import Control.Monad.Trans (MonadIO (..))
import Data.IORef (newIORef, readIORef)
import Foreign.Lua (Lua, LuaException (..))
import GHC.IO.Encoding (getForeignEncoding, setForeignEncoding, utf8)
import Text.Pandoc.Class (PandocIO, getCommonState, getUserDataDir, getMediaBag,
                          setMediaBag)
import Text.Pandoc.Lua.Packages (LuaPackageParams (..),
                                 installPandocPackageSearcher)
import Text.Pandoc.Lua.Util (loadScriptFromDataDir)

import qualified Foreign.Lua as Lua
import qualified Foreign.Lua.Module.Text as Lua

-- | Run the lua interpreter, using pandoc's default way of environment
-- initalization.
runPandocLua :: Lua a -> PandocIO (Either LuaException a)
runPandocLua luaOp = do
  luaPkgParams <- luaPackageParams
  enc <- liftIO $ getForeignEncoding <* setForeignEncoding utf8
  res <- liftIO $ Lua.runLuaEither (initLuaState luaPkgParams *> luaOp)
  liftIO $ setForeignEncoding enc
  newMediaBag <- liftIO (readIORef (luaPkgMediaBag luaPkgParams))
  setMediaBag newMediaBag
  return res

-- | Generate parameters required to setup pandoc's lua environment.
luaPackageParams :: PandocIO LuaPackageParams
luaPackageParams = do
  commonState <- getCommonState
  datadir <- getUserDataDir
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
