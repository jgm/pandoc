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
{-# LANGUAGE NoImplicitPrelude #-}
{- |
   Module      : Text.Pandoc.Lua
   Copyright   : Copyright © 2017-2018 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Functions to initialize the Lua interpreter.
-}
module Text.Pandoc.Lua.Init
  ( LuaException (..)
  , LuaPackageParams (..)
  , runLua
  , luaPackageParams
  ) where

import Prelude
import Control.Monad.Trans (MonadIO (..))
import Data.Data (Data, dataTypeConstrs, dataTypeOf, showConstr)
import Data.IORef (newIORef, readIORef)
import Foreign.Lua (Lua)
import GHC.IO.Encoding (getForeignEncoding, setForeignEncoding, utf8)
import Text.Pandoc.Class (PandocIO, getCommonState, getUserDataDir,
                          getMediaBag, setMediaBag)
import Text.Pandoc.Lua.Global (Global (..), setGlobals)
import Text.Pandoc.Lua.Packages (LuaPackageParams (..),
                                 installPandocPackageSearcher)
import Text.Pandoc.Lua.Util (loadScriptFromDataDir)

import qualified Foreign.Lua as Lua
import qualified Foreign.Lua.Module.Text as Lua
import qualified Text.Pandoc.Definition as Pandoc

-- | Lua error message
newtype LuaException = LuaException String deriving (Show)

-- | Run the lua interpreter, using pandoc's default way of environment
-- initialization.
runLua :: Lua a -> PandocIO (Either LuaException a)
runLua luaOp = do
  luaPkgParams <- luaPackageParams
  globals <- defaultGlobals
  enc <- liftIO $ getForeignEncoding <* setForeignEncoding utf8
  res <- liftIO . Lua.runEither $ do
    setGlobals globals
    initLuaState luaPkgParams
    luaOp
  liftIO $ setForeignEncoding enc
  newMediaBag <- liftIO (readIORef (luaPkgMediaBag luaPkgParams))
  setMediaBag newMediaBag
  return $ case res of
    Left (Lua.Exception msg) -> Left (LuaException msg)
    Right x -> Right x

-- | Global variables which should always be set.
defaultGlobals :: PandocIO [Global]
defaultGlobals = do
  commonState <- getCommonState
  return
    [ PANDOC_API_VERSION
    , PANDOC_STATE commonState
    , PANDOC_VERSION
    ]

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

-- | Initialize the lua state with all required values
initLuaState :: LuaPackageParams -> Lua ()
initLuaState luaPkgParams = do
  Lua.openlibs
  Lua.preloadTextModule "text"
  installPandocPackageSearcher luaPkgParams
  loadScriptFromDataDir (luaPkgDataDir luaPkgParams) "init.lua"
  putConstructorsInRegistry

putConstructorsInRegistry :: Lua ()
putConstructorsInRegistry = do
  Lua.getglobal "pandoc"
  constrsToReg $ Pandoc.Pandoc mempty mempty
  constrsToReg $ Pandoc.Str mempty
  constrsToReg $ Pandoc.Para mempty
  constrsToReg $ Pandoc.Meta mempty
  constrsToReg $ Pandoc.MetaList mempty
  constrsToReg $ Pandoc.Citation mempty mempty mempty Pandoc.AuthorInText 0 0
  putInReg "Attr"  -- used for Attr type alias
  putInReg "ListAttributes"  -- used for ListAttributes type alias
  Lua.pop 1
 where
  constrsToReg :: Data a => a -> Lua ()
  constrsToReg = mapM_ (putInReg . showConstr) . dataTypeConstrs . dataTypeOf

  putInReg :: String -> Lua ()
  putInReg name = do
    Lua.push ("pandoc." ++ name) -- name in registry
    Lua.push name -- in pandoc module
    Lua.rawget (Lua.nthFromTop 3)
    Lua.rawset Lua.registryindex
