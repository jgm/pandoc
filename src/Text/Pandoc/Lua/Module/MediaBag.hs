{-# LANGUAGE NoImplicitPrelude #-}
{- |
   Module      : Text.Pandoc.Lua.Module.MediaBag
   Copyright   : Copyright © 2017-2019 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

The lua module @pandoc.mediabag@.
-}
module Text.Pandoc.Lua.Module.MediaBag
  ( pushModule
  ) where

import Prelude
import Control.Monad (zipWithM_)
import Data.Maybe (fromMaybe)
import Foreign.Lua (Lua, NumResults, Optional, liftIO)
import Text.Pandoc.Class (CommonState (..), fetchItem, putCommonState,
                          runIOorExplode, setMediaBag)
import Text.Pandoc.Lua.Marshaling ()
import Text.Pandoc.Lua.Util (addFunction)
import Text.Pandoc.MIME (MimeType)

import qualified Data.ByteString.Lazy as BL
import qualified Foreign.Lua as Lua
import qualified Text.Pandoc.MediaBag as MB

--
-- MediaBag submodule
--
pushModule :: Lua NumResults
pushModule = do
  Lua.newtable
  addFunction "insert" insertMediaFn
  addFunction "lookup" lookupMediaFn
  addFunction "list" mediaDirectoryFn
  addFunction "fetch" fetch
  return 1

--
-- Port functions from Text.Pandoc.Class to the Lua monad.
-- TODO: reuse existing functions.

-- Get the current CommonState.
getCommonState :: Lua CommonState
getCommonState = do
  Lua.getglobal "PANDOC_STATE"
  Lua.peek Lua.stackTop

-- Replace MediaBag in CommonState.
setCommonState :: CommonState -> Lua ()
setCommonState st = do
  Lua.push st
  Lua.setglobal "PANDOC_STATE"

modifyCommonState :: (CommonState -> CommonState) -> Lua ()
modifyCommonState f = getCommonState >>= setCommonState . f

insertMediaFn :: FilePath
              -> Optional MimeType
              -> BL.ByteString
              -> Lua NumResults
insertMediaFn fp optionalMime contents = do
  modifyCommonState $ \st ->
    let mb = MB.insertMedia fp (Lua.fromOptional optionalMime) contents
                               (stMediaBag st)
    in st { stMediaBag = mb}
  return 0

lookupMediaFn :: FilePath
              -> Lua NumResults
lookupMediaFn fp = do
  res <- MB.lookupMedia fp . stMediaBag <$> getCommonState
  case res of
    Nothing -> Lua.pushnil *> return 1
    Just (mimeType, contents) -> do
      Lua.push mimeType
      Lua.push contents
      return 2

mediaDirectoryFn :: Lua NumResults
mediaDirectoryFn = do
  dirContents <- MB.mediaDirectory . stMediaBag <$> getCommonState
  Lua.newtable
  zipWithM_ addEntry [1..] dirContents
  return 1
 where
  addEntry :: Lua.Integer -> (FilePath, MimeType, Int) -> Lua ()
  addEntry idx (fp, mimeType, contentLength) = do
    Lua.newtable
    Lua.push "path" *> Lua.push fp *> Lua.rawset (-3)
    Lua.push "type" *> Lua.push mimeType *> Lua.rawset (-3)
    Lua.push "length" *> Lua.push contentLength *> Lua.rawset (-3)
    Lua.rawseti (-2) idx

fetch :: String
      -> Lua NumResults
fetch src = do
  commonState <- getCommonState
  let mediaBag = stMediaBag commonState
  (bs, mimeType) <- liftIO . runIOorExplode $ do
    putCommonState commonState
    setMediaBag mediaBag
    fetchItem src
  Lua.push $ fromMaybe "" mimeType
  Lua.push bs
  return 2 -- returns 2 values: contents, mimetype
