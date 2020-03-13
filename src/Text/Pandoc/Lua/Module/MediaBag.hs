{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Lua.Module.MediaBag
   Copyright   : Copyright © 2017-2020 Albert Krewinkel
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
import Foreign.Lua (Lua, NumResults, Optional, liftIO)
import Text.Pandoc.Class (CommonState (..), fetchItem, putCommonState,
                          runIOorExplode, setMediaBag)
import Text.Pandoc.Lua.Marshaling ()
import Text.Pandoc.Lua.Marshaling.MediaBag (pushIterator)
import Text.Pandoc.Lua.Util (addFunction)
import Text.Pandoc.MIME (MimeType)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Foreign.Lua as Lua
import qualified Text.Pandoc.MediaBag as MB

--
-- MediaBag submodule
--
pushModule :: Lua NumResults
pushModule = do
  Lua.newtable
  addFunction "delete" delete
  addFunction "empty" empty
  addFunction "insert" insertMediaFn
  addFunction "items" items
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

-- | Delete a single item from the media bag.
delete :: FilePath -> Lua NumResults
delete fp = 0 <$ modifyCommonState
  (\st -> st { stMediaBag = MB.deleteMedia fp (stMediaBag st) })

-- | Delete all items from the media bag.
empty :: Lua NumResults
empty = 0 <$ modifyCommonState (\st -> st { stMediaBag = mempty })

-- | Insert a new item into the media bag.
insertMediaFn :: FilePath
              -> Optional MimeType
              -> BL.ByteString
              -> Lua NumResults
insertMediaFn fp optionalMime contents = do
  modifyCommonState $ \st ->
    let mb = MB.insertMedia fp (Lua.fromOptional optionalMime) contents
                               (stMediaBag st)
    in st { stMediaBag = mb }
  return 0

-- | Returns iterator values to be used with a Lua @for@ loop.
items :: Lua NumResults
items = stMediaBag <$> getCommonState >>= pushIterator

lookupMediaFn :: FilePath
              -> Lua NumResults
lookupMediaFn fp = do
  res <- MB.lookupMedia fp . stMediaBag <$> getCommonState
  case res of
    Nothing -> 1 <$ Lua.pushnil
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
    Lua.push ("path" :: T.Text) *> Lua.push fp *> Lua.rawset (-3)
    Lua.push ("type" :: T.Text) *> Lua.push mimeType *> Lua.rawset (-3)
    Lua.push ("length" :: T.Text) *> Lua.push contentLength *> Lua.rawset (-3)
    Lua.rawseti (-2) idx

fetch :: T.Text
      -> Lua NumResults
fetch src = do
  commonState <- getCommonState
  let mediaBag = stMediaBag commonState
  (bs, mimeType) <- liftIO . runIOorExplode $ do
    putCommonState commonState
    setMediaBag mediaBag
    fetchItem src
  Lua.push $ maybe "" T.unpack mimeType
  Lua.push bs
  return 2 -- returns 2 values: contents, mimetype
