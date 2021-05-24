{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Lua.Module.MediaBag
   Copyright   : Copyright © 2017-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

The lua module @pandoc.mediabag@.
-}
module Text.Pandoc.Lua.Module.MediaBag
  ( pushModule
  ) where

import Prelude hiding (lookup)
import Control.Monad (zipWithM_)
import Foreign.Lua (Lua, NumResults, Optional)
import Text.Pandoc.Class.CommonState (CommonState (..))
import Text.Pandoc.Class.PandocMonad (fetchItem, getMediaBag, modifyCommonState,
                                      setMediaBag)
import Text.Pandoc.Lua.Marshaling ()
import Text.Pandoc.Lua.Marshaling.MediaBag (pushIterator)
import Text.Pandoc.Lua.PandocLua (PandocLua (..), liftPandocLua, addFunction)
import Text.Pandoc.MIME (MimeType)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Foreign.Lua as Lua
import qualified Text.Pandoc.MediaBag as MB

--
-- MediaBag submodule
--
pushModule :: PandocLua NumResults
pushModule = do
  liftPandocLua Lua.newtable
  addFunction "delete" delete
  addFunction "empty" empty
  addFunction "insert" insert
  addFunction "items" items
  addFunction "lookup" lookup
  addFunction "list" list
  addFunction "fetch" fetch
  return 1

-- | Delete a single item from the media bag.
delete :: FilePath -> PandocLua NumResults
delete fp = 0 <$ modifyCommonState
  (\st -> st { stMediaBag = MB.deleteMedia fp (stMediaBag st) })

-- | Delete all items from the media bag.
empty :: PandocLua NumResults
empty = 0 <$ modifyCommonState (\st -> st { stMediaBag = mempty })

-- | Insert a new item into the media bag.
insert :: FilePath
       -> Optional MimeType
       -> BL.ByteString
       -> PandocLua NumResults
insert fp optionalMime contents = do
  mb <- getMediaBag
  setMediaBag $ MB.insertMedia fp (Lua.fromOptional optionalMime) contents mb
  return (Lua.NumResults 0)

-- | Returns iterator values to be used with a Lua @for@ loop.
items :: PandocLua NumResults
items = getMediaBag >>= liftPandocLua . pushIterator

lookup :: FilePath
       -> PandocLua NumResults
lookup fp = do
  res <- MB.lookupMedia fp <$> getMediaBag
  liftPandocLua $ case res of
    Nothing -> 1 <$ Lua.pushnil
    Just item -> do
      Lua.push $ MB.mediaMimeType item
      Lua.push $ MB.mediaContents item
      return 2

list :: PandocLua NumResults
list = do
  dirContents <- MB.mediaDirectory <$> getMediaBag
  liftPandocLua $ do
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
      -> PandocLua NumResults
fetch src = do
  (bs, mimeType) <- fetchItem src
  liftPandocLua . Lua.push $ maybe "" T.unpack mimeType
  liftPandocLua $ Lua.push bs
  return 2 -- returns 2 values: contents, mimetype
