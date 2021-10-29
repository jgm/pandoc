{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Lua.Module.MediaBag
   Copyright   : Copyright © 2017-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above
   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

The Lua module @pandoc.mediabag@.
-}
module Text.Pandoc.Lua.Module.MediaBag
  ( documentedModule
  ) where

import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)
import HsLua ( LuaE, DocumentedFunction, Module (..)
             , (<#>), (###), (=#>), (=?>), defun, functionResult
             , optionalParameter , parameter)
import Text.Pandoc.Class.CommonState (CommonState (..))
import Text.Pandoc.Class.PandocMonad (fetchItem, getMediaBag, modifyCommonState,
                                      setMediaBag)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.Marshaling ()
import Text.Pandoc.Lua.Marshaling.List (pushPandocList)
import Text.Pandoc.Lua.PandocLua (unPandocLua)
import Text.Pandoc.MIME (MimeType)

import qualified Data.ByteString.Lazy as BL
import qualified HsLua as Lua
import qualified Text.Pandoc.MediaBag as MB

--
-- MediaBag submodule
--
documentedModule :: Module PandocError
documentedModule = Module
  { moduleName = "pandoc.mediabag"
  , moduleDescription = "mediabag access"
  , moduleFields = []
  , moduleFunctions =
      [ delete
      , empty
      , fetch
      , insert
      , items
      , list
      , lookup
      ]
  , moduleOperations = []
  }

-- | Delete a single item from the media bag.
delete :: DocumentedFunction PandocError
delete = defun "delete"
  ### (\fp -> unPandocLua $ modifyCommonState
              (\st -> st { stMediaBag = MB.deleteMedia fp (stMediaBag st) }))
  <#> parameter Lua.peekString "string" "filepath" "filename of item to delete"
  =#> []


-- | Delete all items from the media bag.
empty :: DocumentedFunction PandocError
empty = defun "empty"
  ### unPandocLua (modifyCommonState (\st -> st { stMediaBag = mempty }))
  =#> []

-- | Insert a new item into the media bag.
insert :: DocumentedFunction PandocError
insert = defun "insert"
  ### (\fp mmime contents -> unPandocLua $ do
          mb <- getMediaBag
          setMediaBag $ MB.insertMedia fp mmime contents mb
          return (Lua.NumResults 0))
  <#> parameter Lua.peekString "string" "filepath" "item file path"
  <#> optionalParameter Lua.peekText "string" "mimetype" "the item's MIME type"
  <#> parameter Lua.peekLazyByteString "string" "contents" "binary contents"
  =?> "Nothing"

-- | Returns iterator values to be used with a Lua @for@ loop.
items :: DocumentedFunction PandocError
items = defun "items"
  ### (do
          mb <-unPandocLua getMediaBag
          let pushItem (fp, mimetype, contents) = do
                Lua.pushString fp
                Lua.pushText mimetype
                Lua.pushByteString $ BL.toStrict contents
                return (Lua.NumResults 3)
          Lua.pushIterator pushItem (MB.mediaItems mb))
  =?> "Iterator triple"

-- | Function to lookup a value in the mediabag.
lookup :: DocumentedFunction PandocError
lookup = defun "lookup"
  ### (\fp -> unPandocLua (MB.lookupMedia fp <$> getMediaBag) >>= \case
          Nothing   -> 1 <$ Lua.pushnil
          Just item -> 2 <$ do
            Lua.pushText $ MB.mediaMimeType item
            Lua.pushLazyByteString $ MB.mediaContents item)
  <#> parameter Lua.peekString "string" "filepath" "path of item to lookup"
  =?> "MIME type and contents"

-- | Function listing all mediabag items.
list :: DocumentedFunction PandocError
list = defun "list"
  ### (unPandocLua (MB.mediaDirectory <$> getMediaBag))
  =#> functionResult (pushPandocList pushEntry) "table" "list of entry triples"
 where
  pushEntry :: (FilePath, MimeType, Int) -> LuaE PandocError ()
  pushEntry (fp, mimeType, contentLength) = do
    Lua.newtable
    Lua.pushName "path"   *> Lua.pushString fp              *> Lua.rawset (-3)
    Lua.pushName "type"   *> Lua.pushText mimeType          *> Lua.rawset (-3)
    Lua.pushName "length" *> Lua.pushIntegral contentLength *> Lua.rawset (-3)

-- | Lua function to retrieve a new item.
fetch :: DocumentedFunction PandocError
fetch = defun "fetch"
  ### (\src -> do
          (bs, mimeType) <- unPandocLua $ fetchItem src
          Lua.pushText $ fromMaybe "" mimeType
          Lua.pushByteString bs
          return 2)
  <#> parameter Lua.peekText "string" "src" "URI to fetch"
  =?> "Returns two string values: the fetched contents and the mimetype."
