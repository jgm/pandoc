{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Lua.Module.MediaBag
   Copyright   : Copyright © 2017-2023 Albert Krewinkel
   License     : GNU GPL, version 2 or above
   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

The Lua module @pandoc.mediabag@.
-}
module Text.Pandoc.Lua.Module.MediaBag
  ( documentedModule
  ) where

import Prelude hiding (lookup)
import Data.Maybe (fromMaybe)
import Data.Version (makeVersion)
import HsLua ( LuaE, DocumentedFunction, Module (..)
             , (<#>), (###), (=#>), (=?>), (#?), defun, functionResult
             , opt, parameter, since, stringParam, textParam)
import Text.Pandoc.Class ( CommonState (..), fetchItem, fillMediaBag
                         , getMediaBag, modifyCommonState, setMediaBag)
import Text.Pandoc.Class.IO (writeMedia)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.Marshal.Pandoc (peekPandoc, pushPandoc)
import Text.Pandoc.Lua.Marshal.List (pushPandocList)
import Text.Pandoc.Lua.Orphans ()
import Text.Pandoc.Lua.PandocLua (unPandocLua)
import Text.Pandoc.MIME (MimeType)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
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
      , fill
      , insert
      , items
      , list
      , lookup
      , write
      ]
  , moduleOperations = []
  }

-- | Delete a single item from the media bag.
delete :: DocumentedFunction PandocError
delete = defun "delete"
  ### (\fp -> unPandocLua $ modifyCommonState
              (\st -> st { stMediaBag = MB.deleteMedia fp (stMediaBag st) }))
  <#> stringParam "filepath" "filename of item to delete"
  =#> []


-- | Delete all items from the media bag.
empty :: DocumentedFunction PandocError
empty = defun "empty"
  ### unPandocLua (modifyCommonState (\st -> st { stMediaBag = mempty }))
  =#> []

-- | Fill the mediabag with all images in the document that aren't
-- present yet.
fill :: DocumentedFunction PandocError
fill = defun "fill"
  ### unPandocLua . fillMediaBag
  <#> parameter peekPandoc "Pandoc" "doc"
        "document from which to fill the mediabag"
  =#> functionResult pushPandoc "Pandoc" "modified document"
  #? ("Fills the mediabag with the images in the given document.\n" <>
      "An image that cannot be retrieved will be replaced with a Span\n" <>
      "of class \"image\" that contains the image description.\n" <>
      "" <>
      "Images for which the mediabag already contains an item will\n" <>
      "not be processed again.")

-- | Insert a new item into the media bag.
insert :: DocumentedFunction PandocError
insert = defun "insert"
  ### (\fp mmime contents -> unPandocLua $ do
          mb <- getMediaBag
          setMediaBag $ MB.insertMedia fp mmime contents mb
          return (Lua.NumResults 0))
  <#> stringParam "filepath" "item file path"
  <#> opt (textParam "mimetype" "the item's MIME type")
  <#> parameter Lua.peekLazyByteString "string" "contents" "binary contents"
  =#> []

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
  <#> stringParam "filepath" "path of item to lookup"
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
  <#> textParam "src" "URI to fetch"
  =?> "Returns two string values: the fetched contents and the mimetype."

-- | Extract the mediabag or just a single entry.
write :: DocumentedFunction PandocError
write = defun "write"
  ### (\dir mfp -> do
          mb <- unPandocLua getMediaBag
          case mfp of
            Nothing -> unPandocLua $ mapM_ (writeMedia dir) (MB.mediaItems mb)
            Just fp -> do
              case MB.lookupMedia fp mb of
                Nothing   -> Lua.failLua ("Resource not in mediabag: " <> fp)
                Just item -> unPandocLua $ do
                  let triple = ( MB.mediaPath item
                               , MB.mediaMimeType item
                               , MB.mediaContents item
                               )
                  writeMedia dir triple)
  <#> stringParam "dir" "path of the target directory"
  <#> opt (stringParam "fp" "canonical name (relative path) of resource")
  =#> []
  #? T.unlines
     [ "Writes the contents of  mediabag to the given target directory. If"
     , "`fp` is given, then only the resource with the given name will be"
     , "extracted. Omitting that parameter means that the whole mediabag"
     , "gets extracted. An error is thrown if `fp` is given but cannot be"
     , "found in the mediabag."
     ]
  `since` makeVersion [3, 0]
