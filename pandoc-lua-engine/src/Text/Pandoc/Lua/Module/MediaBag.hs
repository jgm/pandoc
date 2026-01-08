{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Lua.Module.MediaBag
   Copyright   : Copyright © 2017-2024 Albert Krewinkel
   License     : GNU GPL, version 2 or above
   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>

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
import Text.Pandoc.Class ( fetchItem, fillMediaBag, getMediaBag, setMediaBag )
import Text.Pandoc.Class.IO (writeMedia)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.Marshal.Pandoc (peekPandoc, pushPandoc)
import Text.Pandoc.Lua.Marshal.List (pushPandocList)
import Text.Pandoc.Lua.Orphans ()
import Text.Pandoc.Lua.PandocLua (unPandocLua)
import Text.Pandoc.MIME (MimeType)
import Text.Pandoc.SelfContained (makeDataURI)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified HsLua as Lua
import qualified Text.Pandoc.MediaBag as MB

--
-- MediaBag submodule
--
documentedModule :: Module PandocError
documentedModule = Lua.defmodule "pandoc.mediabag"
  `Lua.withDescription` T.unlines
    [ "The `pandoc.mediabag` module allows accessing pandoc's media"
    , "storage. The \"media bag\" is used when pandoc is called with the"
    , "`--extract-media` or (for HTML only) `--embed-resources` option."
    , ""
    , "The module is loaded as part of module `pandoc` and can either"
    , "be accessed via the `pandoc.mediabag` field, or explicitly"
    , "required, e.g.:"
    , ""
    , "    local mb = require 'pandoc.mediabag'"
    ]
  `Lua.withFunctions`
      [ delete  `since` makeVersion [2,7,3]
      , empty   `since` makeVersion [2,7,3]
      , fetch   `since` makeVersion [2,0]
      , fill    `since` makeVersion [2,19]
      , insert  `since` makeVersion [2,0]
      , items   `since` makeVersion [2,7,3]
      , list    `since` makeVersion [2,0]
      , lookup  `since` makeVersion [2,0]
      , make_data_uri `since` makeVersion [3,7,1]
      , write   `since` makeVersion [3,0]
      ]

-- | Delete a single item from the media bag.
delete :: DocumentedFunction PandocError
delete = defun "delete"
  ### (\fp -> unPandocLua $ do
          mb <- getMediaBag
          setMediaBag $ MB.deleteMedia fp mb)
  <#> stringParam "filepath"
      ("Filename of the item to deleted. The media bag will be " <>
       "left unchanged if no entry with the given filename exists.")
  =#> []
  #? "Removes a single entry from the media bag."

-- | Delete all items from the media bag.
empty :: DocumentedFunction PandocError
empty = defun "empty"
  ### unPandocLua (setMediaBag mempty)
  =#> []
  #? "Clear-out the media bag, deleting all items."

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
      "\n" <>
      "Images for which the mediabag already contains an item will\n" <>
      "not be processed again.")

-- | Insert a new item into the media bag.
insert :: DocumentedFunction PandocError
insert = defun "insert"
  ### (\fp mmime contents -> unPandocLua $ do
          mb <- getMediaBag
          setMediaBag $ MB.insertMedia fp mmime contents mb
          return (Lua.NumResults 0))
  <#> stringParam "filepath" "filename and path relative to the output folder."
  <#> opt (textParam "mimetype"
           "the item's MIME type; omit if unknown or unavailable.")
  <#> parameter Lua.peekLazyByteString "string" "contents"
        "the binary contents of the file."
  =#> []
  #? T.unlines
  [ "Adds a new entry to pandoc's media bag. Replaces any existing"
  , "media bag entry the same `filepath`."
  , ""
  , "Usage:"
  , ""
  , "    local fp = 'media/hello.txt'"
  , "    local mt = 'text/plain'"
  , "    local contents = 'Hello, World!'"
  , "    pandoc.mediabag.insert(fp, mt, contents)"
  ]

-- | Returns iterator values to be used with a Lua @for@ loop.
items :: DocumentedFunction PandocError
items = defun "items"
  ### (do
          mb <- unPandocLua getMediaBag
          let pushItem (fp, mimetype, contents) = do
                Lua.pushString fp
                Lua.pushText mimetype
                Lua.pushByteString $ BL.toStrict contents
                return (Lua.NumResults 3)
          Lua.pushIterator pushItem (MB.mediaItems mb))
  =?> T.unlines
  [ "Iterator triple:"
  , ""
  , "-   The iterator function; must be called with the iterator"
  , "    state and the current iterator value."
  , "-   Iterator state -- an opaque value to be passed to the"
  , "    iterator function."
  , "-   Initial iterator value."
  ]
  #? T.unlines
  [ "Returns an iterator triple to be used with Lua's generic `for`"
  , "statement. The iterator returns the filepath, MIME type, and"
  , "content of a media bag item on each invocation. Items are"
  , "processed one-by-one to avoid excessive memory use."
  , ""
  , "This function should be used only when full access to all items,"
  , "including their contents, is required. For all other cases,"
  , "[`list`](#pandoc.mediabag.list) should be preferred."
  , ""
  , "Usage:"
  , ""
  , "    for fp, mt, contents in pandoc.mediabag.items() do"
  , "      -- print(fp, mt, contents)"
  , "    end"
  ]

-- | Function to lookup a value in the mediabag.
lookup :: DocumentedFunction PandocError
lookup = defun "lookup"
  ### (\fp -> unPandocLua (MB.lookupMedia fp <$> getMediaBag))
  <#> stringParam "filepath" "name of the file to look up."
  =#> mconcat
      [ functionResult
          (maybe Lua.pushnil (Lua.pushText . MB.mediaMimeType))
          "string"
          "The entry's MIME type, or nil if the file was not found."
      , functionResult
          (maybe Lua.pushnil (Lua.pushLazyByteString . MB.mediaContents))
          "string"
          "Contents of the file, or nil if the file was not found."
      ]
  #? T.unlines
  [ "Lookup a media item in the media bag, and return its MIME type"
  , "and contents."
  , ""
  , "Usage:"
  , ""
  , "    local filename = 'media/diagram.png'"
  , "    local mt, contents = pandoc.mediabag.lookup(filename)"
  ]

-- | Function listing all mediabag items.
list :: DocumentedFunction PandocError
list = defun "list"
  ### (unPandocLua (MB.mediaDirectory <$> getMediaBag))
  =#> functionResult (pushPandocList pushEntry) "table"
        ("A list of elements summarizing each entry in the media\n" <>
         "bag. The summary item contains the keys `path`, `type`, and\n" <>
         "`length`, giving the filepath, MIME type, and length of\n" <>
         "contents in bytes, respectively.")
  #? T.unlines
  [ "Get a summary of the current media bag contents."
  , ""
  , "Usage:"
  , ""
  , "    -- calculate the size of the media bag."
  , "    local mb_items = pandoc.mediabag.list()"
  , "    local sum = 0"
  , "    for i = 1, #mb_items do"
  , "        sum = sum + mb_items[i].length"
  , "    end"
  , "    print(sum)"
  ]
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
  ### (unPandocLua . fetchItem)
  <#> textParam "source" "path to a resource; either a local file path or URI"
  =#> ( functionResult (Lua.pushText . fromMaybe "" . snd) "string"
        "The entry's MIME type, or `nil` if the file was not found."
        <>
        functionResult (Lua.pushByteString . fst) "string"
        "Contents of the file, or `nil` if the file was not found."
      )
  #? T.unlines
  [ "Fetches the given source from a URL or local file. Returns two"
  , "values: the contents of the file and the MIME type (or an empty"
  , "string)."
  , ""
  , "The function will first try to retrieve `source` from the"
  , "mediabag; if that fails, it will try to download it or read it"
  , "from the local file system while respecting pandoc's \"resource"
  , "path\" setting."
  , ""
  , "Usage:"
  , ""
  , "    local diagram_url = 'https://pandoc.org/diagram.jpg'"
  , "    local mt, contents = pandoc.mediabag.fetch(diagram_url)"
  ]

make_data_uri :: DocumentedFunction PandocError
make_data_uri = defun "make_data_uri"
  ### (\mime raw -> pure $ makeDataURI (mime, raw))
  <#> parameter Lua.peekText "string" "mime_type" "MIME type of the data"
  <#> parameter Lua.peekByteString "string" "raw_data" "data to encode"
  =#> functionResult Lua.pushText "string" "data uri"
  #? T.unlines
  [ "Convert the input data into a data URI as defined by RFC 2397."
  , ""
  , "Example:"
  , ""
  , "    -- Embed an unofficial pandoc logo"
  , "    local pandoc_logo_url = 'https://raw.githubusercontent.com/'"
  , "      .. 'tarleb/pandoc-logo/main/pandoc.svg'"
  , ""
  , "    local datauri = pandoc.mediabag.make_data_uri("
  , "      pandoc.mediabag.fetch(pandoc_logo_url)"
  , "    )"
  , ""
  , "    local image = pandoc.Image('Logo', datauri)"
  ]


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
