{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Text.Pandoc.Lua.Module.Image
Copyright   : © 2024 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>

Lua module for basic image operations.
-}
module Text.Pandoc.Lua.Module.Image (
  -- * Module
    documentedModule

  -- ** Functions
  , size
  , format
  )
where

import Prelude hiding (null)
import Data.Default (Default (def))
import Data.Maybe (fromMaybe)
import Data.Version (makeVersion)
import HsLua.Core
import HsLua.Marshalling
import HsLua.Packaging
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.ImageSize (imageType, imageSize)
import Text.Pandoc.Lua.PandocLua ()
import Text.Pandoc.Lua.Marshal.ImageSize (pushImageType, pushImageSize)
import Text.Pandoc.Lua.Marshal.WriterOptions (peekWriterOptions)

import qualified Data.Text as T

-- | The @pandoc.image@ module specification.
documentedModule :: Module PandocError
documentedModule = defmodule "pandoc.image"
  `withDescription` "Basic image querying functions."
  `withFunctions` functions

--
-- Functions
--

functions :: [DocumentedFunction PandocError]
functions =
  [ size `since` makeVersion [3, 1, 13]
  , format `since` makeVersion [3, 1, 13]
  ]

-- | Find the size of an image.
size :: DocumentedFunction PandocError
size = defun "size"
  ### liftPure2 (\img mwriterOpts -> imageSize (fromMaybe def mwriterOpts) img)
  <#> parameter peekByteString "string" "image" "image data"
  <#> opt (parameter peekWriterOptions "WriterOptions|table" "opts"
           "writer options")
  =#> functionResult (either (failLua . T.unpack) pushImageSize) "table"
        "image size information or error message"
  #? T.unlines
     [ "Returns a table containing the size and resolution of an image;"
     , "throws an error if the given string is not an image, or if the size"
     , "of the image cannot be determined."
     , ""
     , "The resulting table has four entries: *width*, *height*, *dpi\\_horz*,"
     , "and *dpi\\_vert*."
     , ""
     , "The `opts` parameter, when given, should be either a WriterOptions"
     , "object such as `PANDOC_WRITER_OPTIONS`, or a table with a `dpi` entry."
     , "It affects the calculation for vector image formats such as SVG."
     ]

-- | Returns the format of an image.
format :: LuaError e => DocumentedFunction e
format = defun "format"
  ### liftPure imageType
  <#> parameter peekByteString "string" "image" "binary image data"
  =#> functionResult (maybe pushnil pushImageType) "string|nil"
        "image format, or nil if the format cannot be determined"
  #? T.unlines
     [ "Returns the format of an image as a lowercase string."
     , ""
     , "Formats recognized by pandoc include *png*, *gif*, *tiff*, *jpeg*,"
     , "*pdf*, *svg*, *eps*, and *emf*."
     ]
