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

-- | The @aeson@ module specification.
documentedModule :: Module PandocError
documentedModule = Module
  { moduleName = "pandoc.image"
  , moduleDescription = "Basic image querying functions."
  , moduleFields = fields
  , moduleFunctions = functions
  , moduleOperations = []
  , moduleTypeInitializers = []
  }

--
-- Fields
--

-- | Exported fields.
fields :: LuaError e => [Field e]
fields = []

--
-- Functions
--

functions :: [DocumentedFunction PandocError]
functions =
  [ size `since` makeVersion [3, 2, 0]
  , format `since` makeVersion [3, 2, 0]
  ]

-- | Decode a JSON string into a Lua object.
size :: DocumentedFunction PandocError
size = defun "size"
  ### liftPure2
      (\img mwriterOpts -> imageSize (fromMaybe def mwriterOpts) img)
  <#> parameter peekByteString "string" "image" "image data"
  <#> opt (parameter peekWriterOptions "WriterOptions" "writer_options"
            "writer options")
  =#> functionResult (either (failLua . T.unpack) pushImageSize) "string|table"
        "image size object or error message"
  -- #? T.unlines
  --    [ "Creates a Lua object from a JSON string. The function returns an"
  --    , "[[Inline]], [[Block]], [[Pandoc]], [[Inlines]], or [[Blocks]] element"
  --    , "if the input can be decoded into represent any of those types."
  --    , "Otherwise the default decoding is applied, using tables, booleans,"
  --    , "numbers, and [null](#pandoc.json.null) to represent the JSON value."
  --    , ""
  --    , "The special handling of AST elements can be disabled by setting"
  --    , "`pandoc_types` to `false`."
  --    ]

-- | Encode a Lua object as JSON.
format :: LuaError e => DocumentedFunction e
format = defun "format"
  ### liftPure imageType
  <#> parameter peekByteString "string" "image" "binary image data"
  =#> functionResult (maybe pushnil pushImageType) "string|nil"
        "image format, or nil if the format cannot be determined"
  -- #? T.unlines
  --    ["Encodes a Lua object as JSON string."
  --    , ""
  --    , "If the object has a metamethod with name `__tojson`, then the"
  --    , "result is that of a call to that method with `object` passed as"
  --    , "the sole argument. The result of that call is expected to be a"
  --    , "valid JSON string, but this not checked."
  --    ]
