{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Text.Pandoc.Lua.Module.JSON
Copyright   : © 2022-2024 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>

Lua module to work with JSON.
-}
module Text.Pandoc.Lua.Module.JSON (
  -- * Module
    documentedModule

  -- ** Functions
  , decode
  , encode
  )
where

import Prelude hiding (null)
import Data.Maybe (fromMaybe)
import Data.Monoid (Alt (..))
import Data.Version (makeVersion)
import HsLua.Aeson
import HsLua.Core
import HsLua.Marshalling
import HsLua.Packaging
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.PandocLua ()
import Text.Pandoc.Lua.Marshal.AST

import qualified Data.Aeson as Aeson
import qualified Data.Text as T

-- | The @aeson@ module specification.
documentedModule :: Module PandocError
documentedModule = defmodule "pandoc.json"
  `withDescription`
    "JSON module to work with JSON; based on the Aeson Haskell package."
  `withFields` fields
  `withFunctions` functions

--
-- Fields
--

-- | Exported fields.
fields :: LuaError e => [Field e]
fields =
  [ null
  ]

-- | The value used to represent the JSON @null@.
null :: LuaError e => Field e
null = deffield "null"
  `withType` "light userdata"
  `withDescription` "Value used to represent the `null` JSON value."
  `withValue` pushValue Aeson.Null

--
-- Functions
--

functions :: [DocumentedFunction PandocError]
functions =
  [ decode `since` makeVersion [3, 1, 1]
  , encode `since` makeVersion [3, 1, 1]
  ]

-- | Decode a JSON string into a Lua object.
decode :: DocumentedFunction PandocError
decode = defun "decode"
  ### (\str usePandocTypes ->
         fromMaybe pushnil . getAlt . mconcat . map Alt $
         (if usePandocTypes == Just False
          then []
          else [ pushInline  <$> Aeson.decode str
               , pushBlock   <$> Aeson.decode str
               , pushPandoc  <$> Aeson.decode str
               , pushInlines <$> Aeson.decode str
               , pushBlocks  <$> Aeson.decode str
               ])
         ++ [pushValue <$> Aeson.decode str])
  <#> parameter peekLazyByteString "string" "str" "JSON string"
  <#> opt (parameter peekBool "boolean" "pandoc_types"
           "whether to use pandoc types when possible.")
  =#> functionResult pure "any" "decoded object"
  #? T.unlines
     [ "Creates a Lua object from a JSON string. If the input can be decoded"
     , "as representing an [[Inline]], [[Block]], [[Pandoc]], [[Inlines]],"
     , "or [[Blocks]] element the function will return an object of the"
     , "appropriate type. Otherwise, if the input does not represent any"
     , "of the AST types, the default decoding is applied: Objects and"
     , "arrays are represented as tables, the JSON `null` value becomes"
     , "[null](#pandoc.json.null), and JSON booleans, strings, and numbers"
     , "are converted using the Lua types of the same name."
     , ""
     , "The special handling of AST elements can be disabled by setting"
     , "`pandoc_types` to `false`."
     ]

-- | Encode a Lua object as JSON.
encode :: LuaError e => DocumentedFunction e
encode = defun "encode"
  ### liftPure Aeson.encode
  <#> parameter peekValue "any" "object" "object to convert"
  =#> functionResult pushLazyByteString "string"
        "JSON encoding of the given `object`"
  #? T.unlines
     ["Encodes a Lua object as JSON string."
     , ""
     , "If the object has a metamethod with name `__tojson`, then the"
     , "result is that of a call to that method with `object` passed as"
     , "the sole argument. The result of that call is expected to be a"
     , "valid JSON string, but this is not checked."
     ]
