{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Text.Pandoc.Lua.Module.JSON
Copyright   : © 2022-2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert@hslua.org>

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
import Data.Version (Version, makeVersion)
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
documentedModule = Module
  { moduleName = "pandoc.json"
  , moduleDescription = "JSON module based on the Aeson Haskell package."
  , moduleFields = fields
  , moduleFunctions = functions
  , moduleOperations = []
  }

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
null = Field
  { fieldName = "null"
  , fieldDescription = "Value used to represent the `null` JSON value."
  , fieldPushValue = pushValue Aeson.Null
  }

--
-- Functions
--

functions :: [DocumentedFunction PandocError]
functions =
  [ decode
  , encode
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
     [ "Creates a Lua object from a JSON string. The function returns an"
     , "[Inline], [Block], [Pandoc], [Inlines], or [Blocks] element if the"
     , "input can be decoded into represent any of those types. Otherwise"
     , "the default decoding is applied, using tables, booleans, numbers,"
     , "and [null](#pandoc.json.null) to represent the JSON value."
     , ""
     , "The special handling of AST elements can be disabled by setting"
     , "`pandoc_types` to `false`."
     ]
  `since` initialVersion

-- | Encode a Lua object as JSON.
encode :: LuaError e => DocumentedFunction e
encode = defun "encode"
  ### (\idx -> do
          -- ensure that there are no other objects on the stack.
          settop (nthBottom 1)
          getmetafield idx "__tojson" >>= \case
            TypeNil -> do
              -- No metamethod, use default encoder.
              value <- forcePeek $ peekValue idx
              pushLazyByteString $ Aeson.encode value
            _ -> do
              -- Try to use the field value as function
              insert (nth 2)
              call 1 1
              ltype top >>= \case
                TypeString -> pure ()
                _ -> failLua
                     "Call to __tojson metamethod did not yield a string")
  <#> parameter pure "any" "object" "object to convert"
  =#> functionResult pure "string" "JSON encoding of `object`"
  #? T.unlines
     ["Encodes a Lua object as JSON string."
     , ""
     , "If the object has a metamethod with name `__tojson`, then the"
     , "result is that of a call to that method with `object` passed as"
     , "the sole argument. The result of that call is expected to be a"
     , "valid JSON string, but this not checked."
     ]
  `since` initialVersion

-- | First published version of this library.
initialVersion :: Version
initialVersion = makeVersion [1,0,0]
