{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Text.Pandoc.Lua.Module.Text
Copyright   : © 2023 Albert Krewinkel
License     : MIT
Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>

Lua module to work with UTF-8 strings.
-}
module Text.Pandoc.Lua.Module.Text
  ( documentedModule
  ) where

import Data.Version (makeVersion)
import HsLua
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.PandocLua ()
import Text.Pandoc.Writers.Shared (toSubscript, toSuperscript)

import qualified Data.Text as T
import qualified HsLua.Module.Text as TM

-- | The @aeson@ module specification.
documentedModule :: Module PandocError
documentedModule = defmodule "pandoc.text"
  `withFunctions`
    [ TM.fromencoding `since` v[3,0]
    , TM.len          `since` v[2,0,3]
    , TM.lower        `since` v[2,0,3]
    , TM.reverse      `since` v[2,0,3]
    , TM.sub          `since` v[2,0,3]
    , subscript       `since` v[3,8]
    , superscript     `since` v[3,8]
    , TM.toencoding   `since` v[3,0]
    , TM.upper        `since` v[2,0,3]
    ]
  `withDescription` T.unlines
    [ "UTF-8 aware text manipulation functions, implemented in Haskell."
    , ""
    , "The text module can also be loaded under the name `text`, although"
    , "this is discouraged and deprecated."
    , ""
    , "``` lua"
    , "-- uppercase all regular text in a document:"
    , "function Str (s)"
    , "  s.text = pandoc.text.upper(s.text)"
    , "  return s"
    , "end"
    , "```"
    ]
 where
  v = makeVersion

-- | Convert all chars in a string to Unicode subscript.
subscript :: LuaError e => DocumentedFunction e
subscript = defun "subscript"
  ### pure . traverse toSubscript
  <#> stringParam "input" "string to convert to subscript characters"
  =#> functionResult (maybe pushnil pushString) "string|nil"
      "Subscript version of the input, or `nil` if not all characters\
      \ could be converted."
  #? "Tries to convert the string into a Unicode subscript version of the\
     \ string.  Returns `nil` if not all characters of the input can be\
     \ mapped to a subscript Unicode character.\
     \ Supported characters include numbers, parentheses, and plus/minus."

-- | Convert all chars in a string to Unicode superscript.
superscript :: LuaError e => DocumentedFunction e
superscript = defun "superscript"
  ### pure . traverse toSuperscript
  <#> stringParam "input" "string to convert to superscript characters"
  =#> functionResult (maybe pushnil pushString) "string|nil"
      "Superscript version of the input, or `nil` if not all characters\
      \ could be converted."
  #? "Tries to convert the string into a Unicode superscript version of the\
     \ string.  Returns `nil` if not all characters of the input can be\
     \ mapped to a superscript Unicode character.\
     \ Supported characters include numbers, parentheses, and plus/minus."
