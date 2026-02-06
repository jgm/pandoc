{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Lua.Module.Types
   Copyright   : © 2019-2026 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>
   Stability   : alpha

Pandoc data type constructors.
-}
module Text.Pandoc.Lua.Module.Types
  ( documentedModule
  ) where

import Data.Version (Version, makeVersion)
import HsLua ( DocumentedFunction, Module (..)
             , (###), (<#>), (=#>), (#?), associateType
             , defmodule, defun, functionResult, parameter, since
             , withDescription, withFunctions
             )
import HsLua.Module.Version (peekVersionFuzzy, pushVersion, typeVersion)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.Marshal.Sources (peekSources, pushSources, typeSource)
import Text.Pandoc.Lua.PandocLua ()

-- | Push the pandoc.types module on the Lua stack.
documentedModule :: Module PandocError
documentedModule = defmodule "pandoc.types"
  `withDescription`
      "Constructors for types that are not part of the pandoc AST."
  `withFunctions`
    [ mkVersion `since` v[2,7,3]
    , mkSources `since` v[3,9,1]
    ]
  `associateType` typeSource
  `associateType` typeVersion
 where
  v :: [Int] -> Version
  v = makeVersion

mkVersion :: DocumentedFunction PandocError
mkVersion = defun "Version"
  ### return
  <#> parameter peekVersionFuzzy "string|number|{integer,...}|Version"
        "version_specifier"
        (mconcat [ "A version string like `'2.7.3'`, "
                 , "a Lua number like `2.0`, "
                 , "a list of integers like `{2,7,3}`, "
                 , "or a Version object."
                 ])
  =#> functionResult pushVersion "Version" "New Version object."

mkSources :: DocumentedFunction PandocError
mkSources = defun "Sources"
  ### pure
  <#> parameter peekSources "string|{string,...}|table" "srcs"
        "sources"
  =#> functionResult pushSources "{Source,...}" "new Sources object"
  #?
    "Creates a new Sources element, i.e., a list of [[Source]] items.\n\
    \\n\
    \ Pandoc's text readers expect the input text to be paired\
    \ with information on where the text originated, e.g., a\
    \ file name. This abstraction is provided via the `Sources` type.\n\
    \\n\
    \Pandoc accepts a range of objects wherever a *Sources* list is\
    \ expected:\n\
    \\n\
    \  - a list of [[Source]] items;\n\
    \  - a simple string, which becomes an unnamed source;\n\
    \  - a list of table objects, where each table contains the fields\n\
    \    `name` (the filepath) and `text` (the file contents)\n\
    \\n\
    \A Sources list can be converted to a string via the default `tostring`\
    \ Lua function. This will concatenate all source items."
