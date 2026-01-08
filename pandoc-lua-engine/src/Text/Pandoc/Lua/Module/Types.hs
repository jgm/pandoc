{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Lua.Module.Types
   Copyright   : © 2019-2024 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>
   Stability   : alpha

Pandoc data type constructors.
-}
module Text.Pandoc.Lua.Module.Types
  ( documentedModule
  ) where

import Data.Version (makeVersion)
import HsLua ( Module (..), (###), (<#>), (=#>)
             , defmodule, defun, functionResult, parameter, since
             , withDescription, withFunctions
             )
import HsLua.Module.Version (peekVersionFuzzy, pushVersion)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.PandocLua ()

-- | Push the pandoc.types module on the Lua stack.
documentedModule :: Module PandocError
documentedModule = defmodule "pandoc.types"
  `withDescription`
      "Constructors for types that are not part of the pandoc AST."
  `withFunctions`
      [ defun "Version"
        ### return
        <#> parameter peekVersionFuzzy "string|number|{integer,...}|Version"
              "version_specifier"
              (mconcat [ "A version string like `'2.7.3'`, "
                       , "a Lua number like `2.0`, "
                       , "a list of integers like `{2,7,3}`, "
                       , "or a Version object."
                       ])
        =#> functionResult pushVersion "Version" "New Version object."
        `since` makeVersion [2,7,3]
      ]
