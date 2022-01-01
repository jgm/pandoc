{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Lua.Module.Types
   Copyright   : © 2019-2022 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc data type constructors.
-}
module Text.Pandoc.Lua.Module.Types
  ( documentedModule
  ) where

import HsLua ( Module (..), (###), (<#>), (=#>)
             , defun, functionResult, parameter)
import HsLua.Module.Version (peekVersionFuzzy, pushVersion)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.ErrorConversion ()

-- | Push the pandoc.types module on the Lua stack.
documentedModule :: Module PandocError
documentedModule = Module
  { moduleName = "pandoc.types"
  , moduleDescription =
      "Constructors for types that are not part of the pandoc AST."
  , moduleFields = []
  , moduleFunctions =
      [ defun "Version"
        ### return
        <#> parameter peekVersionFuzzy "string|integer|{integer,...}|Version"
              "version_specifier"
              (mconcat [ "either a version string like `'2.7.3'`, "
                       , "a single integer like `2`, "
                       , "list of integers like `{2,7,3}`, "
                       , "or a Version object"
                       ])
        =#> functionResult pushVersion "Version" "A new Version object."
      ]
  , moduleOperations = []
  }
