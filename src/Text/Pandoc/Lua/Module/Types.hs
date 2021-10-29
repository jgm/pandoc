{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Lua.Module.Types
   Copyright   : © 2019-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc data type constructors.
-}
module Text.Pandoc.Lua.Module.Types
  ( documentedModule
  ) where

import HsLua ( LuaE, NumResults, Peeker, Pusher, Module (..), Field (..)
             , defun, functionResult, parameter, (###), (<#>), (=#>))
import HsLua.Module.Version (peekVersionFuzzy, pushVersion)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.ErrorConversion ()
import Text.Pandoc.Lua.Marshaling.AST

import qualified HsLua as Lua

-- | Push the pandoc.types module on the Lua stack.
documentedModule :: Module PandocError
documentedModule = Module
  { moduleName = "pandoc.types"
  , moduleDescription =
      "Constructors for types that are not part of the pandoc AST."
  , moduleFields =
    [ Field
      { fieldName = "clone"
      , fieldDescription = "DEPRECATED! Helper functions for element cloning."
      , fieldPushValue = do
          Lua.newtable
          addFunction "Meta" $ cloneWith peekMeta pushMeta
          addFunction "MetaValue" $ cloneWith peekMetaValue pushMetaValue
      }
    ]
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
 where addFunction name fn = do
         Lua.pushName name
         Lua.pushHaskellFunction fn
         Lua.rawset (Lua.nth 3)

cloneWith :: Peeker PandocError a
          -> Pusher PandocError a
          -> LuaE PandocError NumResults
cloneWith peeker pusher = do
  x <- Lua.forcePeek $ peeker (Lua.nthBottom 1)
  pusher x
  return (Lua.NumResults 1)
