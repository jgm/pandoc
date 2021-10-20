{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.Lua.Module.System
   Copyright   : © 2019-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc's system Lua module.
-}
module Text.Pandoc.Lua.Module.System
  ( pushModule
  ) where

import HsLua hiding (pushModule)
import HsLua.Module.System
  (arch, env, getwd, os, with_env, with_tmpdir, with_wd)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.ErrorConversion ()

import qualified HsLua as Lua

-- | Push the pandoc.system module on the Lua stack.
pushModule :: LuaE PandocError NumResults
pushModule = do
  Lua.pushModule $ Module
    { moduleName = "system"
    , moduleDescription = "system functions"
    , moduleFields =
        [ arch
        , os
        ]
    , moduleFunctions =
        [ setName "environment" env
        , setName "get_working_directory" getwd
        , setName "with_environment" with_env
        , setName "with_temporary_directory" with_tmpdir
        , setName "with_working_directory" with_wd
        ]
    , moduleOperations = []
    }
  return 1
