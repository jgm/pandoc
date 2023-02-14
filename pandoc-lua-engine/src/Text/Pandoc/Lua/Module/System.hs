{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.Lua.Module.System
   Copyright   : © 2019-2023 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc's system Lua module.
-}
module Text.Pandoc.Lua.Module.System
  ( documentedModule
  ) where

import Data.Version (makeVersion)
import HsLua
import HsLua.Module.System
  ( arch, cputime, env, getwd, ls, mkdir, os, rmdir
  , with_env, with_tmpdir, with_wd)

-- | Push the pandoc.system module on the Lua stack.
documentedModule :: LuaError e => Module e
documentedModule = Module
  { moduleName = "pandoc.system"
  , moduleDescription = "system functions"
  , moduleFields =
      [ arch
      , os
      ]
  , moduleFunctions =
      [ cputime `since` makeVersion [3, 1, 1]
      , setName "environment" env
      , setName "get_working_directory" getwd
      , setName "list_directory" ls
      , setName "make_directory" mkdir
      , setName "remove_directory" rmdir
      , setName "with_environment" with_env
      , setName "with_temporary_directory" with_tmpdir
      , setName "with_working_directory" with_wd
      ]
  , moduleOperations = []
  }
