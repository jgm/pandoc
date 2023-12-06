{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
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
import qualified HsLua.Module.System as MSys

-- | Push the pandoc.system module on the Lua stack.
documentedModule :: forall e. LuaError e => Module e
documentedModule = Module
  { moduleName = "pandoc.system"
  , moduleDescription = moduleDescription @e MSys.documentedModule
  , moduleFields =
      [ arch
      , os
      ]
  , moduleFunctions =
      [ cputime                                        `since` v[3,1,1]
      , setName "environment" env                      `since` v[2,7,3]
      , setName "get_working_directory" getwd          `since` v[2,8]
      , setName "list_directory" ls                    `since` v[2,19]
      , setName "make_directory" mkdir                 `since` v[2,19]
      , setName "remove_directory" rmdir               `since` v[2,19]
      , setName "with_environment" with_env            `since` v[2,7,3]
      , setName "with_temporary_directory" with_tmpdir `since` v[2,8]
      , setName "with_working_directory" with_wd       `since` v[2,7,3]
      ]
  , moduleOperations = []
  , moduleTypeInitializers = []
  }
 where
  v = makeVersion
