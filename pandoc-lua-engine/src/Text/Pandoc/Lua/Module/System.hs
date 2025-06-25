{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{- |
   Module      : Text.Pandoc.Lua.Module.System
   Copyright   : © 2019-2024 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>
   Stability   : alpha

Pandoc's system Lua module.
-}
module Text.Pandoc.Lua.Module.System
  ( documentedModule
  ) where

import Data.Version (makeVersion)
import HsLua
import HsLua.Module.System
  ( arch, cmd, cp, cputime, env, getwd, ls, mkdir, os, read_file
  , rename, rm, rmdir, times, with_env, with_tmpdir, with_wd
  , write_file, xdg
  )
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
      , setName "command" cmd                          `since` v[3,7,1]
      , setName "copy" cp                              `since` v[3,7,1]
      , setName "environment" env                      `since` v[2,7,3]
      , setName "get_working_directory" getwd          `since` v[2,8]
      , setName "list_directory" ls                    `since` v[2,19]
      , setName "make_directory" mkdir                 `since` v[2,19]
      , read_file                                      `since` v[3,7,1]
      , rename                                         `since` v[3,7,1]
      , setName "remove" rm                            `since` v[3,7,1]
      , setName "remove_directory" rmdir               `since` v[2,19]
      , times                                          `since` v[3,7,1]
      , setName "with_environment" with_env            `since` v[2,7,3]
      , setName "with_temporary_directory" with_tmpdir `since` v[2,8]
      , setName "with_working_directory" with_wd       `since` v[2,7,3]
      , write_file                                     `since` v[3,7,1]
      , xdg                                            `since` v[3,7,1]
      ]
  , moduleOperations = []
  , moduleTypeInitializers = []
  }
 where
  v = makeVersion
