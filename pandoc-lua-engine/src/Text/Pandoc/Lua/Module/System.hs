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
documentedModule = defmodule "pandoc.system"
  `withDescription` moduleDescription @e MSys.documentedModule
  `withFields` [arch, os]
  `withFunctions`
      [         cputime                                `since` v[3,1,1]
      , setName cmd         "command"                  `since` v[3,7,1]
      , setName cp          "copy"                     `since` v[3,7,1]
      , setName env         "environment"              `since` v[2,7,3]
      , setName getwd       "get_working_directory"    `since` v[2,8]
      , setName ls          "list_directory"           `since` v[2,19]
      , setName mkdir       "make_directory"           `since` v[2,19]
      ,         read_file                              `since` v[3,7,1]
      ,         rename                                 `since` v[3,7,1]
      , setName rm          "remove"                   `since` v[3,7,1]
      , setName rmdir       "remove_directory"         `since` v[2,19]
      ,         times                                  `since` v[3,7,1]
      , setName with_env    "with_environment"         `since` v[2,7,3]
      , setName with_tmpdir "with_temporary_directory" `since` v[2,8]
      , setName with_wd     "with_working_directory"   `since` v[2,7,3]
      ,         write_file                             `since` v[3,7,1]
      ,         xdg                                    `since` v[3,7,1]
      ]
 where
  v = makeVersion
