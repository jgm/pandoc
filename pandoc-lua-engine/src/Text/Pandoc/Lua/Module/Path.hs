{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{- |
   Module      : Text.Pandoc.Lua.Module.Path
   Copyright   : © 2019-2024 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>
   Stability   : alpha

Pandoc's system Lua module.
-}
module Text.Pandoc.Lua.Module.Path
  ( documentedModule
  ) where

import Data.Version (makeVersion)
import HsLua
import qualified HsLua.Module.Path   as MPath
import qualified HsLua.Module.System as MSystem

-- | Push the pandoc.system module on the Lua stack.
documentedModule :: forall e. LuaError e => Module e
documentedModule = defmodule "pandoc.path"
  `withDescription` moduleDescription @e MPath.documentedModule
  `withFields`
      [ MPath.separator
      , MPath.search_path_separator
      ]
  `withFunctions`
      [ MPath.directory              `since` v[2,12]
      , MSystem.exists               `since` v[3,7,1]
      , MPath.filename               `since` v[2,12]
      , MPath.is_absolute            `since` v[2,12]
      , MPath.is_relative            `since` v[2,12]
      , MPath.join                   `since` v[2,12]
      , MPath.make_relative          `since` v[2,12]
      , MPath.normalize              `since` v[2,12]
      , MPath.split                  `since` v[2,12]
      , MPath.split_extension        `since` v[2,12]
      , MPath.split_search_path      `since` v[2,12]
      , MPath.treat_strings_as_paths `since` v[2,12]
      ]
 where
  v = makeVersion
