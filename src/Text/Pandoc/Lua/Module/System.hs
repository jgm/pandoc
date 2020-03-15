{- |
   Module      : Text.Pandoc.Lua.Module.System
   Copyright   : © 2019-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc's system Lua module.
-}
module Text.Pandoc.Lua.Module.System
  ( pushModule
  ) where

import Foreign.Lua (Lua, NumResults)
import Foreign.Lua.Module.System (arch, env, getwd, os,
                                  with_env, with_tmpdir, with_wd)
import Text.Pandoc.Lua.Util (addFunction, addField)

import qualified Foreign.Lua as Lua

-- | Push the pandoc.system module on the Lua stack.
pushModule :: Lua NumResults
pushModule = do
  Lua.newtable
  addField "arch" arch
  addField "os" os
  addFunction "environment" env
  addFunction "get_working_directory" getwd
  addFunction "with_environment" with_env
  addFunction "with_temporary_directory" with_tmpdir
  addFunction "with_working_directory" with_wd
  return 1
