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
import Foreign.Lua.Module.System (arch,
                                  os,
                                  -- * General functions
                                  env,
                                  getwd,
                                  with_env,
                                  with_tmpdir,
                                  with_wd,
                                  -- * Path manipulation functions
                                  take_directory,
                                  take_filename,
                                  take_extensions,
                                  split_directories,
                                  has_extension,
                                  drop_extensions,
                                  join_path,
                                  is_relative,
                                  is_absolute,
                                  normalise)
import Text.Pandoc.Lua.Util (addFunction, addField)

import qualified Foreign.Lua as Lua

-- | Push the pandoc.system module on the Lua stack.
pushModule :: Lua NumResults
pushModule = do
  Lua.newtable

  -- * General functions
  addField "arch" arch
  addField "os" os
  addFunction "environment" env
  addFunction "get_working_directory" getwd
  addFunction "with_environment" with_env
  addFunction "with_temporary_directory" with_tmpdir
  addFunction "with_working_directory" with_wd

  -- * Path manipulation functions
  addFunction "take_directory"    take_directory
  addFunction "take_filename"     take_filename
  addFunction "take_extensions"   take_extensions
  addFunction "split_directories" split_directories
  addFunction "has_extension"     has_extension
  addFunction "drop_extensions"   drop_extensions
  addFunction "join_path"         join_path
  addFunction "is_relative"       is_relative
  addFunction "is_absolute"       is_absolute
  addFunction "normalise"         normalise

  return 1
