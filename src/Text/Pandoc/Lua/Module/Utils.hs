{-
Copyright © 2017 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}
{- |
   Module      : Text.Pandoc.Lua.Module.Utils
   Copyright   : Copyright © 2017 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Utility module for lua, exposing internal helper functions.
-}
module Text.Pandoc.Lua.Module.Utils
  ( pushModule
  ) where

import Data.Digest.Pure.SHA (sha1, showDigest)
import Foreign.Lua (Lua, NumResults)
import Text.Pandoc.Lua.StackInstances ()
import Text.Pandoc.Lua.Util (addFunction)

import qualified Data.ByteString.Lazy as BSL
import qualified Foreign.Lua as Lua

-- | Push the "pandoc.utils" module to the lua stack.
pushModule :: Lua NumResults
pushModule = do
  Lua.newtable
  addFunction "sha1" sha1HashFn
  return 1

-- | Calculate the hash of the given contents.
sha1HashFn :: BSL.ByteString
           -> Lua String
sha1HashFn = return . showDigest . sha1
