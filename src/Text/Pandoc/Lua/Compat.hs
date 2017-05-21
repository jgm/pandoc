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
{-# LANGUAGE CPP #-}
{- |
   Module      : Text.Pandoc.Lua.Compat
   Copyright   : Copyright © 2017 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Compatibility helpers for hslua
-}
module Text.Pandoc.Lua.Compat ( loadstring ) where

import Scripting.Lua ( LuaState )
import qualified Scripting.Lua as Lua

-- | Interpret string as lua code and load into the lua environment.
loadstring :: LuaState -> String -> String -> IO Int
#if MIN_VERSION_hslua(0,5,0)
loadstring lua script _ = Lua.loadstring lua script
#else
loadstring lua script cn = Lua.loadstring lua script cn
#endif
