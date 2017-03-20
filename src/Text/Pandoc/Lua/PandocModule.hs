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
   Module      : Text.Pandoc.Lua.PandocModule
   Copyright   : Copyright © 2017 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc module for lua.
-}
module Text.Pandoc.Lua.PandocModule ( pushPandocModule ) where

import Data.ByteString.Char8 ( unpack )
import Scripting.Lua ( LuaState, loadstring, call)
import Text.Pandoc.Shared ( readDataFile )


-- | Push the "pandoc" on the lua stack.
pushPandocModule :: LuaState -> IO ()
pushPandocModule lua = do
  script <- pandocModuleScript
  status <- loadstring lua script "cn"
  if (status /= 0)
    then return ()
    else do
      call lua 0 1

-- | Get the string representation of the pandoc module
pandocModuleScript :: IO String
pandocModuleScript = unpack <$> readDataFile Nothing "pandoc.lua"
