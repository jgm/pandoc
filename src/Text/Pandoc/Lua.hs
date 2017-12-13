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
   Module      : Text.Pandoc.Lua
   Copyright   : Copyright © 2017 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Running pandoc Lua filters.
-}
module Text.Pandoc.Lua
  ( LuaException (..)
  , runLuaFilter
  , runPandocLua
  , pushPandocModule
  ) where

import Control.Monad (when, (>=>))
import Foreign.Lua (FromLuaStack (peek), Lua, LuaException (..),
                    Status (OK), ToLuaStack (push))
import Text.Pandoc.Class (PandocIO)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Lua.Filter (LuaFilter, walkMWithLuaFilter)
import Text.Pandoc.Lua.Init (runPandocLua)
import Text.Pandoc.Lua.PandocModule (pushPandocModule) -- TODO: remove
import qualified Foreign.Lua as Lua

-- | Run the Lua filter in @filterPath@ for a transformation to target
-- format @format@. Pandoc uses Lua init files to setup the Lua
-- interpreter.
runLuaFilter :: FilePath -> String
             -> Pandoc -> PandocIO (Either LuaException Pandoc)
runLuaFilter filterPath format doc =
  runPandocLua (runLuaFilter' filterPath format doc)

runLuaFilter' :: FilePath -> String
              -> Pandoc -> Lua Pandoc
runLuaFilter' filterPath format pd = do
  -- store module in global "pandoc"
  registerFormat
  top <- Lua.gettop
  stat <- Lua.dofile filterPath
  if stat /= OK
    then do
      luaErrMsg <- peek (-1) <* Lua.pop 1
      Lua.throwLuaError luaErrMsg
    else do
      newtop <- Lua.gettop
      -- Use the implicitly defined global filter if nothing was returned
      when (newtop - top < 1) pushGlobalFilter
      luaFilters <- peek (-1)
      runAll luaFilters pd
 where
  registerFormat = do
    push format
    Lua.setglobal "FORMAT"

pushGlobalFilter :: Lua ()
pushGlobalFilter = do
  Lua.newtable
  Lua.getglobal' "pandoc.global_filter"
  Lua.call 0 1
  Lua.rawseti (-2) 1

runAll :: [LuaFilter] -> Pandoc -> Lua Pandoc
runAll = foldr ((>=>) . walkMWithLuaFilter) return
