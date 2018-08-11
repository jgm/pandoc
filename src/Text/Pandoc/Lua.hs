{-# LANGUAGE NoImplicitPrelude #-}
{-
Copyright © 2017–2018 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

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
   Copyright   : Copyright © 2017–2018 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Running pandoc Lua filters.
-}
module Text.Pandoc.Lua
  ( LuaException (..)
  , runLuaFilter
  , runPandocLua
  ) where

import Prelude
import Control.Monad ((>=>))
import Foreign.Lua (Lua, LuaException (..))
import Text.Pandoc.Class (PandocIO)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Lua.Filter (LuaFilter, walkMWithLuaFilter)
import Text.Pandoc.Lua.Init (runPandocLua, registerScriptPath)
import Text.Pandoc.Lua.Util (popValue)
import Text.Pandoc.Options (ReaderOptions)

import qualified Foreign.Lua as Lua

-- | Run the Lua filter in @filterPath@ for a transformation to target
-- format @format@. Pandoc uses Lua init files to setup the Lua
-- interpreter.
runLuaFilter :: ReaderOptions -> FilePath -> String
             -> Pandoc -> PandocIO (Either LuaException Pandoc)
runLuaFilter ropts filterPath format doc =
  runPandocLua (runLuaFilter' ropts filterPath format doc)

runLuaFilter' :: ReaderOptions -> FilePath -> String
              -> Pandoc -> Lua Pandoc
runLuaFilter' ropts filterPath format pd = do
  registerFormat
  registerReaderOptions
  registerScriptPath filterPath
  top <- Lua.gettop
  stat <- Lua.dofile filterPath
  if stat /= Lua.OK
    then Lua.throwTopMessageAsError
    else do
      newtop <- Lua.gettop
      -- Use the returned filters, or the implicitly defined global filter if
      -- nothing was returned.
      luaFilters <- if newtop - top >= 1
                    then Lua.peek Lua.stackTop
                    else Lua.getglobal "_G" *> fmap (:[]) popValue
      runAll luaFilters pd
 where
  registerFormat = do
    Lua.push format
    Lua.setglobal "FORMAT"

  registerReaderOptions = do
    Lua.push ropts
    Lua.setglobal "PANDOC_READER_OPTIONS"

runAll :: [LuaFilter] -> Pandoc -> Lua Pandoc
runAll = foldr ((>=>) . walkMWithLuaFilter) return
