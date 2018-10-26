{-# LANGUAGE NoImplicitPrelude #-}
{-
Copyright (C) 2006-2018 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Filter.Lua
   Copyright   : Copyright (C) 2006-2018 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Apply Lua filters to modify a pandoc documents programmatically.
-}
module Text.Pandoc.Filter.Lua (apply) where

import Prelude
import Control.Exception (throw)
import Control.Monad ((>=>))
import Foreign.Lua (Lua)
import Text.Pandoc.Class (PandocIO)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Error (PandocError (PandocFilterError))
import Text.Pandoc.Filter.Path (expandFilterPath)
import Text.Pandoc.Lua (LuaException (..), runLua)
import Text.Pandoc.Lua.Filter (LuaFilter, walkMWithLuaFilter)
import Text.Pandoc.Lua.Global (Global (..), setGlobals)
import Text.Pandoc.Lua.Util (dofileWithTraceback)
import Text.Pandoc.Options (ReaderOptions)

import qualified Foreign.Lua as Lua

-- | Run the Lua filter in @filterPath@ for a transformation to the
-- target format (first element in args). Pandoc uses Lua init files to
-- setup the Lua interpreter.
apply :: ReaderOptions
      -> [String]
      -> FilePath
      -> Pandoc
      -> PandocIO Pandoc
apply ropts args f doc = do
  filterPath <- expandFilterPath f
  let format = case args of
                 (x:_) -> x
                 _     -> error "Format not supplied for Lua filter"
  runLua >=> forceResult filterPath $ do
    setGlobals [ FORMAT format
               , PANDOC_READER_OPTIONS ropts
               , PANDOC_SCRIPT_FILE filterPath
               ]
    top <- Lua.gettop
    stat <- dofileWithTraceback filterPath
    if stat /= Lua.OK
      then Lua.throwTopMessage
      else do
        newtop <- Lua.gettop
        -- Use the returned filters, or the implicitly defined global
        -- filter if nothing was returned.
        luaFilters <- if newtop - top >= 1
                      then Lua.peek Lua.stackTop
                      else Lua.pushglobaltable *> fmap (:[]) Lua.popValue
        runAll luaFilters doc

forceResult :: FilePath -> Either LuaException Pandoc -> PandocIO Pandoc
forceResult fp eitherResult = case eitherResult of
  Right x               -> return x
  Left (LuaException s) -> throw (PandocFilterError fp s)

runAll :: [LuaFilter] -> Pandoc -> Lua Pandoc
runAll = foldr ((>=>) . walkMWithLuaFilter) return
