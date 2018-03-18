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
import Text.Pandoc.Class (PandocIO)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Error (PandocError (PandocFilterError))
import Text.Pandoc.Filter.Path (expandFilterPath)
import Text.Pandoc.Lua (LuaException (..), runLuaFilter)
import Text.Pandoc.Options (ReaderOptions)

apply :: ReaderOptions
      -> [String]
      -> FilePath
      -> Pandoc
      -> PandocIO Pandoc
apply ropts args f d = do
  f' <- expandFilterPath f
  let format = case args of
                 (x:_) -> x
                 _     -> error "Format not supplied for lua filter"
  res <- runLuaFilter ropts f' format d
  case res of
    Right x               -> return x
    Left (LuaException s) -> throw (PandocFilterError f s)
