{-# LANGUAGE NoImplicitPrelude #-}
{-
Copyright (C) 2006-2017 John MacFarlane <jgm@berkeley.edu>

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
{-# LANGUAGE TemplateHaskell     #-}

{- |
   Module      : Text.Pandoc.Filter
   Copyright   : Copyright (C) 2006-2017 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Programmatically modifications of pandoc documents.
-}
module Text.Pandoc.Filter
  ( Filter (..)
  , applyFilters
  ) where

import Prelude
import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Foldable (foldrM)
import Text.Pandoc.Class (PandocIO)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Options (ReaderOptions)
import qualified Text.Pandoc.Filter.JSON as JSONFilter
import qualified Text.Pandoc.Filter.Lua as LuaFilter

data Filter = LuaFilter FilePath
            | JSONFilter FilePath
            deriving (Show)

applyFilters :: ReaderOptions
             -> [Filter]
             -> [String]
             -> Pandoc
             -> PandocIO Pandoc
applyFilters ropts filters args d =
  foldrM ($) d $ map applyFilter filters
 where
  applyFilter (JSONFilter f) = JSONFilter.apply ropts args f
  applyFilter (LuaFilter f)  = LuaFilter.apply ropts args f

$(deriveJSON defaultOptions ''Filter)
