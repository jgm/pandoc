{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
#ifdef DERIVE_JSON_VIA_TH
{-# LANGUAGE TemplateHaskell   #-}
#endif
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
   Module      : Text.Pandoc.Filter
   Copyright   : Copyright (C) 2006-2018 John MacFarlane
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
#ifdef DERIVE_JSON_VIA_TH
import Data.Aeson.TH (deriveJSON, defaultOptions)
#else
import Data.Aeson (FromJSON (..), ToJSON (..),
                   defaultOptions, genericToEncoding)
#endif
import Data.Foldable (foldrM)
import GHC.Generics (Generic)
import Text.Pandoc.Class (PandocIO)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Options (ReaderOptions)
import qualified Text.Pandoc.Filter.JSON as JSONFilter
import qualified Text.Pandoc.Filter.Lua as LuaFilter
import qualified Text.Pandoc.Filter.Path as Path

-- | Type of filter and path to filter file.
data Filter = LuaFilter FilePath
            | JSONFilter FilePath
            deriving (Show, Generic)

-- | Modify the given document using a filter.
applyFilters :: ReaderOptions
             -> [Filter]
             -> [String]
             -> Pandoc
             -> PandocIO Pandoc
applyFilters ropts filters args d = do
  expandedFilters <- mapM expandFilterPath filters
  foldrM ($) d $ map applyFilter expandedFilters
 where
  applyFilter (JSONFilter f) = JSONFilter.apply ropts args f
  applyFilter (LuaFilter f)  = LuaFilter.apply ropts args f

-- | Expand paths of filters, searching the data directory.
expandFilterPath :: Filter -> PandocIO Filter
expandFilterPath (LuaFilter fp) = LuaFilter <$> Path.expandFilterPath fp
expandFilterPath (JSONFilter fp) = JSONFilter <$> Path.expandFilterPath fp

#ifdef DERIVE_JSON_VIA_TH
$(deriveJSON defaultOptions ''Filter)
#else
instance ToJSON Filter where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Filter
#endif
