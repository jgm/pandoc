{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{- |
   Module      : Text.Pandoc.Filter
   Copyright   : Copyright (C) 2006-2019 John MacFarlane
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
import Data.Aeson.TH (deriveJSON, defaultOptions)
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

$(deriveJSON defaultOptions ''Filter)
