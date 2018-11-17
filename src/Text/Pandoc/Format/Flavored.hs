{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Format.Flavored
   Copyright   : © 2012-2022 John MacFarlane, 2019-2022 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : John MacFarlane <jgm@berkeley.edu>

Data structures and functions for representing markup extensions.
-}
module Text.Pandoc.Format.Flavored
  ( module Text.Pandoc.Format.Extensions
  , Flavored (..)
  , hasBase
  , hasBaseIn
  )
where

import Prelude
import Text.Pandoc.Format.Extensions

-- | Full description of a format, including the selected set of
-- extensions.
data Flavored f = KnownFormat f Extensions
                | CustomFormat FilePath
  deriving (Show, Read, Eq)

-- | Returns 'True' if the flavored format is based on the given format,
-- and 'False' otherwise.
hasBase :: Eq f => Flavored f -> f -> Bool
hasBase = \case
  KnownFormat f _ -> (f ==)
  _               -> const False

-- | Returns 'True' if the flavored format is based on one of the listed
-- format, and 'False' otherwise.
hasBaseIn :: Eq f => Flavored f -> [f] -> Bool
hasBaseIn = \case
  KnownFormat f _ -> (f `elem`)
  _               -> const False
