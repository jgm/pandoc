{- |
   Module      : Text.Pandoc.Readers.Odt.Generic.SetMap
   Copyright   : Copyright (C) 2015 Martin Linnemann
   License     : GNU GPL, version 2 or above

   Maintainer  : Martin Linnemann <theCodingMarlin@googlemail.com>
   Stability   : alpha
   Portability : portable

A map of values to sets of values.
-}

module Text.Pandoc.Readers.Odt.Generic.SetMap where

import qualified Data.Map as M
import qualified Data.Set as S

type SetMap k v = M.Map k (S.Set v)

empty :: SetMap k v
empty = M.empty

fromList :: (Ord k, Ord v) => [(k,v)] -> SetMap k v
fromList = foldr (uncurry insert) empty

insert :: (Ord k, Ord v) => k -> v -> SetMap k v -> SetMap k v
insert key value setMap = M.insertWith S.union key (S.singleton value) setMap

union3 :: (Ord k) => SetMap k v -> SetMap k v -> SetMap k v -> SetMap k v
union3 sm1 sm2 sm3 = sm1 `M.union` sm2 `M.union` sm3
