{-
Copyright (C) 2015 Martin Linnemann <theCodingMarlin@googlemail.com>

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
