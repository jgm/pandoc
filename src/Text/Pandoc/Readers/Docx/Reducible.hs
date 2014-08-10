{-# LANGUAGE OverloadedStrings, PatternGuards #-}

{-
Copyright (C) 2014 Jesse Rosenthal <jrosenthal@jhu.edu>

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
   Module      : Text.Pandoc.Readers.Docx.Reducible
   Copyright   : Copyright (C) 2014 Jesse Rosenthal
   License     : GNU GPL, version 2 or above

   Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
   Stability   : alpha
   Portability : portable

Typeclass for combining adjacent blocks and inlines correctly.
-}


module Text.Pandoc.Readers.Docx.Reducible ((<++>),
                                           (<+++>),
                                           Reducible,
                                           Container(..),
                                           container,
                                           innards,
                                           reduceList,
                                           reduceListB,
                                           rebuild)
       where

import Text.Pandoc.Builder
import Data.List ((\\), intersect)

data Container a = Container ([a] -> a) | NullContainer

instance (Eq a) => Eq (Container a) where
  (Container x) == (Container y) = ((x []) == (y []))
  NullContainer == NullContainer = True
  _ == _ = False

instance (Show a) => Show (Container a) where
  show (Container x) = "Container {" ++
                       (reverse $ drop 3 $ reverse $ show $ x []) ++
                       "}"
  show (NullContainer) = "NullContainer"

class Reducible a where
  (<++>) :: a -> a -> [a]
  container :: a -> Container a
  innards :: a -> [a]
  isSpace :: a -> Bool

(<+++>) :: (Reducible a) => Many a -> Many a -> Many a
mr <+++> ms = fromList $ reduceList $ toList mr ++ toList ms

reduceListB :: (Reducible a) => Many a -> Many a
reduceListB = fromList . reduceList . toList

reduceList' :: (Reducible a) => [a] -> [a] -> [a]
reduceList' acc [] = acc
reduceList' [] (x:xs) = reduceList' [x] xs
reduceList' as (x:xs) = reduceList' (init as ++ (last as <++> x) ) xs

reduceList :: (Reducible a) => [a] -> [a]
reduceList = reduceList' []

combineReducibles :: (Reducible a, Eq a) => a -> a -> [a]
combineReducibles r s =
  let (conts, rs) = topLevelContainers r
      (conts', ss) = topLevelContainers s
      shared = conts `intersect` conts'
      remaining = conts \\ shared
      remaining' = conts' \\ shared
  in
   case null shared of
     True | (x : xs) <- reverse rs
          , isSpace x ->
             rebuild conts (reverse xs) ++ [x, s]
          | (x : xs) <- ss
          , isSpace x ->
             [r, x] ++ rebuild conts' (xs)
     True  -> [r,s]
     False -> rebuild
              shared $
              reduceList $
              (rebuild remaining rs) ++ (rebuild remaining' ss)

instance Reducible Inline where
  s1@(Span (id1, classes1, kvs1) ils1) <++> s2@(Span (id2, classes2, kvs2) ils2) =
    let classes'  = classes1 `intersect` classes2
        kvs'      = kvs1 `intersect` kvs2
        classes1' = classes1 \\ classes'
        kvs1'     = kvs1 \\ kvs'
        classes2' = classes2 \\ classes'
        kvs2'     = kvs2 \\ kvs'
    in
     case null classes' && null kvs' of
       True -> [s1,s2]
       False -> let attr'  = ("", classes', kvs')
                    attr1' = (id1, classes1', kvs1')
                    attr2' = (id2, classes2', kvs2')
                    s1' = case null classes1' && null kvs1' of
                      True -> ils1
                      False -> [Span attr1' ils1]
                    s2' = case null classes2' && null kvs2' of
                      True -> ils2
                      False -> [Span attr2' ils2]
                in
                 [Span attr' $ reduceList $ s1' ++ s2']

  (Str x) <++> (Str y) = [Str (x++y)]
  il <++> il' = combineReducibles il il'

  container (Emph _) = Container Emph
  container (Strong _) = Container Strong
  container (Strikeout _) = Container Strikeout
  container (Subscript _) = Container Subscript
  container (Superscript _) = Container Superscript
  container (Quoted qt _) = Container $ Quoted qt
  container (Cite cs _) = Container $ Cite cs
  container (Span attr _) = Container $ Span attr
  container _ = NullContainer

  innards (Emph ils) = ils
  innards (Strong ils) = ils
  innards (Strikeout ils) = ils
  innards (Subscript ils) = ils
  innards (Superscript ils) = ils
  innards (Quoted _ ils) = ils
  innards (Cite _ ils) = ils
  innards (Span _ ils) = ils
  innards _ = []

  isSpace Space = True
  isSpace _     = False

instance Reducible Block where
  (Div (ident, classes, kvs) blks) <++> blk | "list-item" `elem` classes =
    [Div (ident, classes, kvs) (reduceList blks), blk]

  blk <++> blk' = combineReducibles blk blk'

  container (BlockQuote _) = Container BlockQuote
  container (Div attr _) = Container $ Div attr
  container _            = NullContainer

  innards (BlockQuote bs) = bs
  innards (Div _ bs) = bs
  innards _          = []

  isSpace _          = False


topLevelContainers' :: (Reducible a) => [a] -> ([Container a], [a])
topLevelContainers' (r : []) = case container r of
  NullContainer -> ([], [r])
  _             ->
    let (conts, inns) = topLevelContainers' (innards r)
    in
    ((container r) : conts, inns)
topLevelContainers' rs = ([], rs)

topLevelContainers :: (Reducible a) => a -> ([Container a], [a])
topLevelContainers il = topLevelContainers' [il]

rebuild :: [Container a] -> [a] -> [a]
rebuild [] xs = xs
rebuild ((Container f) : cs) xs = rebuild cs $ [f xs]
rebuild (NullContainer : cs) xs = rebuild cs $ xs
