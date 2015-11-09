{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns  #-}

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
   Module      : Text.Pandoc.Reader.Odt.Generic.Utils
   Copyright   : Copyright (C) 2015 Martin Linnemann
   License     : GNU GPL, version 2 or above

   Maintainer  : Martin Linnemann <theCodingMarlin@googlemail.com>
   Stability   : alpha
   Portability : portable

General utility functions for the odt reader.
-}

module Text.Pandoc.Readers.Odt.Generic.Utils
( uncurry3
, uncurry4
, uncurry5
, uncurry6
, uncurry7
, uncurry8
, swap
, reverseComposition
, bool
, tryToRead
, Lookupable(..)
, readLookupables
, readLookupable
, readPercent
, findBy
, swing
, composition
) where

import           Control.Category        ( Category, (>>>), (<<<) )
import qualified Control.Category as Cat ( id )
import           Control.Monad           ( msum )

import qualified Data.Foldable    as F   ( Foldable, foldr )
import           Data.Maybe


-- | Aequivalent to
-- > foldr (.) id
-- where '(.)' are 'id' are the ones from "Control.Category"
-- and 'foldr' is the one from "Data.Foldable".
-- The noun-form was chosen to be consistend with 'sum', 'product' etc
-- based on the discussion at
-- <https://groups.google.com/forum/#!topic/haskell-cafe/VkOZM1zaHOI>
-- (that I was not part of)
composition        :: (Category cat, F.Foldable f) => f (cat a a) -> cat a a
composition        = F.foldr (<<<) Cat.id

-- | Aequivalent to
-- > foldr (flip (.)) id
-- where '(.)' are 'id' are the ones from "Control.Category"
-- and 'foldr' is the one from "Data.Foldable".
-- A reversed version of 'composition'.
reverseComposition :: (Category cat, F.Foldable f) => f (cat a a) -> cat a a
reverseComposition = F.foldr (>>>) Cat.id

-- | 'Either' has 'either', 'Maybe' has 'maybe'. 'Bool' should have 'bool'.
-- Note that the first value is selected if the boolean value is 'False'.
-- That makes 'bool' consistent with the other two. Also, 'bool' now takes its
-- arguments in the exact opposite order compared to the normal if construct.
bool :: a -> a -> Bool -> a
bool x _ False = x
bool _ x True  = x

-- | This function often makes it possible to switch values with the functions
-- that are applied to them.
--
-- Examples:
-- > swing map :: [a -> b] -> a -> [b]
-- > swing any :: [a -> Bool] -> a -> Bool
-- > swing foldr :: b -> a -> [a -> b -> b] -> b
-- > swing scanr :: c -> a -> [a -> c -> c] -> c
-- > swing zipWith :: [a -> b -> c] -> a -> [b] -> [c]
-- > swing find :: [a -> Bool] -> a -> Maybe (a -> Bool)
--
-- Stolen from <https://wiki.haskell.org/Pointfree>
swing :: (((a -> b) -> b) -> c -> d) -> c -> a -> d
swing = flip.(.flip id)
-- swing f c a = f ($ a) c


-- | Alternative to 'read'/'reads'. The former of these throws errors
-- (nobody wants that) while the latter returns "to much" for simple purposes.
-- This function instead applies 'reads' and returns the first match (if any)
-- in a 'Maybe'.
tryToRead :: (Read r) => String -> Maybe r
tryToRead = reads >>> listToMaybe >>> fmap fst

-- | A version of 'reads' that requires a '%' sign after the number
readPercent :: ReadS Int
readPercent s = [ (i,s') | (i   , r ) <- reads s
                         , ("%" , s') <- lex   r
              ]

-- | Data that can be looked up.
-- This is mostly a utility to read data with kind *.
class Lookupable a where
  lookupTable :: [(String, a)]

-- | The idea is to use this function as if there was a declaration like
--
-- > instance (Lookupable a) => (Read a) where
-- >   readsPrec _ = readLookupables
-- .
-- But including this code in this form would need UndecideableInstances.
-- That is a bad idea. Luckily 'readLookupable' (without the s at the end)
-- can be used directly in almost any case.
readLookupables :: (Lookupable a) => String -> [(a,String)]
readLookupables s = [ (a,rest) | (word,rest) <- lex s,
                                 let result = lookup word lookupTable,
                                 isJust result,
                                 let Just a = result
                    ]

-- | Very similar to a simple 'lookup' in the 'lookupTable', but with a lexer.
readLookupable :: (Lookupable a) => String -> Maybe a
readLookupable s = msum
                 $ map ((`lookup` lookupTable).fst)
                 $ lex s

uncurry3 :: (a->b->c                -> z) -> (a,b,c          ) -> z
uncurry4 :: (a->b->c->d             -> z) -> (a,b,c,d        ) -> z
uncurry5 :: (a->b->c->d->e          -> z) -> (a,b,c,d,e      ) -> z
uncurry6 :: (a->b->c->d->e->f       -> z) -> (a,b,c,d,e,f    ) -> z
uncurry7 :: (a->b->c->d->e->f->g    -> z) -> (a,b,c,d,e,f,g  ) -> z
uncurry8 :: (a->b->c->d->e->f->g->h -> z) -> (a,b,c,d,e,f,g,h) -> z

uncurry3 fun (a,b,c          ) = fun a b c
uncurry4 fun (a,b,c,d        ) = fun a b c d
uncurry5 fun (a,b,c,d,e      ) = fun a b c d e
uncurry6 fun (a,b,c,d,e,f    ) = fun a b c d e f
uncurry7 fun (a,b,c,d,e,f,g  ) = fun a b c d e f g
uncurry8 fun (a,b,c,d,e,f,g,h) = fun a b c d e f g h

swap :: (a,b) -> (b,a)
swap (a,b) = (b,a)

-- | A version of "Data.List.find" that uses a converter to a Maybe instance.
-- The returned value is the first which the converter returns in a 'Just'
-- wrapper.
findBy :: (a -> Maybe b) -> [a] -> Maybe b
findBy _               []   = Nothing
findBy f ((f -> Just x):_ ) = Just x
findBy f (            _:xs) = findBy f xs

