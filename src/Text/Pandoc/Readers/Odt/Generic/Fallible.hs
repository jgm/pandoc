{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
   Module      : Text.Pandoc.Readers.Odt.Generic.Fallible
   Copyright   : Copyright (C) 2015 Martin Linnemann
   License     : GNU GPL, version 2 or above

   Maintainer  : Martin Linnemann <theCodingMarlin@googlemail.com>
   Stability   : alpha
   Portability : portable

Data types and utilities representing failure. Most of it is based on the
"Either" type in its usual configuration (left represents failure).

In most cases, the failure type is implied or required to be a "Monoid".

The choice of "Either" instead of a custom type makes it easier to write
compatible instances of "ArrowChoice".
-}

-- We export everything
module Text.Pandoc.Readers.Odt.Generic.Fallible where

import           Data.Monoid ((<>))

-- | Default for now. Will probably become a class at some point.
type Failure = ()

type Fallible a = Either Failure a


--
maybeToEither :: Maybe a -> Fallible a
maybeToEither (Just a) = Right a
maybeToEither Nothing  = Left  ()

--
eitherToMaybe :: Either _l a -> Maybe a
eitherToMaybe (Left  _) = Nothing
eitherToMaybe (Right a) = Just a

-- | > fromLeft f === either f id
fromLeft :: (a -> b) -> Either a b -> b
fromLeft f (Left  a) = f a
fromLeft _ (Right b) = b

-- | > recover a === fromLeft (const a) === either (const a) id
recover :: a -> Either _f a -> a
recover a (Left  _) = a
recover _ (Right a) = a

-- | I would love to use 'fail'. Alas, 'Monad.fail'...
failWith :: failure -> Either failure _x
failWith f = Left f

--
failEmpty :: (Monoid failure) => Either failure _x
failEmpty = failWith mempty

--
succeedWith :: a -> Either _x a
succeedWith = Right

--
collapseEither :: Either failure (Either failure x)
               -> Either failure x
collapseEither (Left f         ) = Left f
collapseEither (Right (Left  f)) = Left f
collapseEither (Right (Right x)) = Right x

-- | If either of the values represents a  non-error, the result is a
-- (possibly combined) non-error. If both values represent an error, an error
-- is returned.
chooseMax :: (Monoid a, Monoid b) => Either a b -> Either a b -> Either a b
chooseMax = chooseMaxWith (<>)

-- | If either of the values represents a non-error, the result is a
-- (possibly combined) non-error. If both values represent an error, an error
-- is returned.
chooseMaxWith :: (Monoid a) => (b -> b -> b)
                            -> Either a b
                            -> Either a b
                            -> Either a b
chooseMaxWith (><) (Right a) (Right b) = Right $ a >< b
chooseMaxWith  _   (Left  a) (Left  b) = Left  $ a <> b
chooseMaxWith  _   (Right a)     _     = Right a
chooseMaxWith  _       _     (Right b) = Right b


-- | Class of containers that can escalate contained 'Either's.
-- The word "Vector" is meant in the sense of a disease transmitter.
class ChoiceVector v where
  spreadChoice :: v (Either f a) -> Either f (v a)

instance ChoiceVector ((,) a) where
  spreadChoice (_, Left  f) = Left  f
  spreadChoice (x, Right y) = Right (x,y)
  -- Wasn't there a newtype somewhere with the elements flipped?

-- | Wrapper for a list. While the normal list instance of 'ChoiceVector'
-- fails whenever it can, this type will never fail.
newtype SuccessList a = SuccessList { collectNonFailing :: [a] }
  deriving ( Eq, Ord, Show )

instance ChoiceVector SuccessList  where
  spreadChoice = Right . SuccessList . (foldr unTagRight []) . collectNonFailing
    where unTagRight (Right x) = (x:)
          unTagRight _         = id
