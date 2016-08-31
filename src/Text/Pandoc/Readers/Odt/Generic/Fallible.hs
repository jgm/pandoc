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

import           Control.Applicative
import           Control.Monad

import qualified Data.Foldable       as F
import           Data.Monoid ((<>))

-- | Default for now. Will probably become a class at some point.
type Failure = ()

type Fallible a = Either Failure a


-- | False -> Left (), True -> Right ()
boolToEither :: Bool -> Fallible ()
boolToEither False = Left  ()
boolToEither True  = Right ()

-- | False -> Left (), True -> Right ()
boolToChoice :: Bool -> Fallible ()
boolToChoice False = Left  ()
boolToChoice True  = Right ()

--
maybeToEither :: Maybe a -> Fallible a
maybeToEither (Just a) = Right a
maybeToEither Nothing  = Left  ()

--
eitherToMaybe :: Either _l a -> Maybe a
eitherToMaybe (Left  _) = Nothing
eitherToMaybe (Right a) = Just a

-- | > untagEither === either id id
untagEither :: Either a a -> a
untagEither (Left  a) = a
untagEither (Right a) = a

-- | > fromLeft f === either f id
fromLeft :: (a -> b) -> Either a b -> b
fromLeft f (Left  a) = f a
fromLeft _ (Right b) = b

-- | > fromRight f === either id f
fromRight :: (a -> b) -> Either b a -> b
fromRight _ (Left  b) = b
fromRight f (Right a) = f a

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

-- | If either of the values represents an error, the result is a
-- (possibly combined) error. If both values represent a success,
-- both are returned.
chooseMin :: (Monoid a) => Either a b -> Either a b' -> Either a (b,b')
chooseMin = chooseMinWith (,)

-- | If either of the values represents an error, the result is a
-- (possibly combined) error. If both values represent a success,
-- a combination is returned.
chooseMinWith :: (Monoid a) => (b -> b' -> c)
                            -> Either a b
                            -> Either a b'
                            -> Either a c
chooseMinWith (><) (Right a) (Right b) = Right $ a >< b
chooseMinWith  _   (Left  a) (Left  b) = Left  $ a <> b
chooseMinWith  _   (Left  a)     _     = Left  a
chooseMinWith  _       _     (Left  b) = Left  b

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

-- Let's do a few examples first

instance ChoiceVector Maybe where
  spreadChoice (Just (Left  f))  = Left  f
  spreadChoice (Just (Right x))  = Right (Just x)
  spreadChoice Nothing           = Right Nothing

instance ChoiceVector (Either l) where
  spreadChoice (Right (Left  f)) = Left  f
  spreadChoice (Right (Right x)) = Right (Right x)
  spreadChoice (Left   x       ) = Right (Left x)

instance ChoiceVector ((,) a) where
  spreadChoice (_, Left  f) = Left  f
  spreadChoice (x, Right y) = Right (x,y)
  -- Wasn't there a newtype somewhere with the elements flipped?

--
-- More instances later, first some discussion.
--
-- I'll have to freshen up on type system details to see how (or if) to do
-- something like
--
-- > instance (ChoiceVector a, ChoiceVector b) => ChoiceVector (a b) where
-- >   :
--
-- But maybe it would be even better to use something like
--
-- > class ChoiceVector v v' f | v -> v' f where
-- >   spreadChoice :: v -> Either f v'
--
-- That way, more places in @v@ could spread the cheer, e.g.:
--
-- As before:
-- --                       ( a , Either f b)    (a , b)  f
-- > instance ChoiceVector ((,) a (Either f b)) ((,) a b) f where
-- >   spreadChoice (_, Left  f) = Left f
-- >   spreadChoice (a, Right b) = Right (a,b)
--
-- But also:
-- --                       ( Either f a , b)    (a , b)  f
-- > instance ChoiceVector ((,) (Either f a) b) ((,) a b) f where
-- >   spreadChoice (Right a,b) = Right (a,b)
-- >   spreadChoice (Left  f,_) = Left f
--
-- And maybe even:
-- --                        ( Either f a , Either f b)     (a , b)  f
-- > instance ChoiceVector ((,) (Either f a) (Either f b)) ((,) a b) f where
-- >   spreadChoice (Right a , Right b) = Right (a,b)
-- >   spreadChoice (Left  f , _      ) = Left f
-- >   spreadChoice ( _      , Left  f) = Left f
--
-- Of course that would lead to a lot of overlapping instances...
-- But I can't think of a different way. A selector function might help,
-- but not even a "Data.Traversable" is powerful enough for that.
-- But maybe someone has already solved all this with a lens library.
--
-- Well, it's an interesting academic question. But for practical purposes,
-- I have more than enough right now.

instance ChoiceVector ((,,) a b) where
  spreadChoice (_,_, Left  f) = Left  f
  spreadChoice (a,b, Right x) = Right (a,b,x)

instance ChoiceVector ((,,,) a b c) where
  spreadChoice (_,_,_, Left  f) = Left  f
  spreadChoice (a,b,c, Right x) = Right (a,b,c,x)

instance ChoiceVector ((,,,,) a b c d) where
  spreadChoice (_,_,_,_, Left  f) = Left  f
  spreadChoice (a,b,c,d, Right x) = Right (a,b,c,d,x)

instance ChoiceVector (Const a) where
  spreadChoice (Const c) = Right (Const c) -- need to repackage because of implicit types

-- | Fails on the first error
instance ChoiceVector [] where
  spreadChoice = sequence -- using the monad instance of Either.
  -- Could be generalized to "Data.Traversable" - but why play
  -- with UndecidableInstances unless this is really needed.

-- | Wrapper for a list. While the normal list instance of 'ChoiceVector'
-- fails whenever it can, this type will never fail.
newtype SuccessList a = SuccessList { collectNonFailing :: [a] }
  deriving ( Eq, Ord, Show )

instance ChoiceVector SuccessList  where
  spreadChoice = Right . SuccessList . (foldr unTagRight []) . collectNonFailing
    where unTagRight (Right x) = (x:)
          unTagRight _         = id

-- | Like 'catMaybes', but for 'Either'.
collectRights :: [Either _l r] -> [r]
collectRights = collectNonFailing . untag . spreadChoice . SuccessList
  where untag = fromLeft (error "Unexpected Left")

-- | A version of 'collectRights' generalized to other containers. The
-- container must be both "reducible" and "buildable". Most general containers
-- should fullfill these requirements, but there is no single typeclass
-- (that I know of) for that.
-- Therefore, they are split between 'Foldable' and 'MonadPlus'.
-- (Note that 'Data.Traversable.Traversable' alone would not be enough, either.)
collectRightsF :: (F.Foldable c, MonadPlus c) => c (Either _l r) ->  c r
collectRightsF = F.foldr unTagRight mzero
  where unTagRight (Right x) = mplus $ return x
        unTagRight _         = id
