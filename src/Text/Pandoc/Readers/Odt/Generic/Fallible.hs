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
chooseMax = chooseMaxWith mappend

-- | If either of the values represents a non-error, the result is a
-- (possibly combined) non-error. If both values represent an error, an error
-- is returned.
chooseMaxWith :: (Monoid a) => (b -> b -> b)
                            -> Either a b
                            -> Either a b
                            -> Either a b
chooseMaxWith (><) (Right a) (Right b) = Right $ a >< b
chooseMaxWith  _   (Left  a) (Left  b) = Left  $ a `mappend` b
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
  spreadChoice = Right . SuccessList . foldr unTagRight [] . collectNonFailing
    where unTagRight (Right x) = (x:)
          unTagRight _         = id
