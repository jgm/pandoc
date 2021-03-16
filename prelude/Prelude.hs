{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Amplified Prelude for pandoc
-- comment -- pandoc indicates pandoc additions

module Prelude (

    -- * Standard types, classes and related functions

    -- ** Bool
    Bool(False, True),
    (&&), (||), not, otherwise,

    -- ** Maybe
    Maybe(Nothing, Just),
    maybe,
    isJust, isNothing, fromMaybe,  -- pandoc
    listToMaybe, maybeToList, catMaybes, mapMaybe, -- pandoc

    -- ** Either
    Either(Left, Right),
    either,
    lefts, rights, isLeft, isRight,  -- pandoc
    fromLeft, fromRight, partitionEithers,  -- pandoc

    -- ** Ord
    Ordering(LT, EQ, GT),
    Ord(compare, (<), (<=), (>=), (>), max, min),
    comparing,  -- pandoc
    Down, -- pandoc

    -- ** Char and String
    Char, String,
    lines, words, unlines, unwords,

    -- *** Tuples
    fst, snd, curry, uncurry,
    swap,  -- pandoc

    -- ** Basic type classes
    Eq((==), (/=)),
    Enum(succ, pred, toEnum, fromEnum, enumFrom, enumFromThen,
         enumFromTo, enumFromThenTo),
    Bounded(minBound, maxBound),

    -- ** Numbers

    -- *** Numeric types
    Int, Integer, Float, Double,
    Rational, Word,

    -- *** Numeric type classes
    Num((+), (-), (*), negate, abs, signum, fromInteger),
    Real(toRational),
    Integral(quot, rem, div, mod, quotRem, divMod, toInteger),
    Fractional((/), recip, fromRational),
    Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
             asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh),
    RealFrac(properFraction, truncate, round, ceiling, floor),
    RealFloat(floatRadix, floatDigits, floatRange, decodeFloat,
              encodeFloat, exponent, significand, scaleFloat, isNaN,
              isInfinite, isDenormalized, isIEEE, isNegativeZero, atan2),

    -- *** Numeric functions
    subtract, even, odd, gcd, lcm, (^), (^^),
    fromIntegral, realToFrac,

    -- ** Semigroups and Monoids
    Semigroup((<>)),
    Monoid(mempty, mappend, mconcat),

    -- ** Monads and functors
    Functor(fmap, (<$)), (<$>),
    Applicative(pure, (<*>), (*>), (<*)),
    Monad((>>=), (>>), return),
    MonadPlus (mzero, mplus), msum, -- pandoc
    MonadFail(fail),
    mapM_, sequence_, (=<<),
    forM_, (>=>), (<=<), forever, void, -- pandoc
    filterM, mfilter, join, zipWithM, zipWithM_, foldM, foldM_, -- pandoc
    replicateM, replicateM_, guard, when, unless, -- pandoc
    liftM, liftM2, liftM3, liftM4, liftM5, ap, (<$!>), -- pandoc

    -- ** Folds and traversals
    Foldable.Foldable(elem,      -- :: (Foldable t, Eq a) => a -> t a -> Bool
             fold,      -- :: Monoid m => t m -> m  -- pandoc
             foldMap,   -- :: Monoid m => (a -> m) -> t a -> m
             foldr,     -- :: (a -> b -> b) -> b -> t a -> b
             foldr',    -- :: (a -> b -> b) -> b -> t a -> b  -- pandoc
             -- foldl,     -- :: (b -> a -> b) -> b -> t a -> b -- pandoc
             foldl'     -- :: (b -> a -> b) -> b -> t a -> b  -- pandoc
             -- foldr1, -- :: (a -> a -> a) -> t a -> a  -- pandoc
             -- foldl1, -- :: (a -> a -> a) -> t a -> a  -- pandoc
             -- maximum, -- :: (Foldable t, Ord a) => t a -> a
             -- minimum  -- :: (Foldable t, Ord a) => t a -> a
             -- product, -- :: (Foldable t, Nu => t a -> a -- pandoc
             -- sum     -- :: Num a => t a -> a  -- pandoc
             -- toList     -- :: Foldable t => t a -> [a] -- pandoc
             ),
    maximum, minimum, maximumMay, minimumMay, -- pandoc
    sum', product', -- pandoc

    Traversable.Traversable(traverse, sequenceA, mapM, sequence),
    Traversable.for, Traversable.forM,  -- pandoc
    Traversable.mapAccumL, Traversable.mapAccumR, -- pandoc

    -- ** Miscellaneous functions
    id, const, (.), flip, ($), until,
    asTypeOf, error, errorWithoutStackTrace, undefined,
    seq, ($!),

    -- * List operations
    List.map, (List.++), List.filter,
    -- List.head, List.last, List.tail, List.init, (List.!!), -- pandoc
    head, last, tail, init, (!!), -- pandoc
    headMay, lastMay, tailMay, initMay, (!!?), -- pandoc
    Foldable.null, Foldable.length,
    List.reverse,
    -- *** Special folds
    Foldable.and, Foldable.or, Foldable.any, Foldable.all,
    Foldable.concat, Foldable.concatMap,
    -- ** Building lists
    -- *** Scans
    List.scanl,
    -- List.scanl1, -- pandoc
    List.scanr,
    -- List.scanr1, -- pandoc
    -- *** Infinite lists
    List.iterate, List.repeat, List.replicate, List.cycle,
    -- ** Sublists
    List.take, List.drop,
    List.takeWhile, List.dropWhile,
    List.span, List.break,
    List.splitAt,
    List.isPrefixOf, List.isSuffixOf, List.isInfixOf, -- pandoc
    -- ** Searching lists
    Foldable.notElem,
    List.lookup,
    -- ** Zipping and unzipping lists
    List.zip, List.zip3,
    List.zipWith, List.zipWith3,
    List.unzip, List.unzip3,

    -- * Converting to and from @String@
    -- ** Converting to @String@
    ShowS,
    Show(showsPrec, showList, show),
    shows,
    showChar, showString, showParen,
    -- ** Converting from @String@
    ReadS,
    Read(readsPrec, readList),
    reads,
    readParen,
    read, -- pandoc
    readMay, -- pandoc
    lex,

    -- * Basic Input and output
    IO,
    -- ** Simple I\/O operations
    -- All I/O functions defined here are character oriented.  The
    -- treatment of the newline character will vary on different systems.
    -- For example, two characters of input, return and linefeed, may
    -- read as a single newline character.  These functions cannot be
    -- used portably for binary I/O.
    -- *** Output functions
    putChar,
    putStr, putStrLn, print,
    -- *** Input functions
    getChar,
    getLine, getContents, interact,
    -- *** Files
    FilePath,
    readFile, writeFile, appendFile, readIO, readLn,
    -- ** Exception handling in the I\/O monad
    IOError, ioError, userError,

    -- ** Debugging
    traceShowId, traceShowIdWith, traceIO, trace, traceId, -- pandoc
    traceShow, traceM, traceShowM, traceStack,  -- pandoc
    traceEvent, traceEventIO, traceMarker, traceMarkerIO, -- pandoc

    -- ** Generics
    module Data.Data,   -- pandoc
    module Data.Typeable   -- pandoc

  ) where

import "base" Prelude hiding (read, head, last, init, tail, (!!),
                              maximum, minimum)
import qualified "base" Prelude as StandardPrelude
import Data.Maybe
import Data.Either
import Data.Ord
import qualified Data.List as List
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable
import Data.Tuple (swap)
import Control.Monad
import qualified Debug.Trace as Trace
import Data.Data
import Data.Typeable
import Safe (atMay, headMay, lastMay, initMay, tailMay, readMay)
-- TODO

-- replacements for sum, product
-- nonempty?
-- generics?
-- applicative/alternative?
-- exceptions?

{-# WARNING head "head is a partial function: use headMay instead." #-}
head :: [a] -> a
head = List.head

{-# WARNING last "last is a partial function: use lastMay instead." #-}
last :: [a] -> a
last = List.last

{-# WARNING init "init is a partial function: use initMay instead." #-}
init :: [a] -> [a]
init = List.init

{-# WARNING tail "tail is a partial function: use tailMay instead." #-}
tail :: [a] -> [a]
tail = List.tail

{-# WARNING (!!) "(!!) is a partial function: use (!!?) instead." #-}
(!!) :: [a] -> Int -> a
(!!) = (List.!!)

{-# WARNING maximum "maximum is a partial function: use maximumMay instead." #-}
maximum :: (Foldable t, Ord a) => t a -> a
maximum = Foldable.maximum

{-# WARNING minimum "minimum is a partial function: use minimumMay instead." #-}
minimum :: (Foldable t, Ord a) => t a -> a
minimum = Foldable.minimum

(!!?) :: [a] -> Int -> Maybe a
(!!?) = Safe.atMay

-- borrowed from Safe.Util:
liftMay :: (a -> Bool) -> (a -> b) -> (a -> Maybe b)
liftMay test func val
  | test val  = Nothing
  | otherwise = Just $ func val

maximumMay :: (Foldable t, Ord a) => t a -> Maybe a
maximumMay = liftMay Foldable.null Foldable.maximum

minimumMay :: (Foldable t, Ord a) => t a -> Maybe a
minimumMay = liftMay Foldable.null Foldable.minimum

{-# WARNING read "read is a partial function: use readMay instead." #-}
read :: Read a => String -> a
read = StandardPrelude.read

-- | Strict sum.
sum' :: (Foldable t, Num a) => t a -> a
sum' x = Foldable.foldl' (+) 0 x

-- | Strict product.
product' :: (Foldable t, Num a) => t a -> a
product' x = Foldable.foldl' (*) 0 x

{-# WARNING traceShowId "traceShowId left in code" #-}
traceShowId :: Show a => a -> a
traceShowId = Trace.traceShowId

{-# WARNING traceShowIdWith "traceShowIdWith left in code" #-}
traceShowIdWith :: Show a => (String -> String) -> a -> a
traceShowIdWith f x = Trace.trace (f (show x)) x

{-# WARNING traceIO "traceIO left in code" #-}
traceIO :: String -> IO ()
traceIO = Trace.traceIO

{-# WARNING trace "trace left in code" #-}
trace :: String -> a -> a
trace = Trace.trace

{-# WARNING traceId "traceId left in code" #-}
traceId :: String -> String
traceId = Trace.traceId

{-# WARNING traceShow "traceShow left in code" #-}
traceShow :: Show a => a -> b -> b
traceShow = Trace.traceShow

{-# WARNING traceM "traceM left in code" #-}
traceM :: Applicative f => String -> f ()
traceM = Trace.traceM

{-# WARNING traceShowM "traceShowM left in code" #-}
traceShowM :: (Show a, Applicative f) => a -> f ()
traceShowM = Trace.traceShowM

{-# WARNING traceStack "traceStack left in code" #-}
traceStack :: String -> a -> a
traceStack = Trace.traceStack

{-# WARNING traceEvent "traceEvent left in code" #-}
traceEvent :: String -> a -> a
traceEvent = Trace.traceEvent

{-# WARNING traceEventIO "traceIO left in code" #-}
traceEventIO :: String -> IO ()
traceEventIO = Trace.traceEventIO

{-# WARNING traceMarker "traceMarker left in code" #-}
traceMarker :: String -> a -> a
traceMarker = Trace.traceMarker

{-# WARNING traceMarkerIO "traceMarkerIO left in code" #-}
traceMarkerIO :: String -> IO ()
traceMarkerIO = Trace.traceMarkerIO

