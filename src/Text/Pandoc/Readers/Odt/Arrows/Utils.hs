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
   Module      : Text.Pandoc.Readers.Odt.Arrows.Utils
   Copyright   : Copyright (C) 2015 Martin Linnemann
   License     : GNU GPL, version 2 or above

   Maintainer  : Martin Linnemann <theCodingMarlin@googlemail.com>
   Stability   : alpha
   Portability : portable

Utility functions for Arrows (Kleisli monads).

Some general notes on notation:

* "^" is meant to stand for a pure function that is lifted into an arrow
based on its usage for that purpose in "Control.Arrow".
* "?" is meant to stand for the usage of a 'FallibleArrow' or a pure function
with an equivalent return value.
* "_" stands for the dropping of a value.
-}

-- We export everything
module Text.Pandoc.Readers.Odt.Arrows.Utils where

import           Control.Arrow
import           Control.Monad                         ( join, MonadPlus(..) )

import qualified Data.Foldable                    as F

import           Text.Pandoc.Readers.Odt.Generic.Fallible
import           Text.Pandoc.Readers.Odt.Generic.Utils
import           Text.Pandoc.Compat.Monoid

and2 :: (Arrow a) => a b c -> a b c' -> a b (c,c')
and2 = (&&&)

and3 :: (Arrow a)
     => a b c0->a b c1->a b c2
     -> a b (c0,c1,c2               )
and4 :: (Arrow a)
     => a b c0->a b c1->a b c2->a b c3
     -> a b (c0,c1,c2,c3            )
and5 :: (Arrow a)
     => a b c0->a b c1->a b c2->a b c3->a b c4
     -> a b (c0,c1,c2,c3,c4         )
and6 :: (Arrow a)
     => a b c0->a b c1->a b c2->a b c3->a b c4->a b c5
     -> a b (c0,c1,c2,c3,c4,c5      )
and7 :: (Arrow a)
     => a b c0->a b c1->a b c2->a b c3->a b c4->a b c5->a b c6
     -> a b (c0,c1,c2,c3,c4,c5,c6   )
and8 :: (Arrow a)
     => a b c0->a b c1->a b c2->a b c3->a b c4->a b c5->a b c6->a b c7
     -> a b (c0,c1,c2,c3,c4,c5,c6,c7)

and3 a b c           = (and2 a b          ) &&& c
                       >>^ \((z,y          ) , x) -> (z,y,x          )
and4 a b c d         = (and3 a b c        ) &&& d
                       >>^ \((z,y,x        ) , w) -> (z,y,x,w        )
and5 a b c d e       = (and4 a b c d      ) &&& e
                       >>^ \((z,y,x,w      ) , v) -> (z,y,x,w,v      )
and6 a b c d e f     = (and5 a b c d e    ) &&& f
                       >>^ \((z,y,x,w,v    ) , u) -> (z,y,x,w,v,u    )
and7 a b c d e f g   = (and6 a b c d e f  ) &&& g
                       >>^ \((z,y,x,w,v,u  ) , t) -> (z,y,x,w,v,u,t  )
and8 a b c d e f g h = (and7 a b c d e f g) &&& h
                       >>^ \((z,y,x,w,v,u,t) , s) -> (z,y,x,w,v,u,t,s)

liftA2 :: (Arrow a) => (x -> y -> z) -> a b x -> a b y -> a b z
liftA2 f a b = a &&& b >>^ uncurry f

liftA3 :: (Arrow a) => (z->y->x                -> r)
                    -> a b z->a b y->a b x
                    -> a b r
liftA4 :: (Arrow a) => (z->y->x->w             -> r)
                    -> a b z->a b y->a b x->a b w
                    -> a b r
liftA5 :: (Arrow a) => (z->y->x->w->v          -> r)
                    -> a b z->a b y->a b x->a b w->a b v
                    -> a b r
liftA6 :: (Arrow a) => (z->y->x->w->v->u       -> r)
                    -> a b z->a b y->a b x->a b w->a b v->a b u
                    -> a b r
liftA7 :: (Arrow a) => (z->y->x->w->v->u->t    -> r)
                    -> a b z->a b y->a b x->a b w->a b v->a b u->a b t
                    -> a b r
liftA8 :: (Arrow a) => (z->y->x->w->v->u->t->s -> r)
                    -> a b z->a b y->a b x->a b w->a b v->a b u->a b t->a b s
                    -> a b r

liftA3 fun a b c           = and3 a b c           >>^ uncurry3 fun
liftA4 fun a b c d         = and4 a b c d         >>^ uncurry4 fun
liftA5 fun a b c d e       = and5 a b c d e       >>^ uncurry5 fun
liftA6 fun a b c d e f     = and6 a b c d e f     >>^ uncurry6 fun
liftA7 fun a b c d e f g   = and7 a b c d e f g   >>^ uncurry7 fun
liftA8 fun a b c d e f g h = and8 a b c d e f g h >>^ uncurry8 fun

liftA :: (Arrow a) => (y -> z) -> a b y -> a b z
liftA  fun a = a >>^ fun


-- | Duplicate a value to subsequently feed it into different arrows.
-- Can almost always be replaced with '(&&&)', 'keepingTheValue',
-- or even '(|||)'.
-- Aequivalent to
-- > returnA &&& returnA
duplicate :: (Arrow a) => a b (b,b)
duplicate = arr $ join (,)

-- | Lifts the combination of two values into an arrow.
joinOn :: (Arrow a) => (x -> y -> z) -> a (x,y) z
joinOn = arr.uncurry

-- | Applies a function to the uncurried result-pair of an arrow-application.
-- (The %-symbol was chosen to evoke an association with pairs.)
(>>%) :: (Arrow a) => a x (b,c) -> (b -> c -> d) -> a x d
a >>% f = a >>^ uncurry f

-- | '(>>%)' with its arguments flipped
(%<<) :: (Arrow a) => (b -> c -> d) -> a x (b,c) -> a x d
(%<<) = flip (>>%)

-- | Precomposition with an uncurried function
(%>>) :: (Arrow a) => (b -> c -> d) -> a d r -> a (b,c) r
f %>> a = uncurry f ^>> a

-- | Precomposition with an uncurried function (right to left variant)
(<<%) :: (Arrow a) => a d r -> (b -> c -> d) -> a (b,c) r
(<<%) = flip (%>>)

infixr 2 >>%, %<<, %>>, <<%


-- | Duplicate a value and apply an arrow to the second instance.
-- Aequivalent to
-- > \a -> duplicate >>> second a
-- or
-- > \a -> returnA &&& a
keepingTheValue :: (Arrow a) => a b c -> a b (b,c)
keepingTheValue a = returnA &&& a

-- | Duplicate a value and apply an arrow to the first instance.
-- Aequivalent to
-- > \a -> duplicate >>> first a
-- or
-- > \a -> a &&& returnA
keepingTheValue' :: (Arrow a) => a b c -> a b (c,b)
keepingTheValue' a = a &&& returnA

-- | 'bind' from the "Maybe"-Monad lifted into an 'ArrowChoice'.
-- Actually, it's the more complex '(>=>)', because 'bind' alone does not
-- combine as nicely in arrow form.
-- The current implementation is not the most efficient one, because it can
-- not return directly if a 'Nothing' is encountered. That in turn follows
-- from the type system, as 'Nothing' has an "invisible" type parameter that
-- can not be dropped early.
--
-- Also, there probably is a way to generalize this to other monads
-- or applicatives, but I'm leaving that as an exercise to the reader.
-- I have a feeling there is a new Arrow-typeclass to be found that is less
-- restrictive than 'ArrowApply'. If it is already out there,
-- I have not seen it yet. ('ArrowPlus' for example is not general enough.)
(>>>=) :: (ArrowChoice a) => a x (Maybe b) -> a b (Maybe c) -> a x (Maybe c)
a1 >>>= a2 = a1 >>> maybeToChoice >>> right a2 >>> choiceToMaybe >>^ join

infixr 2 >>>=

-- | 'mplus' Lifted into an arrow. No 'ArrowPlus' required.
-- (But still different from a true bind)
(>++<) :: (Arrow a, MonadPlus m) => a x (m b) -> a x (m b) -> a x (m b)
(>++<) = liftA2 mplus

-- | Left-compose with a pure function
leftLift :: (ArrowChoice a) => (l -> l') -> a (Either l r) (Either l' r)
leftLift = left.arr

-- | Right-compose with a pure function
rightLift :: (ArrowChoice a) => (r -> r') -> a (Either l r) (Either l r')
rightLift = right.arr


( ^+++  ) :: (ArrowChoice a) => (b -> c) ->  a b' c'   -> a (Either b b') (Either c c')
(  +++^ ) :: (ArrowChoice a) =>  a b c   -> (b' -> c') -> a (Either b b') (Either c c')
( ^+++^ ) :: (ArrowChoice a) => (b -> c) -> (b' -> c') -> a (Either b b') (Either c c')

l ^+++  r  = leftLift l >>> right r
l  +++^ r  = left     l >>> rightLift r
l ^+++^ r  = leftLift l >>> rightLift r

infixr 2 ^+++, +++^, ^+++^

( ^|||  ) :: (ArrowChoice a) => (b -> d) ->  a c d   -> a (Either b c) d
(  |||^ ) :: (ArrowChoice a) =>  a b d   -> (c -> d) -> a (Either b c) d
( ^|||^ ) :: (ArrowChoice a) => (b -> d) -> (c -> d) -> a (Either b c) d

l ^|||  r  = arr l |||     r
l  |||^ r  =     l ||| arr r
l ^|||^ r  = arr l ||| arr r

infixr 2 ^||| ,  |||^, ^|||^

( ^&&&  ) :: (Arrow a) => (b -> c) ->  a b c'   -> a b (c,c')
(  &&&^ ) :: (Arrow a) =>  a b c   -> (b -> c') -> a b (c,c')
( ^&&&^ ) :: (Arrow a) => (b -> c) -> (b -> c') -> a b (c,c')

l ^&&&  r = arr l &&&     r
l  &&&^ r =     l &&& arr r
l ^&&&^ r = arr l &&& arr r

infixr 3 ^&&&, &&&^, ^&&&^

( ^***  ) :: (Arrow a) => (b -> c) ->  a b' c'   -> a (b,b') (c,c')
(  ***^ ) :: (Arrow a) =>  a b c   -> (b' -> c') -> a (b,b') (c,c')
( ^***^ ) :: (Arrow a) => (b -> c) -> (b' -> c') -> a (b,b') (c,c')

l ^***  r = arr l ***     r
l  ***^ r =     l *** arr r
l ^***^ r = arr l *** arr r

infixr 3 ^***, ***^, ^***^

-- | A version of
--
-- >>> \p -> arr (\x -> if p x the  Right x else Left x)
--
-- but with p being an arrow
choose :: (ArrowChoice a) => a b Bool -> a b (Either b b)
choose checkValue = keepingTheValue checkValue >>^ select
  where select (x,True  ) = Right x
        select (x,False ) = Left  x

-- | Converts @Right a@ into @Just a@ and @Left _@ into @Nothing@.
choiceToMaybe :: (ArrowChoice a) => a (Either l r) (Maybe r)
choiceToMaybe = arr eitherToMaybe

-- | Converts @Nothing@ into @Left ()@ and @Just a@ into @Right a@.
maybeToChoice :: (ArrowChoice a) => a (Maybe b) (Fallible b)
maybeToChoice = arr maybeToEither

-- | Lifts a constant value into an arrow
returnV :: (Arrow a) => c -> a x c
returnV = arr.const

-- | 'returnA' dropping everything
returnA_ :: (Arrow a) => a _b ()
returnA_  = returnV ()

-- | Wrapper for an arrow that can be evaluated im parallel. All
-- Arrows can be evaluated in parallel, as long as they return a
-- monoid.
newtype ParallelArrow a b c = CoEval { evalParallelArrow :: a b c }
  deriving (Eq, Ord, Show)

instance (Arrow a, Monoid m) => Monoid (ParallelArrow a b m) where
  mempty = CoEval $ returnV mempty
  (CoEval a) `mappend` (CoEval ~b) = CoEval $ a &&& b >>% mappend

-- | Evaluates a collection of arrows in a parallel fashion.
--
-- This is in essence a fold of '(&&&)' over the collection,
-- so the actual execution order and parallelity depends on the
-- implementation of '(&&&)' in the arrow in question.
-- The default implementation of '(&&&)' for example keeps the
-- order as given in the collection.
--
-- This function can be seen as a generalization of
-- 'Control.Applicative.sequenceA' to arrows or as an alternative to
-- a fold with 'Control.Applicative.WrappedArrow', which
-- substitutes the monoid with function application.
--
coEval :: (Arrow a, F.Foldable f, Monoid m) => f (a b m) -> a b m
coEval = evalParallelArrow . (F.foldMap CoEval)

-- | Defines Left as failure, Right as success
type FallibleArrow a input failure success = a input (Either failure success)

type ReFallibleArrow a failure success success'
     = FallibleArrow a (Either failure success) failure success'

-- | Wrapper for fallible arrows. Fallible arrows are all arrows that return
-- an Either value where left is a faliure and right is a success value.
newtype AlternativeArrow a input failure success
  = TryArrow { evalAlternativeArrow :: FallibleArrow a input failure success }


instance (ArrowChoice a, Monoid failure)
         => Monoid (AlternativeArrow a input failure success) where
  mempty = TryArrow $ returnV $ Left mempty
  (TryArrow a) `mappend` (TryArrow b)
         = TryArrow $ a &&& b
                      >>^ \(a',~b')
                          -> ( (\a'' -> left (mappend a'') b') ||| Right )
                               a'

-- | Evaluates a collection of fallible arrows, trying each one in succession.
-- Left values are interpreted as failures, right values as successes.
--
-- The evaluation is stopped once an arrow succeeds.
-- Up to that point, all failures are collected in the failure-monoid.
-- Note that '()' is a monoid, and thus can serve as a failure-collector if
-- you are uninterested in the exact failures.
--
-- This is in essence a fold of '(&&&)' over the collection, enhanced with a
-- little bit of repackaging, so the actual execution order depends on the
-- implementation of '(&&&)' in the arrow in question.
-- The default implementation of '(&&&)' for example keeps the
-- order as given in the collection.
--
tryArrows         :: (ArrowChoice a, F.Foldable f, Monoid failure)
                  => f (FallibleArrow a b failure success)
                  ->    FallibleArrow a b failure success
tryArrows         = evalAlternativeArrow . (F.foldMap TryArrow)

--
liftSuccess       :: (ArrowChoice a)
                  => (success -> success')
                  -> ReFallibleArrow a failure success success'
liftSuccess       = rightLift

--
liftAsSuccess     :: (ArrowChoice a)
                  => a x success
                  -> FallibleArrow a x failure success
liftAsSuccess a   = a >>^ Right

--
asFallibleArrow   :: (ArrowChoice a)
                  => a x success
                  -> FallibleArrow a x failure success
asFallibleArrow a = a >>^ Right

-- | Raises an error into a 'ReFallibleArrow' if the arrow is already in
-- "error mode"
liftError         :: (ArrowChoice a, Monoid failure)
                  => failure
                  -> ReFallibleArrow a failure success success
liftError e       = leftLift (e <>)

-- | Raises an error into a 'FallibleArrow', droping both the arrow input
-- and any previously stored error value.
_raiseA           :: (ArrowChoice a)
                  => failure
                  -> FallibleArrow a x failure success
_raiseA e         = returnV (Left e)

-- | Raises an empty error into a 'FallibleArrow', droping both the arrow input
-- and any previously stored error value.
_raiseAEmpty      :: (ArrowChoice a, Monoid failure)
                  => FallibleArrow a x failure success
_raiseAEmpty      = _raiseA mempty

-- | Raises an error into a 'ReFallibleArrow', possibly appending the new error
-- to an existing one
raiseA            :: (ArrowChoice a, Monoid failure)
                  => failure
                  -> ReFallibleArrow a failure success success
raiseA e          = arr $ Left.(either (<> e) (const e))

-- | Raises an empty error into a 'ReFallibleArrow'. If there already is an
-- error, nothing changes.
-- (Note that this function is only aequivalent to @raiseA mempty@ iff the
-- failure monoid follows the monoid laws.)
raiseAEmpty       :: (ArrowChoice a, Monoid failure)
                  => ReFallibleArrow a failure success success
raiseAEmpty       = arr (fromRight (const mempty) >>> Left)


-- | Execute the second arrow if the first succeeds
(>>?) :: (ArrowChoice a, Monoid failure)
            => FallibleArrow a x       failure success
            -> FallibleArrow a success failure success'
            -> FallibleArrow a x       failure success'
a >>? b = a >>> Left ^||| b

-- | Execute the lifted second arrow if the first succeeds
(>>?^) :: (ArrowChoice a, Monoid failure)
            => FallibleArrow a x       failure success
            -> (success                     -> success')
            -> FallibleArrow a x       failure success'
a >>?^ f = a >>^ Left ^|||^ Right . f

-- | Execute the lifted second arrow if the first succeeds
(>>?^?) :: (ArrowChoice a, Monoid failure)
            => FallibleArrow a x       failure success
            -> (success      -> Either failure success')
            -> FallibleArrow a x       failure success'
a >>?^? b = a >>> Left ^|||^ b

-- | Execute the second arrow if the lifted first arrow succeeds
(^>>?) :: (ArrowChoice a, Monoid failure)
            => (x            -> Either failure success)
            -> FallibleArrow a success failure success'
            -> FallibleArrow a x       failure success'
a ^>>? b = a ^>> Left ^||| b

-- | Execute the lifted second arrow if the lifted first arrow succeeds
(^>>?^) :: (ArrowChoice a, Monoid failure)
            => (x            -> Either failure success)
            -> (success                     -> success')
            -> FallibleArrow a x       failure success'
a ^>>?^ f = arr $ a >>> right f

-- | Execute the lifted second arrow if the lifted first arrow succeeds
(^>>?^?) :: (ArrowChoice a, Monoid failure)
            => (x            -> Either failure success)
            -> (success      -> Either failure success')
            -> FallibleArrow a x       failure success'
a ^>>?^? f = a ^>> Left ^|||^ f

-- | Execute the second, non-fallible arrow if the first arrow succeeds
(>>?!) :: (ArrowChoice a, Monoid failure)
            => FallibleArrow a x       failure success
            ->               a success         success'
            -> FallibleArrow a x       failure success'
a >>?! f = a >>> right f

---
(>>?%) :: (ArrowChoice a, Monoid f)
          => FallibleArrow a x f (b,b')
          -> (b -> b' -> c)
          -> FallibleArrow a x f c
a >>?% f = a >>?^ (uncurry f)

---
(^>>?%) :: (ArrowChoice a, Monoid f)
          => (x -> Either f (b,b'))
          -> (b -> b' -> c)
          -> FallibleArrow a x f c
a ^>>?% f = arr a >>?^ (uncurry f)

---
(>>?%?) :: (ArrowChoice a, Monoid f)
           => FallibleArrow a x f (b,b')
           -> (b -> b' -> (Either f c))
           -> FallibleArrow a x f c
a >>?%? f = a >>?^? (uncurry f)

infixr 1  >>?,  >>?^,  >>?^?
infixr 1 ^>>?, ^>>?^, ^>>?^?, >>?!
infixr 1 >>?%, ^>>?%, >>?%?

-- | Keep values that are Right, replace Left values by a constant.
ifFailedUse :: (ArrowChoice a) => v -> a (Either f v) v
ifFailedUse v = arr $ either (const v) id

-- | '(&&)' lifted into an arrow
(<&&>) :: (Arrow a) => a x Bool -> a x Bool -> a x Bool
(<&&>) = liftA2 (&&)

-- | '(||)' lifted into an arrow
(<||>) :: (Arrow a) => a x Bool -> a x Bool -> a x Bool
(<||>) = liftA2 (||)

-- | An equivalent of '(&&)' in a fallible arrow
(>&&<) :: (ArrowChoice a, Monoid f) => FallibleArrow a x f s
                                    -> FallibleArrow a x f s'
                                    -> FallibleArrow a x f (s,s')
(>&&<) = liftA2 chooseMin

-- | An equivalent of '(||)' in some forms of fallible arrows
(>||<) :: (ArrowChoice a, Monoid f, Monoid s) => FallibleArrow a x f s
                                              -> FallibleArrow a x f s
                                              -> FallibleArrow a x f s
(>||<) = liftA2 chooseMax

-- | An arrow version of a short-circuit (<|>)
ifFailedDo :: (ArrowChoice a)
           => FallibleArrow a x f y
           -> FallibleArrow a x f y
           -> FallibleArrow a x f y
ifFailedDo a b = keepingTheValue a >>> repackage ^>> (b |||^ Right)
  where repackage (x , Left  _) = Left  x
        repackage (_ , Right y) = Right y

infixr 4 <&&>, <||>, >&&<, >||<
infixr 1 `ifFailedDo`


