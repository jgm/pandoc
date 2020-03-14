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

import Control.Arrow
import Control.Monad (join)

import Text.Pandoc.Readers.Odt.Generic.Fallible
import Text.Pandoc.Readers.Odt.Generic.Utils

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

and3 a b c           = and2 a b &&& c
                       >>^ \((z,y          ) , x) -> (z,y,x          )
and4 a b c d         = and3 a b c &&& d
                       >>^ \((z,y,x        ) , w) -> (z,y,x,w        )
and5 a b c d e       = and4 a b c d &&& e
                       >>^ \((z,y,x,w      ) , v) -> (z,y,x,w,v      )
and6 a b c d e f     = and5 a b c d e &&& f
                       >>^ \((z,y,x,w,v    ) , u) -> (z,y,x,w,v,u    )

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

liftA3 fun a b c           = and3 a b c           >>^ uncurry3 fun
liftA4 fun a b c d         = and4 a b c d         >>^ uncurry4 fun
liftA5 fun a b c d e       = and5 a b c d e       >>^ uncurry5 fun
liftA6 fun a b c d e f     = and6 a b c d e f     >>^ uncurry6 fun

liftA :: (Arrow a) => (y -> z) -> a b y -> a b z
liftA  fun a = a >>^ fun


-- | Duplicate a value to subsequently feed it into different arrows.
-- Can almost always be replaced with '(&&&)', 'keepingTheValue',
-- or even '(|||)'.
-- Equivalent to
-- > returnA &&& returnA
duplicate :: (Arrow a) => a b (b,b)
duplicate = arr $ join (,)

-- | Applies a function to the uncurried result-pair of an arrow-application.
-- (The %-symbol was chosen to evoke an association with pairs.)
(>>%) :: (Arrow a) => a x (b,c) -> (b -> c -> d) -> a x d
a >>% f = a >>^ uncurry f

infixr 2 >>%


-- | Duplicate a value and apply an arrow to the second instance.
-- Equivalent to
-- > \a -> duplicate >>> second a
-- or
-- > \a -> returnA &&& a
keepingTheValue :: (Arrow a) => a b c -> a b (b,c)
keepingTheValue a = returnA &&& a

( ^|||  ) :: (ArrowChoice a) => (b -> d) ->  a c d   -> a (Either b c) d
(  |||^ ) :: (ArrowChoice a) =>  a b d   -> (c -> d) -> a (Either b c) d
( ^|||^ ) :: (ArrowChoice a) => (b -> d) -> (c -> d) -> a (Either b c) d

l ^|||  r  = arr l |||     r
l  |||^ r  =     l ||| arr r
l ^|||^ r  = arr l ||| arr r

infixr 2 ^||| ,  |||^, ^|||^

( ^&&&  ) :: (Arrow a) => (b -> c) ->  a b c'   -> a b (c,c')
(  &&&^ ) :: (Arrow a) =>  a b c   -> (b -> c') -> a b (c,c')

l ^&&&  r = arr l &&&     r
l  &&&^ r =     l &&& arr r

infixr 3 ^&&&, &&&^


-- | Converts @Right a@ into @Just a@ and @Left _@ into @Nothing@.
choiceToMaybe :: (ArrowChoice a) => a (Either l r) (Maybe r)
choiceToMaybe = arr eitherToMaybe

-- | Converts @Nothing@ into @Left ()@ and @Just a@ into @Right a@.
maybeToChoice :: (ArrowChoice a) => a (Maybe b) (Fallible b)
maybeToChoice = arr maybeToEither

-- | Lifts a constant value into an arrow
returnV :: (Arrow a) => c -> a x c
returnV = arr.const

-- | Defines Left as failure, Right as success
type FallibleArrow a input failure success = a input (Either failure success)

--
liftAsSuccess     :: (ArrowChoice a)
                  => a x success
                  -> FallibleArrow a x failure success
liftAsSuccess a   = a >>^ Right

-- | Execute the second arrow if the first succeeds
(>>?) :: (ArrowChoice a)
            => FallibleArrow a x       failure success
            -> FallibleArrow a success failure success'
            -> FallibleArrow a x       failure success'
a >>? b = a >>> Left ^||| b

-- | Execute the lifted second arrow if the first succeeds
(>>?^) :: (ArrowChoice a)
            => FallibleArrow a x       failure success
            -> (success                     -> success')
            -> FallibleArrow a x       failure success'
a >>?^ f = a >>^ Left ^|||^ Right . f

-- | Execute the lifted second arrow if the first succeeds
(>>?^?) :: (ArrowChoice a)
            => FallibleArrow a x       failure success
            -> (success      -> Either failure success')
            -> FallibleArrow a x       failure success'
a >>?^? b = a >>> Left ^|||^ b

-- | Execute the second arrow if the lifted first arrow succeeds
(^>>?) :: (ArrowChoice a)
            => (x            -> Either failure success)
            -> FallibleArrow a success failure success'
            -> FallibleArrow a x       failure success'
a ^>>? b = a ^>> Left ^||| b

-- | Execute the second, non-fallible arrow if the first arrow succeeds
(>>?!) :: (ArrowChoice a)
            => FallibleArrow a x       failure success
            ->               a success         success'
            -> FallibleArrow a x       failure success'
a >>?! f = a >>> right f

---
(>>?%) :: (ArrowChoice a)
          => FallibleArrow a x f (b,b')
          -> (b -> b' -> c)
          -> FallibleArrow a x f c
a >>?% f = a >>?^ uncurry f

---
(^>>?%) :: (ArrowChoice a)
          => (x -> Either f (b,b'))
          -> (b -> b' -> c)
          -> FallibleArrow a x f c
a ^>>?% f = arr a >>?^ uncurry f

---
(>>?%?) :: (ArrowChoice a)
           => FallibleArrow a x f (b,b')
           -> (b -> b' -> Either f c)
           -> FallibleArrow a x f c
a >>?%? f = a >>?^? uncurry f

infixr 1  >>?,  >>?^,  >>?^?
infixr 1 ^>>?, >>?!
infixr 1 >>?%, ^>>?%, >>?%?

-- | An arrow version of a short-circuit (<|>)
ifFailedDo :: (ArrowChoice a)
           => FallibleArrow a x f y
           -> FallibleArrow a x f y
           -> FallibleArrow a x f y
ifFailedDo a b = keepingTheValue a >>> repackage ^>> (b |||^ Right)
  where repackage (x , Left  _) = Left  x
        repackage (_ , Right y) = Right y

infixr 1 `ifFailedDo`
