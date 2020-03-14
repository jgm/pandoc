{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}
{- |
   Module      : Text.Pandoc.Readers.Odt.Arrows.State
   Copyright   : Copyright (C) 2015 Martin Linnemann
   License     : GNU GPL, version 2 or above

   Maintainer  : Martin Linnemann <theCodingMarlin@googlemail.com>
   Stability   : alpha
   Portability : portable

An arrow that transports a state. It is in essence a more powerful version of
the standard state monad. As it is such a simple extension, there are
other version out there that do exactly the same.
The implementation is duplicated, though, to add some useful features.
Most of these might be implemented without access to innards, but it's much
faster and easier to implement this way.
-}

module Text.Pandoc.Readers.Odt.Arrows.State where

import Control.Arrow
import qualified Control.Category as Cat
import Control.Monad

import Text.Pandoc.Readers.Odt.Arrows.Utils
import Text.Pandoc.Readers.Odt.Generic.Fallible


newtype ArrowState state a b = ArrowState
  { runArrowState :: (state, a) -> (state, b) }

-- | Constructor
withState           :: (state -> a -> (state, b)) -> ArrowState state a b
withState            = ArrowState . uncurry

-- | Constructor
modifyState         :: (state      ->  state    ) -> ArrowState state a a
modifyState          = ArrowState . first

-- | Constructor
ignoringState       :: (         a ->         b ) -> ArrowState state a b
ignoringState        = ArrowState . second

-- | Constructor
fromState           :: (state      -> (state, b)) -> ArrowState state a b
fromState            = ArrowState . (.fst)

-- | Constructor
extractFromState    :: (state      ->         b ) -> ArrowState state x b
extractFromState   f = ArrowState $ \(state,_) -> (state, f state)

-- | Constructor
tryModifyState      :: (state ->  Either f state)
                    -> ArrowState state a (Either f a)
tryModifyState     f = ArrowState $ \(state,a)
                                  -> (state,).Left ||| (,Right a) $ f state

instance Cat.Category (ArrowState s) where
  id                = ArrowState id
  arrow2 . arrow1   = ArrowState $ runArrowState arrow2 . runArrowState arrow1

instance Arrow (ArrowState state) where
  arr               = ignoringState
  first  a          = ArrowState $ \(s,(aF,aS))
                                    -> second (,aS) $ runArrowState a (s,aF)
  second a          = ArrowState $ \(s,(aF,aS))
                                    -> second (aF,) $ runArrowState a (s,aS)

instance ArrowChoice (ArrowState state) where
  left   a          = ArrowState $ \(s,e) -> case e of
                                 Left  l -> second Left  $ runArrowState a (s,l)
                                 Right r -> (s, Right r)
  right  a          = ArrowState $ \(s,e) -> case e of
                                 Left  l -> (s, Left l)
                                 Right r -> second Right $ runArrowState a (s,r)

instance ArrowApply (ArrowState state) where
   app             = ArrowState $ \(s, (f,b)) -> runArrowState f (s,b)

-- | Switches the type of the state temporarily.
-- Drops the intermediate result state, behaving like a fallible
-- identity arrow, save for side effects in the state.
withSubStateF  :: ArrowState s  x (Either f s')
               -> ArrowState s' s (Either f s )
               -> ArrowState s  x (Either f x )
withSubStateF  unlift a = keepingTheValue (withSubStateF' unlift a)
                          >>^ spreadChoice
                          >>^ fmap fst

-- | Switches the type of the state temporarily.
-- Returns the resulting sub-state.
withSubStateF' :: ArrowState s  x (Either f s')
               -> ArrowState s' s (Either f s )
               -> ArrowState s  x (Either f s')
withSubStateF' unlift a = ArrowState go
  where go p@(s,_) = tryRunning unlift
                                ( tryRunning a (second Right) )
                                p
          where tryRunning a' b v = case runArrowState a' v of
                                      (_ , Left  f) -> (s, Left f)
                                      (x , Right y) -> b (y,x)

-- | Fold a state arrow through something 'Foldable'. Collect the results
-- in a 'Monoid'.
-- Intermediate form of a fold between one with "only" a 'Monoid'
-- and one with any function.
foldS :: (Foldable f, Monoid m) => ArrowState s x m -> ArrowState s (f x) m
foldS a = ArrowState $ \(s,f) -> foldr a' (s,mempty) f
  where a' x (s',m) = second (mappend m)  $ runArrowState a (s',x)

-- | Fold a state arrow through something 'Foldable'. Collect the results in a
-- 'MonadPlus'.
iterateS :: (Foldable f, MonadPlus m)
         => ArrowState s    x     y
         -> ArrowState s (f x) (m y)
iterateS a = ArrowState $ \(s,f) -> foldr a' (s,mzero) f
  where a' x (s',m) = second (mplus m.return) $ runArrowState a (s',x)

-- | Fold a state arrow through something 'Foldable'. Collect the results in a
-- 'MonadPlus'.
iterateSL :: (Foldable f, MonadPlus m)
          => ArrowState s    x     y
          -> ArrowState s (f x) (m y)
iterateSL a = ArrowState $ \(s,f) -> foldl a' (s,mzero) f
  where a' (s',m) x = second (mplus m.return) $ runArrowState a (s',x)


-- | Fold a fallible state arrow through something 'Foldable'.
-- Collect the results in a 'MonadPlus'.
-- If the iteration fails, the state will be reset to the initial one.
iterateS' :: (Foldable f, MonadPlus m)
          => ArrowState s    x  (Either e    y )
          -> ArrowState s (f x) (Either e (m y))
iterateS' a = ArrowState $ \(s,f) -> foldr (a' s) (s,Right mzero) f
  where a' s x (s',Right m) = case runArrowState a (s',x) of
                                (s'',Right m') -> (s'',Right $ mplus m $ return m')
                                (_  ,Left  e ) -> (s  ,Left  e )
        a' _ _   e          = e
