{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
   Module      : Text.Pandoc.Parsing.Future
   Copyright   : Copyright (C) 2006-2023 John MacFarlane
   License     : GPL-2.0-or-later
   Maintainer  : John MacFarlane <jgm@berkeley.edu>

Future type for parsing.
-}

module Text.Pandoc.Parsing.Future
  ( Future (..)
  , runF
  , askF
  , asksF
  , returnF
  )
where

import Prelude hiding (Applicative(..))
import Control.Applicative (Applicative(..))
import Control.Monad.Reader
  ( asks, runReader, MonadReader(ask), Reader, ReaderT(ReaderT) )

-- | Reader monad wrapping the parser state. This is used to possibly
-- delay evaluation until all relevant information has been parsed and
-- made available in the parser state.
newtype Future s a = Future { runDelayed :: Reader s a }
  deriving (Monad, Applicative, Functor)

instance Semigroup a => Semigroup (Future s a) where
  (<>) = liftA2 (<>)

instance (Semigroup a, Monoid a) => Monoid (Future s a) where
  mempty = return mempty
  mappend = (<>)

-- | Run a delayed action with the given state.
runF :: Future s a -> s -> a
runF = runReader . runDelayed

askF :: Future s s
askF = Future ask

asksF :: (s -> a) -> Future s a
asksF f = Future $ asks f

returnF :: Monad m => a -> m (Future s a)
returnF = return . return
