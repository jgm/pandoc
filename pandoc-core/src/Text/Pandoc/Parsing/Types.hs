{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
   Module      : Text.Pandoc.Parsing
   Copyright   : Copyright (C) 2006-2022 John MacFarlane
   License     : GPL-2.0-or-later
   Maintainer  : John MacFarlane <jgm@berkeley.edu>

Types and type-related functions for parsers.
-}

module Text.Pandoc.Parsing.Types
  ( Parser
  , ParserT
  , Future (..)
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
import Text.Parsec ( Parsec , ParsecT )

-- | Generic parser type used by many pandoc readers.
type Parser t s = Parsec t s

-- | Generic parser transformer used by many pandoc readers.
type ParserT = ParsecT

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
