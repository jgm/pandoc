{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}

module Prelude
(
  module P
, Monoid(..)
, Applicative(..)
#if MIN_VERSION_base(4,8,0)
#else
, (<$>)
, (<$)
#endif
, (<>)
)
where

#if MIN_VERSION_base(4,8,0)
import "base" Prelude as P
import Data.Monoid ((<>))
#elif MIN_VERSION_base(4,6,0)
import "base" Prelude as P
import Control.Applicative
import Data.Monoid
#else
import "base" Prelude as P hiding (catch)
import Control.Applicative
import Data.Monoid
#endif

#if MIN_VERSION_base(4,5,0)
#else
infixr 6 <>

-- | An infix synonym for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}
#endif
