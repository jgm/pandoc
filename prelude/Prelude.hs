{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}

-- This custom Prelude emulates the API of the prelude
-- with base 4.8.

module Prelude
(
  module P
#if MIN_VERSION_base(4,8,0)
#else
, Monoid(..)
, Applicative(..)
, (<$>)
, (<$)
#endif
)
where

#if MIN_VERSION_base(4,8,0)
import "base" Prelude as P
#elif MIN_VERSION_base(4,6,0)
import "base" Prelude as P
import Control.Applicative
import Data.Monoid
#else
import "base" Prelude as P hiding (catch)
import Control.Applicative
import Data.Monoid
#endif
