{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}

module Prelude
(
  module P
, Monoid(..)
, Semigroup(..)
, Applicative(..)
#if MIN_VERSION_base(4,9,0)
-- <> is exported in Semigroup
#else
, (<>)
#endif
)
where

import "base" Prelude as P
import Data.Semigroup (Semigroup(..))  -- includes (<>)
#if MIN_VERSION_base(4,11,0)
import Data.Monoid (Monoid(..))
#endif
