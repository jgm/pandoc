{-# LANGUAGE NoImplicitPrelude #-}

-- The intent is that this Prelude provide the API of
-- the base 4.11 Prelude in a way that is portable for
-- all base versions.

module Prelude
(
  module Prelude.Compat
, Semigroup(..)
)
where

import Prelude.Compat
import Data.Semigroup (Semigroup(..))  -- includes (<>)
