{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE CPP #-}

-- This custom Prelude emulates the API of the prelude
-- with base 4.8.

module Prelude
(
  module Prelude.Compat
, module Monoid.Compat
)
where

import Prelude.Compat
import Monoid.Compat
