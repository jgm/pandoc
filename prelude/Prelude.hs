{-# LANGUAGE NoImplicitPrelude #-}

module Prelude
(
  module Relude
, module Relude.Extra.Foldable1
, lookup
)
where

import Relude hiding (Alternative(..), optional, getOption, lookupEnv, getArgs)
import Relude.Extra.Foldable1
-- not sure why these are left out of Relude.List.Reexport
import Data.List (lookup)
