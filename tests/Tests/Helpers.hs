{-# LANGUAGE TypeSynonymInstances #-}
-- Utility functions for the test suite.

module Tests.Helpers where

import Text.Pandoc
import Text.Pandoc.Builder
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

-- in Helpers
class Expect a where
  (=?>) :: Pandoc -> a -> Assertion

infix 8 =?>

(=:) :: TestName -> Assertion -> Test
(=:) = testCase

infix 6 =:

instance Expect Inlines where
  (Pandoc _ [Para ils]) =?> e = assertEqual " " (toList e) ils
  g                     =?> e = assertEqual " " (doc $ para e) g

instance Expect Blocks where
  (Pandoc _ bls)        =?> e = assertEqual " " (toList e) bls

instance Expect Pandoc where
  g =?> e = assertEqual " " e g

