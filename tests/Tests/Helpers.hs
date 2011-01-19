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
  (=?>) :: (String, Pandoc) -> a -> Assertion

infix 8 =?>

(=:) :: TestName -> Assertion -> Test
(=:) = testCase

infix 6 =:

instance Expect Inlines where
  (s, Pandoc _ [Para ils]) =?> e = assertEqual s (toList e) ils
  (s, g)                   =?> e = assertEqual s (doc $ para e) g

instance Expect Blocks where
  (s, Pandoc _ bls)        =?> e = assertEqual s (toList e) bls

instance Expect Pandoc where
  (s, g) =?> e = assertEqual s e g

