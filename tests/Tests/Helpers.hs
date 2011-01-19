{-# LANGUAGE TypeSynonymInstances #-}
-- Utility functions for the test suite.

module Tests.Helpers where

import Text.Pandoc
import Text.Pandoc.Builder
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

-- In the first argument, the String is the input, and the Pandoc
-- the output, of a pandoc reader.  The input is shown in case
-- the test fails.
class Expect a where
  (=?>) :: (String, Pandoc) -> a -> Assertion

infix 8 =?>

(=:) :: TestName -> Assertion -> Test
(=:) = testCase

infix 6 =:

instance Expect Inlines where
  (s, Pandoc _ [Para ils]) =?> e = assertEqual (show s) (toList e) ils
  (s, g)                   =?> e = assertEqual (show s) (doc $ para e) g

instance Expect Blocks where
  (s, Pandoc _ bls)        =?> e = assertEqual (show s) (toList e) bls

instance Expect Pandoc where
  (s, g) =?> e = assertEqual (show s) e g

