{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.SILE (tests) where

import Prelude
import Data.Text (unpack)
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

sile :: (ToPandoc a) => a -> String
sile = sileWithOpts def

sileWithOpts :: (ToPandoc a) => WriterOptions -> a -> String
sileWithOpts opts = unpack . purely (writeSILE opts) . toPandoc

{-
  "my test" =: X =?> Y

is shorthand for

  test sile "my test" $ X =?> Y

which is in turn shorthand for

  test sile "my test" (X,Y)
-}

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> TestTree
(=:) = test sile

tests :: [TestTree]
tests = []
