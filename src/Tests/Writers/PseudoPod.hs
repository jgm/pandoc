{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Tests.Writers.PseudoPod (tests) where

import Test.Framework
import Text.Pandoc.Builder
import Text.Pandoc
import Tests.Helpers
import Tests.Arbitrary()

pseudopod :: (ToString a, ToPandoc a) => a -> String
pseudopod = writePseudoPod defaultWriterOptions . toPandoc

{-
  "my test" =: X =?> Y

is shorthand for

  test pseudopod "my test" $ X =?> Y

which is in turn shorthand for

  test pseudopod "my test" (X,Y)
-}

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> Test
(=:) = test pseudopod

tests :: [Test]
tests = [ "escaped > in string"
             =: (para "string with > in it" )
             =?> "string with E<gt> in it"
        ]
