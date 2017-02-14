{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.Plain (tests) where

import Test.Framework
import Text.Pandoc.Builder
import Text.Pandoc
import Tests.Helpers
import Text.Pandoc.Arbitrary()


infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> Test
(=:) = test (purely (writePlain def) . toPandoc)


tests :: [Test]
tests = [ "strongly emphasized text to uppercase"
             =: strong "Straße"
             =?> "STRASSE"
        ]
