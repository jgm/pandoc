{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.Plain (tests) where

import Test.Framework
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder


infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> Test
(=:) = test (purely (writePlain def) . toPandoc)


tests :: [Test]
tests = [ "strongly emphasized text to uppercase"
             =: strong "Straße"
             =?> "STRASSE"
        ]
