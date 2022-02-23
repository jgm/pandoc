{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.Plain (tests) where

import Test.Tasty
import Test.Tasty.HUnit (HasCallStack)
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder


infix 4 =:
(=:) :: (ToString a, ToPandoc a, HasCallStack)
     => String -> (a, String) -> TestTree
(=:) = test (purely (writePlain def{ writerExtensions =
                          enableExtension Ext_gutenberg plainExtensions }) .
                      toPandoc)


tests :: [TestTree]
tests = [ "strongly emphasized text to uppercase"
             =: strong "Straße"
             =?> "STRASSE"
        ]
