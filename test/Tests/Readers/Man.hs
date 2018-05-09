{-# LANGUAGE OverloadedStrings #-}
module Tests.Readers.Man (tests) where

import Prelude
import Data.Text (Text)
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder
import Text.Pandoc.Readers.Man

man :: Text -> Pandoc
man = purely $ readMan def

infix 4 =:
(=:) :: ToString c
     => String -> (Text, c) -> TestTree
(=:) = test man

tests :: [TestTree]
tests = [
  -- .SH "HEllo bbb" "aaa"" as"
  testGroup "Macros" [
          "Bold" =:
          ".B foo\n"
          =?> (para $ strong "foo")
        , "Italic" =:
          ".I foo\n"
          =?> (para $ emph "foo")
        ]
  ]
