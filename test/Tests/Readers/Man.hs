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
          ".B foo"
          =?> (para $ strong "foo")
        , "Italic" =:
          ".I bar\n"
          =?> (para $ emph "bar")
        , "BoldItalic" =:
          ".BI foo bar"
          =?> (para $ strong $ emph $ str "foo bar")
        , "H1" =:
          ".SH The header\n"
          =?> header 2 (str "The header")
        , "H2" =:
          ".SS The header 2"
          =?> header 3 (str "The header 2")
        , "Macro args" =:
          ".B \"single arg with \"\"Q\"\"\""
          =?> (para $ strong $ str "single arg with \"Q\"")
        ]
  ]
