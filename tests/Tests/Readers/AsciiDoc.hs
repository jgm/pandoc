{-# LANGUAGE OverloadedStrings #-}
module Tests.Readers.AsciiDoc (tests) where

import Text.Pandoc.Definition
import Test.Framework
import Tests.Helpers
import Tests.Arbitrary()
import Text.Pandoc.Builder
import qualified Data.Set as Set
import Text.Pandoc

asciidoc :: String -> Pandoc
asciidoc = readAsciiDoc def

infix 4 =:
(=:) :: ToString c
     => String -> (String, c) -> Test
(=:) = test asciidoc


tests :: [Test]
tests = [ testGroup "Titles"
          [ "level 2 with line prefix =" =:
            "== title level 2"
            =?> header 2 (str "title level 2")

            , "level 5 with line prefix =" =:
            "===== title level 5"
            =?> header 5 (str "title level 5")

            , "level 7 with line prefix = does not exist" =:
            "======= title level 7"
            =?> para (str "======= title level 7")

            , "level 1 with underline" =:
            "title level 1\n============="
            =?> header 1 (str "title level 1")

            , "level 2 with underline" =:
            "title level 2\n-------------"
            =?> header 2 (str "title level 2")

            , "level 1 with underline not enough symbols" =:
            "title level 1\n=========="
            =?> para (str "title level 1\n==========")

            , "level 2 with underline not enough symbols" =:
            "title level 2\n----------"
            =?> para (str "title level 2\n----------")

            , "level 1 with underline too many symbols" =:
            "title level 1\n================"
            =?> para (str "title level 1\n================")

            , "level 2 with underline too many symbols" =:
            "title level 2\n----------------"
            =?> para (str "title level 2\n----------------")
          ]
          , testGroup "HorizontalRule"
          [ "horizontal rule marker with the minimum 3 ' characters" =:
            "'''"
            =?> horizontalRule

            , "horizontal rule marker with more chars" =:
            "'''''''''''''''''"
            =?> horizontalRule
          ]
        ]
