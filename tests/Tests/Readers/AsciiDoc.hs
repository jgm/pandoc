{-# LANGUAGE OverloadedStrings #-}
module Tests.Readers.AsciiDoc (tests) where

import Text.Pandoc.Definition
import Test.Framework
import Tests.Helpers
import Tests.Arbitrary()
import Text.Pandoc.Builder
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
            "== title"
            =?> header 2 (str "title")

            , "level 5 with line prefix =" =:
            "===== title"
            =?> header 5 (str "title")

            , "level 7 with line prefix = does not exist" =:
            "======= title"
            =?> para ((str "=======") <> space <> (str "title"))

            , "level 1 with underline" =:
            "title\n====="
            =?> header 1 (str "title")

            , "level 2 with underline" =:
            "title\n-----"
            =?> header 2 (str "title")

            , "level 1 with underline not enough symbols" =:
            "title\n=="
            =?> para ((str "title") <>
                         space <> (str "=="))

            , "level 2 with underline not enough symbols" =:
            "title\n--"
            =?> para ((str "title") <>
                         space <> (str "--"))

            , "level 1 with underline too many symbols" =:
            "title\n================"
            =?> para ((str "title") <>
                         space <> (str "================"))

            , "level 2 with underline too many symbols" =:
            "title\n----------------"
            =?> para ((str "title") <>
                         space <> (str "----------------"))

            , "level 2 with markdown style" =:
            "## title\n"
            =?> header 2 (str "title")
          ]
          , testGroup "HorizontalRule"
          [ "horizontal rule marker with the minimum 3 ' characters" =:
            "'''"
            =?> horizontalRule

            , "horizontal rule marker with more chars" =:
            "'''''''''''''''''"
            =?> horizontalRule

            , "horizontal rule marker with trailing spaces" =:
            "'''''''          "
            =?> horizontalRule

            , "horizontal rule markers preceded by spaces is not a horizontal rule" =:
            " '''"
            =?> blockQuote ( plain (str "'''") )

            , "horizontal rule markers must be preceded by a blankline" =:
            "\n'''"
            =?> horizontalRule

            , "horizontal rule markers not preceded by a blankline is not a horizontal rule" =:
            "aaa\n'''"
            =?> para ((str "aaa") <> space <> (str "'''"))

            , "horizontal rule markdown markers ---" =:
            "---"
            =?> horizontalRule

            , "horizontal rule markdown markers - - -" =:
            "- - -"
            =?> horizontalRule

            , "horizontal rule markdown markers ***" =:
            "***"
            =?> horizontalRule

            , "horizontal rule markdown markers * * *" =:
            "* * *"
            =?> horizontalRule

            , "horizontal rule markdown markers --- are exact, no additional dashes" =:
            "----"
            =?> para (str "----")
          ]
        ]
