{-# LANGUAGE OverloadedStrings #-}
module Tests.Readers.AsciiDoc (tests) where

import Text.Pandoc.Definition
import qualified Test.Framework as F
import Test.HUnit (assertBool)
import Test.Framework.Providers.HUnit
import Tests.Helpers
import Tests.Arbitrary()
import Text.Pandoc.Builder
import Text.Pandoc

asciidoc :: String -> Pandoc
asciidoc = readAsciiDoc def

asciidocBlocks :: String -> [Block]
asciidocBlocks s = blocks
  where Pandoc _ blocks = asciidoc s

infix 4 =:
(=:) :: ToString c
     => String -> (String, c) -> F.Test
(=:) = test asciidoc


tests :: [F.Test]
tests = [ F.testGroup "Titles"
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
          , F.testGroup "HorizontalRule"
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

            , testCase "horizontal rule markdown markers * * *" $
               assertBool "horizontal rule markdown markers * * *" $
                 elem HorizontalRule (asciidocBlocks "* * *")

            , "horizontal rule markdown markers --- are exact, no additional dashes" =:
            "----"
            =?> para (str "----")

            , testCase "horizontal rule markdown markers --- are exact, no additional dashes" $
               assertBool "horizontal rule markdown markers --- are exact, no additional dashes" $
                 not (elem HorizontalRule (asciidocBlocks "----"))

          ]
        ]
