{-# LANGUAGE OverloadedStrings #-}
module Tests.Readers.AsciiDoc (tests) where

import qualified Test.Framework as F

import Tests.Helpers
import Tests.Arbitrary()
import Text.Pandoc.Definition
import Text.Pandoc.Builder
import Text.Pandoc.Error
import Text.Pandoc

asciidoc :: String -> Pandoc
asciidoc = handleError . (readAsciiDoc def)

infix 4 =:
(=:) :: ToString c
     => String -> (String, c) -> F.Test
(=:) = test asciidoc

infix 4 =!:
(=!:) :: String -> (String, String) -> F.Test
(=!:) = notIn asciidoc

tests :: [F.Test]
tests = [ F.testGroup "Titles"
          [ "level 2 with line prefix =" =:
            "== title"
            =?> header 2 (str "title")

            , "level 5 with line prefix =" =:
            "===== title"
            =?> header 5 (str "title")

            , "level 7 with line prefix = does not exist" =!:
            "======= title"
            =?> "Header"

            , "level 1 with underline" =:
            "title\n====="
            =?> header 1 (str "title")

            , "level 2 with underline" =:
            "title\n-----"
            =?> header 2 (str "title")

            , "level 1 with underline not enough symbols" =!:
            "title\n=="
            =?> "Header"

            , "level 2 with underline not enough symbols" =!:
            "title\n--"
            =?> "Header"

            , "level 1 with underline too many symbols" =!:
            "title\n================"
            =?> "Header"

            , "level 2 with underline too many symbols" =!:
            "title\n----------------"
            =?> "Header"

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

            , "horizontal rule markers mixed * - * -> no horizontalRule" =!:
            "* - *"
            =?> "HorizontalRule"

            , "horizontal rule markers mixed - - * -> no horizontalRule" =!:
            "- - *"
            =?> "HorizontalRule"

            , "horizontal rule markers mixed -** -> no horizontalRule" =!:
            "-**"
            =?> "HorizontalRule"

            , "horizontal rule markdown markers --- are exact, no additional dashes" =!:
            "----"
            =?> "HorizontalRule"

          ]
          , F.testGroup "HyperLinks"
          [ "link without an alias" =:
          "aa http://www.foo.bar"
          =?> para ((str "aa")
                    <> space
                    <> (link ("http://www.foo.bar") ("www.foo.bar") (str "http://www.foo.bar")))

          , "link with an alias" =:
          "aa http://www.foo.bar[foo]"
          =?> para ((str "aa")
                    <> space
                    <> (link ("http://www.foo.bar") ("www.foo.bar") (str "foo")))

          , "link with an empty alias" =:
          "aa http://www.foo.bar[]"
          =?> para ((str "aa")
                    <> space
                    <> (link ("http://www.foo.bar") ("www.foo.bar") (str "http://www.foo.bar")))

          , "link with a complex alias" =:
          "http://www.foo.bar[*foo*]"
          =?> para (link ("http://www.foo.bar") ("www.foo.bar") (strong "foo"))
          ]
          , F.testGroup "Strong"
          [ "a strong word" =:
          "a *strong* word"
          =?> para ((str "a")
                    <> space
                    <> strong (str "strong")
                    <> space
                    <> (str "word"))

          , "strong spanning several lines" =:
          "a *strong\nmultiline* text"
          =?> para ((str "a")
                    <> space
                    <> strong ((str "strong") <> space <> (str "multiline"))
                    <> space
                    <> (str "text"))

          , "not strong due to space after opening char" =!:
          "a * strong* text"
          =?> "Strong"

          , "not strong due to space after opening char" =!:
          "a * strong* text"
          =?> "Strong"

          , "not strong due to space before closing char" =!:
          "a *strong * text"
          =?> "Strong"

          , "not strong due to alphanum after closing char" =!:
          "a *strong*ornot text"
          =?> "Strong"
          ]

          , F.testGroup "Emphasized"
          [ "an emphasized word" =:
          "an _emphasized_ word"
          =?> para ((str "an")
                    <> space
                    <> emph (str "emphasized")
                    <> space
                    <> (str "word"))

          , "emphasized spanning several lines" =:
          "a _emphasized\nmultiline_ text"
          =?> para ((str "a")
                    <> space
                    <> emph ((str "emphasized") <> space <> (str "multiline"))
                    <> space
                    <> (str "text"))

          , "not emphasized due to space after opening char" =!:
          "a * emphasized* text"
          =?> "Emph"

          , "not emphasized due to space after opening char" =!:
          "a * emphasized* text"
          =?> "Emph"

          , "not emphasized due to space before closing char" =!:
          "a *emphasized * text"
          =?> "Emph"

          , "not emphasized due to alphanum after closing char" =!:
          "a *emphasized*ornot text"
          =?> "Emph"
          ]
          , F.testGroup "Strong + Emphasized"
          [
          "both strong and emphasized" =:
          "_*text*_"
          =?> (para . emph . strong . str) "text"
          ]
          , F.testGroup "Page Break"
          [ "a page break" =:
          "a text\n<<<\nwith a page break"
          =?> para ((str "a")
                    <> space <> (str "text") <> space)
              <> para ((str "with") <> space <> (str "a")
                      <> space <> (str "page") <> space <> (str "break"))
          ]
        ]
