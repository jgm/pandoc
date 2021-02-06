{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.Org.Block
   Copyright   : © 2014-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
   Stability   : alpha
   Portability : portable

Tests parsing of org blocks.
-}
module Tests.Readers.Org.Block (tests) where

import Test.Tasty (TestTree, testGroup)
import Tests.Helpers ((=?>))
import Tests.Readers.Org.Shared ((=:), spcSep)
import Text.Pandoc.Builder
import qualified Data.Text as T
import qualified Tests.Readers.Org.Block.CodeBlock as CodeBlock
import qualified Tests.Readers.Org.Block.Figure as Figure
import qualified Tests.Readers.Org.Block.Header as Header
import qualified Tests.Readers.Org.Block.List as List
import qualified Tests.Readers.Org.Block.Table as Table

tests :: [TestTree]
tests =
  [ "Paragraph" =:
      "Paragraph\n" =?>
      para "Paragraph"

  , "Paragraph starting with an asterisk" =:
      "*five" =?>
      para "*five"

  , "Paragraph containing asterisk at beginning of line" =:
      T.unlines [ "lucky"
                , "*star"
                ] =?>
      para ("lucky" <> softbreak <> "*star")

  , "Example block" =:
      T.unlines [ ": echo hello"
                , ": echo dear tester"
                ] =?>
      codeBlockWith ("", ["example"], []) "echo hello\necho dear tester\n"

  , "Example block surrounded by text" =:
      T.unlines [ "Greetings"
                , ": echo hello"
                , ": echo dear tester"
                , "Bye"
                ] =?>
      mconcat [ para "Greetings"
              , codeBlockWith ("", ["example"], [])
                              "echo hello\necho dear tester\n"
              , para "Bye"
              ]

  , "Horizontal Rule" =:
      T.unlines [ "before"
                , "-----"
                , "after"
                ] =?>
      mconcat [ para "before"
              , horizontalRule
              , para "after"
              ]

  , "Not a Horizontal Rule" =:
      "----- em and en dash" =?>
      para "\8212\8211 em and en dash"

  , testGroup "Comments"
    [ "Comment Block" =:
      T.unlines [ "#+begin_comment"
                , "stuff"
                , "bla"
                , "#+end_comment"] =?>
      (mempty::Blocks)

    , "Comment line" =:
      T.unlines [ "# this is a comment" ] =?>
      (mempty :: Blocks)

    , "Empty comment line" =:
      T.unlines [ "  #" ] =?>
      (mempty :: Blocks)
    ]

  , testGroup "Blocks and fragments"
    [ "HTML block" =:
      T.unlines [ "#+begin_html"
                , "<aside>HTML5 is pretty nice.</aside>"
                , "#+end_html"
                ] =?>
      rawBlock "html" "<aside>HTML5 is pretty nice.</aside>\n"

    , "Quote block" =:
      T.unlines [ "#+begin_quote"
                , "/Niemand/ hat die Absicht, eine Mauer zu errichten!"
                , "#+end_quote"
                ] =?>
      blockQuote (para (spcSep [ emph "Niemand", "hat", "die", "Absicht,"
                               , "eine", "Mauer", "zu", "errichten!"
                               ]))

    , "Verse block" =:
      T.unlines [ "The first lines of Goethe's /Faust/:"
                , "#+begin_verse"
                , "Habe nun, ach! Philosophie,"
                , "Juristerei und Medizin,"
                , "Und leider auch Theologie!"
                , "Durchaus studiert, mit heißem Bemühn."
                , "#+end_verse"
                ] =?>
      mconcat
        [ para $ spcSep [ "The", "first", "lines", "of"
                        , "Goethe's", emph "Faust" <> ":"]
        , lineBlock
          [ "Habe nun, ach! Philosophie,"
          , "Juristerei und Medizin,"
          , "Und leider auch Theologie!"
          , "Durchaus studiert, mit heißem Bemühn."
          ]
        ]

    , "Verse block with blank lines" =:
      T.unlines [ "#+begin_verse"
                , "foo"
                , ""
                , "bar"
                , "#+end_verse"
                ] =?>
      lineBlock [ "foo", mempty, "bar" ]

    , "Verse block with varying indentation" =:
      T.unlines [ "#+begin_verse"
                , "  hello darkness"
                , "my old friend"
                , "#+end_verse"
                ] =?>
      lineBlock [ "\160\160hello darkness", "my old friend" ]

    , "Raw block LaTeX" =:
      T.unlines [ "#+begin_latex"
                , "The category $\\cat{Set}$ is adhesive."
                , "#+end_latex"
                ] =?>
      rawBlock "latex" "The category $\\cat{Set}$ is adhesive.\n"

    , "Raw LaTeX line" =:
      "#+latex: \\let\\foo\\bar" =?>
      rawBlock "latex" "\\let\\foo\\bar"

    , "Raw Beamer line" =:
      "#+beamer: \\pause" =?>
      rawBlock "beamer" "\\pause"

    , "Raw HTML line" =:
      "#+html: <aside>not important</aside>" =?>
      rawBlock "html" "<aside>not important</aside>"

    , "Export block HTML" =:
      T.unlines [ "#+begin_export html"
                , "<samp>Hello, World!</samp>"
                , "#+end_export"
                ] =?>
      rawBlock "html" "<samp>Hello, World!</samp>\n"

    , "LaTeX fragment" =:
      T.unlines [ "\\begin{equation}"
                , "X_i = \\begin{cases}"
                , "      G_{\\alpha(i)} & \\text{if }\\alpha(i-1) = \\alpha(i)\\\\"
                , "      C_{\\alpha(i)} & \\text{otherwise}"
                , "      \\end{cases}"
                , "\\end{equation}"
                ] =?>
      rawBlock "latex"
      (T.unlines [ "\\begin{equation}"
               , "X_i = \\begin{cases}"
               , "      G_{\\alpha(i)} & \\text{if }\\alpha(i-1) =" <>
                 " \\alpha(i)\\\\"
               , "      C_{\\alpha(i)} & \\text{otherwise}"
               , "      \\end{cases}"
               , "\\end{equation}"
               ])

    , "Convert blank lines in blocks to single newlines" =:
      T.unlines [ "#+begin_html"
                , ""
                , "<span>boring</span>"
                , ""
                , "#+end_html"
                ] =?>
      rawBlock "html" "\n<span>boring</span>\n\n"

    , "Accept `attr_html` attributes for generic block" =:
      T.unlines [ "#+attr_html: :title hello, world :id test :class fun code"
                , "#+begin_test"
                , "nonsense"
                , "#+end_test"
                ] =?>
      let attr = ("test", ["fun", "code", "test"], [("title", "hello, world")])
      in divWith attr (para "nonsense")
    ]

  , testGroup "Headers" Header.tests
  , testGroup "Figures" Figure.tests
  , testGroup "Lists" List.tests
  , testGroup "CodeBlocks" CodeBlock.tests
  , testGroup "Tables" Table.tests
  ]
