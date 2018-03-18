{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Readers.Org.Block (tests) where

import Prelude
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

  , "Comment Block" =:
      T.unlines [ "#+BEGIN_COMMENT"
                , "stuff"
                , "bla"
                , "#+END_COMMENT"] =?>
      (mempty::Blocks)

  , testGroup "Blocks and fragments"
    [ "HTML block" =:
      T.unlines [ "#+BEGIN_HTML"
                , "<aside>HTML5 is pretty nice.</aside>"
                , "#+END_HTML"
                ] =?>
      rawBlock "html" "<aside>HTML5 is pretty nice.</aside>\n"

    , "Quote block" =:
      T.unlines [ "#+BEGIN_QUOTE"
                , "/Niemand/ hat die Absicht, eine Mauer zu errichten!"
                , "#+END_QUOTE"
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
      T.unlines [ "#+BEGIN_VERSE"
                , "foo"
                , ""
                , "bar"
                , "#+END_VERSE"
                ] =?>
      lineBlock [ "foo", mempty, "bar" ]

    , "Verse block with varying indentation" =:
      T.unlines [ "#+BEGIN_VERSE"
                , "  hello darkness"
                , "my old friend"
                , "#+END_VERSE"
                ] =?>
      lineBlock [ "\160\160hello darkness", "my old friend" ]

    , "Raw block LaTeX" =:
      T.unlines [ "#+BEGIN_LaTeX"
                , "The category $\\cat{Set}$ is adhesive."
                , "#+END_LaTeX"
                ] =?>
      rawBlock "latex" "The category $\\cat{Set}$ is adhesive.\n"

    , "Raw LaTeX line" =:
      "#+LATEX: \\let\\foo\\bar" =?>
      rawBlock "latex" "\\let\\foo\\bar"

    , "Raw Beamer line" =:
      "#+beamer: \\pause" =?>
      rawBlock "beamer" "\\pause"

    , "Raw HTML line" =:
      "#+HTML: <aside>not important</aside>" =?>
      rawBlock "html" "<aside>not important</aside>"

    , "Export block HTML" =:
      T.unlines [ "#+BEGIN_export html"
                , "<samp>Hello, World!</samp>"
                , "#+END_export"
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
      (unlines [ "\\begin{equation}"
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

    , "Accept `ATTR_HTML` attributes for generic block" =:
      T.unlines [ "#+ATTR_HTML: :title hello, world :id test :class fun code"
                , "#+BEGIN_TEST"
                , "nonsense"
                , "#+END_TEST"
                ] =?>
      let attr = ("test", ["fun", "code", "TEST"], [("title", "hello, world")])
      in divWith attr (para "nonsense")
    ]

  , testGroup "Headers" Header.tests
  , testGroup "Figures" Figure.tests
  , testGroup "Lists" List.tests
  , testGroup "CodeBlocks" CodeBlock.tests
  , testGroup "Tables" Table.tests
  ]
