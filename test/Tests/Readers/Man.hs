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
      =?> (para $ strong $ emph $ text "foo bar")
    , "H1" =:
      ".SH The header\n"
      =?> header 2 (text "The header")
    , "H2" =:
      ".SS \"The header 2\""
      =?> header 3 (text "The header 2")
    , "Macro args" =:
      ".B \"single arg with \"\"Q\"\"\""
      =?> (para $ strong $ text "single arg with \"Q\"")
    , "comment" =:
      ".\\\"bla\naaa"
      =?> (para $ str "aaa")
    , "link" =:
      ".BR aa (1)"
      =?> (para $ link "../1/aa.1" "aa" (strong $ str "aa") <> (strong $ str " (1)"))
    ],
  testGroup "Escapes" [
      "fonts" =:
      "aa\\fIbb\\fRcc"
      =?> (para $ str "aa" <> (emph $ str "bb") <> str "cc")
    , "skip" =:
      "a\\%\\{\\}\\\n\\:b\\0"
      =?> (para $ str "ab")
    , "replace" =:
      "\\-\\ \\\\\\[lq]\\[rq]\\[em]\\[en]\\*(lq\\*(rq"
      =?> (para $ text "- \\“”—–“”")
    , "replace2" =:
      "\\t\\e\\`\\^\\|\\'"
      =?> (para $ text "\\`  `")
    , "comment  with \\\"" =:
      "Foo \\\" bar\n"
      =?> (para $ text "Foo")
    , "comment with \\#" =:
      "Foo\\#\nbar\n"
      =?> (para $ text "Foobar")
    ],
  testGroup "Lists" [
      "bullet" =:
      ".IP\nfirst\n.IP\nsecond"
      =?> bulletList [plain $ str "first", plain $ str "second"]
    , "ordered" =:
      ".IP 1 a\nfirst\n.IP 2 a\nsecond"
      =?> orderedListWith (1,Decimal,DefaultDelim) [plain $ str "first", plain $ str "second"]
    , "upper" =:
      ".IP A a\nfirst\n.IP B a\nsecond"
      =?> orderedListWith (1,UpperAlpha,DefaultDelim) [plain $ str "first", plain $ str "second"]
    , "nested" =:
      ".IP\nfirst\n.RS\n.IP\n1a\n.IP\n1b\n.RE"
      =?> bulletList [(plain $ str "first") <> (bulletList [plain $ str "1a", plain $ str "1b"])]
    ],
  testGroup "CodeBlocks" [
      "cb1"=:
      ".nf\naa\n\tbb\n.fi"
      =?> codeBlock "aa\n\tbb"
    ]
  ]
