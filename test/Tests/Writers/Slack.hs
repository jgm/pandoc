{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.Slack (tests) where

import Prelude
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder


infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> TestTree
(=:) = test (purely (writePlain def) . toPandoc)


tests :: [TestTree]
tests = [ testGroup "text formatting"
          [
            "strongly emphasized text"
             =: strong "hello world!"
             =?> "*hello world!*"
          , "italics"
             =: emph "hello world"
             =?> "_hello world_"
          , "strikeout"
             =: strikeout "hello world"
             =?> "~hello world~"
          ]
        , testGroup "lists"
          [
            "simple bullet list"
            =: bulletList [plain (str "alfa"), plain (str "bravo")]
            =?> "* alfa\n* bravo"
          ]
        , testGroup "blockquote"
          [
            "standard blockquote"
            =: "we hold these truths to be self evident"
            =?> "> we hold these truths to be self evident"
          , "multiline blockquote"
            =: ("line 1" <> linebreak <> "line 2")
            =?> "> line 1\n> line 2"
          ]
        , testGroup "monospaced"
          []
        , testGroup "code blocks"
          [
            "single line"
            =: codeBlock "import this"
            =?> "`import this`"
          ]
        ]
