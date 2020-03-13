{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.Creole
   Copyright   : © 2017 Sascha Wilde
                   2017-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : Sascha Wilde <wilde@sha-bang.de>
   Stability   : alpha
   Portability : portable

Tests for the creole reader.
-}
module Tests.Readers.Creole (tests) where

import Prelude
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

creole :: Text -> Pandoc
creole = purely $ readCreole def{ readerStandalone = True }

infix 4 =:
(=:) :: ToString c
     => String -> (Text, c) -> TestTree
(=:) = test creole

tests :: [TestTree]
tests = [
  testGroup "Basic Text Formatting" [
          "bold, single line, fully delimited" =:
          "only **bold** is bold"
          =?> para ("only " <> strong "bold" <> " is bold")
        , "italics, single line, fully delimited" =:
          "only //this// is in italics"
          =?> para ("only " <> emph "this" <> " is in italics")
        , "bold in italics, fully delimited" =:
          "//**this**// is in bold italics"
          =?> para (emph (strong "this") <> " is in bold italics")
        , "italics in bold, fully delimited" =:
          "**//this//** is in bold italics"
          =?> para (strong (emph "this") <> " is in bold italics")

        , "escape bold marker" =:
          "~**not bold" =?> para "**not bold"
        , "escape italics marker" =:
          "~//not in italics" =?> para "//not in italics"

        , "inline nowiki, simple" =:
          "this is {{{**not** ~interpreted}}} at all"
          =?> para ("this is " <> code "**not** ~interpreted" <> " at all")
        , "inline nowiki, curly braces inside" =:
          "this is {{{{{{//including// some `}' chars}}}}}}"
          =?> para ("this is " <> code "{{{//including// some `}' chars}}}")

        , "placeholder" =:
          "foo <<<place holder>>> bar"
          =?> para "foo bar"
        , "placeholder escaped" =:
          "foo ~<<<no place holder>>> bar"
          =?> para "foo <<<no place holder>>> bar"
        ]
  , testGroup "Headers" [
          "header level 1, no space, no trailing =" =:
          "= Top-Level Header"
          =?> header 1 (str "Top-Level Header")
        , "header level 1, leading space, trailing =" =:
          " = Top-Level Header = "
          =?> header 1 (str "Top-Level Header")
        , "header level 2, no space, no trailing =" =:
          "== Second Level"
          =?> header 2 (str "Second Level")
        , "header level 2, leading space, no trailing =" =:
          "   == Second Level"
          =?> header 2 (str "Second Level")
        , "header level 3, no space, no trailing =" =:
          "=== Third"
          =?> header 3 (str "Third")
        , "header level 3, no space, > 3 trailing =" =:
          "=== Third ======="
          =?> header 3 (str "Third")
        , "header level 4, no space, no trailing =" =:
          "==== Fourth Level Heading"
          =?> header 4 (str "Fourth Level Heading")
        , "header level 4, no space, < 4 trailing =" =:
          "==== Fourth Level Heading =="
          =?> header 4 (str "Fourth Level Heading")
        , "header level 5, no space, no trailing =" =:
          "===== Fifth"
          =?> header 5 (str "Fifth")
        , "header level 6, no space, no trailing =" =:
          "====== Sixth"
          =?> header 6 (str "Sixth")
        ]
  , testGroup "Paragraphs" [
          "paragraphs: multiple, one line" =:
          "first line\n\nanother line\n"
          =?> para "first line" <> para "another line"
          ]
  , testGroup "Lists" [
          "unordered list, two entries, one separating space" =:
          "* foo\n* bar"
          =?> bulletList [ plain "foo", plain "bar" ]
        , "unordered list, three entries, one separating space" =:
          "* foo\n* bar\n* baz"
          =?> bulletList [ plain "foo", plain "bar", plain "baz" ]
        , "para followed by, unordered list, two entries, one separating space" =:
          "blubber\n* foo\n* bar"
          =?> para "blubber" <> bulletList [ plain "foo", plain "bar" ]
        , "nested unordered list, one separating space" =:
          "* foo\n** bar\n** baz\n* blubb"
          =?> bulletList [ plain "foo"
                         <> bulletList [ plain "bar", plain "baz" ]
                         , plain "blubb" ]
        , "nested many unordered lists, one separating space" =:
          ("* foo\n** bar\n*** third\n*** third two\n** baz\n*** third again\n"
           <> "**** fourth\n***** fith\n* blubb")
          =?> bulletList [ plain "foo"
                           <> bulletList [ plain "bar"
                                           <> bulletList [ plain "third"
                                                         , plain "third two"]
                                         , plain "baz"
                                           <> bulletList [ plain "third again"
                                                         <> bulletList [
                                                             plain "fourth"
                                                             <> bulletList [
                                                                 plain "fith"
                                                                 ]
                                                             ]
                                                         ]
                                         ]
                         , plain "blubb" ]
        , "nested unordered list, mixed separating space" =:
          "*foo\n   **   bar\n    **baz\n *      blubb"
          =?> bulletList [ plain "foo"
                         <> bulletList [ plain "bar", plain "baz" ]
                         , plain "blubb" ]
        , "nested unordered list, one separating space, trailing space" =:
          "* foo \n** bar  \n** baz \n* blubb  "
          =?> bulletList [ plain "foo"
                         <> bulletList [ plain "bar", plain "baz" ]
                         , plain "blubb" ]
        , "ordered list, two entries, one separating space" =:
          "# foo\n# bar"
          =?> orderedList [ plain "foo", plain "bar" ]
        , "ordered list, three entries, one separating space" =:
          "# foo\n# bar\n# baz"
          =?> orderedList [ plain "foo", plain "bar", plain "baz" ]
        , "para followed by, ordered list, two entries, one separating space" =:
          "blubber\n# foo\n# bar"
          =?> para "blubber" <> orderedList [ plain "foo", plain "bar" ]
        , "nested ordered list, one separating space" =:
          "# foo\n## bar\n## baz\n# blubb"
          =?> orderedList [ plain "foo"
                         <> orderedList [ plain "bar", plain "baz" ]
                         , plain "blubb" ]
        , "nested ordered list, one separating space, trailing space" =:
          "# foo \n## bar  \n## baz \n# blubb  "
          =?> orderedList [ plain "foo"
                         <> orderedList [ plain "bar", plain "baz" ]
                         , plain "blubb" ]
        , "nested many ordered lists, one separating space" =:
          ("# foo\n## bar\n### third\n### third two\n## baz\n### third again\n"
           <> "#### fourth\n##### fith\n# blubb")
          =?> orderedList [ plain "foo"
                           <> orderedList [ plain "bar"
                                           <> orderedList [ plain "third"
                                                         , plain "third two"]
                                         , plain "baz"
                                           <> orderedList [ plain "third again"
                                                         <> orderedList [
                                                             plain "fourth"
                                                             <> orderedList [
                                                                 plain "fith"
                                                                 ]
                                                             ]
                                                         ]
                                         ]
                         , plain "blubb" ]
        , "nested ordered list, mixed separating space" =:
          "#foo\n   ##   bar\n    ##baz\n #      blubb"
          =?> orderedList [ plain "foo"
                         <> orderedList [ plain "bar", plain "baz" ]
                         , plain "blubb" ]
        , "mixed nested ordered and unordered lists, one separating space" =:
          ("# foo\n** bar\n### third\n### third two\n** baz\n### third again\n"
           <> "#### fourth\n***** fith\n# blubb")
          =?> orderedList [ plain "foo"
                           <> bulletList [ plain "bar"
                                           <> orderedList [ plain "third"
                                                         , plain "third two"]
                                         , plain "baz"
                                           <> orderedList [ plain "third again"
                                                         <> orderedList [
                                                             plain "fourth"
                                                             <> bulletList [
                                                                 plain "fith"
                                                                 ]
                                                             ]
                                                         ]
                                         ]
                         , plain "blubb" ]
        ]
  , testGroup "NoWiki" [
          "quoted block, simple" =:
          "{{{\nfoo bar\n  //baz//\n}}}"
          =?> codeBlock "foo bar\n  //baz//"
        , "quoted block, curly bracket exception" =:
          "{{{\nfoo bar\n  }}}\nbaz\n }}}\n}}}"
          =?> codeBlock "foo bar\n }}}\nbaz\n}}}"
        , "forced line breaks" =:
          "{{{no break!\\\\here}}} but a break\\\\here!"
          =?> para (code "no break!\\\\here" <> " but a break"
                    <> linebreak <> "here!"),
          "quoted block, after trailing white space" =:
          "this is a paragraph  \n{{{\nfoo bar\n  //baz//\n}}}"
          =?> para "this is a paragraph" <> codeBlock "foo bar\n  //baz//"
        ]
  , testGroup "Images and Links" [
          "image simple" =:
          "{{foo.png}}" =?> para (image "foo.png" "" (str ""))
        , "image with alt text" =:
          "Image of a bar: {{/path/to/bar.png|A Bar}} look at it!"
          =?> para ("Image of a bar: "
                    <> image "/path/to/bar.png" "" (str "A Bar") <> " look at it!")

        , "auto link" =:
          "foo http://foo.example.com/bar/baz.html bar"
          =?> para ("foo "
                    <> link "http://foo.example.com/bar/baz.html" ""
                     (str "http://foo.example.com/bar/baz.html")
                    <> " bar")
        , "escaped auto link" =:
          "foo ~http://foo.example.com/bar/baz.html bar"
          =?> para "foo http://foo.example.com/bar/baz.html bar"
        , "wiki link simple" =:
          "foo [[http://foo.example.com/foo.png]] bar"
          =?> para ("foo "
                    <> link "http://foo.example.com/foo.png" ""
                     (str "http://foo.example.com/foo.png")
                    <> " bar")
        , "wiki link with name" =:
          "foo [[http://foo.example.com/foo.png|my link]] bar"
          =?> para ("foo "
                    <> link "http://foo.example.com/foo.png" ""
                     (str "my link")
                    <> " bar")
        , "image link" =:
          "[[http://foo.example.com/|{{foo.png}}]]"
          =?> para (link "http://foo.example.com/" "" (image "foo.png" "" (str "")))
        ]
  , testGroup "Table" [
          "Table with Header" =:
          T.unlines [ "|= Foo |= Bar |= Baz  |"
                    , "| One  | Two  | Three |"
                    , "| 1    | 2    | 3     |"
                    , "| A    | B    | C     |"
                    ]
          =?> simpleTable
            [plain "Foo", plain "Bar" , plain "Baz"]
            [[plain "One", plain "Two" , plain "Three"]
            ,[plain "1", plain "2" , plain "3"]
            ,[plain "A", plain "B" , plain "C"]]
        , "Table without Header" =:
          T.unlines [ "| One  | Two  | Three |"
                    , "| 1    | 2    | 3     |"
                    , "| A    | B    | C     |"
                    ]
          =?> simpleTable [mempty]
            [[plain "One", plain "Two" , plain "Three"]
            ,[plain "1", plain "2" , plain "3"]
            ,[plain "A", plain "B" , plain "C"]]
        , "Table without Header, no markers at line ends" =:
          T.unlines [ "| One  | Two  | Three"
                    , "| 1    | 2    | 3"
                    , "| A    | B    | C  "
                    ]
          =?> simpleTable [mempty]
            [[plain "One", plain "Two" , plain "Three"]
            ,[plain "1", plain "2" , plain "3"]
            ,[plain "A", plain "B" , plain "C"]]
        , "Table with Header, with formatting" =:
          T.unlines [ "|= **Foo**        |= **Bar** |= **Baz**  |"
                    , "|//one// element  |//second// elt|Three  |"
                    , "| {{{1}}}    | {{{{}}}}    | [[link]]    |"
                    ]
          =?> simpleTable
            [plain $ strong "Foo", plain $ strong "Bar" , plain $ strong "Baz"]
            [[plain (emph "one" <> " element"), plain (emph "second" <> " elt")
             ,plain "Three"]
            ,[plain $ code "1", plain $ code "{}"
             ,plain $ link "link" "" (str "link")]]
        ]
  ]
