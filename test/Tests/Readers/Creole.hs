{-# LANGUAGE OverloadedStrings #-}
module Tests.Readers.Creole (tests) where

import Data.Text (Text)
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
tests = [ "bold, single line, fully delimited" =:
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

        , "header level 1, no space, no trailing =" =:
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

        , "paragraphs: multiple, one line" =:
          "first line\n\nanother line\n"
          =?> para "first line" <> para "another line"
        , "unordered list, two entries, one separating space" =:
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
        ]
