{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Tests.Readers.RST
   Copyright   : © 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Tests for the RST reader.
-}
module Tests.Readers.RST (tests) where

import Prelude
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

rst :: Text -> Pandoc
rst = purely $ readRST def{ readerStandalone = True }

infix 4 =:
(=:) :: ToString c
     => String -> (Text, c) -> TestTree
(=:) = test rst

tests :: [TestTree]
tests = [ "line block with blank line" =:
          "| a\n|\n|  b" =?> lineBlock [ "a", mempty, "\160b" ]
        , testGroup "field list"
          [ "general" =: T.unlines
             [ "para"
             , ""
             , ":Hostname: media08"
             , ":IP address: 10.0.0.19"
             , ":Size: 3ru"
             , ":Version: 1"
             , ":Indentation: Since the field marker may be quite long, the second"
             , "   and subsequent lines of the field body do not have to line up"
             , "   with the first line, but they must be indented relative to the"
             , "   field name marker, and they must line up with each other."
             , ":Parameter i: integer"
             , ":Final: item"
             , "  on two lines" ]
             =?>
              doc (para "para" <>
                   definitionList [ (str "Hostname", [para "media08"])
                                  , (text "IP address", [para "10.0.0.19"])
                                  , (str "Size", [para "3ru"])
                                  , (str "Version", [para "1"])
                                  , (str "Indentation", [para "Since the field marker may be quite long, the second\nand subsequent lines of the field body do not have to line up\nwith the first line, but they must be indented relative to the\nfield name marker, and they must line up with each other."])
                                  , (text "Parameter i", [para "integer"])
                                  , (str "Final", [para "item\non two lines"])
                                  ])
          , "metadata" =: T.unlines
             [ "====="
             , "Title"
             , "====="
             , "--------"
             , "Subtitle"
             , "--------"
             , ""
             , ":Version: 1"
             ]
             =?>
              setMeta "version" (para "1") (setMeta "title" ("Title" :: Inlines)
                 $ setMeta "subtitle" ("Subtitle" :: Inlines)
                 $ doc mempty)
          , "with inline markup" =: T.unlines
             [ ":*Date*: today"
             , ""
             , ".."
             , ""
             , ":*one*: emphasis"
             , ":two_: reference"
             , ":`three`_: another one"
             , ":``four``: literal"
             , ""
             , ".. _two: http://example.com"
             , ".. _three: http://example.org"
             ]
             =?>
              setMeta "date" (str "today") (doc
                 $ definitionList [ (emph "one", [para "emphasis"])
                                  , (link "http://example.com" "" "two", [para "reference"])
                                  , (link "http://example.org" "" "three", [para "another one"])
                                  , (code "four", [para "literal"])
                                  ])
          ]
        , "URLs with following punctuation" =:
          ("http://google.com, http://yahoo.com; http://foo.bar.baz.\n" <>
           "http://foo.bar/baz_(bam) (http://foo.bar)") =?>
          para (link "http://google.com" "" "http://google.com" <> ", " <>
                link "http://yahoo.com" "" "http://yahoo.com" <> "; " <>
                link "http://foo.bar.baz" "" "http://foo.bar.baz" <> ". " <>
                softbreak <>
                link "http://foo.bar/baz_(bam)" "" "http://foo.bar/baz_(bam)"
                <> " (" <> link "http://foo.bar" "" "http://foo.bar" <> ")")
        , "Reference names with special characters" =:
                   ("A-1-B_2_C:3:D+4+E.5.F_\n\n" <>
                   ".. _A-1-B_2_C:3:D+4+E.5.F: https://example.com\n") =?>
                   para (link "https://example.com" "" "A-1-B_2_C:3:D+4+E.5.F")
        , "Code directive with class and number-lines" =: T.unlines
            [ ".. code::python"
            , "   :number-lines: 34"
            , "   :class: class1 class2 class3"
            , ""
            , "  def func(x):"
            , "    return y"
            ]  =?>
              doc (codeBlockWith
                  ( ""
                  , ["python", "numberLines", "class1", "class2", "class3"]
                  , [ ("startFrom", "34") ]
                  )
                  "def func(x):\n  return y")
        , "Code directive with number-lines, no line specified" =: T.unlines
            [ ".. code::python"
            , "   :number-lines:"
            , ""
            , "  def func(x):"
            , "    return y"
            ]  =?>
              doc (codeBlockWith
                  ( ""
                  , ["python", "numberLines"]
                  , []
                  )
                  "def func(x):\n  return y")
        , testGroup "literal / line / code blocks"
          [ "indented literal block" =: T.unlines
            [ "::"
            , ""
            , "  block quotes"
            , ""
            , "  can go on for many lines"
            , "but must stop here"]
            =?>
              doc (
                 codeBlock "block quotes\n\ncan go on for many lines" <>
                 para "but must stop here")
          , "line block with 3 lines" =: "| a\n| b\n| c"
            =?> lineBlock ["a", "b", "c"]
          , "line blocks with blank lines" =: T.unlines
            [ "|"
            , ""
            , "|"
            , "| a"
            , "| b"
            , "|"
            , ""
            , "|"
            ] =?>
            lineBlock [""] <>
            lineBlock ["", "a", "b", ""] <>
            lineBlock [""]
          , "quoted literal block using >" =: "::\n\n> quoted\n> block\n\nOrdinary paragraph"
            =?> codeBlock "> quoted\n> block" <> para "Ordinary paragraph"
          , "quoted literal block using | (not  a line block)" =: "::\n\n| quoted\n| block\n\nOrdinary paragraph"
            =?> codeBlock "| quoted\n| block" <> para "Ordinary paragraph"
          , "class directive with single paragraph" =: ".. class:: special\n\nThis is a \"special\" paragraph."
            =?> divWith ("", ["special"], []) (para "This is a \"special\" paragraph.")
          , "class directive with two paragraphs" =: ".. class:: exceptional remarkable\n\n    First paragraph.\n\n    Second paragraph."
            =?> divWith ("", ["exceptional", "remarkable"], []) (para "First paragraph." <> para "Second paragraph.")
          , "class directive around literal block" =: ".. class:: classy\n\n::\n\n    a\n    b"
            =?> divWith ("", ["classy"], []) (codeBlock "a\nb")]
        , testGroup "interpreted text roles"
          [ "literal role prefix" =: ":literal:`a`" =?> para (code "a")
          , "literal role postfix" =: "`a`:literal:" =?> para (code "a")
          , "literal text" =: "``text``" =?> para (code "text")
          , "code role" =: ":code:`a`" =?> para (codeWith ("", [], []) "a")
          , "inherited code role" =: ".. role:: codeLike(code)\n\n:codeLike:`a`"
            =?> para (codeWith ("", ["codeLike"], []) "a")
          , "custom code role with language field"
            =: ".. role:: lhs(code)\n    :language: haskell\n\n:lhs:`a`"
            =?> para (codeWith ("", ["lhs", "haskell"], []) "a")
          , "custom role with unspecified parent role"
            =: ".. role:: classy\n\n:classy:`text`"
            =?> para (spanWith ("", ["classy"], []) "text")
          , "role with recursive inheritance"
            =: ".. role:: haskell(code)\n.. role:: lhs(haskell)\n\n:lhs:`text`"
            =?> para (codeWith ("", ["lhs", "haskell"], []) "text")
          , "unknown role" =: ":unknown:`text`" =?>
              para (codeWith ("",["interpreted-text"],[("role","unknown")]) "text")
          ]
        , testGroup "footnotes"
          [ "remove space before note" =: T.unlines
            [ "foo [1]_"
            , ""
            , ".. [1]"
            , "   bar"
            ] =?>
              para ("foo" <> note (para "bar"))
          ]
        , testGroup "inlines"
          [ "links can contain an URI without being parsed twice (#4581)" =:
            "`http://loc <http://loc>`__" =?>
            para (link "http://loc" "" "http://loc")
          , "inline markup cannot be nested" =:
            "**a*b*c**" =?>
            para (strong "a*b*c")
          , "bare URI parsing disabled inside emphasis (#4561)" =:
            "*http://location*" =?>
            para (emph (text "http://location"))
          , "include newlines" =:
            "**before\nafter**" =?>
            para (strong (text "before\nafter"))
          ]
        ]
