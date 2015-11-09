{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Tests.Readers.RST (tests) where

import Text.Pandoc.Definition
import Test.Framework
import Tests.Helpers
import Tests.Arbitrary()
import Text.Pandoc.Builder
import Text.Pandoc
import Text.Pandoc.Error

rst :: String -> Pandoc
rst = handleError . readRST def{ readerStandalone = True }

infix 4 =:
(=:) :: ToString c
     => String -> (String, c) -> Test
(=:) = test rst

tests :: [Test]
tests = [ "line block with blank line" =:
          "| a\n|\n|  b" =?> para (str "a") <>
                             para (str "\160b")
        , testGroup "field list"
          [ "general" =: unlines
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
             =?> ( doc
                 $ para "para" <>
                   definitionList [ (str "Hostname", [para "media08"])
                                  , (text "IP address", [para "10.0.0.19"])
                                  , (str "Size", [para "3ru"])
                                  , (str "Version", [para "1"])
                                  , (str "Indentation", [para "Since the field marker may be quite long, the second and subsequent lines of the field body do not have to line up with the first line, but they must be indented relative to the field name marker, and they must line up with each other."])
                                  , (text "Parameter i", [para "integer"])
                                  , (str "Final", [para "item on two lines"])
                                  ])
          , "metadata" =: unlines
             [ "====="
             , "Title"
             , "====="
             , "--------"
             , "Subtitle"
             , "--------"
             , ""
             , ":Version: 1"
             ]
             =?> ( setMeta "version" (para "1")
                 $ setMeta "title" ("Title" :: Inlines)
                 $ setMeta "subtitle" ("Subtitle" :: Inlines)
                 $ doc mempty )
          , "with inline markup" =: unlines
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
             =?> ( setMeta "date" (str "today")
                 $ doc
                 $ definitionList [ (emph "one", [para "emphasis"])
                                  , (link "http://example.com" "" "two", [para "reference"])
                                  , (link "http://example.org" "" "three", [para "another one"])
                                  , (code "four", [para "literal"])
                                  ])
          ]
        , "URLs with following punctuation" =:
          ("http://google.com, http://yahoo.com; http://foo.bar.baz.\n" ++
           "http://foo.bar/baz_(bam) (http://foo.bar)") =?>
          para (link "http://google.com" "" "http://google.com" <> ", " <>
                link "http://yahoo.com" "" "http://yahoo.com" <> "; " <>
                link "http://foo.bar.baz" "" "http://foo.bar.baz" <> ". " <>
                link "http://foo.bar/baz_(bam)" "" "http://foo.bar/baz_(bam)"
                <> " (" <> link "http://foo.bar" "" "http://foo.bar" <> ")")
        , "Reference names with special characters" =:
                   ("A-1-B_2_C:3:D+4+E.5.F_\n\n" ++
                   ".. _A-1-B_2_C:3:D+4+E.5.F: https://example.com\n") =?>
                   para (link "https://example.com" "" "A-1-B_2_C:3:D+4+E.5.F")
        , testGroup "literal / line / code blocks"
          [ "indented literal block" =: unlines
            [ "::"
            , ""
            , "  block quotes"
            , ""
            , "  can go on for many lines"
            , "but must stop here"]
            =?> (doc $
                 codeBlock "block quotes\n\ncan go on for many lines" <>
                 para "but must stop here")
          , "line block with 3 lines" =: "| a\n| b\n| c"
            =?> para ("a" <> linebreak <>  "b" <> linebreak <> "c")
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
          , "code role" =: ":code:`a`" =?> para (codeWith ("", ["sourceCode"], []) "a")
          , "inherited code role" =: ".. role:: codeLike(code)\n\n:codeLike:`a`"
            =?> para (codeWith ("", ["codeLike", "sourceCode"], []) "a")
          , "custom code role with language field"
            =: ".. role:: lhs(code)\n    :language: haskell\n\n:lhs:`a`"
            =?> para (codeWith ("", ["lhs", "haskell","sourceCode"], []) "a")
          , "custom role with unspecified parent role"
            =: ".. role:: classy\n\n:classy:`text`"
            =?> para (spanWith ("", ["classy"], []) "text")
          , "role with recursive inheritance"
            =: ".. role:: haskell(code)\n.. role:: lhs(haskell)\n\n:lhs:`text`"
            =?> para (codeWith ("", ["lhs", "haskell", "sourceCode"], []) "text")
          , "unknown role" =: ":unknown:`text`" =?> para (str "text")
          ]
        ]
