{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Tests.Readers.RST (tests) where

import Text.Pandoc.Definition
import Test.Framework
import Tests.Helpers
import Tests.Arbitrary()
import Text.Pandoc.Builder
import Text.Pandoc
import Data.Monoid (mempty)

rst :: String -> Pandoc
rst = readRST def{ readerStandalone = True }

infix 4 =:
(=:) :: ToString c
     => String -> (String, c) -> Test
(=:) = test rst

tests :: [Test]
tests = [ "line block with blank line" =:
          "| a\n|\n|  b" =?> para (str "a") <>
                             para (str "\160b")
        , "field list" =: unlines
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
                                , (str "IP address", [para "10.0.0.19"])
                                , (str "Size", [para "3ru"])
                                , (str "Version", [para "1"])
                                , (str "Indentation", [para "Since the field marker may be quite long, the second and subsequent lines of the field body do not have to line up with the first line, but they must be indented relative to the field name marker, and they must line up with each other."])
                                , (str "Parameter i", [para "integer"])
                                , (str "Final", [para "item on two lines"])
                              ])
        , "initial field list" =: unlines
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
        , "URLs with following punctuation" =:
          ("http://google.com, http://yahoo.com; http://foo.bar.baz.\n" ++
           "http://foo.bar/baz_(bam) (http://foo.bar)") =?>
          para (link "http://google.com" "" "http://google.com" <> ", " <>
                link "http://yahoo.com" "" "http://yahoo.com" <> "; " <>
                link "http://foo.bar.baz" "" "http://foo.bar.baz" <> ". " <>
                link "http://foo.bar/baz_(bam)" "" "http://foo.bar/baz_(bam)"
                <> " (" <> link "http://foo.bar" "" "http://foo.bar" <> ")")
        ]

