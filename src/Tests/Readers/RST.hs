{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Tests.Readers.RST (tests) where

import Text.Pandoc.Definition
import Test.Framework
import Tests.Helpers
import Tests.Arbitrary()
import Text.Pandoc.Builder
import Text.Pandoc

rst :: String -> Pandoc
rst = readRST defaultParserState{ stateStandalone = True }

infix 5 =:
(=:) :: ToString c
     => String -> (String, c) -> Test
(=:) = test rst

tests :: [Test]
tests = [ "field list" =:
          [_LIT|
:Hostname: media08
:IP address: 10.0.0.19
:Size: 3ru
:Date: 2001-08-16
:Version: 1
:Authors: - Me
              - Myself
              - I
:Indentation: Since the field marker may be quite long, the second
   and subsequent lines of the field body do not have to line up
   with the first line, but they must be indented relative to the
   field name marker, and they must line up with each other.
:Parameter i: integer
|]         =?> ( setAuthors ["Me","Myself","I"]
               $ setDate "2001-08-16"
               $ doc
               $ definitionList [ (str "Hostname", [para "media08"])
                                , (str "IP address", [para "10.0.0.19"])
                                , (str "Size", [para "3ru"])
                                , (str "Version", [para "1"])
                                , (str "Indentation", [para "Since the field marker may be quite long, the second and subsequent lines of the field body do not have to line up with the first line, but they must be indented relative to the field name marker, and they must line up with each other."])
                                , (str "Parameter i", [para "integer"])
                              ])
        ]

