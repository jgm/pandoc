{-# LANGUAGE OverloadedStrings #-}

module Tests.XML (tests) where

import Data.Maybe (mapMaybe)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Walk (walk)

p_xml_roundtrip :: Pandoc -> Bool
p_xml_roundtrip d = d'' == d'
  where
    d' =
      walk
        ( compressBreaks
            . concatAdjacentStrings
            . compressMultipleSpaces
            . suppressEmptyStrings
        )
        d
    xml = purely (writeXML def) d'
    d'' = purely (readXML def) xml

suppressEmptyStrings :: [Inline] -> [Inline]
suppressEmptyStrings inlines = mapMaybe suppressEmptyString inlines
  where
    suppressEmptyString str@(Str s) = if s == "" then Nothing else Just str
    suppressEmptyString x = Just x

concatAdjacentStrings :: [Inline] -> [Inline]
concatAdjacentStrings ((Str s1) : (Str s2) : xs) = Str (s1 <> s2) : xs
concatAdjacentStrings (x : xs) = x : concatAdjacentStrings xs
concatAdjacentStrings [] = []

compressMultipleSpaces :: [Inline] -> [Inline]
compressMultipleSpaces (Space : Space : xs) = compressMultipleSpaces $ Space : xs
compressMultipleSpaces (x : xs) = x : compressMultipleSpaces xs
compressMultipleSpaces [] = []

compressBreaks :: [Inline] -> [Inline]
compressBreaks (sb1 : sb2 : xs) = case (isSpace sb1 || isBreak sb1, isBreak sb2, isSpace sb2) of
  (True, True, False) -> compressBreaks (sb2 : xs) -- break1, break2 -> break2
  (True, False, True) -> compressBreaks (sb1 : xs) -- break or space, space -> break or space
  _ -> sb1 : compressBreaks (sb2 : xs)
  where
    isBreak mb = mb == LineBreak || mb == SoftBreak
    isSpace ms = ms == Space
compressBreaks (x : xs) = x : compressBreaks xs
compressBreaks [] = []

tests :: [TestTree]
tests = [testProperty "p_xml_roundtrip" p_xml_roundtrip]