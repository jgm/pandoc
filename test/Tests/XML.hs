{-# LANGUAGE OverloadedStrings #-}

module Tests.XML (tests) where

import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Walk (query)
import System.Process (terminateProcess)

p_xml_roundtrip :: Pandoc -> Property
p_xml_roundtrip d = isValidPandoc d ==> d' == d
  where
    xml = purely (writeXML def) d
    d' = purely (readXML def) xml

isValidPandoc :: Pandoc -> Bool
isValidPandoc d = not (hasEmptyStr || hasSuccStrs || hasSuccSpaces)
  where
    inlines = (query extractInlines d) ++ (query extractMetaInlines d)
    hasEmptyStr = any (any (\i -> i == Str "")) inlines
    hasSuccStrs = any  hasSuccessiveStr inlines
    hasSuccSpaces = any hasMultipleSpace inlines

hasSuccessiveStr :: [Inline] -> Bool
hasSuccessiveStr ((Str _) : (Str _) : _) = True
hasSuccessiveStr (_ : xs) = hasSuccessiveStr xs
hasSuccessiveStr [] = False

hasMultipleSpace :: [Inline] -> Bool
hasMultipleSpace (Space : Space : _) = True
hasMultipleSpace (_ : xs) = hasMultipleSpace xs
hasMultipleSpace [] = False

extractMetaInlines :: MetaValue -> [[Inline]]
extractMetaInlines (MetaInlines inlines) = [inlines]
extractMetaInlines _ = []

extractInlines :: Block  -> [[Inline]]
extractInlines (Para inlines) = [inlines]
extractInlines (Plain inlines) = [inlines]
extractInlines (Header _ _ inlines) = [inlines]
extractInlines (Figure _ (Caption (Just (inlines)) _) _) = [inlines]
extractInlines (Table _ (Caption (Just (inlines)) _) _ _ _ _) = [inlines]
extractInlines (LineBlock blines) = blines
extractInlines (DefinitionList items) = map fst items
extractInlines _ = []

tests :: [TestTree]
tests = [testProperty "p_xml_roundtrip" p_xml_roundtrip]