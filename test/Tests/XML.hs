{-# LANGUAGE OverloadedStrings #-}

module Tests.XML (tests) where

import Data.Text (unpack)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Walk (query, walk)
import Text.Pandoc.Builder as B

p_xml_roundtrip :: Pandoc -> Property
p_xml_roundtrip d = isValidPandoc d ==> d'' == d'
  where
    d' = walk normalize d
    xml = purely (writeXML def) d'
    d'' = purely (readXML def) xml

normalize :: Pandoc -> Pandoc
normalize = walk fixInlines
 where
  fixInlines :: [Inline] -> [Inline]
  fixInlines = B.toList . B.fromList

isValidPandoc :: Pandoc -> Bool
isValidPandoc d = not has_ilnesses
  where
    has_ilnesses = hasEmptyStr || hasSpaceAroundBreaks
    -- \|| hasSuccSameInline || hasSuccSpaces
    inlines = (query extractInlines d) ++ (query extractMetaInlines d)
    hasEmptyStr = any (any isIllStr) inlines
    hasSpaceAroundBreaks = any hasSpaceAroundBreak inlines

-- hasSuccSameInline = any hasSuccessiveInline inlines
-- hasSuccSpaces = any hasMultipleSpace inlines

-- a Str is ill if its text is empty or it contains spaces
isIllStr :: Inline -> Bool
isIllStr (Str s) = s == "" || (' ' `elem` (unpack s))
isIllStr _ = False

-- detect two consecutive Inlines of the same type in a [Inline]
hasSuccessiveInline :: [Inline] -> Bool
hasSuccessiveInline ((Str _) : (Str _) : _) = True
hasSuccessiveInline ((Emph _) : (Emph _) : _) = True
hasSuccessiveInline ((Strong _) : (Strong _) : _) = True
hasSuccessiveInline ((Underline _) : (Underline _) : _) = True
hasSuccessiveInline ((SmallCaps _) : (SmallCaps _) : _) = True
hasSuccessiveInline ((Strikeout _) : (Strikeout _) : _) = True
hasSuccessiveInline ((Subscript _) : (Subscript _) : _) = True
hasSuccessiveInline ((Superscript _) : (Superscript _) : _) = True
hasSuccessiveInline ((Quoted q1 _) : (Quoted q2 _) : _) = q1 == q2
-- hasSuccessiveInline ((Math mt1 _) : (Math mt2 _) : _) = mt1 == mt2
-- hasSuccessiveInline ((RawInline f1 _) : (RawInline f2 _) : _) = f1 == f2
hasSuccessiveInline (SoftBreak : SoftBreak : _) = True
hasSuccessiveInline (_ : xs) = hasSuccessiveInline xs
hasSuccessiveInline [] = False

-- detect two consecutive Space in a [Inline]
hasMultipleSpace :: [Inline] -> Bool
hasMultipleSpace (Space : Space : _) = True
hasMultipleSpace (_ : xs) = hasMultipleSpace xs
hasMultipleSpace [] = False

hasSpaceAroundBreak :: [Inline] -> Bool
hasSpaceAroundBreak (Space : SoftBreak : _) = True
hasSpaceAroundBreak (SoftBreak : Space : _) = True
hasSpaceAroundBreak (LineBreak : Space : _) = True
hasSpaceAroundBreak (Space : LineBreak : _) = True
hasSpaceAroundBreak (_ : xs) = hasSpaceAroundBreak xs
hasSpaceAroundBreak _ = False

extractMetaInlines :: MetaValue -> [[Inline]]
extractMetaInlines (MetaInlines inlines) = [inlines]
extractMetaInlines _ = []

extractInlines :: [Inline] -> [[Inline]]
extractInlines inlines = [inlines]

tests :: [TestTree]
tests = [testProperty "p_xml_roundtrip" p_xml_roundtrip]