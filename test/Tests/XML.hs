{-# LANGUAGE OverloadedStrings #-}

module Tests.XML (tests) where

import Data.Text (unpack)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder as B
import Text.Pandoc.Walk (query, walk)
import Control.Monad ((>=>))

p_xml_roundtrip :: Property
p_xml_roundtrip = forAll (normalize <$> arbitrary) testdoc
  where
    testdoc d =
      if isValidPandoc d
        then d == purely (writeXML def >=> readXML def) d
        else discard

normalize :: Pandoc -> Pandoc
normalize d =
  let textual = show d
      normalized = normalize' d
   in if textual == show normalized
        then normalized
        else normalize normalized

normalize' :: Pandoc -> Pandoc
normalize' = walk simplify . (walk simplifyBlocks)
  where
    simplify :: [Inline] -> [Inline]
    simplify ils = (simplifyInlines . B.toList . B.fromList) ils

simplifyInlines :: [Inline] -> [Inline]
simplifyInlines (Str "" : xs) = simplifyInlines xs
simplifyInlines (Space : Space : xs) = simplifyInlines $ Space : xs
simplifyInlines (Str s1 : Str s2 : xs) = simplifyInlines $ Str (s1 <> s2) : xs
simplifyInlines (Space : SoftBreak : xs) = simplifyInlines $ SoftBreak : xs
simplifyInlines (Space : LineBreak : xs) = simplifyInlines $ LineBreak : xs
simplifyInlines (SoftBreak : Space : xs) = simplifyInlines $ SoftBreak : xs
simplifyInlines (LineBreak : Space : xs) = simplifyInlines $ LineBreak : xs
simplifyInlines (LineBreak : SoftBreak : xs) = simplifyInlines $ LineBreak : xs
simplifyInlines (SoftBreak : LineBreak : xs) = simplifyInlines $ LineBreak : xs
simplifyInlines (SoftBreak : SoftBreak : xs) = simplifyInlines $ SoftBreak : xs
simplifyInlines (Emph ils1 : Emph ils2 : xs) = simplifyInlines $ Emph (ils1 ++ ils2) : xs
simplifyInlines (Strong ils1 : Strong ils2 : xs) = simplifyInlines $ Strong (ils1 ++ ils2) : xs
simplifyInlines (Superscript ils1 : Superscript ils2 : xs) = simplifyInlines $ Superscript (ils1 ++ ils2) : xs
simplifyInlines (Subscript ils1 : Subscript ils2 : xs) = simplifyInlines $ Subscript (ils1 ++ ils2) : xs
simplifyInlines (Strikeout ils1 : Strikeout ils2 : xs) = simplifyInlines $ Strikeout (ils1 ++ ils2) : xs
simplifyInlines (Underline ils1 : Underline ils2 : xs) = simplifyInlines $ Strikeout (ils1 ++ ils2) : xs
simplifyInlines (SmallCaps ils1 : SmallCaps ils2 : xs) = simplifyInlines $ SmallCaps (ils1 ++ ils2) : xs
simplifyInlines (x : xs) = x : simplifyInlines xs
simplifyInlines [] = []

simplifyBlocks :: [Block] -> [Block]
simplifyBlocks (Plain []: xs) = simplifyBlocks xs
simplifyBlocks (Para []: xs) = simplifyBlocks xs
simplifyBlocks (Header _ _ []: xs) = simplifyBlocks xs
simplifyBlocks (x:xs ) = x : simplifyBlocks xs
simplifyBlocks [] = []

isValidPandoc :: Pandoc -> Bool
isValidPandoc d = not has_ilnesses
  where
    has_ilnesses = hasEmptyStr
    inlines = (query extractInlines d) ++ (query extractMetaInlines d)
    hasEmptyStr = any (any isIllStr) inlines

-- hasSpaceAroundBreaks = any hasSpaceAroundBreak inlines
-- hasSuccSpaces = any hasMultipleSpace inlines
-- hasSuccSameInline = any hasSuccessiveInline inlines

-- a Str is ill if its text is empty or it contains spaces
isIllStr :: Inline -> Bool
-- isIllStr (Str s) = s == "" || (' ' `elem` (unpack s))
isIllStr (Str s) = ' ' `elem` (unpack s)
isIllStr _ = False

extractMetaInlines :: MetaValue -> [[Inline]]
extractMetaInlines (MetaInlines inlines) = [inlines]
extractMetaInlines _ = []

extractInlines :: [Inline] -> [[Inline]]
extractInlines inlines = [inlines]

tests :: [TestTree]
tests = [testProperty "p_xml_roundtrip" p_xml_roundtrip]