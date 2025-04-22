{-# LANGUAGE OverloadedStrings #-}

module Tests.XML (tests) where

import Control.Monad ((>=>))
import Data.Text as T hiding (reverse)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder as B
import Text.Pandoc.Walk (walk)

p_xml_roundtrip :: Property
p_xml_roundtrip = forAll (normalize <$> arbitrary) testdoc
  where
    testdoc d = d == purely (writeXML def >=> readXML def) d

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
simplifyInlines (Str s1 : Str s2 : xs) = simplifyInlines $ strsAndSpaces (s1 <> s2) ++ xs
simplifyInlines (Str s : xs) = (strsAndSpaces s) ++ simplifyInlines xs
simplifyInlines (x : xs) = x : simplifyInlines xs
simplifyInlines [] = []

strsAndSpaces :: Text -> [Inline]
strsAndSpaces s =
  if T.any (== ' ') s
    then to_inlines (T.split (== ' ') s) []
    else [Str s]
  where
    to_inlines :: [Text] -> [Inline] -> [Inline]
    to_inlines ("" : xs) [] = to_inlines xs [Space]
    to_inlines ("" : xs) (Space : ilss) = to_inlines xs (Space : ilss)
    to_inlines ("" : xs) ils = to_inlines xs (Space : ils)
    to_inlines (x : xs) [] = to_inlines xs [Str x]
    to_inlines (x : xs) (Space : ilss) = to_inlines xs (Str x : Space : ilss)
    to_inlines (x : xs) ils = to_inlines xs (Str x : Space : ils)
    to_inlines [] ils = reverse ils

simplifyBlocks :: [Block] -> [Block]
simplifyBlocks (Plain [] : xs) = simplifyBlocks xs
simplifyBlocks (Para [] : xs) = simplifyBlocks xs
simplifyBlocks (Header _ _ [] : xs) = simplifyBlocks xs
simplifyBlocks (BulletList items : xs) = BulletList (removeEmptyItems items) : simplifyBlocks xs
simplifyBlocks (OrderedList la items : xs) = OrderedList la (removeEmptyItems items) : simplifyBlocks xs
simplifyBlocks (x : xs) = x : simplifyBlocks xs
simplifyBlocks [] = []

removeEmptyItems :: [[Block]] -> [[Block]]
removeEmptyItems ([] : xs) = removeEmptyItems xs
removeEmptyItems (x : xs) = x : removeEmptyItems xs
removeEmptyItems [] = []

tests :: [TestTree]
tests = [testProperty "p_xml_roundtrip" p_xml_roundtrip]