{-# LANGUAGE OverloadedStrings #-}

module Tests.XML (tests) where

import Control.Monad ((>=>))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()

p_xml_roundtrip :: Pandoc -> Bool
p_xml_roundtrip d = d == purely (writeXML def >=> readXML def) d

tests :: [TestTree]
tests = [testProperty "p_xml_roundtrip" p_xml_roundtrip]