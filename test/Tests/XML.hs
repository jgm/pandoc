{-# LANGUAGE OverloadedStrings #-}
{- |
--   Module      : Tests.XML
--   Copyright   : Copyright (C) 2025- Massimiliano Farinella and John MacFarlane
--   License     : GNU GPL, version 2 or above
--
--   Maintainer  : Massimiliano Farinella <massifrg@gmail.com>
--   Stability   : WIP
--   Portability : portable
Runs a roundtrip conversion of an AST trough the XML format:
- first from AST to XML (XML Writer),
- then back to AST (XML Reader),
- and checks that the two ASTs are the same
-}
module Tests.XML (tests) where

import Control.Monad ((>=>))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()

p_xml_roundtrip :: Pandoc -> Bool
p_xml_roundtrip d = d == purely (writeXML def {writerTemplate = Just mempty} >=> readXML def) d

tests :: [TestTree]
tests = [testProperty "p_xml_roundtrip" p_xml_roundtrip]