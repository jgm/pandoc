{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.Pptx
   Copyright   : © 2025 Anton Antic
   License     : GNU GPL, version 2 or above

   Maintainer  : Anton Antic <anton@everworker.ai>
   Stability   : alpha
   Portability : portable

Tests for the PPTX reader.
-}
module Tests.Readers.Pptx (tests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Test.Tasty
import Test.Tasty.Golden.Advanced
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.UTF8 as UTF8

defopts :: ReaderOptions
defopts = def{ readerExtensions = getDefaultExtensions "pptx" }

testCompare :: String -> FilePath -> FilePath -> TestTree
testCompare = testCompareWithOpts defopts

testCompareWithOpts :: ReaderOptions -> String -> FilePath -> FilePath -> TestTree
testCompareWithOpts opts testName pptxFP nativeFP =
  goldenTest
  testName
  (do nf <- UTF8.toText <$> BS.readFile nativeFP
      runIOorExplode (readNative def nf))
  (do df <- B.readFile pptxFP
      runIOorExplode (readPptx opts df))
  (nativeDiff nativeFP)
  (\a -> runIOorExplode (writeNative def{ writerTemplate = Just mempty} a)
            >>= BS.writeFile nativeFP . UTF8.fromText)

tests :: [TestTree]
tests = [ testGroup "basic"
          [ testCompare
            "text extraction"
            "pptx-reader/basic.pptx"
            "pptx-reader/basic.native"
          ]
        ]
