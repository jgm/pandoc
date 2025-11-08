{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.Xlsx
   Copyright   : © 2025 Anton Antic
   License     : GNU GPL, version 2 or above

   Maintainer  : Anton Antic <anton@everworker.ai>
   Stability   : alpha
   Portability : portable

Tests for the XLSX reader.
-}
module Tests.Readers.Xlsx (tests) where

import Data.Algorithm.Diff (getDiff)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.Golden.Advanced
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.UTF8 as UTF8

defopts :: ReaderOptions
defopts = def{ readerExtensions = getDefaultExtensions "xlsx" }

testCompare :: String -> FilePath -> FilePath -> TestTree
testCompare = testCompareWithOpts defopts

nativeDiff :: FilePath -> Pandoc -> Pandoc -> IO (Maybe String)
nativeDiff normPath expectedNative actualNative
  | expectedNative == actualNative = return Nothing
  | otherwise = Just <$> do
      expected <- T.unpack <$> runIOorExplode (writeNative def expectedNative)
      actual <- T.unpack <$> runIOorExplode (writeNative def actualNative)
      let dash = replicate 72 '-'
      let diff = getDiff (lines actual) (lines expected)
      return $ '\n' : dash ++
               "\n--- " ++ normPath ++
               "\n+++ " ++ "test" ++ "\n" ++
               showDiff (1,1) diff ++ dash

testCompareWithOpts :: ReaderOptions -> String -> FilePath -> FilePath -> TestTree
testCompareWithOpts opts testName xlsxFP nativeFP =
  goldenTest
  testName
  (do nf <- UTF8.toText <$> BS.readFile nativeFP
      runIOorExplode (readNative def nf))
  (do df <- B.readFile xlsxFP
      runIOorExplode (readXlsx opts df))
  (nativeDiff nativeFP)
  (\a -> runIOorExplode (writeNative def{ writerTemplate = Just mempty} a)
            >>= BS.writeFile nativeFP . UTF8.fromText)

tests :: [TestTree]
tests = [ testGroup "basic"
          [ testCompare
            "sheet extraction"
            "xlsx-reader/basic.xlsx"
            "xlsx-reader/basic.native"
          ]
        ]
