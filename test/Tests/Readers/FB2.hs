{-# LANGUAGE NoImplicitPrelude #-}
{- |
   Module      : Tests.Readers.FB2
   Copyright   : © 2018-2020 Alexander Krotov
   License     : GNU GPL, version 2 or above

   Maintainer  : © 2018-2020 Alexander Krotov <ilabdsf@gmail.com>
   Stability   : alpha
   Portability : portable

Tests for the EPUB mediabag.
-}
module Tests.Readers.FB2 (tests) where

import Prelude
import Test.Tasty
import Tests.Helpers
import Test.Tasty.Golden (goldenVsString)
import qualified Data.ByteString as BS
import Text.Pandoc
import Text.Pandoc.UTF8 (toText, fromStringLazy)
import Data.Text (Text, unpack)
import System.FilePath (replaceExtension)

fb2ToNative :: Text -> Text
fb2ToNative = purely (writeNative def{ writerTemplate = Just mempty }) . purely (readFB2 def)

fb2Test :: TestName -> FilePath -> TestTree
fb2Test name path = goldenVsString name native
  (fromStringLazy . filter (/='\r') . unpack . fb2ToNative . toText
    <$> BS.readFile path)
  where native = replaceExtension path ".native"

tests :: [TestTree]
tests = [ fb2Test "Emphasis" "fb2/reader/emphasis.fb2"
        , fb2Test "Titles" "fb2/reader/titles.fb2"
        , fb2Test "Epigraph" "fb2/reader/epigraph.fb2"
        , fb2Test "Poem" "fb2/reader/poem.fb2"
        , fb2Test "Meta" "fb2/reader/meta.fb2"
        , fb2Test "Notes" "fb2/reader/notes.fb2"
        ]
