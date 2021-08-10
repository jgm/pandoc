{- |
   Module      : Tests.Readers.RTF
   Copyright   : © 2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : jgm@berkeley.edu
   Stability   : alpha
   Portability : portable

Tests for the RTF reader.
-}
module Tests.Readers.RTF (tests) where

import Test.Tasty
import Tests.Helpers
import Test.Tasty.Golden (goldenVsString)
import qualified Data.ByteString as BS
import Text.Pandoc
import Text.Pandoc.UTF8 (toText, fromStringLazy)
import Data.Text (Text, unpack)
import System.FilePath (replaceExtension, (</>), (<.>))

rtfToNative :: Text -> Text
rtfToNative =
  purely (writeNative def{ writerTemplate = Just mempty }) .
    purely (readRTF def)

rtfTest :: TestName -> TestTree
rtfTest name = goldenVsString name native
  (fromStringLazy . filter (/='\r') . unpack . rtfToNative . toText
    <$> BS.readFile path)
  where native = replaceExtension path ".native"
        path = "rtf" </> name <.> "rtf"

tests :: [TestTree]
tests = map rtfTest [ "footnote"
                    , "accent"
                    , "unicode"
                    , "image"
                    , "link"
                    , "heading"
                    , "formatting"
                    , "list_simple"
                    , "list_complex"
                    , "bookmark"
                    , "table_simple"
                    , "table_error_codes"
                    ]

