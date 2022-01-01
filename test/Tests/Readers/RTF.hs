{- |
   Module      : Tests.Readers.RTF
   Copyright   : © 2021-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : jgm@berkeley.edu
   Stability   : alpha
   Portability : portable

Tests for the RTF reader.
-}
module Tests.Readers.RTF (tests) where

import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import System.FilePath (replaceExtension, (</>), (<.>))

rtfTest :: TestName -> TestTree
rtfTest name = testGolden name native path
   (\t -> runIOorExplode
            (readRTF def t >>=
              writeNative def{ writerTemplate = Just mempty }))
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

