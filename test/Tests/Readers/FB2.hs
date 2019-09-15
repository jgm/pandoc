{-# LANGUAGE NoImplicitPrelude #-}
{- |
   Module      : Tests.Readers.FB2
   Copyright   : © 2018-2019 Alexander Krotov
   License     : GNU GPL, version 2 or above

   Maintainer  : © 2018-2019 Alexander Krotov <ilabdsf@gmail.com>
   Stability   : alpha
   Portability : portable

Tests for the EPUB mediabag.
-}
module Tests.Readers.FB2 (tests) where

import Prelude
import Test.Tasty
import Tests.Helpers
-- import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.Golden.Advanced (goldenTest)
import Text.Printf
import qualified Data.ByteString.Lazy as LBS

import qualified Data.ByteString as BS
import Text.Pandoc
import Text.Pandoc.UTF8 (toText, fromStringLazy)
import Data.Text (Text, unpack)
import System.FilePath (replaceExtension)

simpleCmp :: Eq a => String -> a -> a -> IO (Maybe String)
simpleCmp e x y =
  return $ if x == y then Nothing else Just e


goldenVsString
  :: TestName -- ^ test name
  -> FilePath -- ^ path to the «golden» file (the file that contains correct output)
  -> IO LBS.ByteString -- ^ action that returns a string
  -> TestTree -- ^ the test verifies that the returned string is the same as the golden file contents
goldenVsString name ref act =
  goldenTest
    name
    (BS.readFile ref)
    (LBS.toStrict <$> act)
    cmp
    upd
  where
  cmp x y = simpleCmp msg x y
    where
    msg = printf "Test output from %s was different from %s. It was: %s" ref (show x) (show y)
  upd = BS.writeFile ref

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
