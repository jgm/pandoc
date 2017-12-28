{-# LANGUAGE OverloadedStrings #-}

module Tests.Writers.Powerpoint (tests) where

import Text.Pandoc.Writers.Powerpoint (writePowerpoint)
import Text.Pandoc
import Text.Pandoc.Builder
-- import Text.Pandoc.Error (PandocError(..))
import Test.Tasty
import Test.Tasty.HUnit
import Codec.Archive.Zip
import Text.Pandoc.Class (runPure)
import Tests.Writers.Powerpoint.PureData (pureFileTree)
import Data.List (isPrefixOf, isSuffixOf)

data PowerpointTestError = ErrorFromPandoc PandocError
                         | ErrorFromZipArchive String
                         deriving (Show)

fromPandoc :: Either PandocError a -> Either PowerpointTestError a
fromPandoc x = case x of
  Right r -> Right r
  Left e -> Left $ ErrorFromPandoc e

fromZipArchive :: Either String a -> Either PowerpointTestError a
fromZipArchive x = case x of
  Right r -> Right r
  Left s -> Left $ ErrorFromZipArchive s

----- Number of Slides -----------

numberOfSlides :: WriterOptions -> Pandoc -> Either PowerpointTestError Int
numberOfSlides opts pd = do
  bs <- fromPandoc $ runPure $
        do modifyPureState (\st -> st {stFiles = pureFileTree})
           writePowerpoint opts pd
  archive <- fromZipArchive $ toArchiveOrFail bs
  return $
    length $
    filter (isSuffixOf ".xml") $
    filter (isPrefixOf "ppt/slides/slide") $
    filesInArchive archive

testNumberOfSlides :: TestName -> Int -> WriterOptions -> Pandoc -> TestTree
testNumberOfSlides name n opts pd =
  testCase name $ case numberOfSlides opts pd of
                    Right n' -> n' @=? n
                    Left e -> assertBool (show e) False

numSlideTests :: TestTree
numSlideTests = testGroup "Number of slides in output"
  [ testNumberOfSlides
    "simple one-slide deck" 1
    def
    (doc $ para "foo")
  , testNumberOfSlides
    "with metadata (header slide)" 2
    def
    (setTitle "My Title" $ doc $ para "foo")
  , testNumberOfSlides
    "With h1 slide (using default slide-level)" 2
    def
    (doc $ header 1 "Header" <> para "foo")
  , testNumberOfSlides
    "With h2 slide (using default slide-level)" 2
    def
    (doc $ header 1 "Header" <> header 2 "subeader" <> para "foo")
  , testNumberOfSlides
    "With h1 slide (using default slide-level)" 2
    def
    (doc $ header 1 "Header" <> para "foo")
  , testNumberOfSlides
    "With h2 slide (using default slide-level)" 2
    def
    (doc $ header 1 "Header" <> header 2 "subeader" <> para "foo")
  , testNumberOfSlides
    "With image slide, no header" 3
    def
    (doc $
      para "first slide" <>
      (para $ image "/fakefs/img/lalune.jpg" "" "") <>
      para "foo")
  , testNumberOfSlides
    "With image slide, header" 3
    def
    (doc $
      para "first slide" <>
      header 2 "image header" <>
      (para $ image "/fakefs/img/lalune.jpg" "" "") <>
      para "foo")
  , testNumberOfSlides
    "With table, no header" 3
    def
    (doc $
     para "first slide" <>
     (simpleTable [para "foo" <> para "bar"] [[para "this" <> para "that"]]) <>
     para "foo")
  , testNumberOfSlides
    "With table, header" 3
    def
    (doc $
     para "first slide" <>
     header 2 "table header" <>
     (simpleTable [para "foo" <> para "bar"] [[para "this" <> para "that"]]) <>
     para "foo")
  , testNumberOfSlides
    "hrule" 2
    def
    (doc $
     para "first slide" <> horizontalRule <> para "last slide")
  ]


tests :: [TestTree]
tests = [numSlideTests]
