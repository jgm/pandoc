{-# LANGUAGE OverloadedStrings #-}

module Tests.Writers.Powerpoint (tests) where

import Control.Exception (throwIO)
import Text.Pandoc
import Text.Pandoc.Builder
import Test.Tasty
import Test.Tasty.HUnit
import Codec.Archive.Zip
import Data.List (isPrefixOf, isSuffixOf)

----- Number of Slides -----------

numberOfSlides :: WriterOptions -> Pandoc -> IO Int
numberOfSlides opts pd = do
  mbs <- runIO $
        do setUserDataDir $ Just "../data"
           writePowerpoint opts pd
  case mbs of
       Left e   -> throwIO e
       Right bs -> do
         let archive = toArchive bs
         return $
           length $
           filter (isSuffixOf ".xml") $
           filter (isPrefixOf "ppt/slides/slide") $
           filesInArchive archive

testNumberOfSlides :: TestName -> Int -> WriterOptions -> Pandoc -> TestTree
testNumberOfSlides name n opts pd =
  testCase name $ do
    n' <- numberOfSlides opts pd
    n' @=? n

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
      (para $ image "lalune.jpg" "" "") <>
      para "foo")
  , testNumberOfSlides
    "With image slide, header" 3
    def
    (doc $
      para "first slide" <>
      header 2 "image header" <>
      (para $ image "lalune.jpg" "" "") <>
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
