{-# LANGUAGE OverloadedStrings #-}

module Tests.Writers.Powerpoint (tests) where

import Control.Exception (throwIO)
import Text.Pandoc
import Text.Pandoc.Builder
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Walk
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Codec.Archive.Zip
import Text.XML.Light
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.Maybe (mapMaybe)

getPptxArchive :: WriterOptions -> Pandoc -> IO Archive
getPptxArchive opts pd = do
  mbs <- runIO $
         do setUserDataDir $ Just "../data"
            writePowerpoint opts pd
  case mbs of
       Left e   -> throwIO e
       Right bs -> return $ toArchive bs

----- Number of Slides -----------

numberOfSlides :: WriterOptions -> Pandoc -> IO Int
numberOfSlides opts pd = do
  archive <- getPptxArchive opts pd
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
    "With h1 slide (using slide-level 3)" 2
    def {writerSlideLevel= Just 3}
    (doc $ header 1 "Header" <> para "foo")
  , testNumberOfSlides
    "With h2 slide (using slide-level 3)" 3
    def {writerSlideLevel= Just 3}
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
  , testNumberOfSlides
    "with notes slide" 2
    def
    (doc $
      para $ text "Foo" <> note (para "note text"))
  ]

----- Content Types -----------


contentTypesFileExists :: WriterOptions -> Pandoc -> TestTree
contentTypesFileExists opts pd =
  testCase "Existence of [Content_Types].xml file" $
  do archive <- getPptxArchive opts pd
     assertBool "Missing [Content_Types].xml file" $
       "[Content_Types].xml" `elem` (filesInArchive archive)



-- We want an "Override" entry for each xml file under ppt/.
prop_ContentOverrides :: Pandoc -> IO Bool
prop_ContentOverrides pd = do
  -- remove Math to avoid warnings
  let go :: Inline -> Inline
      go (Math _ _) = Str "Math"
      go i = i
      pd' = walk go pd
  archive <- getPptxArchive def pd'
  let xmlFiles = filter ("[Content_Types].xml" /=) $
                  filter (isSuffixOf ".xml") $
                  filesInArchive archive
  contentTypes <- case findEntryByPath "[Content_Types].xml" archive of
                    Just ent -> return $ fromEntry ent
                    Nothing  -> throwIO $
                      PandocSomeError "Missing [Content_Types].xml file"
  typesElem <- case parseXMLDoc contentTypes of
                    Just element -> return $ element
                    Nothing      -> throwIO $
                      PandocSomeError "[Content_Types].xml cannot be parsed"
  let ns = findAttr (QName "xmlns" Nothing Nothing) typesElem
      overrides = findChildren (QName "Override" ns Nothing) typesElem
      partNames = mapMaybe (findAttr (QName "PartName" Nothing Nothing)) overrides
      -- files in content_types are absolute
      absXmlFiles = map (\fp -> case fp of
                                  ('/':_) -> fp
                                  _       -> '/': fp
                        )
                    xmlFiles
  return $ sort absXmlFiles == sort partNames

contentOverridesTests :: TestTree
contentOverridesTests = localOption (QuickCheckTests 20) $
                       testProperty "Content Overrides for each XML file" $
                       \x -> ioProperty $ prop_ContentOverrides (x :: Pandoc)

contentTypeTests :: TestTree
contentTypeTests = testGroup "[Content_Types].xml file"
  [ contentTypesFileExists def (doc $ para "foo")
  , contentOverridesTests
  ]

tests :: [TestTree]
tests = [ numSlideTests
        , contentTypeTests
        ]
