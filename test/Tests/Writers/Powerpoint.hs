{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf        #-}

module Tests.Writers.Powerpoint (tests) where

-- import Control.Exception (throwIO)
import Text.Pandoc
import Test.Tasty
import Test.Tasty.HUnit
import Codec.Archive.Zip
import Text.XML.Light
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.IO as T
import Data.List (isPrefixOf, isSuffixOf, sort, (\\), intercalate)
import Data.Maybe (fromJust, isNothing)
import Tests.Helpers
import Data.Algorithm.Diff
import Control.Monad (when)


getPptxBytes :: WriterOptions
             -> FilePath
             -> FilePath
             -> IO (BL.ByteString, BL.ByteString)
getPptxBytes opts nativeFp pptxFp = do
  ntvTxt <- T.readFile nativeFp
  ntv <- runIOorExplode $ readNative def ntvTxt
  myPptxBs <- runIOorExplode $ writePowerpoint opts ntv
  goodPptxBs <- BL.readFile pptxFp
  return (myPptxBs, goodPptxBs)


assertSameFileList :: Archive -> Archive -> FilePath -> Assertion
assertSameFileList myArch goodArch pptxFp = do
  let filesMy = filesInArchive myArch
      filesGood = filesInArchive goodArch
      diffMyGood = filesMy \\ filesGood
      diffGoodMy = filesGood \\ filesMy
  if | null diffMyGood && null diffGoodMy -> return ()
     | null diffMyGood ->
         assertFailure $
         "Files in " ++ pptxFp ++ " but not in generated archive:\n" ++
         intercalate ", " diffGoodMy
     | null diffGoodMy ->
         assertFailure $
         "Files in generated archive but not in " ++ pptxFp ++ ":\n" ++
         intercalate ", " diffMyGood
     | otherwise ->
         assertFailure $
         "Files in " ++ pptxFp ++ " but not in generated archive:\n" ++
         intercalate ", " diffGoodMy ++
         "\n" ++
         "Files in generated archive but not in " ++ pptxFp ++ ":\n" ++
         intercalate ", " diffMyGood

compareXMLBool :: Content -> Content -> Bool
-- We make a special exception for times at the moment, and just pass
-- them because we can't control the utctime when running IO. Besides,
-- so long as we have two times, we're okay.
compareXMLBool (Elem myElem) (Elem goodElem)
  | (QName "created" _ (Just "dcterms")) <- elName myElem
  , (QName "created" _ (Just "dcterms")) <- elName goodElem =
      True
compareXMLBool (Elem myElem) (Elem goodElem)
  | (QName "modified" _ (Just "dcterms")) <- elName myElem
  , (QName "modified" _ (Just "dcterms")) <- elName goodElem =
      True
compareXMLBool (Elem myElem) (Elem goodElem) =
  and [ elName myElem == elName goodElem
      , elAttribs myElem == elAttribs goodElem
      , and $
        map (uncurry compareXMLBool) $
        zip (elContent myElem) (elContent goodElem)
      ]
compareXMLBool (Text myCData) (Text goodCData) =
  and [ cdVerbatim myCData == cdVerbatim goodCData
      , cdData myCData == cdData goodCData
      , cdLine myCData == cdLine goodCData
      ]
compareXMLBool (CRef myStr) (CRef goodStr) =
  myStr == goodStr
compareXMLBool _ _ = False

displayDiff :: Content -> Content -> String
displayDiff elemA elemB =
  showDiff (1,1) $ getDiff (lines $ ppContent elemA) (lines $ ppContent elemB)

compareXMLFile :: FilePath -> Archive -> Archive -> Assertion
compareXMLFile fp myArch goodArch = do
  let mbMyEntry = findEntryByPath fp myArch
  when (isNothing mbMyEntry)
    (assertFailure $
    "Can't extract " ++ fp ++ " from generated archive")
  let mbMyXMLDoc = parseXMLDoc $ fromEntry $ fromJust mbMyEntry
  when (isNothing mbMyXMLDoc)
    (assertFailure $
    "Can't parse xml in  " ++ fp ++ " from generated archive")
  let myContent = Elem $ fromJust mbMyXMLDoc

  let mbGoodEntry = findEntryByPath fp goodArch
  when (isNothing mbGoodEntry)
    (assertFailure $
    "Can't extract " ++ fp ++ " from archive in stored pptx file")
  let mbGoodXMLDoc = parseXMLDoc $ fromEntry $ fromJust mbGoodEntry
  when (isNothing mbGoodXMLDoc)
    (assertFailure $
    "Can't parse xml in  " ++ fp ++ " from archive in stored pptx file")
  let goodContent = Elem $ fromJust mbGoodXMLDoc

  assertBool
    ("Non-matching xml in " ++ fp ++ ":\n" ++ displayDiff myContent goodContent)
    (compareXMLBool myContent goodContent)

compareBinaryFile :: FilePath -> Archive -> Archive -> Assertion
compareBinaryFile fp myArch goodArch = do
  let mbMyEntry = findEntryByPath fp myArch
  when (isNothing mbMyEntry)
    (assertFailure $
    "Can't extract " ++ fp ++ " from generated archive")
  let myBytes = fromEntry $ fromJust mbMyEntry

  let mbGoodEntry = findEntryByPath fp goodArch
  when (isNothing mbGoodEntry)
    (assertFailure $
    "Can't extract " ++ fp ++ " from archive in stored pptx file")
  let goodBytes = fromEntry $ fromJust mbGoodEntry

  assertBool (fp ++ " doesn't match") (myBytes == goodBytes)

testSameFileList :: WriterOptions -> FilePath -> FilePath -> TestTree
testSameFileList opts myFp goodFp =
  testCase ("Identical file list in archives") $ do
  (myBS, goodBS) <- getPptxBytes opts myFp goodFp
  let myArch = toArchive myBS
      goodArch = toArchive goodBS
  (assertSameFileList myArch goodArch goodFp)

testSameXML :: WriterOptions -> FilePath -> FilePath -> TestTree
testSameXML opts myFp goodFp = testCaseSteps "Comparing extracted xml files" $
  \step -> do
    (myBS, goodBS) <- getPptxBytes opts myFp goodFp
    let myArch = toArchive myBS
        goodArch = toArchive goodBS

    let xmlFileList = sort $
          filter (\fp -> ".xml" `isSuffixOf` fp || ".rels" `isSuffixOf` fp)
          (filesInArchive myArch)
    mapM_
      (\fp -> step ("- " ++ fp) >> compareXMLFile fp myArch goodArch)
      xmlFileList

testSameMedia :: WriterOptions -> FilePath -> FilePath -> TestTree
testSameMedia opts myFp goodFp = testCaseSteps "Comparing media files" $
  \step -> do
    (myBS, goodBS) <- getPptxBytes opts myFp goodFp
    let myArch = toArchive myBS
        goodArch = toArchive goodBS

    let mediaFileList = sort $
          filter (\fp -> "ppt/media/" `isPrefixOf` fp)
          (filesInArchive myArch)

    mapM_
      (\fp -> step ("- " ++ fp) >> compareBinaryFile fp myArch goodArch)
      mediaFileList

testCompareWithOpts :: String -> WriterOptions ->FilePath -> FilePath -> TestTree
testCompareWithOpts testName opts nativeFp pptxFp =
  testGroup testName [ testSameFileList opts nativeFp pptxFp
                     , testSameXML opts nativeFp pptxFp
                     , testSameMedia opts nativeFp pptxFp
                     ]


testCompare :: String -> FilePath -> FilePath -> TestTree
testCompare testName nativeFp pptxFp =
  testCompareWithOpts testName def nativeFp pptxFp

--------------------------------------------------------------

tests :: [TestTree]
tests = [ testCompare
          "Inline formatting"
          "pptx/inline_formatting.native"
          "pptx/inline_formatting.pptx"
        , testCompare
          "slide breaks (default slide-level)"
          "pptx/slide_breaks.native"
          "pptx/slide_breaks.pptx"
        , testCompareWithOpts
          "slide breaks (slide-level set to 1)"
          def{writerSlideLevel=Just 1}
          "pptx/slide_breaks.native"
          "pptx/slide_breaks_slide_level_1.pptx"
        ]
