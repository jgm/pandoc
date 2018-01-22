{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf        #-}

module Tests.Writers.Powerpoint (tests) where

import Text.Pandoc
import Test.Tasty
import Test.Tasty.Golden.Advanced
import Codec.Archive.Zip
import Text.XML.Light
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.IO as T
import Data.List (isPrefixOf, isSuffixOf, sort, (\\), intercalate, union)
import Data.Maybe (catMaybes, mapMaybe)
import Tests.Helpers
import Data.Algorithm.Diff

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

goldenArchive :: FilePath -> IO Archive
goldenArchive fp = (toArchive . BL.fromStrict) <$> BS.readFile fp

testArchive :: WriterOptions -> FilePath -> IO Archive
testArchive opts fp = do
  txt <- T.readFile fp
  bs <- runIOorExplode $ readNative def txt >>= writePowerpoint opts
  return $ toArchive bs

compareFileList :: FilePath -> Archive -> Archive -> Maybe String
compareFileList goldenFP goldenArch testArch =
  let testFiles = filesInArchive testArch
      goldenFiles = filesInArchive goldenArch
      diffTestGolden = testFiles \\ goldenFiles
      diffGoldenTest = goldenFiles \\ testFiles

      results =
        [ if null diffGoldenTest
          then Nothing
          else Just $
               "Files in " ++ goldenFP ++ " but not in generated archive:\n" ++
               intercalate ", " diffGoldenTest
        , if null diffTestGolden
          then Nothing
          else Just $
               "Files in generated archive but not in " ++ goldenFP ++ ":\n" ++
               intercalate ", " diffTestGolden
        ]
  in
    if null $ catMaybes results
    then Nothing
    else Just $ intercalate "\n" $ catMaybes results

compareXMLFile' :: FilePath -> Archive -> Archive -> Either String ()
compareXMLFile' fp goldenArch testArch = do
  testEntry <- case findEntryByPath fp testArch of
                 Just entry -> Right entry
                 Nothing -> Left $
                   "Can't extract " ++ fp ++ " from generated archive"
  testXMLDoc <- case parseXMLDoc $ fromEntry testEntry of
                  Just doc -> Right doc
                  Nothing -> Left $
                    "Can't parse xml in  " ++ fp ++ " from generated archive"

  goldenEntry <- case findEntryByPath fp goldenArch of
                 Just entry -> Right entry
                 Nothing -> Left $
                   "Can't extract " ++ fp ++ " from archive in stored pptx file"
  goldenXMLDoc <- case parseXMLDoc $ fromEntry goldenEntry of
                  Just doc -> Right doc
                  Nothing -> Left $
                    "Can't parse xml in  " ++ fp ++ " from archive in stored pptx file"

  let testContent = Elem $ testXMLDoc
      goldenContent = Elem $ goldenXMLDoc

  if (compareXMLBool goldenContent testContent)
    then Right ()
    else Left $
    "Non-matching xml in " ++ fp ++ ":\n" ++ displayDiff testContent goldenContent

compareXMLFile :: FilePath -> Archive -> Archive -> Maybe String
compareXMLFile fp goldenArch testArch =
  case compareXMLFile' fp goldenArch testArch of
    Right _ -> Nothing
    Left s -> Just s

compareAllXMLFiles :: Archive -> Archive -> Maybe String
compareAllXMLFiles goldenArch testArch =
  let allFiles = filesInArchive goldenArch `union` filesInArchive testArch
      allXMLFiles = sort $
        filter
        (\fp -> ".xml" `isSuffixOf` fp || ".rels" `isSuffixOf` fp)
        allFiles
      results =
        mapMaybe (\fp -> compareXMLFile fp goldenArch testArch) allXMLFiles
  in
    if null results
    then Nothing
    else Just $ unlines results

compareMediaFile' :: FilePath -> Archive -> Archive -> Either String ()
compareMediaFile' fp goldenArch testArch = do
  testEntry <- case findEntryByPath fp testArch of
                 Just entry -> Right entry
                 Nothing -> Left $
                   "Can't extract " ++ fp ++ " from generated archive"
  goldenEntry <- case findEntryByPath fp goldenArch of
                 Just entry -> Right entry
                 Nothing -> Left $
                   "Can't extract " ++ fp ++ " from archive in stored pptx file"

  if (fromEntry testEntry == fromEntry goldenEntry)
    then Right ()
    else Left $
    "Non-matching binary file: " ++ fp

compareMediaFile :: FilePath -> Archive -> Archive -> Maybe String
compareMediaFile fp goldenArch testArch =
  case compareMediaFile' fp goldenArch testArch of
    Right _ -> Nothing
    Left s -> Just s

compareAllMediaFiles :: Archive -> Archive -> Maybe String
compareAllMediaFiles goldenArch testArch =
  let allFiles = filesInArchive goldenArch `union` filesInArchive testArch
      allMediaFiles = sort $
        filter
        (\fp -> "/ppt/media/" `isPrefixOf` fp)
        allFiles
      results =
        mapMaybe (\fp -> compareMediaFile fp goldenArch testArch) allMediaFiles
  in
    if null results
    then Nothing
    else Just $ unlines results

pptxTest :: String -> WriterOptions -> FilePath -> FilePath -> TestTree
pptxTest testName opts nativeFP goldenFP =
  goldenTest
  testName
  (goldenArchive goldenFP)
  (testArchive opts nativeFP)
  (\goldenArch testArch ->
     let res = catMaybes [ compareFileList goldenFP goldenArch testArch
                         , compareAllXMLFiles goldenArch testArch
                         , compareAllMediaFiles goldenArch testArch
                         ]
     in return $ if null res then Nothing else Just $ unlines res)
  (\a -> BL.writeFile goldenFP $ fromArchive a)

--------------------------------------------------------------

tests :: [TestTree]
tests = [ pptxTest
          "Inline formatting"
          def
          "pptx/inline_formatting.native"
          "pptx/inline_formatting.pptx"
        , pptxTest
          "Slide breaks (default slide-level)"
          def
          "pptx/slide_breaks.native"
          "pptx/slide_breaks.pptx"
        , pptxTest
          "slide breaks (slide-level set to 1)"
          def{ writerSlideLevel = Just 1 }
          "pptx/slide_breaks.native"
          "pptx/slide_breaks_slide_level_1.pptx"
        , pptxTest
          "table of contents"
          def{ writerTableOfContents = True }
          "pptx/slide_breaks.native"
          "pptx/slide_breaks_toc.pptx"
        , pptxTest
          "end notes"
          def
          "pptx/endnotes.native"
          "pptx/endnotes.pptx"
        , pptxTest
          "end notes, with table of contents"
          def { writerTableOfContents = True }
          "pptx/endnotes.native"
          "pptx/endnotes_toc.pptx"
        ]
