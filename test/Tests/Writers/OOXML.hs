{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Writers.OOXML (ooxmlTest) where

import Text.Pandoc
import Test.Tasty
import Test.Tasty.Golden.Advanced
import Codec.Archive.Zip
import Text.XML.Light
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.IO as T
import Data.List (isSuffixOf, sort, (\\), intercalate, union)
import Data.Maybe (catMaybes, mapMaybe)
import Tests.Helpers
import Data.Algorithm.Diff
import System.FilePath.Glob (compile, match)
import qualified Data.Text as T

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
  elName myElem == elName goodElem &&
  elAttribs myElem == elAttribs goodElem &&
  and (zipWith compareXMLBool (elContent myElem) (elContent goodElem))
compareXMLBool (Text myCData) (Text goodCData) =
  cdVerbatim myCData == cdVerbatim goodCData &&
  cdData myCData == cdData goodCData &&
  cdLine myCData == cdLine goodCData
compareXMLBool (CRef myStr) (CRef goodStr) =
  myStr == goodStr
compareXMLBool _ _ = False

displayDiff :: Content -> Content -> Text
displayDiff elemA elemB =
  T.pack $ showDiff (1,1)
    (getDiff (lines $ T.pack $ showContent elemA)
             (lines $ T.pack $ showContent elemB))

goldenArchive :: FilePath -> IO Archive
goldenArchive fp = toArchive . BL.fromStrict <$> BS.readFile fp

testArchive :: (WriterOptions -> Pandoc -> PandocIO BL.ByteString)
            -> WriterOptions
            -> FilePath
            -> IO Archive
testArchive writerFn opts fp = do
  txt <- T.readFile fp
  bs <- runIOorExplode $ readNative def txt >>= writerFn opts
  return $ toArchive bs

compareFileList :: FilePath -> Archive -> Archive -> Maybe Text
compareFileList goldenFP goldenArch testArch =
  let testFiles = filesInArchive testArch
      goldenFiles = filesInArchive goldenArch
      diffTestGolden = testFiles \\ goldenFiles
      diffGoldenTest = goldenFiles \\ testFiles

      results =
        [ if null diffGoldenTest
          then Nothing
          else Just $
               "Files in " <> T.pack goldenFP <>
               " but not in generated archive:\n" <>
               T.pack (intercalate ", " diffGoldenTest)
        , if null diffTestGolden
          then Nothing
          else Just $
               "Files in generated archive but not in " <> T.pack goldenFP <>
               ":\n" <> T.pack (intercalate ", " diffTestGolden)
        ]
  in
    if null $ catMaybes results
    then Nothing
    else Just $ T.intercalate "\n" $ catMaybes results

compareXMLFile' :: FilePath -> Archive -> Archive -> Either Text ()
compareXMLFile' fp goldenArch testArch = do
  testEntry <- case findEntryByPath fp testArch of
                 Just entry -> Right entry
                 Nothing -> Left $
                   "Can't extract " <> T.pack fp <> " from generated archive"
  testXMLDoc <- case parseXMLDoc $ fromEntry testEntry of
                  Just doc -> Right doc
                  Nothing -> Left $
                    "Can't parse xml in  " <> T.pack fp <>
                    " from generated archive"

  goldenEntry <- case findEntryByPath fp goldenArch of
                 Just entry -> Right entry
                 Nothing -> Left $
                   "Can't extract " <> T.pack fp <>
                   " from archive in stored file"
  goldenXMLDoc <- case parseXMLDoc $ fromEntry goldenEntry of
                  Just doc -> Right doc
                  Nothing -> Left $
                    "Can't parse xml in  " <> T.pack fp <>
                    " from archive in stored file"

  let testContent = Elem testXMLDoc
      goldenContent = Elem goldenXMLDoc

  if compareXMLBool goldenContent testContent
    then Right ()
    else Left $ "Non-matching xml in " <> T.pack fp <> ":\n" <>
                displayDiff testContent goldenContent

compareXMLFile :: FilePath -> Archive -> Archive -> Maybe Text
compareXMLFile fp goldenArch testArch =
  case compareXMLFile' fp goldenArch testArch of
    Right _ -> Nothing
    Left s -> Just s

compareAllXMLFiles :: Archive -> Archive -> Maybe Text
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

compareMediaFile' :: FilePath -> Archive -> Archive -> Either Text ()
compareMediaFile' fp goldenArch testArch = do
  testEntry <- case findEntryByPath fp testArch of
                 Just entry -> Right entry
                 Nothing -> Left $
                   "Can't extract " <> T.pack fp <> " from generated archive"
  goldenEntry <- case findEntryByPath fp goldenArch of
                 Just entry -> Right entry
                 Nothing -> Left $
                   "Can't extract " <> T.pack fp <>
                   " from archive in stored file"

  if fromEntry testEntry == fromEntry goldenEntry
    then Right ()
    else Left $
    "Non-matching binary file: " <> T.pack fp

compareMediaFile :: FilePath -> Archive -> Archive -> Maybe Text
compareMediaFile fp goldenArch testArch =
  case compareMediaFile' fp goldenArch testArch of
    Right _ -> Nothing
    Left s -> Just s

compareAllMediaFiles :: Archive -> Archive -> Maybe Text
compareAllMediaFiles goldenArch testArch =
  let allFiles = filesInArchive goldenArch `union` filesInArchive testArch
      mediaPattern = compile "*/media/*"
      allMediaFiles = sort $
        filter (match mediaPattern) allFiles
      results =
        mapMaybe (\fp -> compareMediaFile fp goldenArch testArch) allMediaFiles
  in
    if null results
    then Nothing
    else Just $ unlines results

ooxmlTest :: (WriterOptions -> Pandoc -> PandocIO BL.ByteString)
          -> String
          -> WriterOptions
          -> FilePath
          -> FilePath
          -> TestTree
ooxmlTest writerFn testName opts nativeFP goldenFP =
  goldenTest
  testName
  (goldenArchive goldenFP)
  (testArchive writerFn opts nativeFP)
  (\goldenArch testArch ->
     let res = catMaybes [ compareFileList goldenFP goldenArch testArch
                         , compareAllXMLFiles goldenArch testArch
                         , compareAllMediaFiles goldenArch testArch
                         ]
     in return $ if null res then Nothing else Just $ T.unpack $ unlines res)
  (\a -> BL.writeFile goldenFP $ fromArchive a)
