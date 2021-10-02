{-# LANGUAGE PatternGuards     #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Writers.OOXML (ooxmlTest) where

import Text.Pandoc hiding (Attr)
import Test.Tasty
import Test.Tasty.Golden.Advanced
import Control.Applicative ((<|>))
import Codec.Archive.Zip
import Text.XML.Light
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (asum)
import qualified Data.Text.IO as T
import Data.List (isSuffixOf, sort, (\\), intercalate, union)
import Data.Maybe (catMaybes, mapMaybe)
import Tests.Helpers
import Data.Algorithm.Diff
import System.FilePath.Glob (compile, match)

compareXML :: Content -> Content -> Maybe XMLDifference
-- We make a special exception for times at the moment, and just pass
-- them because we can't control the utctime when running IO. Besides,
-- so long as we have two times, we're okay.
compareXML (Elem goodElem) (Elem myElem)
  | (QName "created" _ (Just "dcterms")) <- elName myElem
  , (QName "created" _ (Just "dcterms")) <- elName goodElem =
      Nothing
compareXML (Elem goodElem) (Elem myElem)
  | (QName "modified" _ (Just "dcterms")) <- elName myElem
  , (QName "modified" _ (Just "dcterms")) <- elName goodElem =
      Nothing
compareXML (Elem goodElem) (Elem myElem) =
  (if elName myElem == elName goodElem
   then Nothing
   else Just
    (ElemNamesDiffer
     (Comparison {mine = elName myElem, good = elName goodElem}))
  )
  <|> (if sort (elAttribs myElem) == sort (elAttribs goodElem)
   then Nothing
   else Just
    (ElemAttributesDiffer
     (Comparison { mine = sort (elAttribs myElem)
                 , good = sort (elAttribs goodElem)
                 })))
  <|> asum (zipWith compareXML (elContent myElem) (elContent goodElem))
compareXML (Text goodCData) (Text myCData) =
  (if cdVerbatim myCData == cdVerbatim goodCData
    && cdData myCData == cdData goodCData
   then Nothing
   else Just (CDatasDiffer (Comparison { mine = myCData, good = goodCData })))
compareXML (CRef goodStr) (CRef myStr) =
  if myStr == goodStr
  then Nothing
  else Just (CRefsDiffer (Comparison { mine = myStr, good = goodStr }))
compareXML g m = Just (OtherContentsDiffer (Comparison {mine = m, good = g}))

data XMLDifference
  = ElemNamesDiffer (Comparison QName)
  | ElemAttributesDiffer (Comparison [Attr])
  | CDatasDiffer (Comparison CData)
  | CRefsDiffer (Comparison String)
  | OtherContentsDiffer (Comparison Content)
  deriving (Show)

data Comparison a = Comparison { good :: a, mine :: a }
  deriving (Show)

displayDiff :: Element -> Element -> String
displayDiff elemA elemB =
  showDiff (1,1)
    (getDiff (lines $ ppElement elemA) (lines $ ppElement elemB))

goldenArchive :: FilePath -> IO Archive
goldenArchive fp = toArchive . BL.fromStrict <$> BS.readFile fp

testArchive :: (WriterOptions -> Pandoc -> PandocIO BL.ByteString)
            -> WriterOptions
            -> FilePath
            -> IO Archive
testArchive writerFn opts fp = do
  txt <- T.readFile fp
  bs <- runIOorExplode $ do
    setTranslations "en-US"
    setVerbosity ERROR -- otherwise test output is confusingly noisy
    readNative def txt >>= writerFn opts
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
                   "Can't extract " ++ fp ++ " from archive in stored file"
  goldenXMLDoc <- case parseXMLDoc $ fromEntry goldenEntry of
                  Just doc -> Right doc
                  Nothing -> Left $
                    "Can't parse xml in  " ++ fp ++ " from archive in stored file"

  let testContent = Elem testXMLDoc
      goldenContent = Elem goldenXMLDoc
      display difference = "Non-matching xml in "
        ++ fp ++ ":\n"
        ++ "* " ++ show difference ++ "\n"
        ++ displayDiff testXMLDoc goldenXMLDoc


  maybe (Right ()) (Left . display) (compareXML goldenContent testContent)

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
                   "Can't extract " ++ fp ++ " from archive in stored file"

  if fromEntry testEntry == fromEntry goldenEntry
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
     in return $ if null res then Nothing else Just $ unlines res)
  (\a -> BL.writeFile goldenFP $ fromArchive a)
