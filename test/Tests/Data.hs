{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{- |
   Module      : Tests.Data
   Copyright   : © 2026 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Tests for data file lookup, including lookup in @PANDOC_DATA_DIRS@.
-}
module Tests.Data (tests) where

import Codec.Archive.Zip (Archive, filesInArchive, findEntryByPath,
                          fromEntry, toArchive)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate)
import qualified Data.Text as T
import System.FilePath ((</>), searchPathSeparator)
import System.Info (os)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Pandoc.Class (PandocPure, FileInfo(..), FileTree,
                          findFileWithDataFallback, getDataDirs,
                          getPandocDataDirs, insertInFileTree,
                          modifyPureState, PureState(..), runIOorExplode,
                          runPure, setUserDataDir)
import Text.Pandoc.Data (readDataFile, readDefaultDataFile)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- | An absolute path that is valid on the current platform.
absPath :: FilePath -> FilePath
absPath d = if os == "mingw32" then "C:\\" </> d else "/" </> d

userDir, dirA, dirB :: FilePath
userDir = absPath "user"
dirA = absPath "dirA"
dirB = absPath "dirB"

pandocDataDirs :: [FilePath] -> T.Text
pandocDataDirs = T.pack . intercalate [searchPathSeparator]

-- | Run a 'PandocPure' action with the given user data directory,
-- value of @PANDOC_DATA_DIRS@, and files.
runWith :: Maybe FilePath -> Maybe T.Text -> FileTree -> [(FilePath, B.ByteString)]
        -> PandocPure a -> Either String a
runWith mbUserDir mbDataDirs defaultFiles files action =
  either (Left . show) Right $ runPure $ do
    modifyPureState $ \st ->
      st{ stEnv = maybe [] (\x -> [("PANDOC_DATA_DIRS", x)]) mbDataDirs
        , stFiles = foldr (\(fp, bs) -> insertInFileTree fp (fileInfo bs))
                          defaultFiles files }
    setUserDataDir mbUserDir
    action

fileInfo :: B.ByteString -> FileInfo
fileInfo bs = FileInfo{ infoFileMTime = posixSecondsToUTCTime 0
                      , infoFileContents = bs }

-- | Default data files that are needed by the tests, placed where
-- 'PandocPure' looks for them if data files are not embedded.
getDefaultFiles :: IO (FileTree, B.ByteString, Archive)
getDefaultFiles = runIOorExplode $ do
  abbrevs <- readDefaultDataFile "abbreviations"
  refdocx <- toArchive . BL.fromStrict <$>
               readDefaultDataFile "reference.docx"
  parts <- mapM (\p -> (p,) <$> readDefaultDataFile ("docx" </> p))
                (filesInArchive refdocx)
  let tree = foldr (\(fp, bs) -> insertInFileTree ("data" </> fp) (fileInfo bs))
                   mempty
                   (("abbreviations", abbrevs) :
                     [("docx" </> p, bs) | (p, bs) <- parts])
  return (tree, abbrevs, refdocx)

-- | Contents of a file in a docx archive.
docxPart :: FilePath -> B.ByteString -> Maybe BL.ByteString
docxPart p = fmap fromEntry . findEntryByPath p . toArchive . BL.fromStrict

tests :: [TestTree]
tests =
  [ testGroup "getPandocDataDirs"
    [ testCase "PANDOC_DATA_DIRS unset" $
        runWith Nothing Nothing mempty [] getPandocDataDirs @?= Right []
    , testCase "ignoring relative and empty entries" $
        runWith Nothing (Just (pandocDataDirs [dirA, "relative", "", dirB]))
          mempty [] getPandocDataDirs
          @?= Right [dirA, dirB]
    ]
  , testGroup "getDataDirs"
    [ testCase "user data dir comes first" $
        runWith (Just userDir) (Just (pandocDataDirs [dirA, dirB])) mempty []
          (getDataDirs "templates/default.html5")
          @?= Right [userDir, dirA, dirB]
    , testCase "no user data dir" $
        runWith Nothing (Just (pandocDataDirs [dirA])) mempty []
          (getDataDirs "abbreviations")
          @?= Right [dirA]
    , testCase "absolute path" $
        runWith (Just userDir) (Just (pandocDataDirs [dirA])) mempty []
          (getDataDirs (absPath "abbreviations"))
          @?= Right []
    , testCase "path relative to parent directory" $
        runWith (Just userDir) (Just (pandocDataDirs [dirA])) mempty []
          (getDataDirs (".." </> "abbreviations"))
          @?= Right []
    ]
  , withResource getDefaultFiles (const (return ())) $ \getDefaults ->
    testGroup "readDataFile"
    [ testCase "user data dir takes precedence" $ do
        (defs, _, _) <- getDefaults
        runWith (Just userDir) (Just (pandocDataDirs [dirA])) defs
          [ (userDir </> "abbreviations", "user")
          , (dirA </> "abbreviations", "dirA") ]
          (readDataFile "abbreviations")
          @?= Right "user"
    , testCase "earlier PANDOC_DATA_DIRS entries take precedence" $ do
        (defs, _, _) <- getDefaults
        runWith (Just userDir) (Just (pandocDataDirs [dirA, dirB])) defs
          [ (dirA </> "abbreviations", "dirA")
          , (dirB </> "abbreviations", "dirB") ]
          (readDataFile "abbreviations")
          @?= Right "dirA"
    , testCase "later PANDOC_DATA_DIRS entries are used" $ do
        (defs, _, _) <- getDefaults
        runWith Nothing (Just (pandocDataDirs [dirA, dirB])) defs
          [ (dirB </> "abbreviations", "dirB") ]
          (readDataFile "abbreviations")
          @?= Right "dirB"
    , testCase "falls back to default data files" $ do
        (defs, abbrevs, _) <- getDefaults
        runWith (Just userDir) (Just (pandocDataDirs [dirA])) defs []
          (readDataFile "abbreviations")
          @?= Right abbrevs
    , testCase "readDefaultDataFile ignores data dirs" $ do
        (defs, abbrevs, _) <- getDefaults
        runWith (Just userDir) (Just (pandocDataDirs [dirA])) defs
          [ (userDir </> "abbreviations", "user")
          , (dirA </> "abbreviations", "dirA") ]
          (readDefaultDataFile "abbreviations")
          @?= Right abbrevs
    , testCase "reference.docx from PANDOC_DATA_DIRS" $ do
        (defs, _, _) <- getDefaults
        runWith Nothing (Just (pandocDataDirs [dirA])) defs
          [ (dirA </> "reference.docx", "dirA") ]
          (readDataFile "reference.docx")
          @?= Right "dirA"
    , testCase "reference.docx component from PANDOC_DATA_DIRS" $ do
        (defs, _, _) <- getDefaults
        let styles = "word" </> "styles.xml"
        runWith Nothing (Just (pandocDataDirs [dirA])) defs
          [ (dirA </> "docx" </> styles, "dirA") ]
          (docxPart styles <$> readDataFile "reference.docx")
          @?= Right (Just "dirA")
    , testCase "readDefaultDataFile ignores reference.docx components" $ do
        (defs, _, refdocx) <- getDefaults
        let styles = "word" </> "styles.xml"
        let expected = fromEntry <$> findEntryByPath styles refdocx
        runWith (Just userDir) (Just (pandocDataDirs [dirA])) defs
          [ (userDir </> "docx" </> styles, "user")
          , (dirA </> "docx" </> styles, "dirA") ]
          (docxPart styles <$> readDefaultDataFile "reference.docx")
          @?= Right expected
    ]
  , testGroup "findFileWithDataFallback"
    [ testCase "file in working directory takes precedence" $
        runWith (Just userDir) (Just (pandocDataDirs [dirA])) mempty
          [ ("f.lua", "")
          , (dirA </> "filters" </> "f.lua", "") ]
          (findFileWithDataFallback "filters" "f.lua")
          @?= Right (Just "f.lua")
    , testCase "user data dir takes precedence" $
        runWith (Just userDir) (Just (pandocDataDirs [dirA])) mempty
          [ (userDir </> "filters" </> "f.lua", "")
          , (dirA </> "filters" </> "f.lua", "") ]
          (findFileWithDataFallback "filters" "f.lua")
          @?= Right (Just (userDir </> "filters" </> "f.lua"))
    , testCase "file in PANDOC_DATA_DIRS" $
        runWith (Just userDir) (Just (pandocDataDirs [dirA, dirB])) mempty
          [ (dirB </> "filters" </> "f.lua", "") ]
          (findFileWithDataFallback "filters" "f.lua")
          @?= Right (Just (dirB </> "filters" </> "f.lua"))
    , testCase "file not found" $
        runWith (Just userDir) (Just (pandocDataDirs [dirA])) mempty []
          (findFileWithDataFallback "filters" "f.lua")
          @?= Right Nothing
    ]
  ]
