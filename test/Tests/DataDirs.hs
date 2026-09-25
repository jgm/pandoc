{-# LANGUAGE OverloadedStrings #-}
-- | Tests for the lookup of data files in the user data directory and
-- the additional data directories (@PANDOC_DATA_DIRS@).
module Tests.DataDirs (tests) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import System.FilePath ((</>), searchPathSeparator)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Pandoc.Class
import Text.Pandoc.Data (readDataFile)

-- | Join directories as in @PANDOC_DATA_DIRS@.
joinDirs :: [FilePath] -> String
joinDirs = foldr1 (\a b -> a ++ searchPathSeparator : b)

-- | Run an action in 'PandocPure' with the given value of
-- @PANDOC_DATA_DIRS@ and the given files, each containing its own path.
runWith :: Maybe [FilePath] -> [FilePath] -> PandocPure a -> a
runWith mbEnvDirs files action =
  either (error . show) id $ runPure $ do
    modifyPureState $ \st -> st
      { stEnv = maybe id (\ds -> (("PANDOC_DATA_DIRS", T.pack (joinDirs ds)) :))
                  mbEnvDirs (stEnv st)
      , stFiles = foldr addFile (stFiles st) files
      }
    action
  where
    addFile fp = insertInFileTree fp
                   FileInfo{ infoFileMTime = read "2026-01-01 00:00:00 UTC"
                           , infoFileContents = B.pack (map (toEnum . fromEnum) fp) }

-- | Read a data file and return its contents, which is the path of the
-- file that was found.
readDataFile' :: PandocMonad m => FilePath -> m String
readDataFile' fp = map (toEnum . fromEnum) . B.unpack <$> readDataFile fp

tests :: [TestTree]
tests =
  [ testCase "splitDataDirs ignores empty entries" $
      splitDataDirs (joinDirs ["", "a", "", "b/c", ""]) @?= ["a", "b/c"]

  , testCase "no additional data dirs by default" $
      runWith Nothing [] getDataDirs @?= []

  , testCase "getDataDirs reads PANDOC_DATA_DIRS" $
      runWith (Just ["a", "", "b"]) [] getDataDirs @?= ["a", "b"]

  , testCase "setDataDirs overrides PANDOC_DATA_DIRS" $
      runWith (Just ["a"]) [] (setDataDirs (Just ["x"]) >> getDataDirs)
        @?= ["x"]

  , testCase "addDataDirs prepends to PANDOC_DATA_DIRS" $
      runWith (Just ["a", "b"]) [] (addDataDirs ["x", "y"] >> getDataDirs)
        @?= ["x", "y", "a", "b"]

  , testCase "user data dir is searched first" $
      runWith (Just ["a"]) ["user/foo.txt", "a/foo.txt"]
        (setUserDataDir (Just "user") >> readDataFile' "foo.txt")
        @?= "user/foo.txt"

  , testCase "data dirs are searched in order" $
      runWith (Just ["a", "b"]) ["a/foo.txt", "b/foo.txt", "b/bar.txt"]
        ((,) <$> readDataFile' "foo.txt" <*> readDataFile' "bar.txt")
        @?= ("a/foo.txt", "b/bar.txt")

  , testCase "data dirs are searched after a missing user data dir file" $
      runWith (Just ["a"]) ["user/other.txt", "a/foo.txt"]
        (setUserDataDir (Just "user") >> readDataFile' "foo.txt")
        @?= "a/foo.txt"

  , testCase "paths relative to a parent dir are not searched" $
      runWith (Just ["a/b"]) ["a/foo.txt"]
        (checkDataDirs ("../foo.txt" :: FilePath))
        @?= []

  , testCase "findFileWithDataFallback searches data dirs" $
      runWith (Just ["a", "b"]) ["b" </> "filters" </> "f.lua"]
        (findFileWithDataFallback "filters" "f.lua")
        @?= Just ("b" </> "filters" </> "f.lua")

  , testCase "findFileWithDataFallback prefers the working directory" $
      runWith (Just ["a"]) ["f.lua", "a" </> "filters" </> "f.lua"]
        (findFileWithDataFallback "filters" "f.lua")
        @?= Just "f.lua"
  ]
