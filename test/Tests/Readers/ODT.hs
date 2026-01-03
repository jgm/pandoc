{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.ODT
   Copyright   : © 2015-2024 John MacFarlane
                   2015 Martin Linnemann
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Tests for the ODT reader.
-}
module Tests.Readers.ODT (tests) where

import Control.Monad (liftM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Data.Text (unpack)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import qualified Text.Pandoc.UTF8 as UTF8

defopts :: ReaderOptions
defopts = def{ readerExtensions = getDefaultExtensions "odt" }

tests :: [TestTree]
tests = testsComparingToMarkdown ++ testsComparingToNative

testsComparingToMarkdown :: [TestTree]
testsComparingToMarkdown    = map nameToTest namesOfTestsComparingToMarkdown
  where nameToTest     name = createTest
                                compareODTToMarkdown
                                name
                                (toODTPath      name)
                                (toMarkdownPath name)
        toODTPath      name = "odt/odt/"      ++ name ++ ".odt"
        toMarkdownPath name = "odt/markdown/" ++ name ++ ".md"

testsComparingToNative   :: [TestTree]
testsComparingToNative      = map nameToTest namesOfTestsComparingToNative
  where nameToTest     name = createTest
                                compareODTToNative
                                name
                                (toODTPath      name)
                                (toNativePath   name)
        toODTPath      name = "odt/odt/"      ++ name ++ ".odt"
        toNativePath   name = "odt/native/"   ++ name ++ ".native"


newtype NoNormPandoc = NoNormPandoc {unNoNorm :: Pandoc}
  deriving ( Show )

instance ToString NoNormPandoc where
  toString d = unpack $
               purely (writeNative def{ writerTemplate = s }) $ toPandoc d
   where s = case d of
                  NoNormPandoc (Pandoc (Meta m) _)
                    | M.null m  -> Nothing
                    | otherwise -> Just mempty -- need this for Meta output

instance ToPandoc NoNormPandoc where
  toPandoc = unNoNorm

getNoNormVia :: (a -> Pandoc) -> String -> Either PandocError a -> NoNormPandoc
getNoNormVia _ readerName (Left  _) = error (readerName ++ " reader failed")
getNoNormVia f _          (Right a) = NoNormPandoc (f a)

type TestCreator =  ReaderOptions
                 -> FilePath -> FilePath
                 -> IO (NoNormPandoc, NoNormPandoc)

compareODTToNative   :: TestCreator
compareODTToNative opts odtPath nativePath = do
   nativeFile   <- UTF8.toText <$> BS.readFile nativePath
   odtFile      <- B.readFile       odtPath
   native       <- getNoNormVia id  "native" <$> runIO (readNative def nativeFile)
   odt          <- getNoNormVia id  "odt"    <$> runIO (readODT  opts odtFile)
   return (odt,native)

compareODTToMarkdown :: TestCreator
compareODTToMarkdown opts odtPath markdownPath = do
   markdownFile <- UTF8.toText <$> BS.readFile markdownPath
   odtFile      <- B.readFile       odtPath
   markdown     <- getNoNormVia id "markdown" <$>
                      runIO (readMarkdown def{ readerExtensions = pandocExtensions }
                              markdownFile)
   odt          <- getNoNormVia id "odt"      <$> runIO (readODT      opts odtFile)
   return (odt,markdown)


createTest :: TestCreator
           -> TestName
           -> FilePath -> FilePath
           -> TestTree
createTest   creator name path1 path2 =
  unsafePerformIO $ liftM (test id name) (creator defopts path1 path2)

{-
--

getMedia :: FilePath -> FilePath -> IO (Maybe B.ByteString)
getMedia archivePath mediaPath = do
  zf <- B.readFile archivePath >>= return . toArchive
  return $ findEntryByPath ("Pictures/" ++ mediaPath) zf >>= (Just . fromEntry)

compareMediaPathIO :: FilePath -> MediaBag -> FilePath -> IO Bool
compareMediaPathIO mediaPath mediaBag odtPath = do
  odtMedia <- getMedia odtPath mediaPath
  let mbBS   = case lookupMedia mediaPath mediaBag of
                 Just (_, bs) -> bs
                 Nothing      -> error ("couldn't find " ++
                                        mediaPath ++
                                        " in media bag")
      odtBS = case odtMedia of
                 Just bs -> bs
                 Nothing -> error ("couldn't find " ++
                                   mediaPath ++
                                   " in media bag")
  return $ mbBS == odtBS

compareMediaBagIO :: FilePath -> IO Bool
compareMediaBagIO odtFile = do
    df <- B.readFile odtFile
    let (_, mb) = readODT def df
    bools <- mapM
             (\(fp, _, _) -> compareMediaPathIO fp mb odtFile)
             (mediaDirectory mb)
    return $ and bools

testMediaBagIO :: String -> FilePath -> IO TestTree
testMediaBagIO name odtFile = do
  outcome <- compareMediaBagIO odtFile
  return $ testCase name (assertBool
                          ("Media didn't match media bag in file " ++ odtFile)
                          outcome)

testMediaBag :: String -> FilePath -> TestTree
testMediaBag name odtFile = buildTest $ testMediaBagIO name odtFile
-}
--



namesOfTestsComparingToMarkdown :: [ String ]
namesOfTestsComparingToMarkdown  = [ "blockquote2"
                                   , "bold"
--                                 , "citation"
                                   , "endnote"
                                   , "externalLink"
                                   , "footnote"
                                   , "formula"
                                   , "headers"
--                                 , "horizontalRule"
                                   , "italic"
--                                 , "listBlocks"
                                   , "paragraph"
                                   , "strikeout"
--                                 , "trackedChanges"
                                   , "underlined"
                                   ]

namesOfTestsComparingToNative  :: [ String ]
namesOfTestsComparingToNative   = [ "blockquote"
                                  , "image"
                                  , "imageIndex"
                                  , "imageWithCaption"
                                  , "inlinedCode"
                                  , "listContinueNumbering"
                                  , "listContinueNumbering2"
                                  , "orderedListMixed"
                                  , "orderedListRoman"
                                  , "orderedListSimple"
                                  , "orderedListHeader"
                                  , "referenceToChapter"
                                  , "referenceToListItem"
                                  , "referenceToText"
                                  , "simpleTable"
                                  , "simpleTableWithCaption"
                                  , "simpleTableWithHeader"
                                  , "simpleTableWithMultipleHeaderRows"
                                  , "tab"
--                                , "table"
                                  , "textMixedStyles"
                                  , "tableWithContents"
                                  , "tableWithSpans"
                                  , "unicode"
                                  , "unorderedList"
                                  , "unorderedListHeader"
                                  ]
