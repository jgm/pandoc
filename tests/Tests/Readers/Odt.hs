module Tests.Readers.Odt (tests) where

import Control.Monad ( liftM )
import Text.Pandoc.Options
import Text.Pandoc.Readers.Native
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Tests.Helpers
import Test.Framework
--import Test.HUnit (assertBool)
--import Test.Framework.Providers.HUnit
import qualified Data.ByteString.Lazy as B
import Text.Pandoc.Readers.Odt
import Text.Pandoc.Writers.Native (writeNative)
import qualified Data.Map as M
--import Text.Pandoc.MediaBag (MediaBag, lookupMedia, mediaDirectory)
--import Codec.Archive.Zip


tests :: [Test]
tests = testsComparingToMarkdown ++ testsComparingToNative

testsComparingToMarkdown :: [Test]
testsComparingToMarkdown    = map nameToTest namesOfTestsComparingToMarkdown
  where nameToTest     name = createTest
                                compareOdtToMarkdown
                                name
                                (toOdtPath      name)
                                (toMarkdownPath name)
        toOdtPath      name = "odt/odt/"      ++ name ++ ".odt"
        toMarkdownPath name = "odt/markdown/" ++ name ++ ".md"

testsComparingToNative   :: [Test]
testsComparingToNative      = map nameToTest namesOfTestsComparingToNative
  where nameToTest     name = createTest
                                compareOdtToNative
                                name
                                (toOdtPath      name)
                                (toNativePath   name)
        toOdtPath      name = "odt/odt/"      ++ name ++ ".odt"
        toNativePath   name = "odt/native/"   ++ name ++ ".native"


newtype NoNormPandoc = NoNormPandoc {unNoNorm :: Pandoc}
  deriving ( Show )

instance ToString NoNormPandoc where
  toString d = writeNative def{ writerStandalone = s } $ toPandoc d
   where s = case d of
                  NoNormPandoc (Pandoc (Meta m) _)
                    | M.null m  -> False
                    | otherwise -> True

instance ToPandoc NoNormPandoc where
  toPandoc = unNoNorm

getNoNormVia :: (a -> Pandoc) -> String -> Either PandocError a -> NoNormPandoc
getNoNormVia _ readerName (Left  _) = error (readerName ++ " reader failed")
getNoNormVia f _          (Right a) = NoNormPandoc (f a)

type TestCreator =  ReaderOptions
                 -> FilePath -> FilePath
                 -> IO (NoNormPandoc, NoNormPandoc)

compareOdtToNative   :: TestCreator
compareOdtToNative opts odtPath nativePath = do
   nativeFile   <- Prelude.readFile nativePath
   odtFile      <- B.readFile       odtPath
   let native   =  getNoNormVia id  "native"   $ readNative        nativeFile
   let odt      =  getNoNormVia fst "odt"      $ readOdt      opts odtFile
   return (odt,native)

compareOdtToMarkdown :: TestCreator
compareOdtToMarkdown opts odtPath markdownPath = do
   markdownFile <- Prelude.readFile markdownPath
   odtFile      <- B.readFile       odtPath
   let markdown =  getNoNormVia id  "markdown" $ readMarkdown opts markdownFile
   let odt      =  getNoNormVia fst "odt"      $ readOdt      opts odtFile
   return (odt,markdown)


createTest :: TestCreator
           -> TestName
           -> FilePath -> FilePath
           -> Test
createTest   creator name path1 path2 =
  buildTest $ liftM (test id name) (creator def path1 path2)

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
    let (_, mb) = readOdt def df
    bools <- mapM
             (\(fp, _, _) -> compareMediaPathIO fp mb odtFile)
             (mediaDirectory mb)
    return $ and bools

testMediaBagIO :: String -> FilePath -> IO Test
testMediaBagIO name odtFile = do
  outcome <- compareMediaBagIO odtFile
  return $ testCase name (assertBool
                          ("Media didn't match media bag in file " ++ odtFile)
                          outcome)

testMediaBag :: String -> FilePath -> Test
testMediaBag name odtFile = buildTest $ testMediaBagIO name odtFile
-}
--



namesOfTestsComparingToMarkdown :: [ String ]
namesOfTestsComparingToMarkdown  = [ "bold"
--                                 , "citation"
                                   , "endnote"
                                   , "externalLink"
                                   , "footnote"
                                   , "headers"
--                                 , "horizontalRule"
--                                 , "image"
                                   , "italic"
--                                 , "listBlocks"
                                   , "paragraph"
                                   , "strikeout"
--                                 , "trackedChanges"
                                   , "underlined"
                                   ]

namesOfTestsComparingToNative  :: [ String ]
namesOfTestsComparingToNative   = [ "blockquote"
                                  , "orderedListMixed"
                                  , "orderedListRoman"
                                  , "orderedListSimple"
                                  , "referenceToChapter"
                                  , "referenceToListItem"
                                  , "referenceToText"
                                  , "simpleTable"
--                                , "table"
                                  , "unicode"
                                  , "unorderedList"
                                  ]