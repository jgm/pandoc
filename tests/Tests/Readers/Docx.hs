module Tests.Readers.Docx (tests) where

import Text.Pandoc.Options
import Text.Pandoc.Readers.Native
import Text.Pandoc.Definition
import Tests.Helpers
import Test.Framework
import Test.HUnit (assertBool)
import Test.Framework.Providers.HUnit
import qualified Data.ByteString.Lazy as B
import Text.Pandoc.Readers.Docx
import Text.Pandoc.Writers.Native (writeNative)
import qualified Data.Map as M
import Text.Pandoc.MediaBag (MediaBag, lookupMedia, mediaDirectory)
import Codec.Archive.Zip

-- We define a wrapper around pandoc that doesn't normalize in the
-- tests. Since we do our own normalization, we want to make sure
-- we're doing it right.

data NoNormPandoc = NoNormPandoc {unNoNorm :: Pandoc}
                 deriving Show

noNorm :: Pandoc -> NoNormPandoc
noNorm = NoNormPandoc

instance ToString NoNormPandoc where
  toString d = writeNative def{ writerStandalone = s } $ toPandoc d
   where s = case d of
                  NoNormPandoc (Pandoc (Meta m) _)
                    | M.null m  -> False
                    | otherwise -> True

instance ToPandoc NoNormPandoc where
  toPandoc = unNoNorm

compareOutput :: ReaderOptions
                 -> FilePath
                 -> FilePath
                 -> IO (NoNormPandoc, NoNormPandoc)
compareOutput opts docxFile nativeFile = do
  df <- B.readFile docxFile
  nf <- Prelude.readFile nativeFile
  let (p, _) = readDocx opts df
  return $ (noNorm p, noNorm (readNative nf))

testCompareWithOptsIO :: ReaderOptions -> String -> FilePath -> FilePath -> IO Test
testCompareWithOptsIO opts name docxFile nativeFile = do
  (dp, np) <- compareOutput opts docxFile nativeFile
  return $ test id name (dp, np)

testCompareWithOpts :: ReaderOptions -> String -> FilePath -> FilePath -> Test
testCompareWithOpts opts name docxFile nativeFile =
  buildTest $ testCompareWithOptsIO opts name docxFile nativeFile

testCompare :: String -> FilePath -> FilePath -> Test
testCompare = testCompareWithOpts def

getMedia :: FilePath -> FilePath -> IO (Maybe B.ByteString)
getMedia archivePath mediaPath = do
  zf <- B.readFile archivePath >>= return . toArchive
  return $ findEntryByPath ("word/" ++ mediaPath) zf >>= (Just . fromEntry)

compareMediaPathIO :: FilePath -> MediaBag -> FilePath -> IO Bool
compareMediaPathIO mediaPath mediaBag docxPath = do
  docxMedia <- getMedia docxPath mediaPath
  let mbBS   = case lookupMedia mediaPath mediaBag of
                 Just (_, bs) -> bs
                 Nothing      -> error ("couldn't find " ++
                                        mediaPath ++
                                        " in media bag")
      docxBS = case docxMedia of
                 Just bs -> bs
                 Nothing -> error ("couldn't find " ++
                                   mediaPath ++
                                   " in media bag")
  return $ mbBS == docxBS

compareMediaBagIO :: FilePath -> IO Bool
compareMediaBagIO docxFile = do
    df <- B.readFile docxFile
    let (_, mb) = readDocx def df
    bools <- mapM
             (\(fp, _, _) -> compareMediaPathIO fp mb docxFile)
             (mediaDirectory mb)
    return $ and bools

testMediaBagIO :: String -> FilePath -> IO Test
testMediaBagIO name docxFile = do
  outcome <- compareMediaBagIO docxFile
  return $ testCase name (assertBool
                          ("Media didn't match media bag in file " ++ docxFile)
                          outcome)

testMediaBag :: String -> FilePath -> Test
testMediaBag name docxFile = buildTest $ testMediaBagIO name docxFile

tests :: [Test]
tests = [ testGroup "inlines"
          [ testCompare
            "font formatting"
            "docx/inline_formatting.docx"
            "docx/inline_formatting.native"
          , testCompare
            "font formatting with character styles"
            "docx/char_styles.docx"
            "docx/char_styles.native"
          , testCompare
            "hyperlinks"
            "docx/links.docx"
            "docx/links.native"
          , testCompare
            "inline image"
            "docx/image.docx"
            "docx/image_no_embed.native"
          , testCompare
            "inline image in links"
            "docx/inline_images.docx"
            "docx/inline_images.native"
          , testCompare
            "handling unicode input"
            "docx/unicode.docx"
            "docx/unicode.native"
          , testCompare
            "literal tabs"
            "docx/tabs.docx"
            "docx/tabs.native"
          , testCompare
            "normalizing inlines"
            "docx/normalize.docx"
            "docx/normalize.native"
          , testCompare
            "normalizing inlines deep inside blocks"
            "docx/deep_normalize.docx"
            "docx/deep_normalize.native"
          , testCompare
            "move trailing spaces outside of formatting"
            "docx/trailing_spaces_in_formatting.docx"
            "docx/trailing_spaces_in_formatting.native"
          , testCompare
            "inline code (with VerbatimChar style)"
            "docx/inline_code.docx"
            "docx/inline_code.native"
          ]
        , testGroup "blocks"
          [ testCompare
            "headers"
            "docx/headers.docx"
            "docx/headers.native"
          , testCompare
            "headers already having auto identifiers"
            "docx/already_auto_ident.docx"
            "docx/already_auto_ident.native"
          , testCompare
            "numbered headers automatically made into list"
            "docx/numbered_header.docx"
            "docx/numbered_header.native"
          , testCompare
            "i18n blocks (headers and blockquotes)"
            "docx/i18n_blocks.docx"
            "docx/i18n_blocks.native"
          , testCompare
            "lists"
            "docx/lists.docx"
            "docx/lists.native"
          , testCompare
            "definition lists"
            "docx/definition_list.docx"
            "docx/definition_list.native"
          , testCompare
            "footnotes and endnotes"
            "docx/notes.docx"
            "docx/notes.native"
          , testCompare
            "blockquotes (parsing indent as blockquote)"
            "docx/block_quotes.docx"
            "docx/block_quotes_parse_indent.native"
          , testCompare
            "hanging indents"
            "docx/hanging_indent.docx"
            "docx/hanging_indent.native"
          , testCompare
            "tables"
            "docx/tables.docx"
            "docx/tables.native"
          , testCompare
            "code block"
            "docx/codeblock.docx"
            "docx/codeblock.native"
          , testCompare
            "dropcap paragraphs"
            "docx/drop_cap.docx"
            "docx/drop_cap.native"
          ]
        , testGroup "track changes"
          [ testCompare
            "insertion (default)"
            "docx/track_changes_insertion.docx"
            "docx/track_changes_insertion_accept.native"
          , testCompareWithOpts def{readerTrackChanges=AcceptChanges}
            "insert insertion (accept)"
            "docx/track_changes_insertion.docx"
            "docx/track_changes_insertion_accept.native"
          , testCompareWithOpts def{readerTrackChanges=RejectChanges}
            "remove insertion (reject)"
            "docx/track_changes_insertion.docx"
            "docx/track_changes_insertion_reject.native"
          , testCompare
            "deletion (default)"
            "docx/track_changes_deletion.docx"
            "docx/track_changes_deletion_accept.native"
          , testCompareWithOpts def{readerTrackChanges=AcceptChanges}
            "remove deletion (accept)"
            "docx/track_changes_deletion.docx"
            "docx/track_changes_deletion_accept.native"
          , testCompareWithOpts def{readerTrackChanges=RejectChanges}
            "insert deletion (reject)"
            "docx/track_changes_deletion.docx"
            "docx/track_changes_deletion_reject.native"
          , testCompareWithOpts def{readerTrackChanges=AllChanges}
            "keep insertion (all)"
            "docx/track_changes_deletion.docx"
            "docx/track_changes_deletion_all.native"
          , testCompareWithOpts def{readerTrackChanges=AllChanges}
            "keep deletion (all)"
            "docx/track_changes_deletion.docx"
            "docx/track_changes_deletion_all.native"
          ]
        , testGroup "media"
          [ testMediaBag
            "image extraction"
            "docx/image.docx"
          ]
        , testGroup "metadata"
          [ testCompareWithOpts def{readerStandalone=True}
            "metadata fields"
            "docx/metadata.docx"
            "docx/metadata.native"
          , testCompareWithOpts def{readerStandalone=True}
            "stop recording metadata with normal text"
            "docx/metadata_after_normal.docx"
            "docx/metadata_after_normal.native"
          ]

        ]
