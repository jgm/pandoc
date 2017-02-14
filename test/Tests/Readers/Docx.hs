module Tests.Readers.Docx (tests) where

import Text.Pandoc
import Tests.Helpers
import Test.Framework
import Test.HUnit (assertBool)
import Test.Framework.Providers.HUnit
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Text.Pandoc.MediaBag (MediaBag, lookupMedia, mediaDirectory)
import Codec.Archive.Zip
import qualified Text.Pandoc.Class as P

-- We define a wrapper around pandoc that doesn't normalize in the
-- tests. Since we do our own normalization, we want to make sure
-- we're doing it right.

data NoNormPandoc = NoNormPandoc {unNoNorm :: Pandoc}
                 deriving Show

noNorm :: Pandoc -> NoNormPandoc
noNorm = NoNormPandoc

defopts :: ReaderOptions
defopts = def{ readerExtensions = getDefaultExtensions "docx" }

instance ToString NoNormPandoc where
  toString d = purely (writeNative def{ writerTemplate = s }) $ toPandoc d
   where s = case d of
                  NoNormPandoc (Pandoc (Meta m) _)
                    | M.null m  -> Nothing
                    | otherwise -> Just "" -- need this to get meta output

instance ToPandoc NoNormPandoc where
  toPandoc = unNoNorm

compareOutput :: ReaderOptions
                 -> FilePath
                 -> FilePath
                 -> IO (NoNormPandoc, NoNormPandoc)
compareOutput opts docxFile nativeFile = do
  df <- B.readFile docxFile
  nf <- Prelude.readFile nativeFile
  p <- runIOorExplode $ readDocx opts df
  df' <- runIOorExplode $ readNative def nf
  return $ (noNorm p, noNorm df')

testCompareWithOptsIO :: ReaderOptions -> String -> FilePath -> FilePath -> IO Test
testCompareWithOptsIO opts name docxFile nativeFile = do
  (dp, np) <- compareOutput opts docxFile nativeFile
  return $ test id name (dp, np)

testCompareWithOpts :: ReaderOptions -> String -> FilePath -> FilePath -> Test
testCompareWithOpts opts name docxFile nativeFile =
  buildTest $ testCompareWithOptsIO opts name docxFile nativeFile

testCompare :: String -> FilePath -> FilePath -> Test
testCompare = testCompareWithOpts defopts

testForWarningsWithOptsIO :: ReaderOptions -> String -> FilePath -> [String] -> IO Test
testForWarningsWithOptsIO opts name docxFile expected = do
  df <- B.readFile docxFile
  logs <-  runIOorExplode (readDocx opts df >> P.getLog)
  let warns = [s | (WARNING, s) <- logs]
  return $ test id name (unlines warns, unlines expected)

testForWarningsWithOpts :: ReaderOptions -> String -> FilePath -> [String] -> Test
testForWarningsWithOpts opts name docxFile expected =
  buildTest $ testForWarningsWithOptsIO opts name docxFile expected

-- testForWarnings :: String -> FilePath -> [String] -> Test
-- testForWarnings = testForWarningsWithOpts defopts

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
    mb <- runIOorExplode (readDocx defopts df >> P.getMediaBag)
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
            "normalizing adjacent hyperlinks"
            "docx/adjacent_links.docx"
            "docx/adjacent_links.native"
          , testCompare
            "inline image"
            "docx/image.docx"
            "docx/image_no_embed.native"
          , testCompare
            "VML image"
            "docx/image_vml.docx"
            "docx/image_vml.native"
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
            "special punctuation"
            "docx/special_punctuation.docx"
            "docx/special_punctuation.native"
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
          , testCompare
            "inline code in subscript and superscript"
            "docx/verbatim_subsuper.docx"
            "docx/verbatim_subsuper.native"
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
            "nested anchor spans in header"
            "docx/nested_anchors_in_header.docx"
            "docx/nested_anchors_in_header.native"
          , testCompare
            "single numbered item not made into list"
            "docx/numbered_header.docx"
            "docx/numbered_header.native"
          , testCompare
            "enumerated headers not made into numbered list"
            "docx/enumerated_headings.docx"
            "docx/enumerated_headings.native"
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
            "custom defined lists in styles"
            "docx/german_styled_lists.docx"
            "docx/german_styled_lists.native"
          , testCompare
            "user deletes bullet after list item (=> part of item par)"
            "docx/dummy_item_after_list_item.docx"
            "docx/dummy_item_after_list_item.native"
          , testCompare
            "user deletes bullet after par (=> new par)"
            "docx/dummy_item_after_paragraph.docx"
            "docx/dummy_item_after_paragraph.native"
          , testCompare
            "footnotes and endnotes"
            "docx/notes.docx"
            "docx/notes.native"
          , testCompare
            "links in footnotes and endnotes"
            "docx/link_in_notes.docx"
            "docx/link_in_notes.native"
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
            "tables with lists in cells"
            "docx/table_with_list_cell.docx"
            "docx/table_with_list_cell.native"
          , testCompare
            "tables with one row"
            "docx/table_one_row.docx"
            "docx/table_one_row.native"
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
          , testCompareWithOpts def{readerTrackChanges=AcceptChanges}
            "move text (accept)"
            "docx/track_changes_move.docx"
            "docx/track_changes_move_accept.native"
          , testCompareWithOpts def{readerTrackChanges=RejectChanges}
            "move text (reject)"
            "docx/track_changes_move.docx"
            "docx/track_changes_move_reject.native"
          , testCompareWithOpts def{readerTrackChanges=AllChanges}
            "move text (all)"
            "docx/track_changes_move.docx"
            "docx/track_changes_move_all.native"
          , testCompareWithOpts def{readerTrackChanges=AcceptChanges}
            "comments (accept -- no comments)"
            "docx/comments.docx"
            "docx/comments_no_comments.native"
          , testCompareWithOpts def{readerTrackChanges=RejectChanges}
            "comments (reject -- comments)"
            "docx/comments.docx"
            "docx/comments_no_comments.native"
          , testCompareWithOpts def{readerTrackChanges=AllChanges}
            "comments (all comments)"
            "docx/comments.docx"
            "docx/comments.native"
          , testForWarningsWithOpts def{readerTrackChanges=AcceptChanges}
            "comment warnings (accept -- no warnings)"
            "docx/comments_warning.docx"
            []
          , testForWarningsWithOpts def{readerTrackChanges=RejectChanges}
            "comment warnings (reject -- no warnings)"
            "docx/comments_warning.docx"
            []
          , testForWarningsWithOpts def{readerTrackChanges=AllChanges}
            "comment warnings (all)"
            "docx/comments_warning.docx"
            ["Docx comment 1 will not retain formatting"]
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
