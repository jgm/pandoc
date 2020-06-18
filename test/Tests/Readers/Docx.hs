{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.Docx
   Copyright   : © 2017-2020 Jesse Rosenthal, John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
   Stability   : alpha
   Portability : portable

Tests for the word docx reader.
-}
module Tests.Readers.Docx (tests) where

import Codec.Archive.Zip
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Maybe
import System.IO.Unsafe
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Helpers
import Text.Pandoc
import qualified Text.Pandoc.Class as P
import qualified Text.Pandoc.MediaBag as MB
import Text.Pandoc.UTF8 as UTF8

-- We define a wrapper around pandoc that doesn't normalize in the
-- tests. Since we do our own normalization, we want to make sure
-- we're doing it right.

newtype NoNormPandoc = NoNormPandoc {unNoNorm :: Pandoc}
                 deriving Show

noNorm :: Pandoc -> NoNormPandoc
noNorm = NoNormPandoc

defopts :: ReaderOptions
defopts = def{ readerExtensions = getDefaultExtensions "docx" }

instance ToString NoNormPandoc where
  toString d = T.unpack $ purely (writeNative def{ writerTemplate = s }) $ toPandoc d
   where s = case d of
                  NoNormPandoc (Pandoc (Meta m) _)
                    | M.null m  -> Nothing
                    | otherwise -> Just mempty -- need this to get meta output

instance ToPandoc NoNormPandoc where
  toPandoc = unNoNorm

compareOutput :: ReaderOptions
                 -> FilePath
                 -> FilePath
                 -> IO (NoNormPandoc, NoNormPandoc)
compareOutput opts docxFile nativeFile = do
  df <- B.readFile docxFile
  nf <- UTF8.toText <$> BS.readFile nativeFile
  p <- runIOorExplode $ readDocx opts df
  df' <- runIOorExplode $ readNative def nf
  return (noNorm p, noNorm df')

testCompareWithOptsIO :: ReaderOptions -> String -> FilePath -> FilePath -> IO TestTree
testCompareWithOptsIO opts name docxFile nativeFile = do
  (dp, np) <- compareOutput opts docxFile nativeFile
  return $ test id name (dp, np)

testCompareWithOpts :: ReaderOptions -> String -> FilePath -> FilePath -> TestTree
testCompareWithOpts opts name docxFile nativeFile =
  unsafePerformIO $ testCompareWithOptsIO opts name docxFile nativeFile

testCompare :: String -> FilePath -> FilePath -> TestTree
testCompare = testCompareWithOpts defopts

testForWarningsWithOptsIO :: ReaderOptions -> String -> FilePath -> [String] -> IO TestTree
testForWarningsWithOptsIO opts name docxFile expected = do
  df <- B.readFile docxFile
  logs <-  runIOorExplode $ setVerbosity ERROR >> readDocx opts df >> P.getLog
  let warns = [m | DocxParserWarning m <- logs]
  return $ test id name (T.unlines warns, unlines expected)

testForWarningsWithOpts :: ReaderOptions -> String -> FilePath -> [String] -> TestTree
testForWarningsWithOpts opts name docxFile expected =
  unsafePerformIO $ testForWarningsWithOptsIO opts name docxFile expected

-- testForWarnings :: String -> FilePath -> [String] -> TestTree
-- testForWarnings = testForWarningsWithOpts defopts

getMedia :: FilePath -> FilePath -> IO (Maybe B.ByteString)
getMedia archivePath mediaPath = fmap fromEntry . findEntryByPath
    ("word/" ++ mediaPath) . toArchive <$> B.readFile archivePath

compareMediaPathIO :: FilePath -> MB.MediaBag -> FilePath -> IO Bool
compareMediaPathIO mediaPath mediaBag docxPath = do
  docxMedia <- getMedia docxPath mediaPath
  let mbBS   = case MB.lookupMedia mediaPath mediaBag of
                 Just item    -> MB.mediaContents item
                 Nothing      -> error ("couldn't find " ++
                                        mediaPath ++
                                        " in media bag")
      docxBS = fromMaybe (error ("couldn't find " ++
                        mediaPath ++
                        " in media bag")) docxMedia
  return $ mbBS == docxBS

compareMediaBagIO :: FilePath -> IO Bool
compareMediaBagIO docxFile = do
    df <- B.readFile docxFile
    mb <- runIOorExplode $ readDocx defopts df >> P.getMediaBag
    bools <- mapM
             (\(fp, _, _) -> compareMediaPathIO fp mb docxFile)
             (MB.mediaDirectory mb)
    return $ and bools

testMediaBagIO :: String -> FilePath -> IO TestTree
testMediaBagIO name docxFile = do
  outcome <- compareMediaBagIO docxFile
  return $ testCase name (assertBool
                          ("Media didn't match media bag in file " ++ docxFile)
                          outcome)

testMediaBag :: String -> FilePath -> TestTree
testMediaBag name docxFile = unsafePerformIO $ testMediaBagIO name docxFile

tests :: [TestTree]
tests = [ testGroup "document"
          [ testCompare
            "allow different document.xml file as defined in _rels/.rels"
            "docx/alternate_document_path.docx"
            "docx/alternate_document_path.native"
          ]
        , testGroup "inlines"
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
            "hyperlinks in <w:instrText> tag"
            "docx/instrText_hyperlink.docx"
            "docx/instrText_hyperlink.native"
          , testCompare
            "inline image"
            "docx/image.docx"
            "docx/image_no_embed.native"
          , testCompare
            "VML image"
            "docx/image_vml.docx"
            "docx/image_vml.native"
          , testCompare
            "VML image as object"
            "docx/image_vml_as_object.docx"
            "docx/image_vml_as_object.native"
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
            "remove trailing spaces from last inline"
            "docx/trim_last_inline.docx"
            "docx/trim_last_inline.native"
          , testCompare
            "inline code (with VerbatimChar style)"
            "docx/inline_code.docx"
            "docx/inline_code.native"
          , testCompare
            "inline code in subscript and superscript"
            "docx/verbatim_subsuper.docx"
            "docx/verbatim_subsuper.native"
          , testCompare
            "inlines inside of Structured Document Tags"
            "docx/sdt_elements.docx"
            "docx/sdt_elements.native"
          , testCompare
            "Structured Document Tags in footnotes"
            "docx/sdt_in_footnote.docx"
            "docx/sdt_in_footnote.native"
          , testCompare
            "nested Structured Document Tags"
            "docx/nested_sdt.docx"
            "docx/nested_sdt.native"
          , testCompare
            "nested Smart Tags"
            "docx/nested_smart_tags.docx"
            "docx/nested_smart_tags.native"
          , testCompare
            "remove anchor spans with nothing pointing to them"
            "docx/unused_anchors.docx"
            "docx/unused_anchors.native"
          , testCompare
            "collapse overlapping targets (anchor spans)"
            "docx/overlapping_targets.docx"
            "docx/overlapping_targets.native"
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
            "avoid zero-level headers"
            "docx/0_level_headers.docx"
            "docx/0_level_headers.native"
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
            "compact lists"
            "docx/lists-compact.docx"
            "docx/lists-compact.native"
          , testCompare
            "lists with level overrides"
            "docx/lists_level_override.docx"
            "docx/lists_level_override.native"
          , testCompare
            "lists continuing after interruption"
            "docx/lists_continuing.docx"
            "docx/lists_continuing.native"
          , testCompare
            "lists restarting after interruption"
            "docx/lists_restarting.docx"
            "docx/lists_restarting.native"
          , testCompare
            "sublists reset numbering to 1"
            "docx/lists_sublist_reset.docx"
            "docx/lists_sublist_reset.native"
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
            "a table with a header which contains rowspans greater than 1"
            "docx/table_header_rowspan.docx"
            "docx/table_header_rowspan.native"
          , testCompare
            "tables with one row"
            "docx/table_one_row.docx"
            "docx/table_one_row.native"
          , testCompare
            "tables with just one row, which is a header"
            "docx/table_one_header_row.docx"
            "docx/table_one_header_row.native"
          , testCompare
            "tables with variable width"
            "docx/table_variable_width.docx"
            "docx/table_variable_width.native"
          , testCompare
            "tables with captions which contain a Table field"
            "docx/table_captions_with_field.docx"
            "docx/table_captions_with_field.native"
          , testCompare
            "tables with captions which don't contain a Table field"
            "docx/table_captions_no_field.docx"
            "docx/table_captions_no_field.native"
          , testCompare
            "code block"
            "docx/codeblock.docx"
            "docx/codeblock.native"
          , testCompare
            "combine adjacent code blocks"
            "docx/adjacent_codeblocks.docx"
            "docx/adjacent_codeblocks.native"
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
          , testCompareWithOpts def{readerTrackChanges=AcceptChanges}
            "paragraph insertion/deletion (accept)"
            "docx/paragraph_insertion_deletion.docx"
            "docx/paragraph_insertion_deletion_accept.native"
          , testCompareWithOpts def{readerTrackChanges=RejectChanges}
            "paragraph insertion/deletion (reject)"
            "docx/paragraph_insertion_deletion.docx"
            "docx/paragraph_insertion_deletion_reject.native"
          , testCompareWithOpts def{readerTrackChanges=AllChanges}
            "paragraph insertion/deletion (all)"
            "docx/paragraph_insertion_deletion.docx"
            "docx/paragraph_insertion_deletion_all.native"
          , testCompareWithOpts def{readerTrackChanges=AllChanges}
            "paragraph insertion/deletion (all)"
            "docx/track_changes_scrubbed_metadata.docx"
            "docx/track_changes_scrubbed_metadata.native"
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
        , testGroup "custom styles"
          [ testCompare
            "custom styles (`+styles`) not enabled (default)"
            "docx/custom-style-reference.docx"
            "docx/custom-style-no-styles.native"
          , testCompareWithOpts
            def{readerExtensions=extensionsFromList [Ext_styles]}
            "custom styles (`+styles`) enabled"
            "docx/custom-style-reference.docx"
            "docx/custom-style-with-styles.native"
          , testCompareWithOpts
            def{readerExtensions=extensionsFromList [Ext_styles]}
            "custom styles (`+styles`): Compact style is removed from output"
            "docx/compact-style-removal.docx"
            "docx/compact-style-removal.native"
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
