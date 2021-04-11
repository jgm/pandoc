module Tests.Writers.Docx (tests) where

import Text.Pandoc
import Test.Tasty
import Tests.Writers.OOXML
import Test.Tasty.HUnit
import Data.List (isPrefixOf)

-- we add an extra check to make sure that we're not writing in the
-- toplevel docx directory. We don't want to accidentally overwrite an
-- Word-generated docx file used to test the reader.
docxTest :: String -> WriterOptions -> FilePath -> FilePath -> TestTree
docxTest testName opts nativeFP goldenFP =
  if "docx/golden/" `isPrefixOf` goldenFP
  then ooxmlTest writeDocx testName opts nativeFP goldenFP
  else testCase testName $
       assertFailure $
       goldenFP ++ " is not in `test/docx/golden`"

tests :: [TestTree]
tests = [ testGroup "inlines"
          [ docxTest
            "font formatting"
            def
            "docx/inline_formatting.native"
            "docx/golden/inline_formatting.docx"
          , docxTest
            "hyperlinks"
            def
            "docx/links.native"
            "docx/golden/links.docx"
          , docxTest
            "inline image"
            def
            "docx/image_writer_test.native"
            "docx/golden/image.docx"
          , docxTest
            "inline images"
            def
            "docx/inline_images_writer_test.native"
            "docx/golden/inline_images.docx"
          , docxTest
            "handling unicode input"
            def
            "docx/unicode.native"
            "docx/golden/unicode.docx"
          , docxTest
            "inline code"
            def
            "docx/inline_code.native"
            "docx/golden/inline_code.docx"
          , docxTest
            "inline code in subscript and superscript"
            def
            "docx/verbatim_subsuper.native"
            "docx/golden/verbatim_subsuper.docx"
          ]
        , testGroup "blocks"
          [ docxTest
            "headers"
            def
            "docx/headers.native"
            "docx/golden/headers.docx"
          , docxTest
            "nested anchor spans in header"
            def
            "docx/nested_anchors_in_header.native"
            "docx/golden/nested_anchors_in_header.docx"
          , docxTest
            "lists"
            def
            "docx/lists.native"
            "docx/golden/lists.docx"
          , docxTest
            "lists continuing after interruption"
            def
            "docx/lists_continuing.native"
            "docx/golden/lists_continuing.docx"
          , docxTest
            "lists restarting after interruption"
            def
            "docx/lists_restarting.native"
            "docx/golden/lists_restarting.docx"
          , docxTest
            "lists with multiple initial list levels"
            def
            "docx/lists_multiple_initial.native"
            "docx/golden/lists_multiple_initial.docx"
          , docxTest
            "definition lists"
            def
            "docx/definition_list.native"
            "docx/golden/definition_list.docx"
          , docxTest
            "footnotes and endnotes"
            def
            "docx/notes.native"
            "docx/golden/notes.docx"
          , docxTest
            "links in footnotes and endnotes"
            def
            "docx/link_in_notes.native"
            "docx/golden/link_in_notes.docx"
          , docxTest
            "blockquotes"
            def
            "docx/block_quotes_parse_indent.native"
            "docx/golden/block_quotes.docx"
          , docxTest
            "tables"
            def
            "docx/tables.native"
            "docx/golden/tables.docx"
          , docxTest
            "tables without explicit column widths"
            def
            "docx/tables-default-widths.native"
            "docx/golden/tables-default-widths.docx"
          , docxTest
            "tables with lists in cells"
            def
            "docx/table_with_list_cell.native"
            "docx/golden/table_with_list_cell.docx"
          , docxTest
            "tables with one row"
            def
            "docx/table_one_row.native"
            "docx/golden/table_one_row.docx"
          , docxTest
            "code block"
            def
            "docx/codeblock.native"
            "docx/golden/codeblock.docx"
          , docxTest
            "raw OOXML blocks"
            def
            "docx/raw-blocks.native"
            "docx/golden/raw-blocks.docx"
          , docxTest
            "raw bookmark markers"
            def
            "docx/raw-bookmarks.native"
            "docx/golden/raw-bookmarks.docx"
          ]
        , testGroup "track changes"
          [ docxTest
            "insertion"
            def
            "docx/track_changes_insertion_all.native"
            "docx/golden/track_changes_insertion.docx"
          , docxTest
            "deletion"
            def
            "docx/track_changes_deletion_all.native"
            "docx/golden/track_changes_deletion.docx"
          , docxTest
            "move text"
            def
            "docx/track_changes_move_all.native"
            "docx/golden/track_changes_move.docx"
          , docxTest
            "comments"
            def
            "docx/comments.native"
            "docx/golden/comments.docx"
          , docxTest
            "scrubbed metadata"
            def
            "docx/track_changes_scrubbed_metadata.native"
            "docx/golden/track_changes_scrubbed_metadata.docx"
          ]
        , testGroup "custom styles"
          [ docxTest "custom styles without reference.docx"
            def
            "docx/custom_style.native"
            "docx/golden/custom_style_no_reference.docx"
          , docxTest "custom styles with reference.docx"
            def{writerReferenceDoc = Just "docx/custom-style-reference.docx"}
            "docx/custom_style.native"
            "docx/golden/custom_style_reference.docx"
          , docxTest "suppress custom style for headers and blockquotes"
            def
            "docx/custom-style-preserve.native"
            "docx/golden/custom_style_preserve.docx"
          ]
        , testGroup "metadata"
          [ docxTest "document properties (core, custom)"
            def
            "docx/document-properties.native"
            "docx/golden/document-properties.docx"
          , docxTest "document properties (short description)"
            def
            "docx/document-properties-short-desc.native"
            "docx/golden/document-properties-short-desc.docx"
          ]
        ]
