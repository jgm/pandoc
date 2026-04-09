{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.Docx (tests) where

import Codec.Archive.Zip (findEntryByPath, fromEntry, toArchive)
import qualified Data.ByteString.Lazy as BL
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as T
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Writers.OOXML
import Text.Pandoc
import Text.XML.Light (QName(QName), findAttr, findElements, parseXMLDoc)

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
            def{ writerExtensions =
                   enableExtension Ext_native_numbering (writerExtensions def) }
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
            "lists with div bullets"
            def
            "docx/lists_div_bullets.native"
            "docx/golden/lists_div_bullets.docx"
          , docxTest
            "definition lists"
            def
            "docx/definition_list.native"
            "docx/golden/definition_list.docx"
          , docxTest
            "task lists"
            def
            "docx/task_list.native"
            "docx/golden/task_list.docx"
          , docxTest
            "issue 9994"
            def
            "docx/lists_9994.native"
            "docx/golden/lists_9994.docx"
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
            "docx/block_quotes.native"
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
            "tables separated with RawBlock"
            def
            "docx/tables_separated_with_rawblock.native"
            "docx/golden/tables_separated_with_rawblock.docx"
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
        , testGroup "top-level-division"
          -- Helper to count occurrences of a substring
          -- Note: counts by splitting on "<w:sectPr" which marks section properties
          [ testCase "no section break before first chapter (#10578)" $ do
              -- With --top-level-division=chapter, there should be no section
              -- break before the first chapter (to avoid blank first page)
              let opts = def{ writerTopLevelDivision = TopLevelChapter }
              bs <- runIOorExplode $ do
                setVerbosity ERROR
                let doc = Pandoc mempty
                          [ Header 1 ("ch1", [], []) [Str "Chapter", Space, Str "1"]
                          , Para [Str "First", Space, Str "chapter."]
                          ]
                writeDocx opts doc
              let archive = toArchive bs
              entry <- case findEntryByPath "word/document.xml" archive of
                Nothing -> assertFailure "Missing word/document.xml in output docx"
                Just e -> return e
              let docXml = show (fromEntry entry)
              -- Count occurrences of "<w:sectPr" (opening tag for section properties)
              let countOccurrences needle haystack =
                    length (filter (needle `isPrefixOf`) (tails haystack))
                    where tails [] = []; tails s@(_:xs) = s : tails xs
              let sectPrCount = countOccurrences "<w:sectPr" docXml
              -- Should have exactly 1 sectPr (the final document section),
              -- not 2 (which would mean one before the chapter heading)
              assertBool ("Expected 1 sectPr (final section only), found " ++ show sectPrCount)
                (sectPrCount == 1)
          , testCase "section breaks between chapters (#11482)" $ do
              -- With --top-level-division=chapter, there should be section
              -- breaks between chapters (but not before the first one)
              let opts = def{ writerTopLevelDivision = TopLevelChapter }
              bs <- runIOorExplode $ do
                setVerbosity ERROR
                let doc = Pandoc mempty
                          [ Header 1 ("ch1", [], []) [Str "Chapter", Space, Str "1"]
                          , Para [Str "First", Space, Str "chapter."]
                          , Header 1 ("ch2", [], []) [Str "Chapter", Space, Str "2"]
                          , Para [Str "Second", Space, Str "chapter."]
                          , Header 1 ("ch3", [], []) [Str "Chapter", Space, Str "3"]
                          , Para [Str "Third", Space, Str "chapter."]
                          ]
                writeDocx opts doc
              let archive = toArchive bs
              entry <- case findEntryByPath "word/document.xml" archive of
                Nothing -> assertFailure "Missing word/document.xml in output docx"
                Just e -> return e
              let docXml = show (fromEntry entry)
              -- Count occurrences of "<w:sectPr" (opening tag for section properties)
              let countOccurrences needle haystack =
                    length (filter (needle `isPrefixOf`) (tails haystack))
                    where tails [] = []; tails s@(_:xs) = s : tails xs
              let sectPrCount = countOccurrences "<w:sectPr" docXml
              -- Should have 3 sectPr elements:
              -- - 1 before chapter 2
              -- - 1 before chapter 3
              -- - 1 final document section
              -- (No section break before chapter 1)
              assertBool ("Expected 3 sectPr elements, found " ++ show sectPrCount)
                (sectPrCount == 3)
          ]
        , testGroup "reference docx"
          [ testCase "no media directory override in content types" $ do
              let opts = def{ writerReferenceDoc = Just "docx/inline_images.docx" }
              txt <- T.readFile "docx/inline_formatting.native"
              bs <- runIOorExplode $ do
                mblang <- toLang (Just (Text.pack "en-US") :: Maybe Text)
                maybe (return ()) setTranslations mblang
                setVerbosity ERROR
                readNative def txt >>= writeDocx opts
              let archive = toArchive bs
              entry <- case findEntryByPath "[Content_Types].xml" archive of
                Nothing -> assertFailure "Missing [Content_Types].xml in output docx"
                Just e -> return e
              doc <- case parseXMLDoc (fromEntry entry) of
                Nothing -> assertFailure "Failed to parse [Content_Types].xml"
                Just d -> return d
              let partNameAttr = QName "PartName" Nothing Nothing
              let overrideName = QName "Override" Nothing Nothing
              let overrides = findElements overrideName doc
              let hasBadOverride =
                    any (\el -> findAttr partNameAttr el == Just "/word/media/")
                        overrides
              assertBool "Found invalid /word/media/ Override in [Content_Types].xml"
                (not hasBadOverride)
          , testCase "language from reference docx is preserved" $ do
              -- First, verify that the german-reference.docx actually has de-DE
              refBs <- BL.readFile "docx/german-reference.docx"
              let refArchive = toArchive refBs
              refEntry <- case findEntryByPath "word/styles.xml" refArchive of
                Nothing -> assertFailure "Missing word/styles.xml in german-reference.docx"
                Just e -> return e
              let refStylesXml = show (fromEntry refEntry)
              let getLangLines = filter ("w:lang" `isInfixOf`) . lines
              assertBool ("german-reference.docx w:lang line: " ++
                  unlines (getLangLines refStylesXml))
                (any ("de-DE" `isInfixOf`) (getLangLines refStylesXml))
              -- Now test that using this reference preserves the language
              let opts = def{ writerReferenceDoc = Just "docx/german-reference.docx" }
              txt <- T.readFile "docx/inline_formatting.native"
              bs <- runIOorExplode $ do
                setVerbosity ERROR
                readNative def txt >>= writeDocx opts
              let archive = toArchive bs
              entry <- case findEntryByPath "word/styles.xml" archive of
                Nothing -> assertFailure "Missing word/styles.xml in output docx"
                Just e -> return e
              let stylesXml = show (fromEntry entry)
              -- Find the w:lang line for debugging
              -- Check that the styles.xml contains the German language
              assertBool ("Language from reference docx not preserved. w:lang lines: " ++ unlines (getLangLines stylesXml))
                (any ("de-DE" `isInfixOf`) (getLangLines stylesXml))
          , testCase "language from metadata overrides reference docx" $ do
              -- Use a reference docx with German language, but specify French in metadata
              let opts = def{ writerReferenceDoc = Just "docx/german-reference.docx" }
              bs <- runIOorExplode $ do
                setVerbosity ERROR
                -- Create a document with French language metadata
                let doc = Pandoc (Meta $ M.fromList [("lang", MetaString "fr-FR")])
                                 [Para [Str "Test"]]
                writeDocx opts doc
              let archive = toArchive bs
              entry <- case findEntryByPath "word/styles.xml" archive of
                Nothing -> assertFailure "Missing word/styles.xml in output docx"
                Just e -> return e
              let stylesXml = show (fromEntry entry)
              -- Check that the styles.xml contains the French language (not German)
              let getLangLines = filter ("w:lang" `isInfixOf`) . lines
              assertBool "Language from metadata did not override reference docx (expected fr-FR)"
                (any ("fr-FR" `isInfixOf`) (getLangLines stylesXml))
          ]
        , testGroup "paragraph styles"
          [ testCase "FirstParagraph after heading with footnote (#11573)" $ do
              let opts = def
              bs <- runIOorExplode $ do
                setVerbosity ERROR
                let doc = Pandoc mempty
                          [ Header 3 ("heading-with-note", [], [])
                              [Note [Para [Str "note"]], Str "Heading"]
                          , Para [Str "Para", Space, Str "after."]
                          ]
                writeDocx opts doc
              let archive = toArchive bs
              entry <- case findEntryByPath "word/document.xml" archive of
                Nothing -> assertFailure "Missing word/document.xml in output docx"
                Just e -> return e
              let docXml = show (fromEntry entry)
              assertBool
                ("Expected FirstParagraph style after heading with footnote, got: "
                  ++ docXml)
                ("FirstParagraph" `isInfixOf` docXml)
          ]
        ]
