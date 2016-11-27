module Tests.Writers.Docx (tests) where

import Text.Pandoc.Options
import Text.Pandoc.Readers.Native
import Text.Pandoc.Definition
import Tests.Helpers
import Test.Framework
import Text.Pandoc.Readers.Docx
import Text.Pandoc.Writers.Docx
import Text.Pandoc.Error
import System.FilePath ((</>))
import Text.Pandoc.Class (runIOorExplode)

type Options = (WriterOptions, ReaderOptions)

compareOutput :: Options
              -> FilePath
              -> FilePath
              -> IO (Pandoc, Pandoc)
compareOutput opts nativeFileIn nativeFileOut = do
  nf <- Prelude.readFile nativeFileIn
  nf' <- Prelude.readFile nativeFileOut
  let wopts = fst opts
  df <- runIOorExplode $ do
            d <- handleError <$> readNative nf
            writeDocx wopts{writerUserDataDir = Just (".." </> "data")} d
  df' <- handleError <$> runIOorExplode (readNative nf')
  let (p, _) = handleError $ readDocx (snd opts) df
  return (p, df')

testCompareWithOptsIO :: Options -> String -> FilePath -> FilePath -> IO Test
testCompareWithOptsIO opts name nativeFileIn nativeFileOut = do
  (dp, np) <- compareOutput opts nativeFileIn nativeFileOut
  return $ test id name (dp, np)

testCompareWithOpts :: Options -> String -> FilePath -> FilePath -> Test
testCompareWithOpts opts name nativeFileIn nativeFileOut =
  buildTest $ testCompareWithOptsIO opts name nativeFileIn nativeFileOut

roundTripCompareWithOpts :: Options -> String -> FilePath -> Test
roundTripCompareWithOpts opts name nativeFile =
  testCompareWithOpts opts name nativeFile nativeFile

-- testCompare :: String -> FilePath -> FilePath -> Test
-- testCompare = testCompareWithOpts def

roundTripCompare :: String -> FilePath -> Test
roundTripCompare = roundTripCompareWithOpts def

tests :: [Test]
tests = [ testGroup "inlines"
          [ roundTripCompare
            "font formatting"
            "docx/inline_formatting_writer.native"
          , roundTripCompare
            "font formatting with character styles"
            "docx/char_styles.native"
          , roundTripCompare
            "hyperlinks"
            "docx/links_writer.native"
          , roundTripCompare
            "inline image"
            "docx/image_no_embed_writer.native"
          , roundTripCompare
            "inline image in links"
            "docx/inline_images_writer.native"
          , roundTripCompare
            "handling unicode input"
            "docx/unicode.native"
          , roundTripCompare
            "literal tabs"
            "docx/tabs.native"
          , roundTripCompare
            "normalizing inlines"
            "docx/normalize.native"
          , roundTripCompare
            "normalizing inlines deep inside blocks"
            "docx/deep_normalize.native"
          , roundTripCompare
            "move trailing spaces outside of formatting"
            "docx/trailing_spaces_in_formatting.native"
          , roundTripCompare
            "inline code (with VerbatimChar style)"
            "docx/inline_code.native"
          , roundTripCompare
            "inline code in subscript and superscript"
            "docx/verbatim_subsuper.native"
          ]
        , testGroup "blocks"
          [ roundTripCompare
            "headers"
            "docx/headers.native"
          , roundTripCompare
            "headers already having auto identifiers"
            "docx/already_auto_ident.native"
          , roundTripCompare
            "numbered headers automatically made into list"
            "docx/numbered_header.native"
          , roundTripCompare
            "i18n blocks (headers and blockquotes)"
            "docx/i18n_blocks.native"
          -- Continuation does not survive round-trip
          , roundTripCompare
            "lists"
            "docx/lists_writer.native"
          , roundTripCompare
            "definition lists"
            "docx/definition_list.native"
          , roundTripCompare
            "custom defined lists in styles"
            "docx/german_styled_lists.native"
          , roundTripCompare
            "footnotes and endnotes"
            "docx/notes.native"
          , roundTripCompare
            "blockquotes (parsing indent as blockquote)"
            "docx/block_quotes_parse_indent.native"
          , roundTripCompare
            "hanging indents"
            "docx/hanging_indent.native"
          -- tables headers do not survive round-trip, should look into that
          , roundTripCompare
            "tables"
            "docx/tables.native"
          , roundTripCompare
            "tables with lists in cells"
            "docx/table_with_list_cell.native"
          , roundTripCompare
            "code block"
            "docx/codeblock.native"
          , roundTripCompare
            "dropcap paragraphs"
            "docx/drop_cap.native"
          ]
        , testGroup "metadata"
          [ roundTripCompareWithOpts (def,def{readerStandalone=True})
            "metadata fields"
            "docx/metadata.native"
          , roundTripCompareWithOpts (def,def{readerStandalone=True})
            "stop recording metadata with normal text"
            "docx/metadata_after_normal.native"
          ]
        , testGroup "customized styles"
          [ testCompareWithOpts
            ( def{writerReferenceDocx=Just "docx/custom-style-reference.docx"}
            , def)
            "simple customized blocks and inlines"
            "docx/custom-style-roundtrip-start.native"
            "docx/custom-style-roundtrip-end.native"
          ]

        ]
