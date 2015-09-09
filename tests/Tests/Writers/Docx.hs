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

type Options = (WriterOptions, ReaderOptions)

compareOutput :: Options
                 -> FilePath
                 -> IO (Pandoc, Pandoc)
compareOutput opts nativeFile = do
  nf <- Prelude.readFile nativeFile
  let wopts = fst opts
  df <- writeDocx wopts{writerUserDataDir = Just (".." </> "data")}
             (handleError $ readNative nf)
  let (p, _) = handleError $ readDocx (snd opts) df
  return (p, handleError $ readNative nf)

testCompareWithOptsIO :: Options -> String -> FilePath -> IO Test
testCompareWithOptsIO opts name nativeFile = do
  (dp, np) <- compareOutput opts nativeFile
  return $ test id name (dp, np)

testCompareWithOpts :: Options -> String -> FilePath -> Test
testCompareWithOpts opts name nativeFile =
  buildTest $ testCompareWithOptsIO opts name nativeFile

testCompare :: String -> FilePath -> Test
testCompare = testCompareWithOpts def

tests :: [Test]
tests = [ testGroup "inlines"
          [ testCompare
            "font formatting"
            "docx/inline_formatting_writer.native"
          , testCompare
            "font formatting with character styles"
            "docx/char_styles.native"
          , testCompare
            "hyperlinks"
            "docx/links_writer.native"
          , testCompare
            "inline image"
            "docx/image_no_embed_writer.native"
          , testCompare
            "inline image in links"
            "docx/inline_images_writer.native"
          , testCompare
            "handling unicode input"
            "docx/unicode.native"
          , testCompare
            "literal tabs"
            "docx/tabs.native"
          , testCompare
            "normalizing inlines"
            "docx/normalize.native"
          , testCompare
            "normalizing inlines deep inside blocks"
            "docx/deep_normalize.native"
          , testCompare
            "move trailing spaces outside of formatting"
            "docx/trailing_spaces_in_formatting.native"
          , testCompare
            "inline code (with VerbatimChar style)"
            "docx/inline_code.native"
          , testCompare
            "inline code in subscript and superscript"
            "docx/verbatim_subsuper.native"
          ]
        , testGroup "blocks"
          [ testCompare
            "headers"
            "docx/headers.native"
          , testCompare
            "headers already having auto identifiers"
            "docx/already_auto_ident.native"
          , testCompare
            "numbered headers automatically made into list"
            "docx/numbered_header.native"
          , testCompare
            "i18n blocks (headers and blockquotes)"
            "docx/i18n_blocks.native"
          -- Continuation does not survive round-trip
          , testCompare
            "lists"
            "docx/lists_writer.native"
          , testCompare
            "definition lists"
            "docx/definition_list.native"
          , testCompare
            "custom defined lists in styles"
            "docx/german_styled_lists.native"
          , testCompare
            "footnotes and endnotes"
            "docx/notes.native"
          , testCompare
            "blockquotes (parsing indent as blockquote)"
            "docx/block_quotes_parse_indent.native"
          , testCompare
            "hanging indents"
            "docx/hanging_indent.native"
          -- tables headers do not survive round-trip, should look into that
          , testCompare
            "tables"
            "docx/tables.native"
          , testCompare
            "tables with lists in cells"
            "docx/table_with_list_cell.native"
          , testCompare
            "code block"
            "docx/codeblock.native"
          , testCompare
            "dropcap paragraphs"
            "docx/drop_cap.native"
          ]
        , testGroup "metadata"
          [ testCompareWithOpts (def,def{readerStandalone=True})
            "metadata fields"
            "docx/metadata.native"
          , testCompareWithOpts (def,def{readerStandalone=True})
            "stop recording metadata with normal text"
            "docx/metadata_after_normal.native"
          ]

        ]
