module Tests.Writers.Docx (tests) where

import System.FilePath ((</>))
import Test.Tasty
import Tests.Helpers
import Text.Pandoc.Class (runIOorExplode)
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Walk
import Text.Pandoc.Readers.Docx
import Text.Pandoc.Readers.Native
import Text.Pandoc.Writers.Docx
import System.IO.Unsafe (unsafePerformIO) -- TODO temporary
import qualified Data.ByteString as BS
import qualified Text.Pandoc.UTF8 as UTF8

type Options = (WriterOptions, ReaderOptions)

compareOutput :: Options
              -> FilePath
              -> FilePath
              -> IO (Pandoc, Pandoc)
compareOutput (wopts, ropts) nativeFileIn nativeFileOut = do
  nf <- UTF8.toText <$> BS.readFile nativeFileIn
  nf' <- UTF8.toText <$> BS.readFile nativeFileOut
  runIOorExplode $ do
    roundtripped <- readNative def nf >>=
            writeDocx wopts{writerUserDataDir = Just (".." </> "data")} >>=
            readDocx ropts
    orig <- readNative def nf'
    return (walk fixImages roundtripped, walk fixImages orig)

-- make all image filenames "image", since otherwise round-trip
-- tests fail because of different behavior of Data.Unique in
-- different ghc versions...
fixImages :: Inline -> Inline
fixImages (Image attr alt (_,tit)) = Image attr alt ("image",tit)
fixImages x = x

testCompareWithOptsIO :: Options -> String -> FilePath -> FilePath -> IO TestTree
testCompareWithOptsIO opts name nativeFileIn nativeFileOut = do
  (dp, np) <- compareOutput opts nativeFileIn nativeFileOut
  return $ test id name (dp, np)

testCompareWithOpts :: Options -> String -> FilePath -> FilePath -> TestTree
testCompareWithOpts opts name nativeFileIn nativeFileOut =
  unsafePerformIO $ testCompareWithOptsIO opts name nativeFileIn nativeFileOut

roundTripCompareWithOpts :: Options -> String -> FilePath -> TestTree
roundTripCompareWithOpts opts name nativeFile =
  testCompareWithOpts opts name nativeFile nativeFile

-- testCompare :: String -> FilePath -> FilePath -> TestTree
-- testCompare = testCompareWithOpts def

roundTripCompare :: String -> FilePath -> TestTree
roundTripCompare = roundTripCompareWithOpts def

tests :: [TestTree]
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
            ( def{writerReferenceDoc=Just "docx/custom-style-reference.docx"}
            , def)
            "simple customized blocks and inlines"
            "docx/custom-style-roundtrip-start.native"
            "docx/custom-style-roundtrip-end.native"
          ]

        ]
