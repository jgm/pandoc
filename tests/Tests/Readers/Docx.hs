module Tests.Readers.Docx (tests) where

import Text.Pandoc.Options
import Text.Pandoc.Readers.Native
import Text.Pandoc.Definition
import Tests.Helpers
import Test.Framework
import qualified Data.ByteString.Lazy as B
import Text.Pandoc.Readers.Docx
import Text.Pandoc.Writers.Native (writeNative)
import qualified Data.Map as M

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
  return $ (noNorm (readDocx opts df), noNorm (readNative nf))

testCompareWithOptsIO :: ReaderOptions -> String -> FilePath -> FilePath -> IO Test
testCompareWithOptsIO opts name docxFile nativeFile = do
  (dp, np) <- compareOutput opts docxFile nativeFile
  return $ test id name (dp, np)

testCompareWithOpts :: ReaderOptions -> String -> FilePath -> FilePath -> Test
testCompareWithOpts opts name docxFile nativeFile =
  buildTest $ testCompareWithOptsIO opts name docxFile nativeFile

testCompare :: String -> FilePath -> FilePath -> Test
testCompare = testCompareWithOpts def


tests :: [Test]
tests = [ testGroup "inlines"
          [ testCompare
            "font formatting"
            "docx.inline_formatting.docx"
            "docx.inline_formatting.native"
          , testCompare
            "hyperlinks"
            "docx.links.docx"
            "docx.links.native"
          , testCompare
            "inline image with reference output"
            "docx.image.docx"
            "docx.image_no_embed.native"
          , testCompare
            "handling unicode input"
            "docx.unicode.docx"
            "docx.unicode.native"
          , testCompare
            "literal tabs"
            "docx.tabs.docx"
            "docx.tabs.native"
          , testCompare
            "normalizing inlines"
            "docx.normalize.docx"
            "docx.normalize.native"
          , testCompare
            "normalizing inlines deep inside blocks"
            "docx.deep_normalize.docx"
            "docx.deep_normalize.native"
          , testCompare
            "move trailing spaces outside of formatting"
            "docx.trailing_spaces_in_formatting.docx"
            "docx.trailing_spaces_in_formatting.native"
          , testCompare
            "inline code (with VerbatimChar style)"
            "docx.inline_code.docx"
            "docx.inline_code.native"
          ]
        , testGroup "blocks"
          [ testCompare
            "headers"
            "docx.headers.docx"
            "docx.headers.native"
          , testCompare
            "lists"
            "docx.lists.docx"
            "docx.lists.native"
          , testCompare
            "footnotes and endnotes"
            "docx.notes.docx"
            "docx.notes.native"
          , testCompare
            "blockquotes (parsing indent as blockquote)"
            "docx.block_quotes.docx"
            "docx.block_quotes_parse_indent.native"
          , testCompare
            "tables"
            "docx.tables.docx"
            "docx.tables.native"
          , testCompare
            "code block"
            "docx.codeblock.docx"
            "docx.codeblock.native"

          ]
        ]

