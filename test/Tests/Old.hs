{-# LANGUAGE NoImplicitPrelude #-}
{- |
   Module      : Tests.Old
   Copyright   : © 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

"Old" style tests (comparing output to golden files).
-}
module Tests.Old (tests) where

import Prelude
import Data.Algorithm.Diff
import Prelude hiding (readFile)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import System.Exit
import System.FilePath (joinPath, splitDirectories, (<.>), (</>))
import qualified System.Environment as Env
import Text.Pandoc.Process (pipeProcess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)
import Tests.Helpers hiding (test)
import qualified Text.Pandoc.UTF8 as UTF8

tests :: FilePath -> [TestTree]
tests pandocPath =
  [ testGroup "markdown"
    [ testGroup "writer"
      $ writerTests' "markdown" ++ lhsWriterTests' "markdown"
    , testGroup "reader"
      [ test' "basic" ["-r", "markdown", "-w", "native", "-s"]
        "testsuite.txt" "testsuite.native"
      , test' "tables" ["-r", "markdown", "-w", "native", "--columns=80"]
        "tables.txt" "tables.native"
      , test' "pipe tables" ["-r", "markdown", "-w", "native", "--columns=80"]
        "pipe-tables.txt" "pipe-tables.native"
      , test' "more" ["-r", "markdown", "-w", "native", "-s"]
        "markdown-reader-more.txt" "markdown-reader-more.native"
      , lhsReaderTest' "markdown+lhs"
      ]
    , testGroup "citations"
      [ test' "citations" ["-r", "markdown", "-w", "native"]
        "markdown-citations.txt" "markdown-citations.native"
      ]
    ]
  , testGroup "rst"
    [ testGroup "writer" (writerTests' "rst" ++ lhsWriterTests' "rst")
    , testGroup "reader"
      [ test' "basic" ["-r", "rst+smart", "-w", "native",
        "-s", "--columns=80"] "rst-reader.rst" "rst-reader.native"
      , test' "tables" ["-r", "rst", "-w", "native", "--columns=80"]
        "tables.rst" "tables-rstsubset.native"
      , lhsReaderTest' "rst+lhs"
      ]
    ]
  , testGroup "latex"
    [ testGroup "writer"
        (writerTests' "latex" ++ lhsWriterTests' "latex")
    , testGroup "reader"
      [ test' "basic" ["-r", "latex+raw_tex", "-w", "native", "-s"]
        "latex-reader.latex" "latex-reader.native"
      , lhsReaderTest' "latex+lhs"
      ]
    ]
  , testGroup "html"
    [ testGroup "writer" (writerTests' "html4" ++ writerTests' "html5" ++
        lhsWriterTests' "html")
    , test' "reader" ["-r", "html", "-w", "native", "-s"]
      "html-reader.html" "html-reader.native"
    ]
  , testGroup "s5"
    [ s5WriterTest' "basic" ["-s"] "s5"
    , s5WriterTest' "fancy" ["-s","--mathjax","-i"] "s5"
    , s5WriterTest' "fragment" [] "html4"
    , s5WriterTest' "inserts"  ["-s", "-H", "insert",
      "-B", "insert", "-A", "insert", "-c", "main.css"] "html4"
    ]
  , testGroup "textile"
    [ testGroup "writer" $ writerTests' "textile"
    , test' "reader" ["-r", "textile", "-w", "native", "-s"]
      "textile-reader.textile" "textile-reader.native"
    ]
  , testGroup "docbook"
    [ testGroup "writer" $ writerTests' "docbook4"
    , test' "reader" ["-r", "docbook", "-w", "native", "-s"]
      "docbook-reader.docbook" "docbook-reader.native"
    , test' "reader" ["-r", "docbook", "-w", "native", "-s"]
      "docbook-chapter.docbook" "docbook-chapter.native"
    , test' "reader" ["-r", "docbook", "-w", "native", "-s"]
      "docbook-xref.docbook" "docbook-xref.native"
    ]
  , testGroup "docbook5"
    [ testGroup "writer" $ writerTests' "docbook5"
    ]
  , testGroup "jats"
    [ testGroup "writer"
      [ testGroup "jats_archiving" $
        writerTests' "jats_archiving"
      , testGroup "jats_articleauthoring" $
        writerTests' "jats_articleauthoring"
      , testGroup "jats_publishing" $
        writerTests' "jats_publishing"
      ]
    , test' "reader" ["-r", "jats", "-w", "native", "-s"]
      "jats-reader.xml" "jats-reader.native"
    ]
  , testGroup "jira"
    [ testGroup "writer" $ writerTests' "jira"
    , test' "reader" ["-r", "jira", "-w", "native", "-s"]
      "jira-reader.jira" "jira-reader.native"
    ]
  , testGroup "native"
    [ testGroup "writer" $ writerTests' "native"
    , test' "reader" ["-r", "native", "-w", "native", "-s"]
      "testsuite.native" "testsuite.native"
    ]
  , testGroup "fb2"
    [ fb2WriterTest' "basic" [] "fb2/basic.markdown" "fb2/basic.fb2"
    , fb2WriterTest' "titles" [] "fb2/titles.markdown" "fb2/titles.fb2"
    , fb2WriterTest' "images" [] "fb2/images.markdown" "fb2/images.fb2"
    , fb2WriterTest' "images-embedded" [] "fb2/images-embedded.html" "fb2/images-embedded.fb2"
    , fb2WriterTest' "math" [] "fb2/math.markdown" "fb2/math.fb2"
    , fb2WriterTest' "meta" [] "fb2/meta.markdown" "fb2/meta.fb2"
    , fb2WriterTest' "tables" [] "tables.native" "tables.fb2"
    , fb2WriterTest' "testsuite" [] "testsuite.native" "writer.fb2"
    ]
  , testGroup "mediawiki"
    [ testGroup "writer" $ writerTests' "mediawiki"
    , test' "reader" ["-r", "mediawiki", "-w", "native", "-s"]
      "mediawiki-reader.wiki" "mediawiki-reader.native"
    ]
  , testGroup "vimwiki"
    [ test' "reader" ["-r", "vimwiki", "-w", "native", "-s"]
      "vimwiki-reader.wiki" "vimwiki-reader.native"
    ]
  , testGroup "dokuwiki"
    [ testGroup "writer" $ writerTests' "dokuwiki"
    , test' "inline_formatting" ["-r", "native", "-w", "dokuwiki", "-s"]
      "dokuwiki_inline_formatting.native" "dokuwiki_inline_formatting.dokuwiki"
    , test' "multiblock table" ["-r", "native", "-w", "dokuwiki", "-s"]
      "dokuwiki_multiblock_table.native" "dokuwiki_multiblock_table.dokuwiki"
    , test' "external images" ["-r", "native", "-w", "dokuwiki", "-s"]
      "dokuwiki_external_images.native" "dokuwiki_external_images.dokuwiki"
    ]
  , testGroup "opml"
    [ test' "basic" ["-r", "native", "-w", "opml", "--columns=78", "-s"]
       "testsuite.native" "writer.opml"
    , test' "reader" ["-r", "opml", "-w", "native", "-s"]
      "opml-reader.opml" "opml-reader.native"
    ]
  , testGroup "haddock"
    [ testGroup "writer" $ writerTests' "haddock"
    , test' "reader" ["-r", "haddock", "-w", "native", "-s"]
      "haddock-reader.haddock" "haddock-reader.native"
    ]
  , testGroup "txt2tags"
    [ test' "reader" ["-r", "t2t", "-w", "native", "-s"]
        "txt2tags.t2t" "txt2tags.native" ]
  , testGroup "epub" [
      test' "features" ["-r", "epub", "-w", "native"]
        "epub/features.epub" "epub/features.native"
    , test' "wasteland" ["-r", "epub", "-w", "native"]
        "epub/wasteland.epub" "epub/wasteland.native"
    , test' "formatting" ["-r", "epub", "-w", "native"]
        "epub/formatting.epub" "epub/formatting.native"
    ]
  , testGroup "twiki"
    [ test' "reader" ["-r", "twiki", "-w", "native", "-s"]
        "twiki-reader.twiki" "twiki-reader.native" ]
  , testGroup "tikiwiki"
    [ test' "reader" ["-r", "tikiwiki", "-w", "native", "-s"]
        "tikiwiki-reader.tikiwiki" "tikiwiki-reader.native" ]
  , testGroup "other writers" $ map (\f -> testGroup f $ writerTests' f)
    [ "opendocument" , "context" , "texinfo", "icml", "tei"
    , "man" , "plain" , "rtf", "org", "asciidoc", "asciidoctor"
    , "xwiki", "zimwiki"
    ]
  , testGroup "writers-lang-and-dir"
    [ test' "latex" ["-f", "native", "-t", "latex", "-s"]
      "writers-lang-and-dir.native" "writers-lang-and-dir.latex"
    , test' "context" ["-f", "native", "-t", "context", "-s"]
      "writers-lang-and-dir.native" "writers-lang-and-dir.context"
    ]
  , testGroup "muse"
    [ testGroup "writer" $ writerTests' "muse"
    ]
  , testGroup "ms"
    [ testGroup "writer" $ writerTests' "ms"
    ]
  , testGroup "creole"
    [ test' "reader" ["-r", "creole", "-w", "native", "-s"]
      "creole-reader.txt" "creole-reader.native"
    ]
  , testGroup "custom writer"
    [ test' "basic" ["-f", "native", "-t", "../data/sample.lua"]
      "testsuite.native" "writer.custom"
    , test' "tables" ["-f", "native", "-t", "../data/sample.lua"]
      "tables.native" "tables.custom"
    ]
  , testGroup "man"
    [ test' "reader" ["-r", "man", "-w", "native", "-s"]
      "man-reader.man" "man-reader.native"
    ]
  , testGroup "org"
    [ test' "reader" ["-r", "org", "-w", "native", "-s"]
      "org-select-tags.org" "org-select-tags.native"
    ]
  , testGroup "ipynb"
    [ test' "reader" ["-f", "ipynb-raw_html-raw_tex+raw_attribute",
                      "-t", "native", "-s"]
      "ipynb/simple.ipynb" "ipynb/simple.out.native"
    , test' "writer" ["-f", "native", "-t",
                      "ipynb-raw_html-raw_tex+raw_attribute", "-s"]
      "ipynb/simple.in.native" "ipynb/simple.ipynb"
    ]
  ]
 where
    test'           = test pandocPath
    writerTests'    = writerTests pandocPath
    s5WriterTest'   = s5WriterTest pandocPath
    fb2WriterTest'  = fb2WriterTest pandocPath
    lhsWriterTests' = lhsWriterTests pandocPath
    lhsReaderTest'  = lhsReaderTest pandocPath

-- makes sure file is fully closed after reading
readFile' :: FilePath -> IO String
readFile' f = do s <- UTF8.readFile f
                 return $! (length s `seq` s)

lhsWriterTests :: FilePath -> String -> [TestTree]
lhsWriterTests pandocPath format
  = [ t "lhs to normal" format
    , t "lhs to lhs"    (format ++ "+lhs")
    ]
  where
    t n f = test pandocPath
             n ["--wrap=preserve", "-r", "native", "-s", "-w", f]
             "lhs-test.native" ("lhs-test" <.> f)

lhsReaderTest :: FilePath -> String -> TestTree
lhsReaderTest pandocPath format =
  test pandocPath "lhs" ["-r", format, "-w", "native"]
    ("lhs-test" <.> format) norm
   where norm = if format == "markdown+lhs"
                   then "lhs-test-markdown.native"
                   else "lhs-test.native"

writerTests :: FilePath -> String -> [TestTree]
writerTests pandocPath format
  = [ test pandocPath
        "basic"  (opts ++ ["-s"]) "testsuite.native" ("writer" <.> format)
    , test pandocPath
       "tables" opts             "tables.native"    ("tables" <.> format)
    ]
  where
    opts = ["-r", "native", "-w", format, "--columns=78",
            "--variable", "pandoc-version="]

s5WriterTest :: FilePath -> String -> [String] -> String -> TestTree
s5WriterTest pandocPath modifier opts format
  = test pandocPath (format ++ " writer (" ++ modifier ++ ")")
    (["-r", "native", "-w", format] ++ opts)
    "s5.native"  ("s5-" ++ modifier <.> "html")

fb2WriterTest :: FilePath -> String -> [String] -> String -> String -> TestTree
fb2WriterTest pandocPath title opts inputfile normfile =
  testWithNormalize (ignoreBinary . formatXML) pandocPath
                    title (["-t", "fb2"]++opts) inputfile normfile
  where
    formatXML xml = splitTags $ zip xml (drop 1 xml)
    splitTags []               = []
    splitTags [end]            = fst end : snd end : []
    splitTags (('>','<'):rest) = ">\n" ++ splitTags rest
    splitTags ((c,_):rest)     = c : splitTags rest
    ignoreBinary = unlines . filter (not . startsWith "<binary ") . lines
    startsWith tag str = all (uncurry (==)) $ zip tag str

-- | Run a test without normalize function, return True if test passed.
test :: FilePath  -- ^ Path of pandoc executable
     -> String    -- ^ Title of test
     -> [String]  -- ^ Options to pass to pandoc
     -> String    -- ^ Input filepath
     -> FilePath  -- ^ Norm (for test results) filepath
     -> TestTree
test = testWithNormalize id

-- | Run a test with normalize function, return True if test passed.
testWithNormalize  :: (String -> String) -- ^ Normalize function for output
                   -> FilePath  -- ^ Path to pandoc executable
                   -> String    -- ^ Title of test
                   -> [String]  -- ^ Options to pass to pandoc
                   -> String    -- ^ Input filepath
                   -> FilePath  -- ^ Norm (for test results) filepath
                   -> TestTree
testWithNormalize normalizer pandocPath testname opts inp norm =
  goldenTest testname getExpected getActual
    (compareValues norm options) updateGolden
  where getExpected = normalizer <$> readFile' norm
        getActual   = do
              mldpath   <- Env.lookupEnv "LD_LIBRARY_PATH"
              mdyldpath <- Env.lookupEnv "DYLD_LIBRARY_PATH"
              let mbDynlibDir = findDynlibDir (reverse $
                                 splitDirectories pandocPath)
              let dynlibEnv = [("DYLD_LIBRARY_PATH", intercalate ":" $ catMaybes [mbDynlibDir, mdyldpath])
                              ,("LD_LIBRARY_PATH",   intercalate ":" $ catMaybes [mbDynlibDir, mldpath])]
              let env = dynlibEnv ++
                        [("TMP","."),("LANG","en_US.UTF-8"),("HOME", "./")]
              (ec, out) <- pipeProcess (Just env) pandocPath options mempty
              if ec == ExitSuccess
                 then return $ filter (/='\r') . normalizer
                             $ UTF8.toStringLazy out
                   -- filter \r so the tests will work on Windows machines
                 else fail $ "Pandoc failed with error code " ++ show ec
        updateGolden = UTF8.writeFile norm
        options = ["--data-dir=../data","--quiet"] ++ [inp] ++ opts

compareValues :: FilePath -> [String] -> String -> String -> IO (Maybe String)
compareValues norm options expected actual = do
  pandocPath <- findPandoc
  let cmd  = pandocPath ++ " " ++ unwords options
  let dash = replicate 72 '-'
  let diff = getDiff (lines actual) (lines expected)
  if expected == actual
     then return Nothing
     else return $ Just $
        '\n' : dash ++
        "\n--- " ++ norm ++
        "\n+++ " ++ cmd ++ "\n" ++
        showDiff (1,1) diff ++ dash

findDynlibDir :: [FilePath] -> Maybe FilePath
findDynlibDir []           = Nothing
findDynlibDir ("build":xs) = Just $ joinPath (reverse xs) </> "build"
findDynlibDir (_:xs)       = findDynlibDir xs
