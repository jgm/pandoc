module Tests.Old (tests) where

import Tests.Helpers hiding (test)
import Test.Framework (testGroup, Test )
import Test.Framework.Providers.HUnit
import Test.HUnit ( assertBool )
import System.IO ( openTempFile, stderr )
import System.Process ( runProcess, waitForProcess )
import System.FilePath ( (</>), (<.>), splitDirectories, joinPath )
import System.Directory
import System.Exit
import Data.Algorithm.Diff
import Prelude hiding ( readFile )
import qualified Text.Pandoc.UTF8 as UTF8

tests :: [Test]
tests = [ testGroup "markdown"
          [ testGroup "writer"
            $ writerTests "markdown" ++ lhsWriterTests "markdown"
          , testGroup "reader"
            [ test "basic" ["-r", "markdown", "-w", "native", "-s"]
              "testsuite.txt" "testsuite.native"
            , test "tables" ["-r", "markdown", "-w", "native", "--columns=80"]
              "tables.txt" "tables.native"
            , test "pipe tables" ["-r", "markdown", "-w", "native", "--columns=80"]
              "pipe-tables.txt" "pipe-tables.native"
            , test "more" ["-r", "markdown", "-w", "native", "-s"]
              "markdown-reader-more.txt" "markdown-reader-more.native"
            , lhsReaderTest "markdown+lhs"
            ]
          , testGroup "citations"
            [ test "citations" ["-r", "markdown", "-w", "native"]
              "markdown-citations.txt" "markdown-citations.native"
            ]
          ]
        , testGroup "rst"
          [ testGroup "writer" (writerTests "rst" ++ lhsWriterTests "rst")
          , testGroup "reader"
            [ test "basic" ["-r", "rst+smart", "-w", "native",
              "-s", "--columns=80"] "rst-reader.rst" "rst-reader.native"
            , test "tables" ["-r", "rst", "-w", "native", "--columns=80"]
              "tables.rst" "tables-rstsubset.native"
            , lhsReaderTest "rst+lhs"
            ]
          ]
        , testGroup "latex"
          [ testGroup "writer" (writerTests "latex" ++ lhsWriterTests "latex")
          , testGroup "reader"
            [ test "basic" ["-r", "latex+raw_tex", "-w", "native", "-s"]
              "latex-reader.latex" "latex-reader.native"
            , lhsReaderTest "latex+lhs"
            ]
          ]
        , testGroup "html"
          [ testGroup "writer" (writerTests "html4" ++ writerTests "html5" ++
                                lhsWriterTests "html")
          , test "reader" ["-r", "html", "-w", "native", "-s"]
            "html-reader.html" "html-reader.native"
          ]
        , testGroup "s5"
          [ s5WriterTest "basic" ["-s"] "s5"
          , s5WriterTest "fancy" ["-s","-m","-i"] "s5"
          , s5WriterTest "fragment" [] "html4"
          , s5WriterTest "inserts"  ["-s", "-H", "insert",
            "-B", "insert", "-A", "insert", "-c", "main.css"] "html4"
          ]
        , testGroup "textile"
          [ testGroup "writer" $ writerTests "textile"
          , test "reader" ["-r", "textile", "-w", "native", "-s"]
            "textile-reader.textile" "textile-reader.native"
          ]
        , testGroup "docbook"
          [ testGroup "writer" $ writerTests "docbook4"
          , test "reader" ["-r", "docbook", "-w", "native", "-s"]
            "docbook-reader.docbook" "docbook-reader.native"
          , test "reader" ["-r", "docbook", "-w", "native", "-s"]
            "docbook-xref.docbook" "docbook-xref.native"
          ]
        , testGroup "docbook5"
          [ testGroup "writer" $ writerTests "docbook5"
          ]
        , testGroup "native"
          [ testGroup "writer" $ writerTests "native"
          , test "reader" ["-r", "native", "-w", "native", "-s"]
            "testsuite.native" "testsuite.native"
          ]
        , testGroup "fb2"
          [ fb2WriterTest "basic" [] "fb2/basic.markdown" "fb2/basic.fb2"
          , fb2WriterTest "titles" [] "fb2/titles.markdown" "fb2/titles.fb2"
          , fb2WriterTest "images" [] "fb2/images.markdown" "fb2/images.fb2"
          , fb2WriterTest "images-embedded" [] "fb2/images-embedded.html" "fb2/images-embedded.fb2"
          , fb2WriterTest "math" [] "fb2/math.markdown" "fb2/math.fb2"
          , fb2WriterTest "tables" [] "tables.native" "tables.fb2"
          , fb2WriterTest "testsuite" [] "testsuite.native" "writer.fb2"
          ]
        , testGroup "mediawiki"
          [ testGroup "writer" $ writerTests "mediawiki"
          , test "reader" ["-r", "mediawiki", "-w", "native", "-s"]
            "mediawiki-reader.wiki" "mediawiki-reader.native"
          ]
        , testGroup "dokuwiki"
          [ testGroup "writer" $ writerTests "dokuwiki"
          , test "inline_formatting" ["-r", "native", "-w", "dokuwiki", "-s"]
            "dokuwiki_inline_formatting.native" "dokuwiki_inline_formatting.dokuwiki"
          , test "multiblock table" ["-r", "native", "-w", "dokuwiki", "-s"]
            "dokuwiki_multiblock_table.native" "dokuwiki_multiblock_table.dokuwiki"
          , test "external images" ["-r", "native", "-w", "dokuwiki", "-s"]
            "dokuwiki_external_images.native" "dokuwiki_external_images.dokuwiki"
          ]
        , testGroup "opml"
          [ test "basic" ["-r", "native", "-w", "opml", "--columns=78", "-s"]
             "testsuite.native" "writer.opml"
          , test "reader" ["-r", "opml", "-w", "native", "-s"]
            "opml-reader.opml" "opml-reader.native"
          ]
        , testGroup "haddock"
          [ testGroup "writer" $ writerTests "haddock"
          , test "reader" ["-r", "haddock", "-w", "native", "-s"]
            "haddock-reader.haddock" "haddock-reader.native"
          ]
        , testGroup "txt2tags"
          [ test "reader" ["-r", "t2t", "-w", "native", "-s"]
              "txt2tags.t2t" "txt2tags.native" ]
        , testGroup "epub" [
            test "features" ["-r", "epub", "-w", "native"]
              "epub/features.epub" "epub/features.native"
          , test "wasteland" ["-r", "epub", "-w", "native"]
              "epub/wasteland.epub" "epub/wasteland.native"
          , test "formatting" ["-r", "epub", "-w", "native"]
              "epub/formatting.epub" "epub/formatting.native"
          ]
        , testGroup "twiki"
          [ test "reader" ["-r", "twiki", "-w", "native", "-s"]
              "twiki-reader.twiki" "twiki-reader.native" ]
        , testGroup "other writers" $ map (\f -> testGroup f $ writerTests f)
          [ "opendocument" , "context" , "texinfo", "icml", "tei"
          , "man" , "plain" , "rtf", "org", "asciidoc", "zimwiki"
          ]
        , testGroup "writers-lang-and-dir"
          [ test "latex" ["-f", "native", "-t", "latex", "-s"]
            "writers-lang-and-dir.native" "writers-lang-and-dir.latex"
          , test "context" ["-f", "native", "-t", "context", "-s"]
            "writers-lang-and-dir.native" "writers-lang-and-dir.context"
          ]
        ]

-- makes sure file is fully closed after reading
readFile' :: FilePath -> IO String
readFile' f = do s <- UTF8.readFile f
                 return $! (length s `seq` s)

lhsWriterTests :: String -> [Test]
lhsWriterTests format
  = [ t "lhs to normal" format
    , t "lhs to lhs"    (format ++ "+lhs")
    ]
  where
    t n f = test n ["--wrap=preserve", "-r", "native", "-s", "-w", f]
             "lhs-test.native" ("lhs-test" <.> f)

lhsReaderTest :: String -> Test
lhsReaderTest format =
  test "lhs" ["-r", format, "-w", "native"]
    ("lhs-test" <.> format) norm
   where norm = if format == "markdown+lhs"
                   then "lhs-test-markdown.native"
                   else "lhs-test.native"

writerTests :: String -> [Test]
writerTests format
  = [ test "basic"  (opts ++ ["-s"]) "testsuite.native" ("writer" <.> format)
    , test "tables" opts             "tables.native"    ("tables" <.> format)
    ]
  where
    opts = ["-r", "native", "-w", format, "--columns=78",
            "--variable", "pandoc-version="]

s5WriterTest :: String -> [String] -> String -> Test
s5WriterTest modifier opts format
  = test (format ++ " writer (" ++ modifier ++ ")")
    (["-r", "native", "-w", format] ++ opts)
    "s5.native"  ("s5-" ++ modifier <.> "html")

fb2WriterTest :: String -> [String] -> String -> String -> Test
fb2WriterTest title opts inputfile normfile =
  testWithNormalize (ignoreBinary . formatXML)
                    title (["-t", "fb2"]++opts) inputfile normfile
  where
    formatXML xml = splitTags $ zip xml (drop 1 xml)
    splitTags [] = []
    splitTags [end] = fst end : snd end : []
    splitTags (('>','<'):rest) = ">\n" ++ splitTags rest
    splitTags ((c,_):rest) = c : splitTags rest
    ignoreBinary = unlines . filter (not . startsWith "<binary ") . lines
    startsWith tag str = all (uncurry (==)) $ zip tag str

-- | Run a test without normalize function, return True if test passed.
test :: String    -- ^ Title of test
     -> [String]  -- ^ Options to pass to pandoc
     -> String    -- ^ Input filepath
     -> FilePath  -- ^ Norm (for test results) filepath
     -> Test
test = testWithNormalize id

-- | Run a test with normalize function, return True if test passed.
testWithNormalize  :: (String -> String) -- ^ Normalize function for output
                   -> String    -- ^ Title of test
                   -> [String]  -- ^ Options to pass to pandoc
                   -> String    -- ^ Input filepath
                   -> FilePath  -- ^ Norm (for test results) filepath
                   -> Test
testWithNormalize normalizer testname opts inp norm = testCase testname $ do
  -- find pandoc executable relative to test-pandoc
  -- First, try in same directory (e.g. if both in ~/.cabal/bin)
  -- Second, try ../pandoc (e.g. if in dist/XXX/build/test-pandoc)
  pandocPath <- findPandoc
  (outputPath, hOut) <- openTempFile "" "pandoc-test"
  let inpPath = inp
  let normPath = norm
  let options = ["--quiet", "--data-dir", ".." </> "data"] ++ [inpPath] ++ opts
  let cmd = pandocPath ++ " " ++ unwords options
  let findDynlibDir [] = Nothing
      findDynlibDir ("build":xs) = Just $ joinPath (reverse xs) </> "build"
      findDynlibDir (_:xs) = findDynlibDir xs
  let mbDynlibDir = findDynlibDir (reverse $ splitDirectories pandocPath)
  let dynlibEnv = case mbDynlibDir of
                       Nothing  -> []
                       Just d   -> [("DYLD_LIBRARY_PATH", d),
                                    ("LD_LIBRARY_PATH", d)]
  let env = dynlibEnv ++ [("TMP","."),("LANG","en_US.UTF-8"),("HOME", "./")]
  ph <- runProcess pandocPath options Nothing
        (Just env) Nothing (Just hOut) (Just stderr)
  ec <- waitForProcess ph
  result  <- if ec == ExitSuccess
                then do
                  -- filter \r so the tests will work on Windows machines
                  outputContents <- readFile' outputPath >>=
                    return . filter (/='\r') . normalizer
                  normContents <- readFile' normPath >>=
                    return . filter (/='\r') . normalizer
                  if outputContents == normContents
                     then return TestPassed
                     else return
                          $ TestFailed cmd normPath
                          $ getDiff (lines outputContents) (lines normContents)
                else return $ TestError ec
  removeFile outputPath
  assertBool (show result) (result == TestPassed)
