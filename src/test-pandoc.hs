{-# OPTIONS_GHC -Wall #-}
-- RunTests.hs - run test suite for pandoc
-- This script is designed to be run from the tests directory.
-- It assumes the pandoc executable is in dist/build/pandoc.
--
-- runhaskell -i.. RunTests.hs [lhs]
--
-- If the lhs argument is provided, tests for lhs support will be
-- run.  These presuppose that pandoc has been compiled with the
-- -fhighlighting flag, so these tests are not run by default.
--
-- This program assumes that the Diff package has been installed:
-- cabal install Diff

module Main where

import Test.Framework (defaultMain, testGroup, Test )
import Test.Framework.Providers.HUnit

import Test.HUnit hiding ( Test )

import System.IO ( openTempFile, stderr )
import System.Process ( runProcess, waitForProcess )
import System.FilePath ( (</>), (<.>) )
import System.Directory
import System.Exit
import Data.Algorithm.Diff
import Text.Pandoc.Shared ( substitute )
import Prelude hiding ( readFile )
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.UTF8 (toString)

readFileUTF8 :: FilePath -> IO String
readFileUTF8 f = B.readFile f >>= return . toString

pandocPath :: FilePath
pandocPath = ".." </> "dist" </> "build" </> "pandoc" </> "pandoc"

data TestResult = TestPassed
                | TestError ExitCode
                | TestFailed [(DI, String)]
     deriving (Eq)

instance Show TestResult where
  show TestPassed     = "PASSED"
  show (TestError ec) = "ERROR " ++ show ec
  show (TestFailed d) = "FAILED\n" ++ showDiff d

showDiff :: [(DI, String)] -> String
showDiff []             = ""
showDiff ((F, ln) : ds) = "|TEST| " ++ ln ++ "\n" ++ showDiff ds
showDiff ((S, ln) : ds) = "|NORM| " ++ ln ++ "\n" ++ showDiff ds
showDiff ((B, _ ) : ds) = showDiff ds

lhsWriterFormats :: [String]
lhsWriterFormats = [ "markdown"
                   , "markdown+lhs"
                   , "rst"
                   , "rst+lhs"
                   , "latex"
                   , "latex+lhs"
                   , "html"
                   , "html+lhs"
                   ]

lhsReaderFormats :: [String]
lhsReaderFormats = [ "markdown+lhs"
                   , "rst+lhs"
                   , "latex+lhs"
                   ]

markdownCitationTest :: Test
markdownCitationTest
  = testGroup "citations" $ map styleToTest ["chicago-author-date","ieee","mhra"]
                            ++ [runTest "no-citeproc" wopts "markdown-citations.txt" "markdown-citations.txt"]
  where
    ropts             = ["-r", "markdown", "-w", "markdown", "--bibliography", "biblio.bib", "--no-wrap"]
    wopts             = ropts ++ ["--no-citeproc"]
    styleToTest style = runTest style (ropts ++ ["--csl", style ++ ".csl"])
                        "markdown-citations.txt" ("markdown-citations." ++ style ++ ".txt")


tests :: [Test]
tests = [ testGroup "markdown" [ runWriterTest "" "markdown"
                               , testGroup "reader" [ runTest "basic" ["-r", "markdown", "-w", "native", "-s", "-S"]
                                                      "testsuite.txt" "testsuite.native"
                                                    , runTest "tables" ["-r", "markdown", "-w", "native"]
                                                      "tables.txt" "tables.native"
                                                    , runTest "more" ["-r", "markdown", "-w", "native", "-S"]
                                                      "markdown-reader-more.txt" "markdown-reader-more.native"
                                                    ]
                               , markdownCitationTest
                               ]
        , testGroup "rst"      [ runWriterTest "" "rst"
                               , testGroup "reader" [ runTest "basic" ["-r", "rst", "-w", "native", "-s", "-S"]
                                                      "rst-reader.rst" "rst-reader.native"
                                                    , runTest "tables" ["-r", "rst", "-w", "native"]
                                                      "tables.rst" "tables-rstsubset.native"
                                                    ]
                               ]
        , testGroup "latex"    [ runWriterTest "" "latex"
                               , runTest "reader" ["-r", "latex", "-w", "native", "-s", "-R"]
                                 "latex-reader.latex" "latex-reader.native"
                               , runLatexCitationTests "biblatex"
                               , runLatexCitationTests "natbib"
                               ]
        , testGroup "html"     [ runWriterTest "" "html"
                               , runTest "reader" ["-r", "html", "-w", "native", "-s"]
                                 "html-reader.html" "html-reader.native"
                               ]
        , testGroup "s5"       [ runS5WriterTest "basic" ["-s"] "s5"
                               , runS5WriterTest "fancy" ["-s","-m","-i"] "s5"
                               , runS5WriterTest "fragment" [] "html"
                               , runS5WriterTest "inserts"  ["-s", "-H", "insert",
                                                             "-B", "insert", "-A", "insert", "-c", "main.css"] "html"
                               ]
        , testGroup "textile"  [ runWriterTest "" "textile"
                               , runTest "reader" ["-r", "textile", "-w", "native", "-s"]
                                 "textile-reader.textile" "textile-reader.native"
                               ]
        , testGroup "native"   [ runWriterTest "" "native"
                               , runTest "reader" ["-r", "native", "-w", "native", "-s"]
                                 "testsuite.native" "testsuite.native"
                               ]
        , testGroup "other writers" $ map (\f -> runWriterTest f f) [ "docbook", "opendocument" , "context" , "texinfo"
                                                                    , "man" , "plain" , "mediawiki", "rtf", "org"
                                                                    ]
        , testGroup "lhs" [ testGroup "writer" $ map runLhsWriterTest lhsWriterFormats
                          , testGroup "reader" $ map runLhsReaderTest lhsReaderFormats
                          ]
        ]

main :: IO ()
main = defaultMain tests

-- makes sure file is fully closed after reading
readFile' :: FilePath -> IO String
readFile' f = do s <- readFileUTF8 f
                 return $! (length s `seq` s)

runLhsWriterTest :: String -> Test
runLhsWriterTest format =
  runTest format ["--columns=78", "-r", "native", "-s", "-w", format] "lhs-test.native" ("lhs-test" <.> format)

runLhsReaderTest :: String -> Test
runLhsReaderTest format =
  runTest format ["-r", format, "-w", "html+lhs"] ("lhs-test" <.> format) "lhs-test.fragment.html+lhs"


runLatexCitationTests :: String -> Test
runLatexCitationTests n
  = testGroup (n ++ " citations")
    [ rt ("latex reader (" ++ n ++ " citations)") (["-r", "latex", "-w", "markdown", "-s", "--no-wrap"] ++ o)
      f "markdown-citations.txt"
    , rt ("latex writer (" ++ n ++ " citations)") (["-r", "markdown", "-w", "latex", "-s", "--no-wrap"] ++ o)
      "markdown-citations.txt" f
    ]
  where
    o = ["--bibliography", "biblio.bib", "--csl", "chicago-author-date.csl", "--no-citeproc", "--" ++ n]
    f  = n ++ "-citations.latex"
    normalize = substitute "\160" " " . substitute "\8211" "-"
    rt        = runTestWithNormalize normalize

runWriterTest :: String -> String -> Test
runWriterTest prefix format 
  = testGroup name [ runTest "basic"  (opts ++ ["-s"]) "testsuite.native" ("writer" <.> format)
                   , runTest "tables" opts             "tables.native"    ("tables" <.> format)
                   ]
  where
    name = if (null prefix) then "writer" else prefix ++ " writer"
    opts = ["-r", "native", "-w", format, "--columns=78"]

runS5WriterTest :: String -> [String] -> String -> Test
runS5WriterTest modifier opts format = runTest (format ++ " writer (" ++ modifier ++ ")")
                     (["-r", "native", "-w", format] ++ opts) "s5.native"  ("s5." ++ modifier <.> "html")


-- | Run a test without normalize function, return True if test passed.
runTest :: String    -- ^ Title of test
        -> [String]  -- ^ Options to pass to pandoc
        -> String    -- ^ Input filepath
        -> FilePath  -- ^ Norm (for test results) filepath
        -> Test
runTest = runTestWithNormalize id

-- | Run a test with normalize function, return True if test passed.
runTestWithNormalize  :: (String -> String) -- ^ Normalize function for output
                      -> String    -- ^ Title of test
                      -> [String]  -- ^ Options to pass to pandoc
                      -> String    -- ^ Input filepath
                      -> FilePath  -- ^ Norm (for test results) filepath
                      -> Test
runTestWithNormalize normalize testname opts inp norm = testCase testname $ do
  (outputPath, hOut) <- openTempFile "" "pandoc-test"
  let inpPath = inp
  let normPath = norm
  ph <- runProcess pandocPath (["--columns=80"] ++ [inpPath] ++ ["--data-dir", ".."] ++ opts) Nothing
        (Just [("LANG","en_US.UTF-8"),("HOME", "./")]) Nothing (Just hOut) (Just stderr)
  ec <- waitForProcess ph
  result  <- if ec == ExitSuccess
                then do
                  -- filter \r so the tests will work on Windows machines
                  outputContents <- readFile' outputPath >>= return . filter (/='\r') . normalize
                  normContents <- readFile' normPath >>= return . filter (/='\r')
                  if outputContents == normContents
                     then return TestPassed
                     else return $ TestFailed $ getDiff (lines outputContents) (lines normContents)
                else return $ TestError ec
  removeFile outputPath
  assertBool (show result) (result == TestPassed)
