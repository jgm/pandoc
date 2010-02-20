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
import System.IO.UTF8
import System.IO ( openTempFile, stderr, stdout, hFlush )
import Prelude hiding ( putStrLn, putStr, readFile )
import System.Process ( runProcess, waitForProcess )
import System.FilePath ( (</>), (<.>) )
import System.Directory
import System.Environment
import System.Exit
import Text.Printf
import Data.Algorithm.Diff

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

writerFormats :: [String]
writerFormats = [ "native"
                , "html"
                , "docbook"
                , "opendocument"
                , "latex"
                , "context"
                , "texinfo"
                , "man"
                , "markdown"
                , "rst"
                , "mediawiki"
                , "rtf"
                ]

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

main :: IO ()
main = do
  args <- getArgs
  let runLhsTests = "lhs" `elem` args
  r1s <- mapM runWriterTest writerFormats
  r2 <- runS5WriterTest "basic" ["-s"] "s5"
  r3 <- runS5WriterTest "fancy" ["-s","-m","-i"] "s5"
  r4 <- runS5WriterTest "fragment" [] "html"
  r5 <- runS5WriterTest "inserts"  ["-s", "-H", "insert",
            "-B", "insert", "-A", "insert", "-c", "main.css"] "html"
  r6 <- runTest "markdown reader" ["-r", "markdown", "-w", "native", "-s", "-S"]
             "testsuite.txt" "testsuite.native"
  r7 <- runTest "markdown reader (tables)" ["-r", "markdown", "-w", "native"]
             "tables.txt" "tables.native"
  r7a <- runTest "markdown reader (more)" ["-r", "markdown", "-w", "native"]
             "markdown-reader-more.txt" "markdown-reader-more.native"
  r8 <- runTest "rst reader" ["-r", "rst", "-w", "native", "-s", "-S"]
             "rst-reader.rst" "rst-reader.native"
  r8a <- runTest "rst reader (tables)" ["-r", "rst", "-w", "native"]
             "tables.rst" "tables-rstsubset.native"
  r9 <- runTest "html reader" ["-r", "html", "-w", "native", "-s"]
             "html-reader.html" "html-reader.native"
  r10 <- runTest "latex reader" ["-r", "latex", "-w", "native", "-s", "-R"]
             "latex-reader.latex" "latex-reader.native"
  r11 <- runTest "native reader" ["-r", "native", "-w", "native", "-s"]
             "testsuite.native" "testsuite.native"
  r12s <- if runLhsTests
             then mapM runLhsWriterTest lhsWriterFormats
             else putStrLn "Skipping lhs writer tests because they presuppose highlighting support" >> return []
  r13s <- if runLhsTests
             then mapM runLhsReaderTest lhsReaderFormats
             else putStrLn "Skipping lhs reader tests because they presuppose highlighting support" >> return []
  let results = r1s ++ [r2, r3, r4, r5, r6, r7, r7a, r8, r8a, r9, r10, r11] ++ r12s ++ r13s
  if all id results
     then do
       putStrLn "\nAll tests passed."
       exitWith ExitSuccess
     else do
       let failures = length $ filter not results
       putStrLn $ "\n" ++ show failures ++ " tests failed."
       exitWith (ExitFailure failures)

-- makes sure file is fully closed after reading
readFile' :: FilePath -> IO String
readFile' f = do s <- readFile f
                 return $! (length s `seq` s)

runLhsWriterTest :: String -> IO Bool
runLhsWriterTest format =
  runTest ("(lhs) " ++ format ++ " writer") ["-r", "native", "-s", "-w", format] "lhs-test.native" ("lhs-test" <.> format)

runLhsReaderTest :: String -> IO Bool
runLhsReaderTest format =
  runTest ("(lhs) " ++ format ++ " reader") ["-r", format, "-w", "html+lhs"] ("lhs-test" <.> format) "lhs-test.fragment.html+lhs"

runWriterTest :: String -> IO Bool
runWriterTest format = do
  r1 <- runTest (format ++ " writer") ["-r", "native", "-s", "-w", format] "testsuite.native" ("writer" <.> format)
  r2 <- runTest (format ++ " writer (tables)") ["-r", "native", "-w", format] "tables.native" ("tables" <.> format)
  return (r1 && r2)

runS5WriterTest :: String -> [String] -> String -> IO Bool
runS5WriterTest modifier opts format = runTest (format ++ " writer (" ++ modifier ++ ")")
                     (["-r", "native", "-w", format] ++ opts) "s5.native"  ("s5." ++ modifier <.> "html")

-- | Run a test, return True if test passed.
runTest  :: String                      -- ^ Title of test
         -> [String]                    -- ^ Options to pass to pandoc
         -> String                      -- ^ Input filepath
         -> FilePath                    -- ^ Norm (for test results) filepath
         -> IO Bool
runTest testname opts inp norm = do
  putStr $ printf "%-28s ---> " testname
  (outputPath, hOut) <- openTempFile "" "pandoc-test"
  let inpPath = inp
  let normPath = norm
  hFlush stdout
  env <- getEnvironment  -- we need at least HOME so pandoc can find data files
  -- Note: COLUMNS must be set for markdown table reader
  ph <- runProcess pandocPath (opts ++ [inpPath] ++ ["--data-dir", ".."]) Nothing (Just (("COLUMNS", "80"):env)) Nothing (Just hOut) (Just stderr)
  ec <- waitForProcess ph
  result  <- if ec == ExitSuccess
                then do
                  -- filter \r so the tests will work on Windows machines
                  outputContents <- readFile' outputPath >>= return . filter (/='\r')
                  normContents <- readFile' normPath >>= return . filter (/='\r')
                  if outputContents == normContents
                     then return TestPassed
                     else return $ TestFailed $ getDiff (lines outputContents) (lines normContents)
                else return $ TestError ec
  removeFile outputPath
  putStrLn (show result)
  return (result == TestPassed)
