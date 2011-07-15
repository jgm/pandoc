module Tests.Old (tests) where

import Test.Framework (testGroup, Test )
import Test.Framework.Providers.HUnit
import Test.HUnit ( assertBool )

import System.IO ( openTempFile, stderr )
import System.Process ( runProcess, waitForProcess )
import System.FilePath ( (</>), (<.>) )
import System.Directory
import System.Exit
import Data.Algorithm.Diff
import Text.Pandoc.Shared ( normalize, defaultWriterOptions )
import Text.Pandoc.Writers.Native ( writeNative )
import Text.Pandoc.Readers.Native ( readNative )
import Text.Pandoc.Highlighting ( languages )
import Prelude hiding ( readFile )
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.UTF8 (toString)
import Text.Printf

readFileUTF8 :: FilePath -> IO String
readFileUTF8 f = B.readFile f >>= return . toString

pandocPath :: FilePath
pandocPath = ".." </> "dist" </> "build" </> "pandoc" </> "pandoc"

data TestResult = TestPassed
                | TestError ExitCode
                | TestFailed String FilePath [(DI, String)]
     deriving (Eq)

instance Show TestResult where
  show TestPassed     = "PASSED"
  show (TestError ec) = "ERROR " ++ show ec
  show (TestFailed cmd file d) = '\n' : dash ++
                                 "\n--- " ++ file ++
                                 "\n+++ " ++ cmd ++ "\n" ++ showDiff (1,1) d ++
                                 dash
    where dash = replicate 72 '-'

showDiff :: (Int,Int) -> [(DI, String)] -> String
showDiff _ []             = ""
showDiff (l,r) ((F, ln) : ds) =
  printf "+%4d " l ++ ln ++ "\n" ++ showDiff (l+1,r) ds
showDiff (l,r) ((S, ln) : ds) =
  printf "-%4d " r ++ ln ++ "\n" ++ showDiff (l,r+1) ds
showDiff (l,r) ((B, _ ) : ds) =
  showDiff (l+1,r+1) ds

tests :: [Test]
tests = [ testGroup "markdown"
          [ testGroup "writer"
            $ writerTests "markdown" ++ lhsWriterTests "markdown"
          , testGroup "reader"
            [ test "basic" ["-r", "markdown", "-w", "native", "-s", "-S"]
              "testsuite.txt" "testsuite.native"
            , test "tables" ["-r", "markdown", "-w", "native", "--columns=80"]
              "tables.txt" "tables.native"
            , test "more" ["-r", "markdown", "-w", "native", "-S"]
              "markdown-reader-more.txt" "markdown-reader-more.native"
            , lhsReaderTest "markdown+lhs"
            ]
          , testGroup "citations" markdownCitationTests
          ]
        , testGroup "rst"
          [ testGroup "writer" (writerTests "rst" ++ lhsWriterTests "rst")
          , testGroup "reader"
            [ test "basic" ["-r", "rst", "-w", "native",
              "-s", "-S", "--columns=80"] "rst-reader.rst" "rst-reader.native"
            , test "tables" ["-r", "rst", "-w", "native", "--columns=80"]
              "tables.rst" "tables-rstsubset.native"
            , lhsReaderTest "rst+lhs"
            ]
          ]
        , testGroup "latex"
          [ testGroup "writer" (writerTests "latex" ++ lhsWriterTests "latex")
          , testGroup "reader"
            [ test "basic" ["-r", "latex", "-w", "native", "-s", "-R"]
              "latex-reader.latex" "latex-reader.native"
            , lhsReaderTest "latex+lhs"
            ]
          ]
        , testGroup "html"
          [ testGroup "writer" (writerTests "html" ++ lhsWriterTests "html")
          , test "reader" ["-r", "html", "-w", "native", "-s"]
            "html-reader.html" "html-reader.native"
          ]
        , testGroup "s5"
          [ s5WriterTest "basic" ["-s"] "s5"
          , s5WriterTest "fancy" ["-s","-m","-i"] "s5"
          , s5WriterTest "fragment" [] "html"
          , s5WriterTest "inserts"  ["-s", "-H", "insert",
            "-B", "insert", "-A", "insert", "-c", "main.css"] "html"
          ]
        , testGroup "textile"
          [ testGroup "writer" $ writerTests "textile"
          , test "reader" ["-r", "textile", "-w", "native", "-s"]
            "textile-reader.textile" "textile-reader.native"
          ]
        , testGroup "native"
          [ testGroup "writer" $ writerTests "native"
          , test "reader" ["-r", "native", "-w", "native", "-s"]
            "testsuite.native" "testsuite.native"
          ]
        , testGroup "other writers" $ map (\f -> testGroup f $ writerTests f)
          [ "docbook", "opendocument" , "context" , "texinfo"
          , "man" , "plain" , "mediawiki", "rtf", "org"
          ]
        ]

-- makes sure file is fully closed after reading
readFile' :: FilePath -> IO String
readFile' f = do s <- readFileUTF8 f
                 return $! (length s `seq` s)

lhsWriterTests :: String -> [Test]
lhsWriterTests format
  = [ t "lhs to normal" format
    , t "lhs to lhs"    (format ++ "+lhs")
    ]
  where
    t n f = test n ["--columns=78", "-r", "native", "-s", "-w", f]
             "lhs-test.native" ("lhs-test" <.> ext f)
    ext f = if null languages && format == "html"
               then "nohl" <.> f
               else f

lhsReaderTest :: String -> Test
lhsReaderTest format =
  testWithNormalize normalizer "lhs" ["-r", format, "-w", "native"]
    ("lhs-test" <.> format) "lhs-test.native"
   where normalizer = writeNative defaultWriterOptions . normalize . readNative

writerTests :: String -> [Test]
writerTests format
  = [ test "basic"  (opts ++ ["-s"]) "testsuite.native" ("writer" <.> format)
    , test "tables" opts             "tables.native"    ("tables" <.> format)
    ]
  where
    opts = ["-r", "native", "-w", format, "--columns=78"]

s5WriterTest :: String -> [String] -> String -> Test
s5WriterTest modifier opts format 
  = test (format ++ " writer (" ++ modifier ++ ")")
    (["-r", "native", "-w", format] ++ opts) 
    "s5.native"  ("s5." ++ modifier <.> "html")

markdownCitationTests :: [Test]
markdownCitationTests
  =  map styleToTest ["chicago-author-date","ieee","mhra"] 
     ++ [test "natbib" wopts "markdown-citations.txt"
         "markdown-citations.txt"]
  where
    ropts             = ["-r", "markdown", "-w", "markdown", "--bibliography",
                         "biblio.bib", "--no-wrap"]
    wopts             = ropts ++ ["--natbib"]
    styleToTest style = test style (ropts ++ ["--csl", style ++ ".csl"])
                        "markdown-citations.txt"
                        ("markdown-citations." ++ style ++ ".txt")

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
  (outputPath, hOut) <- openTempFile "" "pandoc-test"
  let inpPath = inp
  let normPath = norm
  let options = ["--data-dir", ".."] ++ [inpPath] ++ opts
  let cmd = pandocPath ++ " " ++ unwords options
  ph <- runProcess pandocPath options Nothing
        (Just [("LANG","en_US.UTF-8"),("HOME", "./")]) Nothing (Just hOut)
        (Just stderr)
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
