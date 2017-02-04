module Tests.Command (findPandoc, runTest, tests)
where

import Test.Framework (testGroup, Test )
import Test.Framework.Providers.HUnit
import Test.HUnit ( assertBool )
import System.Environment.Executable (getExecutablePath)
import System.FilePath ( (</>), takeDirectory, splitDirectories,
                         joinPath )
import System.Directory
import System.Exit
import Data.Algorithm.Diff
import Prelude hiding ( readFile )
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Printf
import Text.Pandoc.Process (pipeProcess)

data TestResult = TestPassed
                | TestError ExitCode
                | TestFailed String FilePath [Diff String]
     deriving (Eq)

instance Show TestResult where
  show TestPassed     = "PASSED"
  show (TestError ec) = "ERROR " ++ show ec
  show (TestFailed cmd file d) = '\n' : dash ++
                                 "\n--- " ++ file ++
                                 "\n+++ " ++ cmd ++ "\n" ++ showDiff (1,1) d ++
                                 dash
    where dash = replicate 72 '-'

showDiff :: (Int,Int) -> [Diff String] -> String
showDiff _ []             = ""
showDiff (l,r) (First ln : ds) =
  printf "+%4d " l ++ ln ++ "\n" ++ showDiff (l+1,r) ds
showDiff (l,r) (Second ln : ds) =
  printf "-%4d " r ++ ln ++ "\n" ++ showDiff (l,r+1) ds
showDiff (l,r) (Both _ _ : ds) =
  showDiff (l+1,r+1) ds

-- | Find pandoc executable relative to test-pandoc
-- First, try in same directory (e.g. if both in ~/.cabal/bin)
-- Second, try ../pandoc (e.g. if in dist/XXX/build/test-pandoc)
findPandoc :: IO FilePath
findPandoc = do
  testExePath <- getExecutablePath
  let testExeDir = takeDirectory testExePath
  found <- doesFileExist (testExeDir </> "pandoc")
  return $ if found
              then testExeDir </> "pandoc"
              else case splitDirectories testExeDir of
                         [] -> error "test-pandoc: empty testExeDir"
                         xs -> joinPath (init xs) </> "pandoc" </> "pandoc"

-- | Run a test with normalize function, return True if test passed.
runTest :: FilePath  -- ^ pandoc executable path
        -> String    -- ^ Title of test
        -> [String]  -- ^ Options to pass to pandoc
        -> String    -- ^ Input text
        -> String    -- ^ Expected output
        -> Test
runTest pandocPath testname opts inp norm = testCase testname $ do
  let options = ["--quiet", "--data-dir", ".." </> "data"] ++ opts
  let cmd = unwords ((pandocPath </> "pandoc") : options)
  let findDynlibDir [] = Nothing
      findDynlibDir ("build":xs) = Just $ joinPath (reverse xs) </> "build"
      findDynlibDir (_:xs) = findDynlibDir xs
  let mbDynlibDir = findDynlibDir (reverse $ splitDirectories pandocPath)
  let dynlibEnv = case mbDynlibDir of
                       Nothing  -> []
                       Just d   -> [("DYLD_LIBRARY_PATH", d),
                                    ("LD_LIBRARY_PATH", d)]
  let env = dynlibEnv ++ [("TMP","."),("LANG","en_US.UTF-8"),("HOME", "./")]
  (ec, outbs) <- pipeProcess (Just env) pandocPath options
                     (UTF8.fromStringLazy inp)
  -- filter \r so the tests will work on Windows machines
  let out = filter (/= '\r') $ UTF8.toStringLazy outbs
  result  <- if ec == ExitSuccess
                then do
                  if out == norm
                     then return TestPassed
                     else return
                          $ TestFailed cmd "expected"
                          $ getDiff (lines out) (lines norm)
                else return $ TestError ec
  assertBool (show result) (result == TestPassed)

tests :: [Test]
tests = [ testGroup "commands"
          [ ]
        ]
