{- |
   Module      : Tests.Command
   Copyright   : © 2006-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Run commands, and test results, defined in markdown files.
-}
module Tests.Command (runTest, tests)
where

import Data.Algorithm.Diff
import System.Environment (getExecutablePath)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.List (isSuffixOf)
import System.Directory
import System.Exit
import System.FilePath ((</>))
import System.IO (hPutStr, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden.Advanced (goldenTest)
import Tests.Helpers
import Text.Pandoc
import qualified Text.Pandoc.UTF8 as UTF8

-- | Run a test with and return output.
execTest :: String    -- ^ Path to test executable
         -> String    -- ^ Shell command
         -> String    -- ^ Input text
         -> IO (ExitCode, String)  -- ^ Exit code and actual output
execTest testExePath cmd inp = do
  env' <- setupEnvironment testExePath
  let pr = (shell (pandocToEmulate True cmd)){ env = Just env' }
  (ec, out', err') <- readCreateProcessWithExitCode pr inp
  -- filter \r so the tests will work on Windows machines
  let out = filter (/= '\r') $ err' ++ out'
  case ec of
    ExitFailure _ -> hPutStr stderr err'
    ExitSuccess   -> return ()
  return (ec, out)

pandocToEmulate :: Bool -> String -> String
pandocToEmulate True ('p':'a':'n':'d':'o':'c':cs) =
  "test-pandoc --emulate" ++ pandocToEmulate False cs
pandocToEmulate False ('|':' ':'p':'a':'n':'d':'o':'c':cs) =
  "| " ++ "test-pandoc --emulate" ++ pandocToEmulate False cs
pandocToEmulate _ (c:cs) = c : pandocToEmulate False cs
pandocToEmulate _ [] = []

-- | Run a test, return True if test passed.
runTest :: String    -- ^ Path to test executable
        -> String    -- ^ Title of test
        -> String    -- ^ Shell command
        -> String    -- ^ Input text
        -> String    -- ^ Expected output
        -> TestTree
runTest testExePath testname cmd inp norm = testCase testname $ do
  (ec, out) <- execTest testExePath cmd inp
  result  <- if ec == ExitSuccess
                then
                  if out == norm
                     then return TestPassed
                     else return
                          $ TestFailed cmd "expected"
                          $ getDiff (lines out) (lines norm)
                else return $ TestError ec
  assertBool (show result) (result == TestPassed)

tests :: TestTree
{-# NOINLINE tests #-}
tests = unsafePerformIO $ do
  files <- filter (".md" `isSuffixOf`) <$>
               getDirectoryContents "command"
  testExePath <- getExecutablePath
  let cmds = map (extractCommandTest testExePath) files
  return $ testGroup "Command:" cmds

isCodeBlock :: Block -> Bool
isCodeBlock (CodeBlock _ _) = True
isCodeBlock _               = False

extractCode :: Block -> String
extractCode (CodeBlock _ code) = T.unpack code
extractCode _                  = ""

dropPercent :: String -> String
dropPercent ('%':xs) = dropWhile (== ' ') xs
dropPercent xs       = xs

runCommandTest :: FilePath -> FilePath -> Int -> String -> TestTree
runCommandTest testExePath fp num code =
  goldenTest testname getExpected getActual compareValues updateGolden
 where
  testname = "#" <> show num
  codelines = lines code
  (continuations, r1) = span ("\\" `isSuffixOf`) codelines
  cmd = dropPercent (unwords (map init continuations ++ take 1 r1))
  r2 = drop 1 r1
  (inplines, r3) = break (=="^D") r2
  normlines = takeWhile (/=".") (drop 1 r3)
  input = unlines inplines
  norm = unlines normlines
  getExpected = return norm
  getActual = snd <$> execTest testExePath cmd input
  compareValues expected actual
    | actual == expected = return Nothing
    | otherwise = return $ Just $ "--- test/command/" ++ fp ++ "\n+++ " ++
                                cmd ++ "\n" ++ showDiff (1,1)
                                   (getDiff (lines actual) (lines expected))
  updateGolden newnorm = do
    let fp' = "command" </> fp
    raw <- UTF8.readFile fp'
    let cmdline = "% " <> cmd
    let x = cmdline <> "\n" <> input <> "^D\n" <> norm
    let y = cmdline <> "\n" <> input <> "^D\n" <> newnorm
    let updated = T.replace (T.pack x) (T.pack y) raw
    UTF8.writeFile fp' updated

extractCommandTest :: FilePath -> FilePath -> TestTree
extractCommandTest testExePath fp = unsafePerformIO $ do
  contents <- UTF8.toText <$> BS.readFile ("command" </> fp)
  Pandoc _ blocks <- runIOorExplode (readMarkdown
                        def{ readerExtensions = pandocExtensions } contents)
  let codeblocks = map extractCode $ filter isCodeBlock blocks
  let cases = zipWith (runCommandTest testExePath fp) [1..] codeblocks
  return $ testGroup fp
         $ if null cases
              then [testCase "!!" $ assertFailure "No command tests defined"]
              else cases
