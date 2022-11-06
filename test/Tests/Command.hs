{-# LANGUAGE BangPatterns #-}
{- |
   Module      : Tests.Command
   Copyright   : © 2006-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Run commands, and test results, defined in markdown files.

A command test is a code block with the following format:

> ```
> % pandoc -f markdown -t latex
> *hi*
> ^D
> \emph{hi}
> ```

- The first line, after "%", should contain a command to run.
- Then comes zero or more lines of text which will be passed
  to the command as stdin.
- The stdin terminates with a line containing "^D".
- The following lines are typically the expected output
  on stdout.
- If any output on stderr is expected, it should come first
  and each stderr line should be preceded by the string "2> ".
- If a nonzero exit status is expected, the last line should
  contain "=> " followed by the exit status.

-}
module Tests.Command (tests)
where

import Data.Maybe (fromMaybe)
import Data.Algorithm.Diff
import System.Environment (getExecutablePath)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.List (isSuffixOf)
import System.Directory
import System.Exit
import System.FilePath ((</>))
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
  (!ec, out', err') <- readCreateProcessWithExitCode pr inp
  let err = unlines . map ("2> " ++) . lines $ err'
  -- filter \r so the tests will work on Windows machines
  let out'' = filter (/= '\r') $ err ++ out'
  let out = out'' ++ case ec of
                        ExitFailure !n -> "=> " ++ show n ++ "\n"
                        ExitSuccess    -> ""
  return (ec, out)

pandocToEmulate :: Bool -> String -> String
pandocToEmulate True ('p':'a':'n':'d':'o':'c':cs) =
  "test-pandoc --emulate" ++ pandocToEmulate False cs
pandocToEmulate False ('|':' ':'p':'a':'n':'d':'o':'c':cs) =
  "| " ++ "test-pandoc --emulate" ++ pandocToEmulate False cs
pandocToEmulate _ (c:cs) = c : pandocToEmulate False cs
pandocToEmulate _ [] = []

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

dropPercent :: String -> Maybe String
dropPercent ('%':xs) = Just $ dropWhile (== ' ') xs
dropPercent _        = Nothing

runCommandTest :: FilePath -> FilePath -> Int -> String -> TestTree
runCommandTest testExePath fp num code = do
  goldenTest testname getExpected getActual compareValues' updateGolden
 where
  testname = "#" <> show num
  codelines = lines code
  (continuations, r1) = span ("\\" `isSuffixOf`) codelines
  cmd = fromMaybe (error "Command test line does not begin with %")
            (dropPercent (unwords (map init continuations ++ take 1 r1)))
  r2 = drop 1 r1
  (inplines, r3) = break (=="^D") r2
  normlines = takeWhile (/=".") (drop 1 r3)
  input = unlines inplines
  norm = unlines normlines
  getExpected = return norm
  getActual = snd <$> execTest testExePath cmd input
  compareValues' expected actual
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
