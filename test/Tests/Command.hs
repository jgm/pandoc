{-# LANGUAGE NoImplicitPrelude #-}
module Tests.Command (findPandoc, runTest, tests)
where

import Prelude
import Data.Algorithm.Diff
import qualified Data.ByteString as BS
import Data.List (isSuffixOf)
import Prelude hiding (readFile)
import System.Directory
import System.Exit
import System.FilePath (joinPath, splitDirectories, takeDirectory, (</>))
import System.IO (hPutStr, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import Test.Tasty
import Test.Tasty.HUnit
import Tests.Helpers
import Text.Pandoc
import qualified Text.Pandoc.UTF8 as UTF8

-- | Run a test with normalize function, return True if test passed.
runTest :: String    -- ^ Title of test
        -> FilePath  -- ^ Path to pandoc
        -> String    -- ^ Shell command
        -> String    -- ^ Input text
        -> String    -- ^ Expected output
        -> TestTree
runTest testname pandocpath cmd inp norm = testCase testname $ do
  let findDynlibDir []           = Nothing
      findDynlibDir ("build":xs) = Just $ joinPath (reverse xs) </> "build"
      findDynlibDir (_:xs)       = findDynlibDir xs
  let mbDynlibDir = findDynlibDir (reverse $ splitDirectories $
                                   takeDirectory $ takeWhile (/=' ') cmd)
  let dynlibEnv = case mbDynlibDir of
                       Nothing  -> []
                       Just d   -> [("DYLD_LIBRARY_PATH", d),
                                    ("LD_LIBRARY_PATH", d)]
  let env' = dynlibEnv ++ [("PATH",takeDirectory pandocpath),("TMP","."),("LANG","en_US.UTF-8"),("HOME", "./"),("pandoc_datadir", "..")]
  let pr = (shell cmd){ env = Just env' }
  (ec, out', err') <- readCreateProcessWithExitCode pr inp
  -- filter \r so the tests will work on Windows machines
  let out = filter (/= '\r') $ err' ++ out'
  result  <- if ec == ExitSuccess
                then
                  if out == norm
                     then return TestPassed
                     else return
                          $ TestFailed cmd "expected"
                          $ getDiff (lines out) (lines norm)
                else do
                  hPutStr stderr err'
                  return $ TestError ec
  assertBool (show result) (result == TestPassed)

tests :: TestTree
{-# NOINLINE tests #-}
tests = unsafePerformIO $ do
  pandocpath <- findPandoc
  files <- filter (".md" `isSuffixOf`) <$>
               getDirectoryContents "command"
  let cmds = map (extractCommandTest pandocpath) files
  return $ testGroup "Command:" cmds

isCodeBlock :: Block -> Bool
isCodeBlock (CodeBlock _ _) = True
isCodeBlock _               = False

extractCode :: Block -> String
extractCode (CodeBlock _ code) = code
extractCode _                  = ""

dropPercent :: String -> String
dropPercent ('%':xs) = dropWhile (== ' ') xs
dropPercent xs       = xs

runCommandTest :: FilePath -> (Int, String) -> TestTree
runCommandTest pandocpath (num, code) =
  let codelines = lines code
      (continuations, r1) = span ("\\" `isSuffixOf`) codelines
      (cmd, r2) = (dropPercent (unwords (map init continuations ++ take 1 r1)),
                   drop 1 r1)
      (inplines, r3) = break (=="^D") r2
      normlines = takeWhile (/=".") (drop 1 r3)
      input = unlines inplines
      norm = unlines normlines
      shcmd = cmd -- trimr $ takeDirectory pandocpath </> cmd
  in  runTest ("#" ++ show num) pandocpath shcmd input norm

extractCommandTest :: FilePath -> FilePath -> TestTree
extractCommandTest pandocpath fp = unsafePerformIO $ do
  contents <- UTF8.toText <$> BS.readFile ("command" </> fp)
  Pandoc _ blocks <- runIOorExplode (readMarkdown
                        def{ readerExtensions = pandocExtensions } contents)
  let codeblocks = map extractCode $ filter isCodeBlock blocks
  let cases = map (runCommandTest pandocpath) $ zip [1..] codeblocks
  return $ testGroup fp cases
