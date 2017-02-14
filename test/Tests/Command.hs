module Tests.Command (findPandoc, runTest, tests)
where

import Tests.Helpers
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit ( assertBool )
import System.FilePath ( (</>), takeDirectory, splitDirectories,
                         joinPath )
import System.Process
import System.Directory
import System.Exit
import Text.Pandoc
import Data.Algorithm.Diff
import Prelude hiding ( readFile )
import qualified Text.Pandoc.UTF8 as UTF8
import Data.List (isSuffixOf)
import Text.Pandoc.Shared (trimr)

-- | Run a test with normalize function, return True if test passed.
runTest :: String    -- ^ Title of test
        -> String    -- ^ Shell command
        -> String    -- ^ Input text
        -> String    -- ^ Expected output
        -> Test
runTest testname cmd inp norm = testCase testname $ do
  let cmd' = cmd ++ " --quiet --data-dir ../data"
  let findDynlibDir [] = Nothing
      findDynlibDir ("build":xs) = Just $ joinPath (reverse xs) </> "build"
      findDynlibDir (_:xs) = findDynlibDir xs
  let mbDynlibDir = findDynlibDir (reverse $ splitDirectories $
                                   takeDirectory $ takeWhile (/=' ') cmd)
  let dynlibEnv = case mbDynlibDir of
                       Nothing  -> []
                       Just d   -> [("DYLD_LIBRARY_PATH", d),
                                    ("LD_LIBRARY_PATH", d)]
  let env' = dynlibEnv ++ [("TMP","."),("LANG","en_US.UTF-8"),("HOME", "./")]
  let pr = (shell cmd'){ env = Just env' }
  (ec, out', _err) <- readCreateProcessWithExitCode pr inp
  -- filter \r so the tests will work on Windows machines
  let out = filter (/= '\r') out'
  result  <- if ec == ExitSuccess
                then do
                  if out == norm
                     then return TestPassed
                     else return
                          $ TestFailed cmd "expected"
                          $ getDiff (lines out) (lines norm)
                else return $ TestError ec
  assertBool (show result) (result == TestPassed)

tests :: Test
tests = buildTest $ do
  files <- filter (".md" `isSuffixOf`) <$>
               getDirectoryContents "command"
  let cmds = map extractCommandTest files
  return $ testGroup "Command:" cmds

isCodeBlock :: Block -> Bool
isCodeBlock (CodeBlock _ _) = True
isCodeBlock _ = False

extractCode :: Block -> String
extractCode (CodeBlock _ code) = code
extractCode _ = ""

dropPercent :: String -> String
dropPercent ('%':xs) = dropWhile (== ' ') xs
dropPercent xs = xs

runCommandTest :: FilePath -> (Int, String) -> IO Test
runCommandTest pandocpath (num, code) = do
  let codelines = lines code
  let (continuations, r1) = span ("\\" `isSuffixOf`) codelines
  let (cmd, r2) = (dropPercent (unwords (map init continuations ++ take 1 r1)),
                   drop 1 r1)
  let (inplines, r3) = break (=="^D") r2
  let normlines = takeWhile (/=".") (drop 1 r3)
  let input = unlines inplines
  let norm = unlines normlines
  let shcmd = trimr $ takeDirectory pandocpath </> cmd
  return $ runTest ("#" ++ show num) shcmd input norm

extractCommandTest :: FilePath -> Test
extractCommandTest fp = buildTest $ do
  pandocpath <- findPandoc
  contents <- UTF8.readFile ("command" </> fp)
  Pandoc _ blocks <- runIOorExplode (readMarkdown
                        def{ readerExtensions = pandocExtensions } contents)
  let codeblocks = map extractCode $ filter isCodeBlock $ blocks
  cases <- mapM (runCommandTest pandocpath) $ zip [1..] codeblocks
  return $ testGroup fp cases

