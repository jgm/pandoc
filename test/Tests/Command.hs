{-# LANGUAGE NoImplicitPrelude #-}
{- |
   Module      : Tests.Command
   Copyright   : © 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Run commands, and test results, defined in markdown files.
-}
module Tests.Command (findPandoc, runTest, tests)
where

import Prelude
import Data.Algorithm.Diff
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.List (isSuffixOf, intercalate)
import Data.Maybe (catMaybes)
import System.Directory
import qualified System.Environment as Env
import System.Exit
import System.FilePath (joinPath, splitDirectories, takeDirectory, (</>))
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
execTest :: FilePath  -- ^ Path to pandoc
         -> String    -- ^ Shell command
         -> String    -- ^ Input text
         -> IO (ExitCode, String)  -- ^ Exit code and actual output
execTest pandocpath cmd inp = do
  mldpath   <- Env.lookupEnv "LD_LIBRARY_PATH"
  mdyldpath <- Env.lookupEnv "DYLD_LIBRARY_PATH"
  let findDynlibDir []           = Nothing
      findDynlibDir ("build":xs) = Just $ joinPath (reverse xs) </> "build"
      findDynlibDir (_:xs)       = findDynlibDir xs
  let mbDynlibDir = findDynlibDir (reverse $ splitDirectories $
                                   takeDirectory $ takeWhile (/=' ') cmd)
  let dynlibEnv = [("DYLD_LIBRARY_PATH",
                    intercalate ":" $ catMaybes [mbDynlibDir, mdyldpath])
                  ,("LD_LIBRARY_PATH",
                    intercalate ":" $ catMaybes [mbDynlibDir, mldpath])]
  let env' = dynlibEnv ++ [("PATH",takeDirectory pandocpath),("TMP","."),
                           ("LANG","en_US.UTF-8"),
                           ("HOME", "./"),
                           ("pandoc_datadir", "..")]
  let pr = (shell cmd){ env = Just env' }
  (ec, out', err') <- readCreateProcessWithExitCode pr inp
  -- filter \r so the tests will work on Windows machines
  let out = filter (/= '\r') $ err' ++ out'
  case ec of
    ExitFailure _ -> hPutStr stderr err'
    ExitSuccess   -> return ()
  return (ec, out)

-- | Run a test, return True if test passed.
runTest :: String    -- ^ Title of test
        -> FilePath  -- ^ Path to pandoc
        -> String    -- ^ Shell command
        -> String    -- ^ Input text
        -> String    -- ^ Expected output
        -> TestTree
runTest testname pandocpath cmd inp norm = testCase testname $ do
  (ec, out) <- execTest pandocpath cmd inp
  result  <- if ec == ExitSuccess
                then
                  if out == norm
                     then return TestPassed
                     else return
                          $ TestFailed cmd "expected"
                          $ getDiff (lines out) (lines norm)
                else return $ TestError ec
  assertBool (show result) (result == TestPassed)

tests :: FilePath -> TestTree
{-# NOINLINE tests #-}
tests pandocPath = unsafePerformIO $ do
  files <- filter (".md" `isSuffixOf`) <$>
               getDirectoryContents "command"
  let cmds = map (extractCommandTest pandocPath) files
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
runCommandTest pandocpath fp num code =
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
  getActual = snd <$> execTest pandocpath cmd input
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
    let updated = T.unpack $ T.replace (T.pack x) (T.pack y) (T.pack raw)
    UTF8.writeFile fp' updated

extractCommandTest :: FilePath -> FilePath -> TestTree
extractCommandTest pandocpath fp = unsafePerformIO $ do
  contents <- UTF8.toText <$> BS.readFile ("command" </> fp)
  Pandoc _ blocks <- runIOorExplode (readMarkdown
                        def{ readerExtensions = pandocExtensions } contents)
  let codeblocks = map extractCode $ filter isCodeBlock blocks
  let cases = zipWith (runCommandTest pandocpath fp) [1..] codeblocks
  return $ testGroup fp cases
