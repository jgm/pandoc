{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- Utility functions for the test suite.

module Tests.Helpers ( test
                     , TestResult(..)
                     , showDiff
                     , findPandoc
                     , (=?>)
                     , purely
                     , property
                     , ToString(..)
                     , ToPandoc(..)
                     )
                     where

import Text.Pandoc.Definition
import Text.Pandoc.Builder (Inlines, Blocks, doc, plain)
import Text.Pandoc.Class
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit (assertBool)
import Text.Pandoc.Shared (trimr)
import Text.Pandoc.Options
import Text.Pandoc.Writers.Native (writeNative)
import Text.Printf
import System.Environment.Executable (getExecutablePath)
import qualified Test.QuickCheck.Property as QP
import Data.Algorithm.Diff
import qualified Data.Map as M
import System.Exit
import System.Directory
import System.FilePath

test :: (ToString a, ToString b, ToString c)
     => (a -> b)  -- ^ function to test
     -> String    -- ^ name of test case
     -> (a, c)    -- ^ (input, expected value)
     -> Test
test fn name (input, expected) =
  testCase name $ assertBool msg (actual' == expected')
     where msg = nl ++ dashes "input" ++ nl ++ input' ++ nl ++
                 dashes "result" ++ nl ++
                 unlines (map vividize diff) ++
                 dashes ""
           nl = "\n"
           input'  = toString input
           actual' = lines $ toString $ fn input
           expected' = lines $ toString expected
           diff = getDiff expected' actual'
           dashes "" = replicate 72 '-'
           dashes x  = replicate (72 - length x - 5) '-' ++ " " ++ x ++ " ---"

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


vividize :: Diff String -> String
vividize (Both s _) = "  " ++ s
vividize (First s)  = "- " ++ s
vividize (Second s) = "+ " ++ s

property :: QP.Testable a => TestName -> a -> Test
property = testProperty

purely :: (b -> PandocPure a) -> b -> a
purely f = either (error . show) id . runPure . f

infix 5 =?>
(=?>) :: a -> b -> (a,b)
x =?> y = (x, y)

class ToString a where
  toString :: a -> String

instance ToString Pandoc where
  toString d = purely (writeNative def{ writerTemplate = s }) $ toPandoc d
   where s = case d of
                  (Pandoc (Meta m) _)
                    | M.null m  -> Nothing
                    | otherwise -> Just "" -- need this to get meta output

instance ToString Blocks where
  toString = purely (writeNative def) . toPandoc

instance ToString Inlines where
  toString = trimr . purely (writeNative def) . toPandoc

instance ToString String where
  toString = id

class ToPandoc a where
  toPandoc :: a -> Pandoc

instance ToPandoc Pandoc where
  toPandoc = id

instance ToPandoc Blocks where
  toPandoc = doc

instance ToPandoc Inlines where
  toPandoc = doc . plain
