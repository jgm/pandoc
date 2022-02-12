{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances    #-}
{- |
   Module      : Tests.Helpers
   Copyright   : © 2006-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Utility functions for the test suite.
-}
module Tests.Helpers ( test
                     , TestResult(..)
                     , setupEnvironment
                     , showDiff
                     , testGolden
                     , (=?>)
                     , purely
                     , ToString(..)
                     , ToPandoc(..)
                     )
                     where

import System.FilePath
import Data.Algorithm.Diff
import qualified Data.Map as M
import qualified Text.Pandoc.UTF8 as UTF8
import Data.Text (Text, unpack)
import qualified Data.Text as T
import System.Exit
import qualified System.Environment as Env
import Test.Tasty
import Test.Tasty.Golden.Advanced (goldenTest)
import Test.Tasty.HUnit
import Text.Pandoc.Builder (Blocks, Inlines, doc, plain)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Shared (trimr)
import Text.Pandoc.Writers.Native (writeNative)
import Text.Printf

test :: (ToString a, ToString b, ToString c, HasCallStack)
     => (a -> b)  -- ^ function to test
     -> String    -- ^ name of test case
     -> (a, c)    -- ^ (input, expected value)
     -> TestTree
test fn name (input, expected) =
  testCase name' $ assertBool msg (actual' == expected')
     where msg = nl ++ dashes "input" ++ nl ++ input' ++ nl ++
                 dashes "result" ++ nl ++
                 unlines (map vividize diff) ++
                 dashes ""
           nl = "\n"
           name'   = if length name > 54
                        then take 52 name ++ "..."  -- avoid wide output
                        else name
           input'  = toString input
           actual' = lines $ toString $ fn input
           expected' = lines $ toString expected
           diff = getDiff expected' actual'
           dashes "" = replicate 72 '-'
           dashes x  = replicate (72 - length x - 5) '-' ++ " " ++ x ++ " ---"

testGolden :: TestName -> FilePath -> FilePath -> (Text -> IO Text) -> TestTree
testGolden  name expectedPath inputPath fn =
  goldenTest
    name
    (UTF8.readFile expectedPath)
    (UTF8.readFile inputPath >>= fn)
    compareVals
    (UTF8.writeFile expectedPath)
 where
  compareVals expected actual
    | expected == actual = return Nothing
    | otherwise =  return $ Just $
        "\n--- " ++ expectedPath ++ "\n+++\n" ++
        showDiff (1,1)
          (getDiff (lines . filter (/='\r') $ T.unpack actual)
                   (lines . filter (/='\r') $ T.unpack expected))

-- | Set up environment for pandoc command tests.
setupEnvironment :: FilePath -> IO [(String, String)]
setupEnvironment testExePath = do
  mldpath   <- Env.lookupEnv "LD_LIBRARY_PATH"
  mdyldpath <- Env.lookupEnv "DYLD_LIBRARY_PATH"
  mpdd <- Env.lookupEnv "pandoc_datadir"
  -- Note that Cabal sets the pandoc_datadir environment variable
  -- to point to the source directory, since otherwise getDataFilename
  -- will look in the data directory into which pandoc will be installed
  -- (but has not yet been).  So when we spawn a new process with
  -- pandoc, we need to make sure this environment variable is set.
  return $ ("PATH",takeDirectory testExePath) :
           ("TMP",".") :
           ("LANG","en_US.UTF-8") :
           ("HOME", "./") :
           maybe [] ((:[]) . ("pandoc_datadir",)) mpdd ++
           maybe [] ((:[]) . ("LD_LIBRARY_PATH",)) mldpath ++
           maybe [] ((:[]) . ("DYLD_LIBRARY_PATH",)) mdyldpath

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

vividize :: Diff String -> String
vividize (Both s _) = "  " ++ s
vividize (First s)  = "- " ++ s
vividize (Second s) = "+ " ++ s

purely :: (b -> PandocPure a) -> b -> a
purely f = either (error . show) id . runPure . f

infix 5 =?>
(=?>) :: a -> b -> (a,b)
x =?> y = (x, y)

class ToString a where
  toString :: a -> String

instance ToString Pandoc where
  toString d = unpack $
     purely (writeNative def{ writerTemplate = s }) $ toPandoc d
   where s = case d of
                  (Pandoc (Meta m) _)
                    | M.null m  -> Nothing
                    | otherwise -> Just mempty -- need this to get meta output

instance ToString Blocks where
  toString = unpack . purely (writeNative def) . toPandoc

instance ToString [Block] where
  toString = toString . B.fromList

instance ToString Block where
  toString = toString . B.singleton 

instance ToString Inlines where
  toString = unpack . trimr . purely (writeNative def) . toPandoc

instance ToString String where
  toString = id

instance ToString Text where
  toString = unpack

class ToPandoc a where
  toPandoc :: a -> Pandoc

instance ToPandoc Pandoc where
  toPandoc = id

instance ToPandoc Blocks where
  toPandoc = doc

instance ToPandoc Inlines where
  toPandoc = doc . plain
