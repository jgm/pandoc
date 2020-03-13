{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{- |
   Module      : Tests.Helpers
   Copyright   : © 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Utility functions for the test suite.
-}
module Tests.Helpers ( test
                     , TestResult(..)
                     , showDiff
                     , findPandoc
                     , (=?>)
                     , purely
                     , ToString(..)
                     , ToPandoc(..)
                     )
                     where

import Prelude
import Data.Algorithm.Diff
import qualified Data.Map as M
import Data.Text (Text, unpack)
import System.Directory
import System.Environment.Executable (getExecutablePath)
import System.Exit
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit
import Text.Pandoc.Builder (Blocks, Inlines, doc, plain)
import Text.Pandoc.Class
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Shared (trimr)
import Text.Pandoc.Writers.Native (writeNative)
import Text.Printf

test :: (ToString a, ToString b, ToString c)
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
findPandoc :: IO FilePath
findPandoc = do
  testExePath <- getExecutablePath
  let pandocDir =
        case reverse (splitDirectories (takeDirectory testExePath)) of
             -- cabalv2 with --disable-optimization
             "test-pandoc" : "build" : "noopt" : "test-pandoc" : "t" : ps
               -> joinPath (reverse ps) </>
                  "x" </> "pandoc" </> "noopt" </> "build" </> "pandoc"
             -- cabalv2 without --disable-optimization
             "test-pandoc" : "build" : "test-pandoc" : "t" : ps
               -> joinPath (reverse ps) </>
                  "x" </> "pandoc" </> "build" </> "pandoc"
             -- cabalv1
             "test-pandoc" : "build" : ps
               -> joinPath (reverse ps) </> "build" </> "pandoc"
             _ -> error $ "findPandoc: could not find pandoc executable"
  let pandocPath = pandocDir </> "pandoc"
#ifdef _WINDOWS
                             <.> "exe"
#endif
  found <- doesFileExist pandocPath
  if found
     then return pandocPath
     else error $ "findPandoc: could not find pandoc executable at "
                   ++ pandocPath

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
