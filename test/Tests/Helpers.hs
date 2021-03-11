{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{- |
   Module      : Tests.Helpers
   Copyright   : © 2006-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Utility functions for the test suite.
-}
module Tests.Helpers ( test
                     , TestResult(..)
                     , showDiff
                     , (=?>)
                     , purely
                     , ToString(..)
                     , ToText(..)
                     , ToPandoc(..)
                     )
                     where

import Data.Algorithm.Diff
import qualified Data.Map as M
import qualified Data.Text as T
import System.Exit
import Test.Tasty
import Test.Tasty.HUnit
import Text.Pandoc.Builder (Blocks, Inlines, doc, plain, nullMeta)
import Text.Pandoc.Class
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Shared (trimr)
import Text.Pandoc.Writers.Native (writeNative)
import Text.Printf
import qualified GHC.Show
import Prelude hiding (First)

test :: (ToText a, ToText b, ToText c, HasCallStack)
     => (a -> b)  -- ^ function to test
     -> String    -- ^ name of test case
     -> (a, c)    -- ^ (input, expected value)
     -> TestTree
test fn name (input, expected) =
  testCase name' $ assertBool (T.unpack msg) (actual' == expected')
     where msg = nl <> dashes "input" <> nl <> input' <> nl <>
                 dashes "result" <> nl <>
                 T.unlines (map vividize diff) <>
                 dashes ""
           nl = "\n"
           name'   = if length name > 54
                        then Prelude.take 52 name ++ "..."  -- avoid wide output
                        else name
           input'  = toText input
           actual' = T.lines $ toText $ fn input
           expected' = T.lines $ toText expected
           diff = getDiff expected' actual'
           dashes "" = T.replicate 72 "-"
           dashes x  = T.replicate (72 - T.length x - 5) "-" <>
                        " " <> x <> " ---"

data TestResult = TestPassed
                | TestError ExitCode
                | TestFailed String FilePath [Diff Text]
     deriving (Eq)

instance Show TestResult where
  show TestPassed     = "PASSED"
  show (TestError ec) = "ERROR " <> show ec
  show (TestFailed cmd file d) = "\n" <> dash <>
                                 "\n--- " <> file <>
                                 "\n+++ " <> cmd <> "\n" <>
                                 showDiff (1,1) d <> dash
    where dash = replicate 72 '-'

showDiff :: ToString a => (Int,Int) -> [Diff a] -> String
showDiff _ []             = ""
showDiff (l,r) (First ln : ds) =
  printf "+%4d " l <> toString ln <> "\n" <> showDiff (l+1,r) ds
showDiff (l,r) (Second ln : ds) =
  printf "-%4d " r <> toString ln <> "\n" <> showDiff (l,r+1) ds
showDiff (l,r) (Both _ _ : ds) =
  showDiff (l+1,r+1) ds

vividize :: Diff Text -> Text
vividize (Both s _) = "  " <> s
vividize (First s)  = "- " <> s
vividize (Second s) = "+ " <> s

purely :: (b -> PandocPure a) -> b -> a
purely f = either (error . show) id . runPure . f

infix 5 =?>
(=?>) :: a -> b -> (a,b)
x =?> y = (x, y)

instance ToString Pandoc where
  toString = T.unpack . toText

instance ToString Blocks where
  toString = T.unpack . toText

instance ToString Inlines where
  toString = T.unpack . toText

instance ToText Pandoc where
  toText d@(Pandoc (Meta m) _)
    | M.null m
    = purely (writeNative def) $ toPandoc d
  toText d@(Pandoc m _)
   = purely (writeNative def{ writerTemplate = Just mempty }) $ toPandoc d
                              -- need this to get meta output

instance ToText Blocks where
  toText = purely (writeNative def) . toPandoc

instance ToText Inlines where
  toText = T.stripEnd . purely (writeNative def) . toPandoc

class ToPandoc a where
  toPandoc :: a -> Pandoc

instance ToPandoc Pandoc where
  toPandoc = id

instance ToPandoc Blocks where
  toPandoc = doc

instance ToPandoc Inlines where
  toPandoc = doc . plain
