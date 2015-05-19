{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- Utility functions for the test suite.

module Tests.Helpers ( test
                     , notIn
                     , (=?>)
                     , property
                     , ToString(..)
                     , ToPandoc(..)
                     )
                     where

import Text.Pandoc.Definition
import Text.Pandoc.Builder (Inlines, Blocks, doc, plain)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit (assertBool)
import Text.Pandoc.Shared (normalize, trimr)
import Text.Pandoc.Options
import Text.Pandoc.Writers.Native (writeNative)
import qualified Test.QuickCheck.Property as QP
import Data.Algorithm.Diff
import qualified Data.Map as M
import Data.List (isInfixOf)

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

notIn :: (ToString a, ToString b)
      => (a -> b)  -- ^ function to test
      -> String    -- ^ name of test case
      -> (a, String)    -- ^ (input, expected value)
      -> Test
notIn fn name (input, notExpected) =
  testCase name $ assertBool msg $ not included
    where actual = toString $ fn input
          included = notExpected `isInfixOf` actual
          msg = notExpected ++ " should not be found in the result of parsing " ++ (toString input)

vividize :: Diff String -> String
vividize (Both s _) = "  " ++ s
vividize (First s)  = "- " ++ s
vividize (Second s) = "+ " ++ s

property :: QP.Testable a => TestName -> a -> Test
property = testProperty

infix 5 =?>
(=?>) :: a -> b -> (a,b)
x =?> y = (x, y)

class ToString a where
  toString :: a -> String

instance ToString Pandoc where
  toString d = writeNative def{ writerStandalone = s } $ toPandoc d
   where s = case d of
                  (Pandoc (Meta m) _)
                    | M.null m  -> False
                    | otherwise -> True

instance ToString Blocks where
  toString = writeNative def . toPandoc

instance ToString Inlines where
  toString = trimr . writeNative def . toPandoc

instance ToString String where
  toString = id

class ToPandoc a where
  toPandoc :: a -> Pandoc

instance ToPandoc Pandoc where
  toPandoc = normalize

instance ToPandoc Blocks where
  toPandoc = normalize . doc

instance ToPandoc Inlines where
  toPandoc = normalize . doc . plain
