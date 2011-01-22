{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell #-}
-- Utility functions for the test suite.

module Tests.Helpers ( lit
                     , test
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
import Text.Pandoc.Shared (normalize, defaultWriterOptions,
                           WriterOptions(..), removeTrailingSpace)
import Text.Pandoc.Writers.Native (writeNative)
import Language.Haskell.TH.Quote
import qualified Test.QuickCheck.Property as QP

lit :: QuasiQuoter
lit = QuasiQuoter ((\a -> let b = rnl a in [|b|]) . filter (/= '\r')) $
         error "Cannot use lit as a pattern"
       where rnl ('\n':xs) = xs
             rnl xs        = xs

test :: (ToString a, ToString b, ToString c)
     => (a -> b)  -- ^ function to test
     -> String    -- ^ name of test case
     -> (a, c)    -- ^ (input, expected value)
     -> Test
test fn name (input, expected) =
  testCase name $ assertBool msg (actual' == expected')
     where msg = dashes "input" ++ input' ++
                 dashes "expected" ++ expected' ++
                 dashes "got" ++ actual' ++
                 dashes ""
           input'  = toString input
           actual' = toString $ fn input
           expected' = toString expected
           dashes "" = '\n' : replicate 72 '-'
           dashes x  = '\n' : replicate (72 - length x - 5) '-' ++ " " ++
                              x ++ " ---\n"

property :: QP.Testable a => TestName -> a -> Test
property = testProperty

infix 6 =?>
(=?>) :: a -> b -> (a,b)
x =?> y = (x, y)

class ToString a where
  toString :: a -> String

instance ToString Pandoc where
  toString d = writeNative defaultWriterOptions{ writerStandalone = s }
               $ toPandoc d
   where s = case d of
                  (Pandoc (Meta [] [] []) _) -> False
                  _                          -> True

instance ToString Blocks where
  toString = writeNative defaultWriterOptions . toPandoc

instance ToString Inlines where
  toString = removeTrailingSpace . writeNative defaultWriterOptions .
             toPandoc

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
