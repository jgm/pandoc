{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell #-}
-- Utility functions for the test suite.

module Tests.Helpers ( lit
                     , file
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
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Q, runIO)
import qualified Test.QuickCheck.Property as QP
import System.Console.ANSI
import Data.Algorithm.Diff

lit :: QuasiQuoter
lit = QuasiQuoter {
           quoteExp = (\a -> let b = rnl a in [|b|]) . filter (/= '\r')
         , quotePat = error "Cannot use lit as a pattern"
         }
       where rnl ('\n':xs) = xs
             rnl xs        = xs

file :: QuasiQuoter
file = quoteFile lit

-- adapted from TH 2.5 code
quoteFile :: QuasiQuoter -> QuasiQuoter
quoteFile (QuasiQuoter { quoteExp = qe, quotePat = qp }) =
  QuasiQuoter { quoteExp = get qe, quotePat = get qp }
  where
    get :: (String -> Q a) -> String -> Q a
    get old_quoter file_name = do { file_cts <- runIO (readFile file_name)
                                  ; old_quoter file_cts }

test :: (ToString a, ToString b, ToString c)
     => (a -> b)  -- ^ function to test
     -> String    -- ^ name of test case
     -> (a, c)    -- ^ (input, expected value)
     -> Test
test fn name (input, expected) =
  testCase name $ assertBool msg (actual' == expected')
     where msg = nl ++ dashes "input" ++ nl ++ input' ++ nl ++
                 dashes "expected" ++ nl ++ expected'' ++
                 dashes "got" ++ nl ++ actual'' ++
                 dashes ""
           nl = "\n"
           input'  = toString input
           actual' = toString $ fn input
           expected' = toString expected
           diff = getDiff (lines expected') (lines actual')
           expected'' = unlines $ map vividize $ filter (\(d,_) -> d /= S) diff
           actual''   = unlines $ map vividize $ filter (\(d,_) -> d /= F) diff
           dashes "" = replicate 72 '-'
           dashes x  = replicate (72 - length x - 5) '-' ++ " " ++ x ++ " ---"

vividize :: (DI,String) -> String
vividize (B,s) = s
vividize (_,s) = vivid s

property :: QP.Testable a => TestName -> a -> Test
property = testProperty

vivid :: String -> String
vivid s = setSGRCode [SetColor Background Dull Red
                     , SetColor Foreground Vivid White] ++ s
          ++ setSGRCode [Reset]

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
