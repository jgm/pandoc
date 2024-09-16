{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.Mdoc
   Copyright   : © 2024 Evan Silberman
   License     : GNU GPL, version 2 or above

   Maintainer  : 
   Stability   : alpha
   Portability : portable

Tests for the Mdoc reader.
-}

module Tests.Readers.Mdoc (tests) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit (HasCallStack)
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

mdoc :: Text -> Pandoc
mdoc = purely $ readMdoc def

infix 4 =:
(=:) :: (ToString c, HasCallStack)
     => String -> (Text, c) -> TestTree
(=:) = test mdoc

tests :: [TestTree]
tests = [
  testGroup "one-line enclosures"
    [ "Dq" =:
        ".Dq hello world" =?>
        para (doubleQuoted "hello world")
    , "Sq" =:
        ".Sq hello world" =?>
        para (singleQuoted "hello world")
    ]
  , testGroup "inlines"
    [ "Sy" =:
        ".Sy hello world" =?>
        para (strong "hello world")
    , "delimiters" =:
        ".Sy ( hello world )" =?>
        para (mconcat ["(", strong "hello world", ")"])
    , "multiple" =:
        ".Sy hello Em world" =?>
        para (strong "hello" <> space <> emph "world")
    ]
  , testGroup "Ns macro"
    [ "at the beginning of a macro line (mandoc delta)" =:
        T.unlines [".Op before", ".Ns Op after"] =?>
        para "[before][after]"
    , "after a block closing macro" =:
        T.unlines [".Oo before", ".Oc Ns Op after"] =?>
        para "[before][after]"
    , "in the middle of a macro line" =:
        ".Oo before Oc Ns Op after" =?>
        para "[before][after]"
    , "before closing punctuation" =:
        ".Oo before Oc Ns : Op after" =?>
        para "[before]: [after]"
    , "after closing punctuation" =:
        ".Oo before Oc : Ns Op after" =?>
        para "[before]:[after]"
    , "at the end of a macro line" =:
        T.unlines [".Oo before Oc Ns", ".Op after"] =?>
        para "[before][after]"
    , "at the end of a partial-implicit line" =:
        T.unlines [".Op before Ns", ".Op after"] =?>
        para "[before][after]"
    , "normal words" =:
        ".No no Ns ns No no" =?>
        para ("nons" <> space <> "no")
    , "opening punctuation" =:
        ".No no Ns \"(\" ns No no" =?>
        para ("no(ns" <> space <> "no")
    , "closing punctuation" =:
        ".No no \"Ns\" ns \")\" No no" =?>
        para ("nons)" <> space <> "no")
    ]
  ]
