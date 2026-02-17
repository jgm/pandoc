{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.MoinMoin
   Copyright   : © 2026 Jonathan Dowland
   License     : GNU GPL, version 2 or above

   Maintainer  : Jonathan Dowland <jon@dow.land>
   Stability   : alpha
   Portability : portable

Tests for the MoinMoin reader.
-}
module Tests.Readers.MoinMoin (tests) where

import Text.Pandoc -- readMoinMoin
import Text.Pandoc.Sources -- toSources
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Text as T
import Data.Either (fromRight)

-- @?= imposes Eq for which PandocError doesn't have an instance
-- so we remove the Either layer, replacing errors with nullDoc.
nullDoc :: Pandoc
nullDoc = Pandoc nullMeta []
runMM :: String -> Pandoc
runMM = fromRight nullDoc . runPure . readMoinMoin def . toSources . T.pack

tests :: [TestTree]
tests =
  [ testCase "basic"     $ runMM "hi" @?= Pandoc nullMeta [Para [Str "hi"]]
  , testCase "bold"      $ runMM "'''hi'''" @?= Pandoc nullMeta [Para [Strong [Str "hi"]]]
  , testCase "italic"    $ runMM "''hi''" @?= Pandoc nullMeta [Para [Emph [Str "hi"]]]
  , testCase "underline" $ runMM "__hi__" @?= Pandoc nullMeta [Para [Underline [Str "hi"]]]

  , testCase "italic then bold"
    $ runMM "''hello'' '''world'''" @?=
      Pandoc nullMeta [Para [Emph [Str "hello"], Space,Strong [Str "world"]]]

  , testCase "bold then italic"
    $ runMM "'''hello''' ''world''" @?=
      Pandoc nullMeta [Para [Strong [Str "hello"], Space,Emph [Str "world"]]]
  ]

main :: IO ()
main = defaultMain $ testGroup "." tests
