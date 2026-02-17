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

readsTo :: String -> [Block] -> Assertion
readsTo s b = runMM s @?= Pandoc nullMeta b

tests :: [TestTree]
tests =
  [ testCase "basic"     $ "hi"       `readsTo` [Para [Str "hi"]]
  , testCase "bold"      $ "'''hi'''" `readsTo` [Para [Strong [Str "hi"]]]
  , testCase "italic"    $ "''hi''"   `readsTo` [Para [Emph [Str "hi"]]]
  , testCase "underline" $ "__hi__"   `readsTo` [Para [Underline [Str "hi"]]]

  , testCase "italic then bold"
    $ "''hello'' '''world'''" `readsTo`
      [Para [Emph [Str "hello"], Space,Strong [Str "world"]]]

  , testCase "bold then italic"
    $ "'''hello''' ''world''" `readsTo`
      [Para [Strong [Str "hello"], Space,Emph [Str "world"]]]
  ]

main :: IO ()
main = defaultMain $ testGroup "." tests
