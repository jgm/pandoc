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

  , testCase "italic and bold" $
    "'''''hello world'''''" `readsTo`
    [Para [Strong [Emph [Str "hello", Space, Str "world"]]]]

  , testCase "heading 1"    $ "= 1 ="           `readsTo` [Header 1 ("",[],[]) [Str "1"]]
  , testCase "heading 2"    $ "== 2 =="         `readsTo` [Header 2 ("",[],[]) [Str "2"]]
  , testCase "heading 3"    $ "=== 3 ==="       `readsTo` [Header 3 ("",[],[]) [Str "3"]]
  , testCase "heading 4"    $ "==== 4 ===="     `readsTo` [Header 4 ("",[],[]) [Str "4"]]
  , testCase "heading 5"    $ "===== 5 ====="   `readsTo` [Header 5 ("",[],[]) [Str "5"]]
  , testCase "no heading 6" $ "====== 6 ======" `readsTo` [Para [Str "======",Space,Str "6",Space,Str "======"]]

  , testCase "superscript"    $ "^2^" `readsTo` [Para [Superscript [Str "2"]]]
  , testCase "subscript"      $ ",,low,," `readsTo` [Para [Subscript [Str "low"]]]

  -- XXX: add tests for annotations
  , testCase "strikeout" $ "--(delete)--" `readsTo` [Para [Strikeout [Str "delete"]]]
  , testCase "larger"    $ "~+larger+~"   `readsTo` [Para [Str "larger"]]
  , testCase "smaller"   $ "~-smaller-~"  `readsTo` [Para [Str "smaller"]]

  , testGroup "links"
    [ testCase "CamelCase" $ "FooBar"     `readsTo` [Para [Link ("",[],[]) [Str "FooBar"] ("FooBar","")]]
    , testCase "/SubCase1" $ "/SubCase1"  `readsTo` [Para [Link ("",[],[]) [Str "/SubCase1"] ("/SubCase1","")]]
    , testCase "Sub/Case2" $ "Sub/Case2"  `readsTo` [Para [Link ("",[],[]) [Str "Sub/Case2"] ("Sub/Case2","")]]
    , testCase "bracket1"  $ "[[foo]]"    `readsTo` [Para [Link ("",[],[]) [Str "foo"] ("foo","")]]
    , testCase "labelled"  $ "[[foo|bar]]"`readsTo` [Para [Link ("",[],[]) [Str "bar"] ("foo","")]]
    , testCase "banglink"  $ "!NotLink"  `readsTo` [Para [Str "!NotLink"]]
    , testCase "notalink"  $ "Not''''''Link" `readsTo` [Para [Str "NotLink"]]
    , testCase "singular1" $ "SinGular''''''s" `readsTo` [Para [Link ("",[],[]) [Str "SinGular"] ("SinGular","")]]
    , testCase "singular2" $ "SinGular``s" `readsTo` [Para [Link ("",[],[]) [Str "SinGular"] ("SinGular","")]]
    ]

  ]

main :: IO ()
main = defaultMain $ testGroup "." tests
