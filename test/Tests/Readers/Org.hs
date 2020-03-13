{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Shared
   Copyright   : © 2014-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
   Stability   : alpha
   Portability : portable

Tests of the org reader.
-}
module Tests.Readers.Org (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Tests.Readers.Org.Block as Block
import qualified Tests.Readers.Org.Directive as Directive
import qualified Tests.Readers.Org.Inline as Inline
import qualified Tests.Readers.Org.Meta as Meta

tests :: [TestTree]
tests =
  [ testGroup "Inlines" Inline.tests
  , testGroup "Basic Blocks" Block.tests
  , testGroup "Meta Information" Meta.tests
  , testGroup "Directives" Directive.tests
  ]
