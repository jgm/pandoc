{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Tests.Lua.Marshalling
   Copyright   : © 2017-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
   Stability   : alpha
   Portability : portable

Tests marshalling and unmarshalling of pandoc types.
-}
module Tests.Lua.Marshalling ( tests ) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import Text.Pandoc.Definition (Caption (..))
import Tests.Lua.Helpers (runLuaTest)

import qualified Foreign.Lua as Lua

tests :: [TestTree]
tests =
  [ testCase "empty table as empty caption" $ do
      cptn <- runLuaTest $ do
        Lua.newtable
        Lua.peek Lua.stackTop
      assertEqual "error while unmarshaling table" cptn
                  (Caption Nothing [])
  ]
