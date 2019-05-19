{- |
Module      : Tests.Lua.Module
Copyright   : © 2019 Albert Krewinkel
License     : GNU GPL, version 2 or above

Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
Stability   : alpha
Portability : portable

Lua module tests
-}
module Tests.Lua.Module (tests) where

import System.FilePath ((</>))
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Lua (testLuaFile)

import Tests.Lua (runLuaTest)

tests :: [TestTree]
tests =
  [ testPandocLua "pandoc" ("lua" </> "module" </> "pandoc.lua")
  , testPandocLua "pandoc.util" ("lua" </> "module" </> "pandoc.utils.lua")
  ]

testPandocLua :: TestName -> FilePath -> TestTree
testPandocLua = testLuaFile runLuaTest
