{-# LANGUAGE NoImplicitPrelude #-}
module Tests.Shared (tests) where

import Prelude
import System.FilePath.Posix (joinPath)
import Test.Tasty
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder
import Text.Pandoc.Shared

tests :: [TestTree]
tests = [ testGroup "compactifyDL"
          [ testCase "compactifyDL with empty def" $
              assertBool "compactifyDL"
              (let x = [(str "word", [para (str "def"), mempty])]
               in  compactifyDL x == x)
          ]
        , testGroup "collapseFilePath" testCollapse
        ]

testCollapse :: [TestTree]
testCollapse = map (testCase "collapse")
 [  collapseFilePath (joinPath [ ""]) @?= (joinPath [ ""])
 ,  collapseFilePath (joinPath [ ".","foo"]) @?= (joinPath [ "foo"])
 ,  collapseFilePath (joinPath [ ".",".","..","foo"]) @?= (joinPath [ joinPath ["..", "foo"]])
 ,  collapseFilePath (joinPath [ "..","foo"]) @?= (joinPath [ "..","foo"])
 ,  collapseFilePath (joinPath [ "","bar","..","baz"]) @?= (joinPath [ "","baz"])
 ,  collapseFilePath (joinPath [ "","..","baz"]) @?= (joinPath [ "","..","baz"])
 ,  collapseFilePath (joinPath [ ".","foo","..",".","bar","..",".",".","baz"]) @?= (joinPath [ "baz"])
 ,  collapseFilePath (joinPath [ ".",""]) @?= (joinPath [ ""])
 ,  collapseFilePath (joinPath [ ".",".",""]) @?= (joinPath [ ""])
 ,  collapseFilePath (joinPath [ "..",""]) @?= (joinPath [ ".."])
 ,  collapseFilePath (joinPath [ "..",".",""]) @?= (joinPath [ ".."])
 ,  collapseFilePath (joinPath [ ".","..",""]) @?= (joinPath [ ".."])
 ,  collapseFilePath (joinPath [ "..","..",""]) @?= (joinPath [ "..",".."])
 ,  collapseFilePath (joinPath [ "parent","foo","baz","..","bar"]) @?= (joinPath [ "parent","foo","bar"])
 ,  collapseFilePath (joinPath [ "parent","foo","baz","..","..","bar"]) @?= (joinPath [ "parent","bar"])
 ,  collapseFilePath (joinPath [ "parent","foo",".."]) @?= (joinPath [ "parent"])
 ,  collapseFilePath (joinPath [ "","parent","foo","..","..","bar"]) @?= (joinPath [ "","bar"])
 ,  collapseFilePath (joinPath [ "",".","parent","foo"]) @?= (joinPath [ "","parent","foo"])]
