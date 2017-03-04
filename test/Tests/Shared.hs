module Tests.Shared (tests) where

import System.FilePath.Posix (joinPath)
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (assertBool, (@?=))
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder
import Text.Pandoc.Shared

tests :: [Test]
tests = [ testGroup "compactifyDL"
          [ testCase "compactifyDL with empty def" $
              assertBool "compactifyDL"
              (let x = [(str "word", [para (str "def"), mempty])]
               in  compactifyDL x == x)
          ]
        , testGroup "collapseFilePath" testCollapse
        ]

testCollapse :: [Test]
testCollapse = map (testCase "collapse")
 [  (collapseFilePath (joinPath [ ""]) @?= (joinPath [ ""]))
 ,  (collapseFilePath (joinPath [ ".","foo"]) @?= (joinPath [ "foo"]))
 ,  (collapseFilePath (joinPath [ ".",".","..","foo"]) @?= (joinPath [ joinPath ["..", "foo"]]))
 ,  (collapseFilePath (joinPath [ "..","foo"]) @?= (joinPath [ "..","foo"]))
 ,  (collapseFilePath (joinPath [ "","bar","..","baz"]) @?= (joinPath [ "","baz"]))
 ,  (collapseFilePath (joinPath [ "","..","baz"]) @?= (joinPath [ "","..","baz"]))
 ,  (collapseFilePath (joinPath [ ".","foo","..",".","bar","..",".",".","baz"]) @?= (joinPath [ "baz"]))
 ,  (collapseFilePath (joinPath [ ".",""]) @?= (joinPath [ ""]))
 ,  (collapseFilePath (joinPath [ ".",".",""]) @?= (joinPath [ ""]))
 ,  (collapseFilePath (joinPath [ "..",""]) @?= (joinPath [ ".."]))
 ,  (collapseFilePath (joinPath [ "..",".",""]) @?= (joinPath [ ".."]))
 ,  (collapseFilePath (joinPath [ ".","..",""]) @?= (joinPath [ ".."]))
 ,  (collapseFilePath (joinPath [ "..","..",""]) @?= (joinPath [ "..",".."]))
 ,  (collapseFilePath (joinPath [ "parent","foo","baz","..","bar"]) @?= (joinPath [ "parent","foo","bar"]))
 ,  (collapseFilePath (joinPath [ "parent","foo","baz","..","..","bar"]) @?= (joinPath [ "parent","bar"]))
 ,  (collapseFilePath (joinPath [ "parent","foo",".."]) @?= (joinPath [ "parent"]))
 ,  (collapseFilePath (joinPath [ "","parent","foo","..","..","bar"]) @?= (joinPath [ "","bar"]))
 ,  (collapseFilePath (joinPath [ "",".","parent","foo"]) @?= (joinPath [ "","parent","foo"]))]
