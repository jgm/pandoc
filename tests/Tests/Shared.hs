module Tests.Shared (tests) where

import Text.Pandoc.Shared
import Test.Framework
import Text.Pandoc.Arbitrary()
import Test.Framework.Providers.HUnit
import Test.HUnit ( assertBool, (@?=) )
import Text.Pandoc.Builder
import System.FilePath.Posix (joinPath)

tests :: [Test]
tests = [ testGroup "compactify'DL"
          [ testCase "compactify'DL with empty def" $
              assertBool "compactify'DL"
              (let x = [(str "word", [para (str "def"), mempty])]
               in  compactify'DL x == x)
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
