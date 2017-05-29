module Tests.Shared (tests) where

import System.FilePath.Posix (joinPath)
import Test.Tasty
import Test.Tasty.HUnit (assertBool, (@?=), testCase)
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
        , testGroup "hierarchicalize" testHierarchicalize
        ]

testCollapse :: [TestTree]
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


blocks1 :: [Block]
blocks1 =
  [ Header 1 ("header-one",[],[]) [Str "Header",Space,Str "One"]
  , Header 2 ("header-two",[],[]) [Str "Header",Space,Str "Two"]
  , Div ("div1",[],[])
    [ Header 1 ("header-three",[],[]) [Str "Header",Space,Str "Three"]
    ]
  , Div ("div2",[],[])
    [ Header 2 ("header-five",[],[]) [Str "Header",Space,Str "Five"]
    ]
  ]

elems1 :: [Element]
elems1 =
  [ Sec 1 [1] ("header-one",[],[]) [Str "Header",Space,Str "One"]
      [ Sec 2 [1,1] ("header-two",[],[]) [Str "Header",Space,Str "Two"] []
      ]
  , Sec 1 [2] ("div1",[],[]) [Str "Header",Space,Str "Three"]
      [ Sec 2 [2,1] ("div2",[],[]) [Str "Header",Space,Str "Five"] []
      ]
  ]

blocks2 :: [Block]
blocks2 =
  [ Header 3 ("header-one",[],[]) [Str "Header",Space,Str "One"]
  , Header 1 ("header-two",[],[]) [Str "Header",Space,Str "Two"]
  , Div ("div1",[],[])
    [ Header 2 ("header-three",[],[]) [Str "Header",Space,Str "Three"]
    ]
  , Div ("div2",[],[])
    [ Header 2 ("header-five",[],[]) [Str "Header",Space,Str "Five"]
    ]
  ]

elems2 :: [Element]
elems2 =
  [ Sec 3 [0,0,1] ("header-one",[],[]) [Str "Header",Space,Str "One"] []
  , Sec 1 [1] ("header-two",[],[]) [Str "Header",Space,Str "Two"]
    [ Sec 2 [1,1] ("div1",[],[]) [Str "Header",Space,Str "Three"] []
    , Sec 2 [1,2] ("div2",[],[]) [Str "Header",Space,Str "Five"] []
    ]
  ]

testHierarchicalize :: [TestTree]
testHierarchicalize = map (testCase "hierarchicalize")
  [ hierarchicalize blocks1 @?= elems1
  , hierarchicalize blocks2 @?= elems2
  ]
