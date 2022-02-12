{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Shared
   Copyright   : © 2006-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Tests for functions used in many parts of the library.
-}
module Tests.Shared (tests) where

import System.FilePath.Posix (joinPath)
import Test.Tasty
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder
import Text.Pandoc.Shared
import Test.Tasty.HUnit
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Writers.Shared
import qualified Data.Text as T

tests :: [TestTree]
tests = [ testGroup "compactifyDL"
          [ testCase "compactifyDL with empty def" $
              assertBool "compactifyDL"
              (let x = [(str "word", [para (str "def"), mempty])]
               in  compactifyDL x == x)
          ]
        , testGroup "collapseFilePath" testCollapse
        , testGroup "toLegacyTable" testLegacyTable
        , testGroup "table of contents" testTOC
        ] 

givesTOC :: String -> (Blocks, Blocks) -> TestTree
givesTOC desc (blocks, toc) = test (toTableOfContents def) desc (toList blocks, head . toList $ toc)

linkId :: T.Text -> T.Text -> T.Text -> Inlines -> Inlines
linkId lId = linkWith (lId,[],[])

headerId :: T.Text -> Int -> Inlines -> Blocks
headerId hId = headerWith (hId,[],[])

testTOC :: [TestTree]
testTOC = [ givesTOC "empty case" $ mempty =?> bulletList []
          , givesTOC "no headers" $ horizontalRule =?> bulletList []
          , givesTOC "unlinked header" $
              header 1 "H1" =?>
              bulletList [plain "H1"]
          , givesTOC "linked header" $
              headerId "h1" 1 "H1" =?>
              bulletList [plain $ linkId "toc-h1" "#h1" "" "H1"]
          , givesTOC "nested headlines" $
              header 1 "H1a" <> header 2 "H2" =?>
              bulletList [plain "H1a" <> bulletList [plain "H2"]]
          , givesTOC "only referenced headers" $
              header 1 "H1a" <> headerId "h2" 2 "H2" =?>
              bulletList [plain "H1a" <> 
                          bulletList [plain $ linkId "toc-h2" "#h2" "" "H2"]]
          , givesTOC "section id used as backup" $
              divWith ("sec",["section"],[]) (header 1 "H1") =?>
              bulletList [plain $ linkId "toc-sec" "#sec" "" "H1"]             
          ]

testCollapse :: [TestTree]
testCollapse = map (testCase "collapse")
 [  collapseFilePath (joinPath [ ""]) @?= joinPath [ ""]
 ,  collapseFilePath (joinPath [ ".","foo"]) @?= joinPath [ "foo"]
 ,  collapseFilePath (joinPath [ ".",".","..","foo"]) @?= joinPath [ joinPath ["..", "foo"]]
 ,  collapseFilePath (joinPath [ "..","foo"]) @?= joinPath [ "..","foo"]
 ,  collapseFilePath (joinPath [ "","bar","..","baz"]) @?= joinPath [ "","baz"]
 ,  collapseFilePath (joinPath [ "","..","baz"]) @?= joinPath [ "","..","baz"]
 ,  collapseFilePath (joinPath [ ".","foo","..",".","bar","..",".",".","baz"]) @?= joinPath [ "baz"]
 ,  collapseFilePath (joinPath [ ".",""]) @?= joinPath [ ""]
 ,  collapseFilePath (joinPath [ ".",".",""]) @?= joinPath [ ""]
 ,  collapseFilePath (joinPath [ "..",""]) @?= joinPath [ ".."]
 ,  collapseFilePath (joinPath [ "..",".",""]) @?= joinPath [ ".."]
 ,  collapseFilePath (joinPath [ ".","..",""]) @?= joinPath [ ".."]
 ,  collapseFilePath (joinPath [ "..","..",""]) @?= joinPath [ "..",".."]
 ,  collapseFilePath (joinPath [ "parent","foo","baz","..","bar"]) @?= joinPath [ "parent","foo","bar"]
 ,  collapseFilePath (joinPath [ "parent","foo","baz","..","..","bar"]) @?= joinPath [ "parent","bar"]
 ,  collapseFilePath (joinPath [ "parent","foo",".."]) @?= joinPath [ "parent"]
 ,  collapseFilePath (joinPath [ "","parent","foo","..","..","bar"]) @?= joinPath [ "","bar"]
 ,  collapseFilePath (joinPath [ "",".","parent","foo"]) @?= joinPath [ "","parent","foo"]]

testLegacyTable :: [TestTree]
testLegacyTable =
  [ testCase "decomposes a table with head" $ gen1 @?= expect1
  , testCase "decomposes a table without head" $ gen2 @?= expect2
  , testCase "decomposes the table from issue 7683" $ gen3 @?= expect3
  ]
  where
    pln = toList . plain . str
    cl a h w = Cell ("", [], []) AlignDefault h w $ pln a
    rws = map $ Row nullAttr
    th = TableHead nullAttr . rws
    tb n x y = TableBody nullAttr n (rws x) (rws y)
    tf = TableFoot nullAttr . rws

    headRows1 =
      [[cl "a" 1 1, cl "b" 2 2]
      ,[cl "c" 1 1]
      ]
    body1 = tb 1
      [[cl "e" 3 1,cl "f" 3 2]
      ,[]
      ,[]
      ]
      [[emptyCell,emptyCell,emptyCell]
      ,[cl "g" 1 1,emptyCell,emptyCell]
      ]
    footRows1 =
      [[cl "h" 1 2,cl "i" 2 1]
      ,[cl "j" 1 2]]
    caption1 = simpleCaption $ plain "caption"
    spec1 = replicate 2 (AlignDefault, ColWidth 0.5) ++ [(AlignRight, ColWidthDefault)]
    expect1 = ( [Str "caption"]
              , replicate 2 AlignDefault ++ [AlignRight]
              , replicate 2 0.5 ++ [0]
              , [pln "a", pln "b", mempty]
              , [[pln "c", mempty, mempty]
                ,[pln "e", pln "f", mempty]
                ,[mempty, mempty, mempty]
                ,[mempty, mempty, mempty]
                ,[mempty, mempty, mempty]
                ,[pln "g", mempty, mempty]
                ,[pln "h", mempty, pln "i"]
                ,[pln "j", mempty, mempty]]
              )
    gen1 = toLegacyTable caption1 spec1 (th headRows1) [body1] (tf footRows1)

    expect2 = ( []
              , replicate 2 AlignDefault ++ [AlignRight]
              , replicate 2 0.5 ++ [0]
              , []
              , [[pln "e", pln "f", mempty]
                ,[mempty, mempty, mempty]
                ,[mempty, mempty, mempty]
                ,[mempty, mempty, mempty]
                ,[pln "g", mempty, mempty]
                ,[pln "h", mempty, pln "i"]
                ,[pln "j", mempty, mempty]]
              )
    gen2 = toLegacyTable emptyCaption spec1 (th []) [body1] (tf footRows1)

    spec3 = replicate 4 (AlignDefault, ColWidthDefault)
    body3 = tb 0
      []
      [[cl "a" 2 1, cl "b" 1 2, cl "c" 2 1]
      ,[cl "d" 1 1, cl "e" 1 1]
      ]
    expect3 = ( []
              , replicate 4 AlignDefault
              , replicate 4 0
              , []
              , [[pln "a", pln "b", mempty, pln "c"]
                ,[mempty, pln "d", pln "e", mempty]]
              )
    gen3 = toLegacyTable emptyCaption spec3 (th []) [body3] (tf [])
