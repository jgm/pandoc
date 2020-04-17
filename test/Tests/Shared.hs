{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Shared
   Copyright   : © 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Tests for functions used in many parts of the library.
-}
module Tests.Shared (tests) where

import Prelude
import System.FilePath.Posix (joinPath)
import Test.Tasty
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder
import Text.Pandoc.Shared
import Text.Pandoc.Writers.Shared (toLegacyTable)

tests :: [TestTree]
tests = [ testGroup "compactifyDL"
          [ testCase "compactifyDL with empty def" $
              assertBool "compactifyDL"
              (let x = [(str "word", [para (str "def"), mempty])]
               in  compactifyDL x == x)
          ]
        , testGroup "collapseFilePath" testCollapse
        , testGroup "toLegacyTable" testLegacyTable
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

testLegacyTable :: [TestTree]
testLegacyTable =
  [ testCase "decomposes a table with head" $ gen1 @?= expect1
  , testCase "decomposes a table without head" $ gen2 @?= expect2
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
