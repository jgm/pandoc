{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Writers.Tables
   Copyright   : 2020 Christian Despres
   License     : GNU GPL, version 2 or above

   Maintainer  : Christian Despres <christian.j.j.despres@gmail.com>
   Stability   : alpha
   Portability : portable

Tests for the table helper functions.
-}
module Tests.Writers.Tables
  ( tests
  )
where

import           Prelude
import qualified Data.Foldable                 as F
import qualified Data.List.NonEmpty            as NonEmpty
import           Test.Tasty
import           Test.Tasty.HUnit               ( testCase
                                                , (@?=)
                                                )
import           Test.Tasty.QuickCheck          ( QuickCheckTests(..)
                                                , Property
                                                , Testable
                                                , conjoin
                                                , forAll
                                                , testProperty
                                                , (===)
                                                , vectorOf
                                                , choose
                                                , arbitrary
                                                , elements
                                                )
import           Text.Pandoc.Arbitrary          ( )
import           Text.Pandoc.Builder
import           Text.Pandoc.Writers.Tables

tests :: [TestTree]
tests = [testGroup "toAnnTable" $ testAnnTable <> annTableProps]

getSpec :: AnnCell -> [ColSpec]
getSpec (AnnCell colspec _ _) = F.toList colspec

catHeaderSpec :: AnnHeaderRow -> [ColSpec]
catHeaderSpec (AnnHeaderRow _ _ x) = concatMap getSpec x

catBodySpec :: AnnBodyRow -> [ColSpec]
catBodySpec (AnnBodyRow _ _ x y) = concatMap getSpec x <> concatMap getSpec y

-- Test if the first list can be obtained from the second by deleting
-- elements from it.
isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf (x : xs) (y : ys) | x == y    = isSubsetOf xs ys
                             | otherwise = isSubsetOf (x : xs) ys
isSubsetOf [] _  = True
isSubsetOf _  [] = False

testAnnTable :: [TestTree]
testAnnTable =
  [testCase "annotates a sample table properly" $ generated @?= expected]
 where
  spec1 = (AlignRight, ColWidthDefault)
  spec2 = (AlignLeft, ColWidthDefault)
  spec3 = (AlignCenter, ColWidthDefault)
  spec  = [spec1, spec2, spec3]

  cl a h w = Cell (a, [], []) AlignDefault h w []
  rws = map $ Row nullAttr
  th  = TableHead nullAttr . rws
  tb n x y = TableBody nullAttr n (rws x) (rws y)
  tf           = TableFoot nullAttr . rws
  initialHeads = [[cl "a" 1 1, cl "b" 3 2], [cl "c" 2 2, cl "d" 1 1]]
  initialTB1   = tb 1
                    [[], [cl "e" 5 1, cl "f" (-7) 0]]
                    [[cl "g" 4 3, cl "h" 4 3], [], [emptyCell]]
  initialTB2 = tb 2 [] [[cl "i" 4 3, cl "j" 4 3]]
  generated  = toAnnTable nullAttr
                          emptyCaption
                          spec
                          (th initialHeads)
                          [initialTB1, initialTB2]
                          (tf initialHeads)

  acl al n a h w =
    AnnCell (NonEmpty.fromList al) n $ Cell (a, [], []) AlignDefault h w []
  emptyAnnCell al n = acl al n "" 1 1
  ahrw    = AnnHeaderRow nullAttr
  abrw    = AnnBodyRow nullAttr
  ath     = AnnTableHead nullAttr
  atb     = AnnTableBody nullAttr
  atf     = AnnTableFoot nullAttr

  finalTH = ath
    [ ahrw 0 [acl [spec1] 0 "a" 1 1, acl [spec2, spec3] 1 "b" 2 2]
    , ahrw 1 [acl [spec1] 0 "c" 1 1]
    ]
  finalTB1 = atb
    1
    [ ahrw
      2
      [emptyAnnCell [spec1] 0, emptyAnnCell [spec2] 1, emptyAnnCell [spec3] 2]
    , ahrw
      3
      [acl [spec1] 0 "e" 1 1, acl [spec2] 1 "f" 1 1, emptyAnnCell [spec3] 2]
    ]
    [ abrw 4 [acl [spec1] 0 "g" 3 1] [acl [spec2, spec3] 1 "h" 3 2]
    , abrw 5 []                      []
    , abrw 6 []                      []
    ]
  finalTB2 =
    atb 2 [] [abrw 7 [acl [spec1, spec2] 0 "i" 1 2] [acl [spec3] 2 "j" 1 1]]
  finalTF = atf
    [ ahrw 8 [acl [spec1] 0 "a" 1 1, acl [spec2, spec3] 1 "b" 2 2]
    , ahrw 9 [acl [spec1] 0 "c" 1 1]
    ]
  expected =
    AnnTable nullAttr emptyCaption spec finalTH [finalTB1, finalTB2] finalTF

withColSpec :: Testable prop => ([ColSpec] -> prop) -> Property
withColSpec = forAll arbColSpec
 where
  arbColSpec = do
    cs <- choose (1 :: Int, 6)
    vectorOf
      cs
      ((,) <$> arbitrary <*> elements
        [ColWidthDefault, ColWidth (1 / 3), ColWidth 0.25]
      )

annTableProps :: [TestTree]
annTableProps =
  localOption (QuickCheckTests 50)
    <$> [ testProperty "normalizes like the table builder" propBuilderAnnTable
        , testProperty "has valid final cell columns"      propColNumber
        , testProperty "has valid first row column data"   propFirstRowCols
        , testProperty "has valid all row column data"     propColSubsets
        , testProperty "has valid cell column data lengths"
                       propCellColLengths
        ]

-- The property that toAnnTable will normalize a table identically to
-- the table builder. This should mean that toAnnTable is at least as
-- rigorous as Builder.table in that respect without repeating those
-- tests here (see the pandoc-types Table tests for examples).
propBuilderAnnTable :: TableHead -> [TableBody] -> TableFoot -> Property
propBuilderAnnTable th tbs tf = withColSpec $ \cs ->
  convertTable (table emptyCaption cs th tbs tf)
    === convertAnnTable (toAnnTable nullAttr emptyCaption cs th tbs tf)
 where
  convertTable blks = case toList blks of
    [Table _ _ colspec a b c] -> Right (colspec, a, b, c)
    x                         -> Left x
  convertAnnTable x = case fromAnnTable x of
    (_, _, colspec, a, b, c) -> Right (colspec, a, b, c)

-- The property of toAnnTable that if the last cell in the first row
-- of a table section has ColSpan w and ColNumber n, then w + n is the
-- width of the table.
propColNumber :: TableHead -> [TableBody] -> TableFoot -> Property
propColNumber th tbs tf = withColSpec $ \cs ->
  let twidth = length cs
      AnnTable _ _ _ ath atbs atf =
          toAnnTable nullAttr emptyCaption cs th tbs tf
  in  conjoin
        $  [colNumTH twidth ath]
        <> (colNumTB twidth <$> atbs)
        <> [colNumTF twidth atf]
 where
  colNumTH n (AnnTableHead _ rs) = firstly (isHeaderValid n) rs
  colNumTB n (AnnTableBody _ _ rs ts) =
    firstly (isHeaderValid n) rs && firstly (isBodyValid n) ts
  colNumTF n (AnnTableFoot _ rs) = firstly (isHeaderValid n) rs

  isHeaderValid n (AnnHeaderRow _ _ x) = isSegmentValid n x
  isBodyValid n (AnnBodyRow _ _ _ x) = isSegmentValid n x

  firstly f (x : _) = f x
  firstly _ []      = True
  lastly f [x     ] = f x
  lastly f (_ : xs) = lastly f xs
  lastly _ []       = True

  isSegmentValid twidth cs = flip lastly cs
    $ \(AnnCell _ (ColNumber n) (Cell _ _ _ (ColSpan w) _)) -> n + w == twidth

-- The property of an AnnTable from toAnnTable that if the NonEmpty
-- ColSpec data of the cells in the first row of a table section are
-- concatenated, the result should equal the [ColSpec] of the entire
-- table.
propFirstRowCols :: TableHead -> [TableBody] -> TableFoot -> Property
propFirstRowCols th tbs tf = withColSpec $ \cs ->
  let AnnTable _ _ _ ath atbs atf =
          toAnnTable nullAttr emptyCaption cs th tbs tf
  in  conjoin
        $  [firstRowTH cs ath]
        <> (firstRowTB cs <$> atbs)
        <> [firstRowTF cs atf]
 where
  firstly f (x : _) = f x
  firstly _ []      = True

  firstHeaderValid cs = firstly $ \r -> cs == catHeaderSpec r
  firstBodyValid cs = firstly $ \r -> cs == catBodySpec r

  firstRowTH cs (AnnTableHead _ rs) = firstHeaderValid cs rs
  firstRowTB cs (AnnTableBody _ _ rs ts) =
    firstHeaderValid cs rs && firstBodyValid cs ts
  firstRowTF cs (AnnTableFoot _ rs) = firstHeaderValid cs rs

-- The property that in any row in an AnnTable from toAnnTable, the
-- NonEmpty ColSpec annotations on cells, when concatenated, form a
-- subset (really sublist) of the [ColSpec] of the entire table.
propColSubsets :: TableHead -> [TableBody] -> TableFoot -> Property
propColSubsets th tbs tf = withColSpec $ \cs ->
  let AnnTable _ _ _ ath atbs atf =
          toAnnTable nullAttr emptyCaption cs th tbs tf
  in  conjoin
        $  subsetTH cs ath
        <> concatMap (subsetTB cs) atbs
        <> subsetTF cs atf
 where
  subsetTH cs (AnnTableHead _ rs) = map (subsetHeader cs) rs
  subsetTB cs (AnnTableBody _ _ rs ts) =
    map (subsetHeader cs) rs <> map (subsetBody cs) ts
  subsetTF cs (AnnTableFoot _ rs) = map (subsetHeader cs) rs

  subsetHeader cs r = catHeaderSpec r `isSubsetOf` cs
  subsetBody cs r = catBodySpec r `isSubsetOf` cs

-- The property that in any cell in an AnnTable from toAnnTable, the
-- NonEmpty ColSpec annotation on a cell is equal in length to its
-- ColSpan.
propCellColLengths :: TableHead -> [TableBody] -> TableFoot -> Property
propCellColLengths th tbs tf = withColSpec $ \cs ->
  let AnnTable _ _ _ ath atbs atf =
          toAnnTable nullAttr emptyCaption cs th tbs tf
  in  conjoin $ cellColTH ath <> concatMap cellColTB atbs <> cellColTF atf
 where
  cellColTH (AnnTableHead _ rs) = concatMap cellColHeader rs
  cellColTB (AnnTableBody _ _ rs ts) =
    concatMap cellColHeader rs <> concatMap cellColBody ts
  cellColTF (AnnTableFoot _ rs) = concatMap cellColHeader rs

  cellColHeader (AnnHeaderRow _ _ x) = fmap validLength x
  cellColBody (AnnBodyRow _ _ x y) = fmap validLength x <> fmap validLength y

  validLength (AnnCell colspec _ (Cell _ _ _ (ColSpan w) _)) =
    length colspec == w
