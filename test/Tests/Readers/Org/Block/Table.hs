{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Tests.Readers.Org.Block.Table
   Copyright   : © 2014-2019 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
   Stability   : alpha
   Portability : portable

Test parsing of org tables.
-}
module Tests.Readers.Org.Block.Table (tests) where

import Prelude
import Test.Tasty (TestTree)
import Tests.Helpers ((=?>))
import Tests.Readers.Org.Shared ((=:), spcSep)
import Text.Pandoc.Builder
import qualified Data.Text as T

simpleTable' :: Int
             -> [Blocks]
             -> [[Blocks]]
             -> Blocks
simpleTable' n = table "" (replicate n (AlignDefault, 0.0))

tests :: [TestTree]
tests =
  [ "Single cell table" =:
      "|Test|" =?>
      simpleTable' 1 mempty [[plain "Test"]]

  , "Multi cell table" =:
      "| One | Two |" =?>
       simpleTable' 2 mempty [ [ plain "One", plain "Two" ] ]

  , "Multi line table" =:
      T.unlines [ "| One   |"
                , "| Two   |"
                , "| Three |"
                ] =?>
       simpleTable' 1 mempty
                    [ [ plain "One" ]
                    , [ plain "Two" ]
                    , [ plain "Three" ]
                    ]

  , "Empty table" =:
      "||" =?>
      simpleTable' 1 mempty [[mempty]]

  , "Glider Table" =:
      T.unlines [ "| 1 | 0 | 0 |"
                , "| 0 | 1 | 1 |"
                , "| 1 | 1 | 0 |"
                ] =?>
      simpleTable' 3 mempty
                   [ [ plain "1", plain "0", plain "0" ]
                   , [ plain "0", plain "1", plain "1" ]
                   , [ plain "1", plain "1", plain "0" ]
                   ]

  , "Table between Paragraphs" =:
      T.unlines [ "Before"
                , "| One | Two |"
                , "After"
                ] =?>
      mconcat [ para "Before"
              , simpleTable' 2 mempty [ [ plain "One", plain "Two" ] ]
              , para "After"
              ]

  , "Table with Header" =:
      T.unlines [ "| Species      | Status       |"
                , "|--------------+--------------|"
                , "| cervisiae    | domesticated |"
                , "| paradoxus    | wild         |"
                ] =?>
      simpleTable [ plain "Species", plain "Status" ]
                  [ [ plain "cervisiae", plain "domesticated" ]
                  , [ plain "paradoxus", plain "wild" ]
                  ]

  , "Table with final hline" =:
      T.unlines [ "| cervisiae    | domesticated |"
                , "| paradoxus    | wild         |"
                , "|--------------+--------------|"
                ] =?>
      simpleTable' 2 mempty
            [ [ plain "cervisiae", plain "domesticated" ]
             , [ plain "paradoxus", plain "wild" ]
            ]

  , "Table in a box" =:
      T.unlines [ "|---------|---------|"
                , "| static  | Haskell |"
                , "| dynamic | Lisp    |"
                , "|---------+---------|"
                ] =?>
      simpleTable' 2 mempty
            [ [ plain "static", plain "Haskell" ]
            , [ plain "dynamic", plain "Lisp" ]
            ]

  , "Table with empty cells" =:
      "|||c|" =?>
      simpleTable' 3 mempty [[mempty, mempty, plain "c"]]

  , "Table with empty rows" =:
      T.unlines [ "| first  |"
                , "|        |"
                , "| third  |"
                ] =?>
      simpleTable' 1 mempty [[plain "first"], [mempty], [plain "third"]]

  , "Table with alignment row" =:
      T.unlines [ "| Numbers | Text | More |"
                , "| <c>     | <r>  |      |"
                , "| 1       | One  | foo  |"
                , "| 2       | Two  | bar  |"
                ] =?>
      table "" (zip [AlignCenter, AlignRight, AlignDefault] [0, 0, 0])
            []
            [ [ plain "Numbers", plain "Text", plain "More" ]
            , [ plain "1"      , plain "One" , plain "foo"  ]
            , [ plain "2"      , plain "Two" , plain "bar"  ]
            ]

  , "Pipe within text doesn't start a table" =:
      "Ceci n'est pas une | pipe " =?>
      para (spcSep [ "Ceci", "n'est", "pas", "une", "|", "pipe" ])

  , "Missing pipe at end of row" =:
      "|incomplete-but-valid" =?>
      simpleTable' 1 mempty [ [ plain "incomplete-but-valid" ] ]

  , "Table with differing row lengths" =:
      T.unlines [ "| Numbers | Text "
                , "|-"
                , "| <c>     | <r>  |"
                , "| 1       | One  | foo  |"
                , "| 2"
                ] =?>
      table "" (zip [AlignCenter, AlignRight] [0, 0])
            [ plain "Numbers", plain "Text" ]
            [ [ plain "1" , plain "One" , plain "foo" ]
            , [ plain "2" ]
            ]

  , "Table with caption" =:
      T.unlines [ "#+CAPTION: Hitchhiker's Multiplication Table"
                , "| x |  6 |"
                , "| 9 | 42 |"
                ] =?>
      table "Hitchhiker's Multiplication Table"
            [(AlignDefault, 0), (AlignDefault, 0)]
            []
            [ [ plain "x", plain "6" ]
            , [ plain "9", plain "42" ]
            ]
  ]
