module Tests.Shared (tests) where

import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Test.Framework
import Tests.Helpers
import Tests.Arbitrary()
import Test.Framework.Providers.HUnit
import Test.HUnit ( assertBool, (@?=) )
import Text.Pandoc.Builder
import Data.Monoid
import System.FilePath (joinPath)

tests :: [Test]
tests = [ testGroup "normalize"
          [ property "p_normalize_blocks_rt" p_normalize_blocks_rt
          , property "p_normalize_inlines_rt" p_normalize_inlines_rt
          , property "p_normalize_no_trailing_spaces"
              p_normalize_no_trailing_spaces
          ]
        , testGroup "compactify'DL"
          [ testCase "compactify'DL with empty def" $
              assertBool "compactify'DL"
              (let x = [(str "word", [para (str "def"), mempty])]
               in  compactify'DL x == x)
          ]
        , testGroup "collapseFilePath" testCollapse
        ]

p_normalize_blocks_rt :: [Block] -> Bool
p_normalize_blocks_rt bs =
  normalizeBlocks bs == normalizeBlocks (normalizeBlocks bs)

p_normalize_inlines_rt :: [Inline] -> Bool
p_normalize_inlines_rt ils =
  normalizeInlines ils == normalizeInlines (normalizeInlines ils)

p_normalize_no_trailing_spaces :: [Inline] -> Bool
p_normalize_no_trailing_spaces ils = null ils' || last ils' /= Space
  where ils' = normalizeInlines $ ils ++ [Space]

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
