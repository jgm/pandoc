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
 [  (collapseFilePath "" @?= "")
 ,  (collapseFilePath "./foo" @?= "foo")
 ,  (collapseFilePath "././../foo" @?= "../foo")
 ,  (collapseFilePath "../foo" @?= "../foo")
 ,  (collapseFilePath "/bar/../baz" @?= "/baz")
 ,  (collapseFilePath "/../baz" @?= "/../baz")
 ,  (collapseFilePath  "./foo/.././bar/../././baz" @?= "baz")
 ,  (collapseFilePath "./" @?=  "")
 ,  (collapseFilePath "././" @?=  "")
 ,  (collapseFilePath "../" @?=  "..")
 ,  (collapseFilePath ".././" @?=  "..")
 ,  (collapseFilePath "./../" @?=  "..")
 ,  (collapseFilePath "../../" @?=  "../..")
 ,  (collapseFilePath "parent/foo/baz/../bar" @?=  "parent/foo/bar")
 ,  (collapseFilePath "parent/foo/baz/../../bar" @?=  "parent/bar")
 ,  (collapseFilePath "parent/foo/.." @?=  "parent")
 ,  (collapseFilePath "/parent/foo/../../bar" @?=  "/bar")
 ,  (collapseFilePath "/./parent/foo" @?=  "/parent/foo")]
