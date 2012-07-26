module Tests.Shared (tests) where

import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Test.Framework
import Tests.Helpers
import Tests.Arbitrary()

tests :: [Test]
tests = [ testGroup "normalize"
          [ property "p_normalize_blocks_rt" p_normalize_blocks_rt
          , property "p_normalize_inlines_rt" p_normalize_inlines_rt
          , property "p_normalize_no_trailing_spaces"
              p_normalize_no_trailing_spaces
          ]
        ]

p_normalize_blocks_rt :: [Block] -> Bool
p_normalize_blocks_rt bs = normalize bs == normalize (normalize bs)

p_normalize_inlines_rt :: [Inline] -> Bool
p_normalize_inlines_rt ils = normalize ils == normalize (normalize ils)

p_normalize_no_trailing_spaces :: [Inline] -> Bool
p_normalize_no_trailing_spaces ils = null ils' || last ils' /= Space
  where ils' = normalize $ ils ++ [Space]
