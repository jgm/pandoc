module Tests.Writers.Native (tests) where

import Test.Framework
import Text.Pandoc.Builder
import Text.Pandoc
import Tests.Helpers
import Text.Pandoc.Arbitrary()

p_write_rt :: Pandoc -> Bool
p_write_rt d =
  read (purely (writeNative def{ writerTemplate = Just "" }) d) == d

p_write_blocks_rt :: [Block] -> Bool
p_write_blocks_rt bs = length bs > 20 ||
  read (purely (writeNative def) (Pandoc nullMeta bs)) ==
  bs

tests :: [Test]
tests = [ property "p_write_rt" p_write_rt
        , property "p_write_blocks_rt" p_write_blocks_rt
        ]
