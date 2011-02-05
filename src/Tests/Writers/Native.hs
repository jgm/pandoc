module Tests.Writers.Native (tests) where

import Test.Framework
import Text.Pandoc.Builder
import Text.Pandoc
import Tests.Helpers
import Tests.Arbitrary()

p_write_rt :: Pandoc -> Bool
p_write_rt d =
  read (writeNative defaultWriterOptions{ writerStandalone = True } d) == d

p_write_blocks_rt :: [Block] -> Bool
p_write_blocks_rt bs = length bs > 20 ||
  read (writeNative defaultWriterOptions (Pandoc (Meta [] [] []) bs)) ==
  bs

tests :: [Test]
tests = [ property "p_write_rt" p_write_rt
        , property "p_write_blocks_rt" p_write_blocks_rt
        ]
