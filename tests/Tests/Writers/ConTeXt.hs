{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.ConTeXt (tests) where

import Test.Framework
import Text.Pandoc.Builder
import Text.Pandoc
import Text.Pandoc.Shared (removeTrailingSpace)
import Tests.Helpers

inlines :: Inlines -> (Inlines, String)
inlines ils = (ils, removeTrailingSpace .
                    writeConTeXt defaultWriterOptions . doc . plain $ ils)

blocks :: Blocks -> (Blocks, String)
blocks bls =  (bls, writeConTeXt defaultWriterOptions . doc $ bls)

tests :: [Test]
tests = [ testGroup "inline code"
          [ "with '}'" =:
            inlines (code "}") --> "\\mono{\\letterclosebrace{x}}"
          , "without '}'" =:
            inlines (code "]") --> "\\type{]}"
          ]
        , testGroup "headers"
          [ "level 1" =:
            blocks (header 1 "My header") --> "\\subject{My header}"
          ]
        ]

