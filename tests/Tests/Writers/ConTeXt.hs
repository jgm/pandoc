{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.ConTeXt (tests) where

import Test.Framework
import Text.Pandoc.Builder
import Text.Pandoc
import Text.Pandoc.Shared (removeTrailingSpace)
import Tests.Helpers

inlines :: Inlines -> String
inlines = removeTrailingSpace .
          writeConTeXt defaultWriterOptions . doc . plain

blocks :: Blocks -> String
blocks =  writeConTeXt defaultWriterOptions . doc

tests :: [Test]
tests = [ testGroup "inline code"
          [ "with '}'" =:
            inlines (code "}") --> "\\mono{\\letterclosebrace{}}"
          , "without '}'" =:
            inlines (code "]") --> "\\type{]}"
          ]
        , testGroup "headers"
          [ "level 1" =:
            blocks (header 1 "My header") --> "\\subject{My header}"
          ]
        ]

