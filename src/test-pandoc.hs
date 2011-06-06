{-# OPTIONS_GHC -Wall #-}

module Main where

import Test.Framework

import qualified Tests.Old
import qualified Tests.Readers.LaTeX
import qualified Tests.Readers.Markdown
import qualified Tests.Readers.RST
import qualified Tests.Writers.ConTeXt
import qualified Tests.Writers.HTML
import qualified Tests.Writers.Native
import qualified Tests.Writers.Markdown
import qualified Tests.Shared

tests :: [Test]
tests = [ testGroup "Old" Tests.Old.tests
        , testGroup "Shared" Tests.Shared.tests
        , testGroup "Writers"
          [ testGroup "Native" Tests.Writers.Native.tests
          , testGroup "ConTeXt" Tests.Writers.ConTeXt.tests
          , testGroup "HTML" Tests.Writers.HTML.tests
          , testGroup "Markdown" Tests.Writers.Markdown.tests
          ]
        , testGroup "Readers"
          [ testGroup "LaTeX" Tests.Readers.LaTeX.tests
          , testGroup "Markdown" Tests.Readers.Markdown.tests
          , testGroup "RST" Tests.Readers.RST.tests
          ]
        ]

main :: IO ()
main = defaultMain tests
