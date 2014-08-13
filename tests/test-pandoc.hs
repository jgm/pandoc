{-# OPTIONS_GHC -Wall #-}

module Main where

import Test.Framework
import GHC.IO.Encoding
import qualified Tests.Old
import qualified Tests.Readers.LaTeX
import qualified Tests.Readers.Markdown
import qualified Tests.Readers.Org
import qualified Tests.Readers.RST
import qualified Tests.Readers.Docx
import qualified Tests.Readers.Txt2Tags
import qualified Tests.Readers.EPUB
import qualified Tests.Writers.ConTeXt
import qualified Tests.Writers.LaTeX
import qualified Tests.Writers.HTML
import qualified Tests.Writers.Docbook
import qualified Tests.Writers.Native
import qualified Tests.Writers.Markdown
import qualified Tests.Writers.Plain
import qualified Tests.Writers.AsciiDoc
import qualified Tests.Shared
import qualified Tests.Walk
import Text.Pandoc.Shared (inDirectory)
import System.Environment (getArgs)

tests :: [Test]
tests = [ testGroup "Old" Tests.Old.tests
        , testGroup "Shared" Tests.Shared.tests
        , testGroup "Walk" Tests.Walk.tests
        , testGroup "Writers"
          [ testGroup "Native" Tests.Writers.Native.tests
          , testGroup "ConTeXt" Tests.Writers.ConTeXt.tests
          , testGroup "LaTeX" Tests.Writers.LaTeX.tests
          , testGroup "HTML" Tests.Writers.HTML.tests
          , testGroup "Docbook" Tests.Writers.Docbook.tests
          , testGroup "Markdown" Tests.Writers.Markdown.tests
          , testGroup "Plain" Tests.Writers.Plain.tests
          , testGroup "AsciiDoc" Tests.Writers.AsciiDoc.tests
          ]
        , testGroup "Readers"
          [ testGroup "LaTeX" Tests.Readers.LaTeX.tests
          , testGroup "Markdown" Tests.Readers.Markdown.tests
          , testGroup "Org" Tests.Readers.Org.tests
          , testGroup "RST" Tests.Readers.RST.tests
          , testGroup "Docx" Tests.Readers.Docx.tests
          , testGroup "Txt2Tags" Tests.Readers.Txt2Tags.tests
          , testGroup "EPUB" Tests.Readers.EPUB.tests
          ]
        ]

main :: IO ()
main = do
  setLocaleEncoding utf8
  args <- getArgs
  inDirectory "tests" $ defaultMainWithArgs tests args
