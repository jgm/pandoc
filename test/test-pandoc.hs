{-# OPTIONS_GHC -Wall #-}

module Main where

import GHC.IO.Encoding
import Test.Tasty
import qualified Tests.Command
import qualified Tests.Lua
import qualified Tests.Old
import qualified Tests.Readers.Docx
import qualified Tests.Readers.EPUB
import qualified Tests.Readers.HTML
import qualified Tests.Readers.LaTeX
import qualified Tests.Readers.Markdown
import qualified Tests.Readers.Odt
import qualified Tests.Readers.Org
import qualified Tests.Readers.RST
import qualified Tests.Readers.Txt2Tags
import qualified Tests.Readers.Muse
import qualified Tests.Shared
import qualified Tests.Writers.AsciiDoc
import qualified Tests.Writers.ConTeXt
import qualified Tests.Writers.Docbook
import qualified Tests.Writers.Docx
import qualified Tests.Writers.HTML
import qualified Tests.Writers.LaTeX
import qualified Tests.Writers.Markdown
import qualified Tests.Writers.Native
import qualified Tests.Writers.Org
import qualified Tests.Writers.Plain
import qualified Tests.Writers.RST
import qualified Tests.Writers.TEI
import qualified Tests.Writers.Muse
import Text.Pandoc.Shared (inDirectory)

tests :: TestTree
tests = testGroup "pandoc tests" [ Tests.Command.tests
        , testGroup "Old" Tests.Old.tests
        , testGroup "Shared" Tests.Shared.tests
        , testGroup "Writers"
          [ testGroup "Native" Tests.Writers.Native.tests
          , testGroup "ConTeXt" Tests.Writers.ConTeXt.tests
          , testGroup "LaTeX" Tests.Writers.LaTeX.tests
          , testGroup "HTML" Tests.Writers.HTML.tests
          , testGroup "Docbook" Tests.Writers.Docbook.tests
          , testGroup "Markdown" Tests.Writers.Markdown.tests
          , testGroup "Org" Tests.Writers.Org.tests
          , testGroup "Plain" Tests.Writers.Plain.tests
          , testGroup "AsciiDoc" Tests.Writers.AsciiDoc.tests
          , testGroup "Docx" Tests.Writers.Docx.tests
          , testGroup "RST" Tests.Writers.RST.tests
          , testGroup "TEI" Tests.Writers.TEI.tests
          , testGroup "Muse" Tests.Writers.Muse.tests
          ]
        , testGroup "Readers"
          [ testGroup "LaTeX" Tests.Readers.LaTeX.tests
          , testGroup "Markdown" Tests.Readers.Markdown.tests
          , testGroup "HTML" Tests.Readers.HTML.tests
          , testGroup "Org" Tests.Readers.Org.tests
          , testGroup "RST" Tests.Readers.RST.tests
          , testGroup "Docx" Tests.Readers.Docx.tests
          , testGroup "Odt" Tests.Readers.Odt.tests
          , testGroup "Txt2Tags" Tests.Readers.Txt2Tags.tests
          , testGroup "EPUB" Tests.Readers.EPUB.tests
          , testGroup "Muse" Tests.Readers.Muse.tests
          ]
        , testGroup "Lua filters" Tests.Lua.tests
        ]

main :: IO ()
main = do
  setLocaleEncoding utf8
  inDirectory "test" $ defaultMain tests
