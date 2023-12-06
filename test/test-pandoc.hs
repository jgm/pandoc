{-# OPTIONS_GHC -Wall #-}

module Main where

import System.Environment (getArgs, getExecutablePath)
import qualified Control.Exception as E
import Text.Pandoc.App (convertWithOpts, handleOptInfo, defaultOpts, options,
                        parseOptionsFromArgs)
import Text.Pandoc.Error (handleError)
import Text.Pandoc.Scripting (noEngine)
import GHC.IO.Encoding
import Test.Tasty
import qualified Tests.Command
import qualified Tests.Old
import qualified Tests.Readers.Creole
import qualified Tests.Readers.Docx
import qualified Tests.Readers.DokuWiki
import qualified Tests.Readers.EPUB
import qualified Tests.Readers.FB2
import qualified Tests.Readers.HTML
import qualified Tests.Readers.JATS
import qualified Tests.Readers.Jira
import qualified Tests.Readers.LaTeX
import qualified Tests.Readers.Markdown
import qualified Tests.Readers.Muse
import qualified Tests.Readers.ODT
import qualified Tests.Readers.Org
import qualified Tests.Readers.RST
import qualified Tests.Readers.RTF
import qualified Tests.Readers.Txt2Tags
import qualified Tests.Readers.Man
import qualified Tests.Shared
import qualified Tests.Writers.AsciiDoc
import qualified Tests.Writers.ConTeXt
import qualified Tests.Writers.DocBook
import qualified Tests.Writers.Docx
import qualified Tests.Writers.FB2
import qualified Tests.Writers.HTML
import qualified Tests.Writers.JATS
import qualified Tests.Writers.Jira
import qualified Tests.Writers.LaTeX
import qualified Tests.Writers.Markdown
import qualified Tests.Writers.Ms
import qualified Tests.Writers.Muse
import qualified Tests.Writers.Native
import qualified Tests.Writers.Org
import qualified Tests.Writers.Plain
import qualified Tests.Writers.Powerpoint
import qualified Tests.Writers.RST
import qualified Tests.Writers.AnnotatedTable
import qualified Tests.Writers.TEI
import qualified Tests.Writers.Markua
import qualified Tests.MediaBag
import Text.Pandoc.Shared (inDirectory)

tests :: FilePath -> TestTree
tests pandocPath = testGroup "pandoc tests"
        [ Tests.Command.tests
        , testGroup "Old" (Tests.Old.tests pandocPath)
        , testGroup "Shared" Tests.Shared.tests
        , testGroup "MediaBag" Tests.MediaBag.tests
        , testGroup "Writers"
          [ testGroup "Native" Tests.Writers.Native.tests
          , testGroup "ConTeXt" Tests.Writers.ConTeXt.tests
          , testGroup "LaTeX" Tests.Writers.LaTeX.tests
          , testGroup "HTML" Tests.Writers.HTML.tests
          , testGroup "JATS" Tests.Writers.JATS.tests
          , testGroup "Jira" Tests.Writers.Jira.tests
          , testGroup "Docbook" Tests.Writers.DocBook.tests
          , testGroup "Markdown" Tests.Writers.Markdown.tests
          , testGroup "Org" Tests.Writers.Org.tests
          , testGroup "Plain" Tests.Writers.Plain.tests
          , testGroup "AsciiDoc" Tests.Writers.AsciiDoc.tests
          , testGroup "Docx" Tests.Writers.Docx.tests
          , testGroup "RST" Tests.Writers.RST.tests
          , testGroup "TEI" Tests.Writers.TEI.tests
          , testGroup "markua" Tests.Writers.Markua.tests
          , testGroup "Muse" Tests.Writers.Muse.tests
          , testGroup "FB2" Tests.Writers.FB2.tests
          , testGroup "PowerPoint" Tests.Writers.Powerpoint.tests
          , testGroup "Ms" Tests.Writers.Ms.tests
          , testGroup "AnnotatedTable" Tests.Writers.AnnotatedTable.tests
          ]
        , testGroup "Readers"
          [ testGroup "LaTeX" Tests.Readers.LaTeX.tests
          , testGroup "Markdown" Tests.Readers.Markdown.tests
          , testGroup "HTML" Tests.Readers.HTML.tests
          , testGroup "JATS" Tests.Readers.JATS.tests
          , testGroup "Jira" Tests.Readers.Jira.tests
          , testGroup "Org" Tests.Readers.Org.tests
          , testGroup "RST" Tests.Readers.RST.tests
          , testGroup "RTF" Tests.Readers.RTF.tests
          , testGroup "Docx" Tests.Readers.Docx.tests
          , testGroup "ODT" Tests.Readers.ODT.tests
          , testGroup "Txt2Tags" Tests.Readers.Txt2Tags.tests
          , testGroup "EPUB" Tests.Readers.EPUB.tests
          , testGroup "Muse" Tests.Readers.Muse.tests
          , testGroup "Creole" Tests.Readers.Creole.tests
          , testGroup "Man" Tests.Readers.Man.tests
          , testGroup "FB2" Tests.Readers.FB2.tests
          , testGroup "DokuWiki" Tests.Readers.DokuWiki.tests
          ]
        ]

main :: IO ()
main = do
  setLocaleEncoding utf8
  args <- getArgs
  case args of
    "--emulate":args' -> -- emulate pandoc executable
          E.catch
            (do
              res <- parseOptionsFromArgs options defaultOpts "pandoc" args'
              case res of
                Left e -> handleOptInfo noEngine e
                Right opts -> convertWithOpts noEngine opts)
            (handleError . Left)
    _ -> inDirectory "test" $ do
           fp <- getExecutablePath
           -- putStrLn $ "Using pandoc executable at " ++ fp
           defaultMain $ tests fp
