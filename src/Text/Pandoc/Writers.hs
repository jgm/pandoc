{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{- |
   Module      : Text.Pandoc
   Copyright   : Copyright (C) 2006-2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

This helper module exports all writers functions.
-}
module Text.Pandoc.Writers
  (
    -- * Writers: converting /from/ Pandoc format
      Writer(..)
    , writers
    , writeANSI
    , writeAsciiDoc
    , writeAsciiDocLegacy
    , writeAsciiDoctor
    , writeBeamer
    , writeBibTeX
    , writeBibLaTeX
    , writeChunkedHTML
    , writeCommonMark
    , writeConTeXt
    , writeCslJson
    , writeDZSlides
    , writeDjot
    , writeDocBook4
    , writeDocBook5
    , writeDocx
    , writeDokuWiki
    , writeEPUB2
    , writeEPUB3
    , writeFB2
    , writeIpynb
    , writeHaddock
    , writeHtml4
    , writeHtml4String
    , writeHtml5
    , writeHtml5String
    , writeICML
    , writeJATS
    , writeJatsArchiving
    , writeJatsArticleAuthoring
    , writeJatsPublishing
    , writeJSON
    , writeJira
    , writeLaTeX
    , writeMan
    , writeMarkdown
    , writeMarkua
    , writeMediaWiki
    , writeMs
    , writeMuse
    , writeNative
    , writeODT
    , writeOPML
    , writeOpenDocument
    , writeOrg
    , writePlain
    , writePowerpoint
    , writeRST
    , writeRTF
    , writeRevealJs
    , writeS5
    , writeSILE
    , writeSlideous
    , writeSlidy
    , writeTEI
    , writeTexinfo
    , writeTextile
    , writeTypst
    , writeXWiki
    , writeZimWiki
    , getWriter
    ) where

import Control.Monad.Except (throwError)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Text.Pandoc.Class
import Text.Pandoc.Definition
import qualified Text.Pandoc.Format as Format
import Text.Pandoc.Options
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Error
import Text.Pandoc.Writers.ANSI
import Text.Pandoc.Writers.AsciiDoc
import Text.Pandoc.Writers.BibTeX
import Text.Pandoc.Writers.ChunkedHTML
import Text.Pandoc.Writers.CommonMark
import Text.Pandoc.Writers.ConTeXt
import Text.Pandoc.Writers.CslJson
import Text.Pandoc.Writers.Djot
import Text.Pandoc.Writers.DocBook
import Text.Pandoc.Writers.Docx
import Text.Pandoc.Writers.DokuWiki
import Text.Pandoc.Writers.EPUB
import Text.Pandoc.Writers.FB2
import Text.Pandoc.Writers.Ipynb
import Text.Pandoc.Writers.Haddock
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Writers.ICML
import Text.Pandoc.Writers.JATS
import Text.Pandoc.Writers.Jira
import Text.Pandoc.Writers.LaTeX
import Text.Pandoc.Writers.Man
import Text.Pandoc.Writers.Markdown
import Text.Pandoc.Writers.MediaWiki
import Text.Pandoc.Writers.Ms
import Text.Pandoc.Writers.Muse
import Text.Pandoc.Writers.Native
import Text.Pandoc.Writers.ODT
import Text.Pandoc.Writers.OpenDocument
import Text.Pandoc.Writers.OPML
import Text.Pandoc.Writers.Org
import Text.Pandoc.Writers.Powerpoint
import Text.Pandoc.Writers.RST
import Text.Pandoc.Writers.RTF
import Text.Pandoc.Writers.SILE
import Text.Pandoc.Writers.TEI
import Text.Pandoc.Writers.Texinfo
import Text.Pandoc.Writers.Textile
import Text.Pandoc.Writers.Typst
import Text.Pandoc.Writers.XWiki
import Text.Pandoc.Writers.ZimWiki

data Writer m = TextWriter (WriterOptions -> Pandoc -> m Text)
              | ByteStringWriter (WriterOptions -> Pandoc -> m BL.ByteString)

-- | Association list of formats and writers.
writers :: PandocMonad m => [ (Text, Writer m) ]
writers = [
   ("native"       , TextWriter writeNative)
  ,("json"         , TextWriter writeJSON)
  ,("docx"         , ByteStringWriter writeDocx)
  ,("odt"          , ByteStringWriter writeODT)
  ,("pptx"         , ByteStringWriter writePowerpoint)
  ,("epub"         , ByteStringWriter writeEPUB3)
  ,("epub2"        , ByteStringWriter writeEPUB2)
  ,("epub3"        , ByteStringWriter writeEPUB3)
  ,("fb2"          , TextWriter writeFB2)
  ,("ipynb"        , TextWriter writeIpynb)
  ,("html"         , TextWriter writeHtml5String)
  ,("html4"        , TextWriter writeHtml4String)
  ,("html5"        , TextWriter writeHtml5String)
  ,("icml"         , TextWriter writeICML)
  ,("s5"           , TextWriter writeS5)
  ,("slidy"        , TextWriter writeSlidy)
  ,("slideous"     , TextWriter writeSlideous)
  ,("dzslides"     , TextWriter writeDZSlides)
  ,("revealjs"     , TextWriter writeRevealJs)
  ,("docbook"      , TextWriter writeDocBook5)
  ,("docbook4"     , TextWriter writeDocBook4)
  ,("docbook5"     , TextWriter writeDocBook5)
  ,("jats"         , TextWriter writeJatsArchiving)
  ,("jats_articleauthoring", TextWriter writeJatsArticleAuthoring)
  ,("jats_publishing" , TextWriter writeJatsPublishing)
  ,("jats_archiving" , TextWriter writeJatsArchiving)
  ,("jira"         , TextWriter writeJira)
  ,("opml"         , TextWriter writeOPML)
  ,("opendocument" , TextWriter writeOpenDocument)
  ,("latex"        , TextWriter writeLaTeX)
  ,("beamer"       , TextWriter writeBeamer)
  ,("context"      , TextWriter writeConTeXt)
  ,("sile"         , TextWriter writeSILE)
  ,("texinfo"      , TextWriter writeTexinfo)
  ,("man"          , TextWriter writeMan)
  ,("ms"           , TextWriter writeMs)
  ,("markdown"     , TextWriter writeMarkdown)
  ,("markdown_strict" , TextWriter writeMarkdown)
  ,("markdown_phpextra" , TextWriter writeMarkdown)
  ,("markdown_github" , TextWriter writeMarkdown)
  ,("markdown_mmd" , TextWriter writeMarkdown)
  ,("plain"        , TextWriter writePlain)
  ,("rst"          , TextWriter writeRST)
  ,("mediawiki"    , TextWriter writeMediaWiki)
  ,("dokuwiki"     , TextWriter writeDokuWiki)
  ,("xwiki"        , TextWriter writeXWiki)
  ,("zimwiki"      , TextWriter writeZimWiki)
  ,("textile"      , TextWriter writeTextile)
  ,("typst"        , TextWriter writeTypst)
  ,("rtf"          , TextWriter writeRTF)
  ,("org"          , TextWriter writeOrg)
  ,("asciidoc"     , TextWriter writeAsciiDoc)
  ,("asciidoctor"  , TextWriter writeAsciiDoc)
  ,("asciidoc_legacy" , TextWriter writeAsciiDocLegacy)
  ,("haddock"      , TextWriter writeHaddock)
  ,("commonmark"   , TextWriter writeCommonMark)
  ,("commonmark_x" , TextWriter writeCommonMark)
  ,("gfm"          , TextWriter writeCommonMark)
  ,("tei"          , TextWriter writeTEI)
  ,("muse"         , TextWriter writeMuse)
  ,("csljson"      , TextWriter writeCslJson)
  ,("bibtex"       , TextWriter writeBibTeX)
  ,("biblatex"     , TextWriter writeBibLaTeX)
  ,("markua"       , TextWriter writeMarkua)
  ,("chunkedhtml"  , ByteStringWriter writeChunkedHTML)
  ,("djot"         , TextWriter writeDjot)
  ,("ansi"         , TextWriter writeANSI)
  ]

-- | Retrieve writer, extensions based on formatSpec (format+extensions).
getWriter :: PandocMonad m => Format.FlavoredFormat -> m (Writer m, Extensions)
getWriter flvrd = do
  let writerName = Format.formatName flvrd
  case lookup writerName writers of
    Nothing  -> throwError $ PandocUnknownWriterError writerName
    Just  w  -> (w,) <$>
      Format.applyExtensionsDiff (Format.getExtensionsConfig writerName) flvrd

writeJSON :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeJSON _ = return . UTF8.toText . BL.toStrict . encode
