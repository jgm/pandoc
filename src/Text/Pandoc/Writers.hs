{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{- |
   Module      : Text.Pandoc
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
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
    , writeAsciiDoc
    , writeAsciiDoctor
    , writeBeamer
    , writeCommonMark
    , writeConTeXt
    , writeCustom
    , writeDZSlides
    , writeDocbook4
    , writeDocbook5
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
    , writeSlideous
    , writeSlidy
    , writeTEI
    , writeTexinfo
    , writeTextile
    , writeXWiki
    , writeZimWiki
    , getWriter
    ) where

import Control.Monad.Except (throwError)
import Control.Monad (unless)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Class
import Text.Pandoc.Definition
import Text.Pandoc.Options
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Error
import Text.Pandoc.Writers.AsciiDoc
import Text.Pandoc.Writers.CommonMark
import Text.Pandoc.Writers.ConTeXt
import Text.Pandoc.Writers.Custom
import Text.Pandoc.Writers.Docbook
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
import Text.Pandoc.Writers.TEI
import Text.Pandoc.Writers.Texinfo
import Text.Pandoc.Writers.Textile
import Text.Pandoc.Writers.XWiki
import Text.Pandoc.Writers.ZimWiki
import Text.Parsec.Error

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
  ,("docbook"      , TextWriter writeDocbook5)
  ,("docbook4"     , TextWriter writeDocbook4)
  ,("docbook5"     , TextWriter writeDocbook5)
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
  ,("rtf"          , TextWriter writeRTF)
  ,("org"          , TextWriter writeOrg)
  ,("asciidoc"     , TextWriter writeAsciiDoc)
  ,("asciidoctor"  , TextWriter writeAsciiDoctor)
  ,("haddock"      , TextWriter writeHaddock)
  ,("commonmark"   , TextWriter writeCommonMark)
  ,("commonmark_x" , TextWriter writeCommonMark)
  ,("gfm"          , TextWriter writeCommonMark)
  ,("tei"          , TextWriter writeTEI)
  ,("muse"         , TextWriter writeMuse)
  ]

-- | Retrieve writer, extensions based on formatSpec (format+extensions).
getWriter :: PandocMonad m => Text -> m (Writer m, Extensions)
getWriter s =
  case parseFormatSpec s of
        Left e  -> throwError $ PandocAppError
                    $ T.intercalate "\n" [T.pack m | Message m <- errorMessages e]
        Right (writerName, extsToEnable, extsToDisable) ->
           case lookup writerName writers of
                   Nothing  -> throwError $
                                 PandocUnknownWriterError writerName
                   Just  w  -> do
                     let allExts = getAllExtensions writerName
                     let exts = foldr disableExtension
                           (foldr enableExtension
                             (getDefaultExtensions writerName)
                                   extsToEnable) extsToDisable
                     mapM_ (\ext ->
                              unless (extensionEnabled ext allExts) $
                                throwError $
                                   PandocUnsupportedExtensionError
                                   (T.drop 4 $ T.pack $ show ext) writerName)
                          (extsToEnable ++ extsToDisable)
                     return (w, exts)


writeJSON :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeJSON _ = return . UTF8.toText . BL.toStrict . encode
