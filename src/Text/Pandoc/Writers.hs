{-
Copyright (C) 2006-2017 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
   Module      : Text.Pandoc
   Copyright   : Copyright (C) 2006-2017 John MacFarlane
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
    , writeHaddock
    , writeHtml4
    , writeHtml4String
    , writeHtml5
    , writeHtml5String
    , writeICML
    , writeJATS
    , writeJSON
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
    , writeRST
    , writeRTF
    , writeRevealJs
    , writeS5
    , writeSlideous
    , writeSlidy
    , writeTEI
    , writeTexinfo
    , writeTextile
    , writeZimWiki
    , getWriter
    ) where

import Data.Aeson
import Data.List (intercalate)
import Data.Text (Text)
import Text.Pandoc.Class
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Writers.AsciiDoc
import Text.Pandoc.Writers.CommonMark
import Text.Pandoc.Writers.ConTeXt
import Text.Pandoc.Writers.Custom
import Text.Pandoc.Writers.Docbook
import Text.Pandoc.Writers.Docx
import Text.Pandoc.Writers.DokuWiki
import Text.Pandoc.Writers.EPUB
import Text.Pandoc.Writers.FB2
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Writers.Haddock
import Text.Pandoc.Writers.ICML
import Text.Pandoc.Writers.JATS
import Text.Pandoc.Writers.LaTeX
import Text.Pandoc.Writers.Man
import Text.Pandoc.Writers.Markdown
import Text.Pandoc.Writers.MediaWiki
import Text.Pandoc.Writers.Ms
import Text.Pandoc.Writers.Muse
import Text.Pandoc.Writers.Native
import Text.Pandoc.Writers.ODT
import Text.Pandoc.Writers.OPML
import Text.Pandoc.Writers.OpenDocument
import Text.Pandoc.Writers.Org
import Text.Pandoc.Writers.RST
import Text.Pandoc.Writers.RTF
import Text.Pandoc.Writers.TEI
import Text.Pandoc.Writers.Texinfo
import Text.Pandoc.Writers.Textile
import Text.Pandoc.Writers.ZimWiki
import Text.Parsec.Error
import qualified Text.Pandoc.UTF8 as UTF8
import qualified Data.ByteString.Lazy as BL

data Writer m = TextWriter (WriterOptions -> Pandoc -> m Text)
              | ByteStringWriter (WriterOptions -> Pandoc -> m BL.ByteString)

-- | Association list of formats and writers.
writers :: PandocMonad m => [ ( String, Writer m) ]
writers = [
   ("native"       , TextWriter writeNative)
  ,("json"         , TextWriter $ \o d -> return $ writeJSON o d)
  ,("docx"         , ByteStringWriter writeDocx)
  ,("odt"          , ByteStringWriter writeODT)
  ,("epub"         , ByteStringWriter writeEPUB3)
  ,("epub2"        , ByteStringWriter writeEPUB2)
  ,("epub3"        , ByteStringWriter writeEPUB3)
  ,("fb2"          , TextWriter writeFB2)
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
  ,("jats"         , TextWriter writeJATS)
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
  ,("zimwiki"      , TextWriter writeZimWiki)
  ,("textile"      , TextWriter writeTextile)
  ,("rtf"          , TextWriter writeRTF)
  ,("org"          , TextWriter writeOrg)
  ,("asciidoc"     , TextWriter writeAsciiDoc)
  ,("haddock"      , TextWriter writeHaddock)
  ,("commonmark"   , TextWriter writeCommonMark)
  ,("tei"          , TextWriter writeTEI)
  ,("muse"         , TextWriter writeMuse)
  ]

-- | Retrieve writer, extensions based on formatSpec (format+extensions).
getWriter :: PandocMonad m => String -> Either String (Writer m, Extensions)
getWriter s
  = case parseFormatSpec s of
         Left e  -> Left $ intercalate "\n" [m | Message m <- errorMessages e]
         Right (writerName, setExts) ->
             case lookup writerName writers of
                     Nothing -> Left $ "Unknown writer: " ++ writerName
                     Just r -> Right (r, setExts $
                                  getDefaultExtensions writerName)

writeJSON :: WriterOptions -> Pandoc -> Text
writeJSON _ = UTF8.toText . BL.toStrict . encode
