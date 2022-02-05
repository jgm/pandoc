{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.App.FormatHeuristics
   Copyright   : Copyright (C) 2006-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Guess the format of a file from its name.
-}
module Text.Pandoc.App.FormatHeuristics
  ( formatFromFilePaths
  ) where

import Data.Char (toLower)
import Data.Foldable (asum)
import Data.Text (Text)
import System.FilePath (takeExtension)

-- | Determines default format based on file extensions; uses the format
-- of the first extension that's associated with a format.
--
-- Examples:
--
-- > formatFromFilePaths ["text.unknown", "no-extension"]
-- Nothing
--
-- > formatFromFilePaths ["my.md", "other.rst"]
-- Just "markdown"
formatFromFilePaths :: [FilePath] -> Maybe Text
formatFromFilePaths = asum . map formatFromFilePath

-- | Determines format based on file extension.
formatFromFilePath :: FilePath -> Maybe Text
formatFromFilePath x =
  case takeExtension (map toLower x) of
    ".adoc"     -> Just "asciidoc"
    ".asciidoc" -> Just "asciidoc"
    ".context"  -> Just "context"
    ".ctx"      -> Just "context"
    ".db"       -> Just "docbook"
    ".doc"      -> Just "doc"  -- so we get an "unknown reader" error
    ".docx"     -> Just "docx"
    ".dokuwiki" -> Just "dokuwiki"
    ".epub"     -> Just "epub"
    ".fb2"      -> Just "fb2"
    ".htm"      -> Just "html"
    ".html"     -> Just "html"
    ".icml"     -> Just "icml"
    ".json"     -> Just "json"
    ".latex"    -> Just "latex"
    ".lhs"      -> Just "markdown+lhs"
    ".ltx"      -> Just "latex"
    ".markdown" -> Just "markdown"
    ".markua"   -> Just "markua"
    ".mkdn"     -> Just "markdown"
    ".mkd"      -> Just "markdown"
    ".mdwn"     -> Just "markdown"
    ".mdown"    -> Just "markdown"
    ".Rmd"      -> Just "markdown"
    ".md"       -> Just "markdown"
    ".ms"       -> Just "ms"
    ".muse"     -> Just "muse"
    ".native"   -> Just "native"
    ".odt"      -> Just "odt"
    ".opml"     -> Just "opml"
    ".org"      -> Just "org"
    ".pdf"      -> Just "pdf"  -- so we get an "unknown reader" error
    ".pptx"     -> Just "pptx"
    ".ris"      -> Just "ris"
    ".roff"     -> Just "ms"
    ".rst"      -> Just "rst"
    ".rtf"      -> Just "rtf"
    ".s5"       -> Just "s5"
    ".t2t"      -> Just "t2t"
    ".tei"      -> Just "tei"
    ".tex"      -> Just "latex"
    ".texi"     -> Just "texinfo"
    ".texinfo"  -> Just "texinfo"
    ".text"     -> Just "markdown"
    ".textile"  -> Just "textile"
    ".txt"      -> Just "markdown"
    ".wiki"     -> Just "mediawiki"
    ".xhtml"    -> Just "html"
    ".ipynb"    -> Just "ipynb"
    ".csv"      -> Just "csv"
    ".bib"      -> Just "biblatex"
    ['.',y]     | y `elem` ['1'..'9'] -> Just "man"
    _           -> Nothing
