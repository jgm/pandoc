{-# LANGUAGE NoImplicitPrelude #-}
{-
Copyright (C) 2006-2018 John MacFarlane <jgm@berkeley.edu>

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

{- |
   Module      : Text.Pandoc.App.FormatHeuristics
   Copyright   : Copyright (C) 2006-2018 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Guess the format of a file from its name.
-}
module Text.Pandoc.App.FormatHeuristics
  ( formatFromFilePaths
  ) where

import Prelude
import Data.Char (toLower)
import System.FilePath (takeExtension)

-- Determine default reader based on source file extensions.
formatFromFilePaths :: String -> [FilePath] -> String
formatFromFilePaths fallback [] = fallback
formatFromFilePaths fallback (x:xs) =
  case formatFromFilePath x of
    Just f     -> f
    Nothing    -> formatFromFilePaths fallback xs

-- Determine format based on file extension
formatFromFilePath :: FilePath -> Maybe String
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
    ".md"       -> Just "markdown"
    ".ms"       -> Just "ms"
    ".muse"     -> Just "muse"
    ".native"   -> Just "native"
    ".odt"      -> Just "odt"
    ".opml"     -> Just "opml"
    ".org"      -> Just "org"
    ".pdf"      -> Just "pdf"  -- so we get an "unknown reader" error
    ".pptx"     -> Just "pptx"
    ".roff"     -> Just "ms"
    ".rst"      -> Just "rst"
    ".rtf"      -> Just "rtf"
    ".s5"       -> Just "s5"
    ".t2t"      -> Just "t2t"
    ".tei"      -> Just "tei"
    ".tei.xml"  -> Just "tei"
    ".tex"      -> Just "latex"
    ".texi"     -> Just "texinfo"
    ".texinfo"  -> Just "texinfo"
    ".text"     -> Just "markdown"
    ".textile"  -> Just "textile"
    ".txt"      -> Just "markdown"
    ".wiki"     -> Just "mediawiki"
    ".xhtml"    -> Just "html"
    ".ipynb"    -> Just "ipynb"
    ['.',y]     | y `elem` ['1'..'9'] -> Just "man"
    _           -> Nothing
