{-
Copyright (C) 2012 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Options
   Copyright   : Copyright (C) 2012 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Data structures and functions for representing parser and writer
options.
-}
module Text.Pandoc.Options ( Extension(..)
                           , ReaderOptions(..)
                           , HTMLMathMethod (..)
                           , CiteMethod (..)
                           , ObfuscationMethod (..)
                           , HTMLSlideVariant (..)
                           , WriterOptions (..)
                           , def
                           ) where
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Default
import Text.Pandoc.Highlighting (Style, pygments)

-- | Individually selectable syntax extensions.
data Extension = Ext_footnotes
               | Ext_inline_notes
               | Ext_pandoc_title_blocks
               | Ext_table_captions
               | Ext_simple_tables
               | Ext_multiline_tables
               | Ext_grid_tables
               | Ext_pipe_tables
               | Ext_citations
               | Ext_raw_tex
               | Ext_tex_math
               | Ext_latex_macros
               | Ext_delimited_code_blocks
               | Ext_inline_code_attributes
               | Ext_markdown_in_html_blocks
               | Ext_escaped_line_breaks
               | Ext_autolink_code_spans
               | Ext_fancy_lists
               | Ext_startnum
               | Ext_definition_lists
               | Ext_header_identifiers
               | Ext_all_symbols_escapable
               | Ext_intraword_underscores
               | Ext_blank_before_blockquote
               | Ext_blank_before_header
               | Ext_significant_bullets
               | Ext_strikeout
               | Ext_superscript
               | Ext_subscript
               deriving (Show, Read, Enum, Eq, Ord, Bounded)

data ReaderOptions = ReaderOptions{
         readerExtensions      :: Set Extension  -- ^ Syntax extensions
       , readerSmart           :: Bool -- ^ Smart punctuation
       , readerStrict          :: Bool -- ^ FOR TRANSITION ONLY
       , readerStandalone      :: Bool -- ^ Standalone document with header
       , readerParseRaw        :: Bool -- ^ Parse raw HTML, LaTeX
       , readerColumns         :: Int  -- ^ Number of columns in terminal
       , readerTabStop         :: Int  -- ^ Tab stop
       , readerOldDashes       :: Bool -- ^ Use pandoc <= 1.8.2.1 behavior
                                       --   in parsing dashes; -- is em-dash;
                                       --   - before numerial is en-dash
       , readerLiterateHaskell :: Bool -- ^ Interpret as literate Haskell
       , readerCitations       :: [String] -- ^ List of available citations
       , readerApplyMacros     :: Bool -- ^ Apply macros to TeX math
       , readerIndentedCodeClasses :: [String] -- ^ Default classes for
                                       -- indented code blocks
} deriving (Show, Read)

instance Default ReaderOptions
  where def = ReaderOptions{
                 readerExtensions          = Set.fromList [minBound..maxBound]
               , readerSmart               = False
               , readerStrict              = False
               , readerStandalone          = False
               , readerParseRaw            = False
               , readerColumns             = 80
               , readerTabStop             = 4
               , readerOldDashes           = False
               , readerLiterateHaskell     = False
               , readerCitations           = []
               , readerApplyMacros         = True
               , readerIndentedCodeClasses = []
               }

--
-- Writer options
--

data HTMLMathMethod = PlainMath
                    | LaTeXMathML (Maybe String)  -- url of LaTeXMathML.js
                    | JsMath (Maybe String)       -- url of jsMath load script
                    | GladTeX
                    | WebTeX String               -- url of TeX->image script.
                    | MathML (Maybe String)       -- url of MathMLinHTML.js
                    | MathJax String              -- url of MathJax.js
                    deriving (Show, Read, Eq)

data CiteMethod = Citeproc                        -- use citeproc to render them
                  | Natbib                        -- output natbib cite commands
                  | Biblatex                      -- output biblatex cite commands
                deriving (Show, Read, Eq)

-- | Methods for obfuscating email addresses in HTML.
data ObfuscationMethod = NoObfuscation
                       | ReferenceObfuscation
                       | JavascriptObfuscation
                       deriving (Show, Read, Eq)

-- | Varieties of HTML slide shows.
data HTMLSlideVariant = S5Slides
                      | SlidySlides
                      | SlideousSlides
                      | DZSlides
                      | NoSlides
                      deriving (Show, Read, Eq)

-- | Options for writers
{-# DEPRECATED writerXeTeX "writerXeTeX no longer does anything" #-}
data WriterOptions = WriterOptions
  { writerStandalone       :: Bool   -- ^ Include header and footer
  , writerTemplate         :: String -- ^ Template to use in standalone mode
  , writerVariables        :: [(String, String)] -- ^ Variables to set in template
  , writerEPUBMetadata     :: String -- ^ Metadata to include in EPUB
  , writerTabStop          :: Int    -- ^ Tabstop for conversion btw spaces and tabs
  , writerTableOfContents  :: Bool   -- ^ Include table of contents
  , writerSlideVariant     :: HTMLSlideVariant -- ^ Are we writing S5, Slidy or Slideous?
  , writerIncremental      :: Bool   -- ^ True if lists should be incremental
  , writerXeTeX            :: Bool   -- ^ Create latex suitable for use by xetex
  , writerHTMLMathMethod   :: HTMLMathMethod  -- ^ How to print math in HTML
  , writerIgnoreNotes      :: Bool   -- ^ Ignore footnotes (used in making toc)
  , writerNumberSections   :: Bool   -- ^ Number sections in LaTeX
  , writerSectionDivs      :: Bool   -- ^ Put sections in div tags in HTML
  , writerExtensions       :: Set Extension -- ^ Markdown extensions that can be used
  , writerReferenceLinks   :: Bool   -- ^ Use reference links in writing markdown, rst
  , writerWrapText         :: Bool   -- ^ Wrap text to line length
  , writerColumns          :: Int    -- ^ Characters in a line (for text wrapping)
  , writerLiterateHaskell  :: Bool   -- ^ Write as literate haskell
  , writerEmailObfuscation :: ObfuscationMethod -- ^ How to obfuscate emails
  , writerIdentifierPrefix :: String -- ^ Prefix for section & note ids in HTML
  , writerSourceDirectory  :: FilePath -- ^ Directory path of 1st source file
  , writerUserDataDir      :: Maybe FilePath -- ^ Path of user data directory
  , writerCiteMethod       :: CiteMethod -- ^ How to print cites
  , writerBiblioFiles      :: [FilePath] -- ^ Biblio files to use for citations
  , writerHtml5            :: Bool       -- ^ Produce HTML5
  , writerBeamer           :: Bool       -- ^ Produce beamer LaTeX slide show
  , writerSlideLevel       :: Maybe Int  -- ^ Force header level of slides
  , writerChapters         :: Bool       -- ^ Use "chapter" for top-level sects
  , writerListings         :: Bool       -- ^ Use listings package for code
  , writerHighlight        :: Bool       -- ^ Highlight source code
  , writerHighlightStyle   :: Style      -- ^ Style to use for highlighting
  , writerSetextHeaders    :: Bool       -- ^ Use setext headers for levels 1-2 in markdown
  , writerTeXLigatures     :: Bool       -- ^ Use tex ligatures quotes, dashes in latex
  , writerEpubStylesheet   :: Maybe String -- ^ EPUB stylesheet specified at command line
  , writerEpubFonts        :: [FilePath] -- ^ Paths to fonts to embed
  , writerReferenceODT     :: Maybe FilePath -- ^ Path to reference ODT if specified
  , writerReferenceDocx    :: Maybe FilePath -- ^ Ptah to reference DOCX if specified
  } deriving Show

instance Default WriterOptions where
  def = WriterOptions { writerStandalone       = False
                      , writerTemplate         = ""
                      , writerVariables        = []
                      , writerEPUBMetadata     = ""
                      , writerTabStop          = 4
                      , writerTableOfContents  = False
                      , writerSlideVariant     = NoSlides
                      , writerIncremental      = False
                      , writerXeTeX            = False
                      , writerHTMLMathMethod   = PlainMath
                      , writerIgnoreNotes      = False
                      , writerNumberSections   = False
                      , writerSectionDivs      = False
                      , writerExtensions       = Set.fromList [minBound..maxBound]
                      , writerReferenceLinks   = False
                      , writerWrapText         = True
                      , writerColumns          = 72
                      , writerLiterateHaskell  = False
                      , writerEmailObfuscation = JavascriptObfuscation
                      , writerIdentifierPrefix = ""
                      , writerSourceDirectory  = "."
                      , writerUserDataDir      = Nothing
                      , writerCiteMethod       = Citeproc
                      , writerBiblioFiles      = []
                      , writerHtml5            = False
                      , writerBeamer           = False
                      , writerSlideLevel       = Nothing
                      , writerChapters         = False
                      , writerListings         = False
                      , writerHighlight        = False
                      , writerHighlightStyle   = pygments
                      , writerSetextHeaders    = True
                      , writerTeXLigatures     = True
                      , writerEpubStylesheet   = Nothing
                      , writerEpubFonts        = []
                      , writerReferenceODT     = Nothing
                      , writerReferenceDocx    = Nothing
                      }

