{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-
Copyright (C) 2012-2016 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2012-2016 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Data structures and functions for representing parser and writer
options.
-}
module Text.Pandoc.Options ( Extension(..)
                           , pandocExtensions
                           , plainExtensions
                           , strictExtensions
                           , phpMarkdownExtraExtensions
                           , githubMarkdownExtensions
                           , multimarkdownExtensions
                           , ReaderOptions(..)
                           , HTMLMathMethod (..)
                           , CiteMethod (..)
                           , ObfuscationMethod (..)
                           , HTMLSlideVariant (..)
                           , EPUBVersion (..)
                           , WrapOption (..)
                           , WriterOptions (..)
                           , TrackChanges (..)
                           , def
                           , isEnabled
                           ) where
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Default
import Text.Pandoc.Highlighting (Style, pygments)
import Text.Pandoc.MediaBag (MediaBag)
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- | Individually selectable syntax extensions.
data Extension =
      Ext_footnotes           -- ^ Pandoc/PHP/MMD style footnotes
    | Ext_inline_notes        -- ^ Pandoc-style inline notes
    | Ext_pandoc_title_block  -- ^ Pandoc title block
    | Ext_yaml_metadata_block -- ^ YAML metadata block
    | Ext_mmd_title_block     -- ^ Multimarkdown metadata block
    | Ext_table_captions      -- ^ Pandoc-style table captions
    | Ext_implicit_figures    -- ^ A paragraph with just an image is a figure
    | Ext_simple_tables       -- ^ Pandoc-style simple tables
    | Ext_multiline_tables    -- ^ Pandoc-style multiline tables
    | Ext_grid_tables         -- ^ Grid tables (pandoc, reST)
    | Ext_pipe_tables         -- ^ Pipe tables (as in PHP markdown extra)
    | Ext_citations           -- ^ Pandoc/citeproc citations
    | Ext_raw_tex             -- ^ Allow raw TeX (other than math)
    | Ext_raw_html            -- ^ Allow raw HTML
    | Ext_tex_math_dollars    -- ^ TeX math between $..$ or $$..$$
    | Ext_tex_math_single_backslash  -- ^ TeX math btw \(..\) \[..\]
    | Ext_tex_math_double_backslash  -- ^ TeX math btw \\(..\\) \\[..\\]
    | Ext_latex_macros        -- ^ Parse LaTeX macro definitions (for math only)
    | Ext_fenced_code_blocks  -- ^ Parse fenced code blocks
    | Ext_fenced_code_attributes  -- ^ Allow attributes on fenced code blocks
    | Ext_backtick_code_blocks    -- ^ GitHub style ``` code blocks
    | Ext_inline_code_attributes  -- ^ Allow attributes on inline code
    | Ext_markdown_in_html_blocks -- ^ Interpret as markdown inside HTML blocks
    | Ext_native_divs             -- ^ Use Div blocks for contents of <div> tags
    | Ext_native_spans            -- ^ Use Span inlines for contents of <span>
    | Ext_markdown_attribute      -- ^ Interpret text inside HTML as markdown
                                  --   iff container has attribute 'markdown'
    | Ext_escaped_line_breaks     -- ^ Treat a backslash at EOL as linebreak
    | Ext_link_attributes         -- ^ link and image attributes
    | Ext_mmd_link_attributes     -- ^ MMD style reference link attributes
    | Ext_autolink_bare_uris  -- ^ Make all absolute URIs into links
    | Ext_fancy_lists         -- ^ Enable fancy list numbers and delimiters
    | Ext_lists_without_preceding_blankline -- ^ Allow lists without preceding blank
    | Ext_startnum            -- ^ Make start number of ordered list significant
    | Ext_definition_lists    -- ^ Definition lists as in pandoc, mmd, php
    | Ext_compact_definition_lists  -- ^ Definition lists without
                               -- space between items, and disallow laziness
    | Ext_example_lists       -- ^ Markdown-style numbered examples
    | Ext_all_symbols_escapable  -- ^ Make all non-alphanumerics escapable
    | Ext_intraword_underscores  -- ^ Treat underscore inside word as literal
    | Ext_blank_before_blockquote -- ^ Require blank line before a blockquote
    | Ext_blank_before_header     -- ^ Require blank line before a header
    | Ext_strikeout           -- ^ Strikeout using ~~this~~ syntax
    | Ext_superscript         -- ^ Superscript using ^this^ syntax
    | Ext_subscript           -- ^ Subscript using ~this~ syntax
    | Ext_hard_line_breaks    -- ^ All newlines become hard line breaks
    | Ext_ignore_line_breaks  -- ^ Newlines in paragraphs are ignored
    | Ext_east_asian_line_breaks  -- ^ Newlines in paragraphs are ignored between
                              -- East Asian wide characters
    | Ext_literate_haskell    -- ^ Enable literate Haskell conventions
    | Ext_abbreviations       -- ^ PHP markdown extra abbreviation definitions
    | Ext_emoji               -- ^ Support emoji like :smile:
    | Ext_auto_identifiers    -- ^ Automatic identifiers for headers
    | Ext_ascii_identifiers   -- ^ ascii-only identifiers for headers
    | Ext_header_attributes   -- ^ Explicit header attributes {#id .class k=v}
    | Ext_mmd_header_identifiers -- ^ Multimarkdown style header identifiers [myid]
    | Ext_implicit_header_references -- ^ Implicit reference links for headers
    | Ext_line_blocks         -- ^ RST style line blocks
    | Ext_epub_html_exts      -- ^ Recognise the EPUB extended version of HTML
    | Ext_shortcut_reference_links -- ^ Shortcut reference links
    deriving (Show, Read, Enum, Eq, Ord, Bounded, Data, Typeable, Generic)

pandocExtensions :: Set Extension
pandocExtensions = Set.fromList
  [ Ext_footnotes
  , Ext_inline_notes
  , Ext_pandoc_title_block
  , Ext_yaml_metadata_block
  , Ext_table_captions
  , Ext_implicit_figures
  , Ext_simple_tables
  , Ext_multiline_tables
  , Ext_grid_tables
  , Ext_pipe_tables
  , Ext_citations
  , Ext_raw_tex
  , Ext_raw_html
  , Ext_tex_math_dollars
  , Ext_latex_macros
  , Ext_fenced_code_blocks
  , Ext_fenced_code_attributes
  , Ext_backtick_code_blocks
  , Ext_inline_code_attributes
  , Ext_markdown_in_html_blocks
  , Ext_native_divs
  , Ext_native_spans
  , Ext_escaped_line_breaks
  , Ext_fancy_lists
  , Ext_startnum
  , Ext_definition_lists
  , Ext_example_lists
  , Ext_all_symbols_escapable
  , Ext_intraword_underscores
  , Ext_blank_before_blockquote
  , Ext_blank_before_header
  , Ext_strikeout
  , Ext_superscript
  , Ext_subscript
  , Ext_auto_identifiers
  , Ext_header_attributes
  , Ext_link_attributes
  , Ext_implicit_header_references
  , Ext_line_blocks
  , Ext_shortcut_reference_links
  ]

plainExtensions :: Set Extension
plainExtensions = Set.fromList
  [ Ext_table_captions
  , Ext_implicit_figures
  , Ext_simple_tables
  , Ext_multiline_tables
  , Ext_grid_tables
  , Ext_latex_macros
  , Ext_fancy_lists
  , Ext_startnum
  , Ext_definition_lists
  , Ext_example_lists
  , Ext_intraword_underscores
  , Ext_blank_before_blockquote
  , Ext_blank_before_header
  , Ext_strikeout
  ]

phpMarkdownExtraExtensions :: Set Extension
phpMarkdownExtraExtensions = Set.fromList
  [ Ext_footnotes
  , Ext_pipe_tables
  , Ext_raw_html
  , Ext_markdown_attribute
  , Ext_fenced_code_blocks
  , Ext_definition_lists
  , Ext_intraword_underscores
  , Ext_header_attributes
  , Ext_link_attributes
  , Ext_abbreviations
  , Ext_shortcut_reference_links
  ]

githubMarkdownExtensions :: Set Extension
githubMarkdownExtensions = Set.fromList
  [ Ext_pipe_tables
  , Ext_raw_html
  , Ext_fenced_code_blocks
  , Ext_auto_identifiers
  , Ext_ascii_identifiers
  , Ext_backtick_code_blocks
  , Ext_autolink_bare_uris
  , Ext_intraword_underscores
  , Ext_strikeout
  , Ext_hard_line_breaks
  , Ext_emoji
  , Ext_lists_without_preceding_blankline
  , Ext_shortcut_reference_links
  ]

multimarkdownExtensions :: Set Extension
multimarkdownExtensions = Set.fromList
  [ Ext_pipe_tables
  , Ext_raw_html
  , Ext_markdown_attribute
  , Ext_mmd_link_attributes
  , Ext_raw_tex
  , Ext_tex_math_double_backslash
  , Ext_intraword_underscores
  , Ext_mmd_title_block
  , Ext_footnotes
  , Ext_definition_lists
  , Ext_all_symbols_escapable
  , Ext_implicit_header_references
  , Ext_auto_identifiers
  , Ext_mmd_header_identifiers
  , Ext_implicit_figures
  -- Note: MMD's syntax for superscripts and subscripts
  -- is a bit more permissive than pandoc's, allowing
  -- e^2 and a~1 instead of e^2^ and a~1~, so even with
  -- these options we don't have full support for MMD
  -- superscripts and subscripts, but there's no reason
  -- not to include these:
  , Ext_superscript
  , Ext_subscript
  ]

strictExtensions :: Set Extension
strictExtensions = Set.fromList
  [ Ext_raw_html
  , Ext_shortcut_reference_links
  ]

data ReaderOptions = ReaderOptions{
         readerExtensions      :: Set Extension  -- ^ Syntax extensions
       , readerSmart           :: Bool -- ^ Smart punctuation
       , readerStandalone      :: Bool -- ^ Standalone document with header
       , readerParseRaw        :: Bool -- ^ Parse raw HTML, LaTeX
       , readerColumns         :: Int  -- ^ Number of columns in terminal
       , readerTabStop         :: Int  -- ^ Tab stop
       , readerOldDashes       :: Bool -- ^ Use pandoc <= 1.8.2.1 behavior
                                       --   in parsing dashes; -- is em-dash;
                                       --   - before numerial is en-dash
       , readerApplyMacros     :: Bool -- ^ Apply macros to TeX math
       , readerIndentedCodeClasses :: [String] -- ^ Default classes for
                                       -- indented code blocks
       , readerDefaultImageExtension :: String -- ^ Default extension for images
       , readerTrace           :: Bool -- ^ Print debugging info
       , readerTrackChanges    :: TrackChanges
       , readerFileScope      :: Bool -- ^ Parse before combining
} deriving (Show, Read, Data, Typeable, Generic)

instance Default ReaderOptions
  where def = ReaderOptions{
                 readerExtensions            = pandocExtensions
               , readerSmart                 = False
               , readerStandalone            = False
               , readerParseRaw              = False
               , readerColumns               = 80
               , readerTabStop               = 4
               , readerOldDashes             = False
               , readerApplyMacros           = True
               , readerIndentedCodeClasses   = []
               , readerDefaultImageExtension = ""
               , readerTrace                 = False
               , readerTrackChanges          = AcceptChanges
               , readerFileScope             = False
               }

--
-- Writer options
--

data EPUBVersion = EPUB2 | EPUB3 deriving (Eq, Show, Read, Data, Typeable, Generic)

data HTMLMathMethod = PlainMath
                    | LaTeXMathML (Maybe String)  -- url of LaTeXMathML.js
                    | JsMath (Maybe String)       -- url of jsMath load script
                    | GladTeX
                    | WebTeX String               -- url of TeX->image script.
                    | MathML (Maybe String)       -- url of MathMLinHTML.js
                    | MathJax String              -- url of MathJax.js
                    | KaTeX String String -- url of stylesheet and katex.js
                    deriving (Show, Read, Eq, Data, Typeable, Generic)

data CiteMethod = Citeproc                        -- use citeproc to render them
                  | Natbib                        -- output natbib cite commands
                  | Biblatex                      -- output biblatex cite commands
                deriving (Show, Read, Eq, Data, Typeable, Generic)

-- | Methods for obfuscating email addresses in HTML.
data ObfuscationMethod = NoObfuscation
                       | ReferenceObfuscation
                       | JavascriptObfuscation
                       deriving (Show, Read, Eq, Data, Typeable, Generic)

-- | Varieties of HTML slide shows.
data HTMLSlideVariant = S5Slides
                      | SlidySlides
                      | SlideousSlides
                      | DZSlides
                      | RevealJsSlides
                      | NoSlides
                      deriving (Show, Read, Eq, Data, Typeable, Generic)

-- | Options for accepting or rejecting MS Word track-changes.
data TrackChanges = AcceptChanges
                  | RejectChanges
                  | AllChanges
                  deriving (Show, Read, Eq, Data, Typeable, Generic)

-- | Options for wrapping text in the output.
data WrapOption = WrapAuto        -- ^ Automatically wrap to width
                | WrapNone        -- ^ No non-semantic newlines
                | WrapPreserve    -- ^ Preserve wrapping of input source
                deriving (Show, Read, Eq, Data, Typeable, Generic)

-- | Options for writers
data WriterOptions = WriterOptions
  { writerStandalone       :: Bool   -- ^ Include header and footer
  , writerTemplate         :: String -- ^ Template to use in standalone mode
  , writerVariables        :: [(String, String)] -- ^ Variables to set in template
  , writerTabStop          :: Int    -- ^ Tabstop for conversion btw spaces and tabs
  , writerTableOfContents  :: Bool   -- ^ Include table of contents
  , writerSlideVariant     :: HTMLSlideVariant -- ^ Are we writing S5, Slidy or Slideous?
  , writerIncremental      :: Bool   -- ^ True if lists should be incremental
  , writerHTMLMathMethod   :: HTMLMathMethod  -- ^ How to print math in HTML
  , writerIgnoreNotes      :: Bool   -- ^ Ignore footnotes (used in making toc)
  , writerNumberSections   :: Bool   -- ^ Number sections in LaTeX
  , writerNumberOffset     :: [Int]  -- ^ Starting number for section, subsection, ...
  , writerSectionDivs      :: Bool   -- ^ Put sections in div tags in HTML
  , writerExtensions       :: Set Extension -- ^ Markdown extensions that can be used
  , writerReferenceLinks   :: Bool   -- ^ Use reference links in writing markdown, rst
  , writerDpi              :: Int    -- ^ Dpi for pixel to/from inch/cm conversions
  , writerWrapText         :: WrapOption  -- ^ Option for wrapping text
  , writerColumns          :: Int    -- ^ Characters in a line (for text wrapping)
  , writerEmailObfuscation :: ObfuscationMethod -- ^ How to obfuscate emails
  , writerIdentifierPrefix :: String -- ^ Prefix for section & note ids in HTML
                                     -- and for footnote marks in markdown
  , writerSourceURL        :: Maybe String  -- ^ Absolute URL + directory of 1st source file
  , writerUserDataDir      :: Maybe FilePath -- ^ Path of user data directory
  , writerCiteMethod       :: CiteMethod -- ^ How to print cites
  , writerHtml5            :: Bool       -- ^ Produce HTML5
  , writerHtmlQTags        :: Bool       -- ^ Use @<q>@ tags for quotes in HTML
  , writerBeamer           :: Bool       -- ^ Produce beamer LaTeX slide show
  , writerSlideLevel       :: Maybe Int  -- ^ Force header level of slides
  , writerChapters         :: Bool       -- ^ Use "chapter" for top-level sects
  , writerListings         :: Bool       -- ^ Use listings package for code
  , writerHighlight        :: Bool       -- ^ Highlight source code
  , writerHighlightStyle   :: Style      -- ^ Style to use for highlighting
  , writerSetextHeaders    :: Bool       -- ^ Use setext headers for levels 1-2 in markdown
  , writerTeXLigatures     :: Bool       -- ^ Use tex ligatures quotes, dashes in latex
  , writerEpubVersion      :: Maybe EPUBVersion -- ^ Nothing or EPUB version
  , writerEpubMetadata     :: String     -- ^ Metadata to include in EPUB
  , writerEpubStylesheet   :: Maybe String -- ^ EPUB stylesheet specified at command line
  , writerEpubFonts        :: [FilePath] -- ^ Paths to fonts to embed
  , writerEpubChapterLevel :: Int            -- ^ Header level for chapters (separate files)
  , writerTOCDepth         :: Int            -- ^ Number of levels to include in TOC
  , writerReferenceODT     :: Maybe FilePath -- ^ Path to reference ODT if specified
  , writerReferenceDocx    :: Maybe FilePath -- ^ Path to reference DOCX if specified
  , writerMediaBag         :: MediaBag       -- ^ Media collected by docx or epub reader
  , writerVerbose          :: Bool           -- ^ Verbose debugging output
  , writerLaTeXArgs        :: [String]       -- ^ Flags to pass to latex-engine
  } deriving (Show, Data, Typeable, Generic)

instance Default WriterOptions where
  def = WriterOptions { writerStandalone       = False
                      , writerTemplate         = ""
                      , writerVariables        = []
                      , writerTabStop          = 4
                      , writerTableOfContents  = False
                      , writerSlideVariant     = NoSlides
                      , writerIncremental      = False
                      , writerHTMLMathMethod   = PlainMath
                      , writerIgnoreNotes      = False
                      , writerNumberSections   = False
                      , writerNumberOffset     = [0,0,0,0,0,0]
                      , writerSectionDivs      = False
                      , writerExtensions       = pandocExtensions
                      , writerReferenceLinks   = False
                      , writerDpi              = 96
                      , writerWrapText         = WrapAuto
                      , writerColumns          = 72
                      , writerEmailObfuscation = JavascriptObfuscation
                      , writerIdentifierPrefix = ""
                      , writerSourceURL        = Nothing
                      , writerUserDataDir      = Nothing
                      , writerCiteMethod       = Citeproc
                      , writerHtml5            = False
                      , writerHtmlQTags        = False
                      , writerBeamer           = False
                      , writerSlideLevel       = Nothing
                      , writerChapters         = False
                      , writerListings         = False
                      , writerHighlight        = False
                      , writerHighlightStyle   = pygments
                      , writerSetextHeaders    = True
                      , writerTeXLigatures     = True
                      , writerEpubVersion      = Nothing
                      , writerEpubMetadata     = ""
                      , writerEpubStylesheet   = Nothing
                      , writerEpubFonts        = []
                      , writerEpubChapterLevel = 1
                      , writerTOCDepth         = 3
                      , writerReferenceODT     = Nothing
                      , writerReferenceDocx    = Nothing
                      , writerMediaBag         = mempty
                      , writerVerbose          = False
                      , writerLaTeXArgs        = []
                      }

-- | Returns True if the given extension is enabled.
isEnabled :: Extension -> WriterOptions -> Bool
isEnabled ext opts = ext `Set.member` (writerExtensions opts)
