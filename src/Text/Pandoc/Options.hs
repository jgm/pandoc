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
                           , def
                           ) where
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Default

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
               | Ext_markdown_in_html_blocks
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
