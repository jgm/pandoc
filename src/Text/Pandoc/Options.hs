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
data Extension = Footnotes
               | Tex_math
               | Delimited_code_blocks
               | Markdown_in_html_blocks
               | Fancy_lists
               | Startnum
               | Definition_lists
               | Header_identifiers
               | All_symbols_escapable
               | Intraword_underscores
               | Blank_before_blockquote
               | Blank_before_header
               | Significant_bullets
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
