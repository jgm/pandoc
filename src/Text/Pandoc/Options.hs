{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TemplateHaskell    #-}
{-
Copyright (C) 2012-2018 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2012-2018 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Data structures and functions for representing parser and writer
options.
-}
module Text.Pandoc.Options ( module Text.Pandoc.Extensions
                           , ReaderOptions(..)
                           , HTMLMathMethod (..)
                           , CiteMethod (..)
                           , ObfuscationMethod (..)
                           , HTMLSlideVariant (..)
                           , EPUBVersion (..)
                           , WrapOption (..)
                           , TopLevelDivision (..)
                           , WriterOptions (..)
                           , TrackChanges (..)
                           , ReferenceLocation (..)
                           , def
                           , isEnabled
                           ) where
import Prelude
import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Data (Data)
import Data.Default
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Skylighting (SyntaxMap, defaultSyntaxMap)
import Text.Pandoc.Extensions
import Text.Pandoc.Highlighting (Style, pygments)

class HasSyntaxExtensions a where
  getExtensions :: a -> Extensions

data ReaderOptions = ReaderOptions{
         readerExtensions            :: Extensions  -- ^ Syntax extensions
       , readerStandalone            :: Bool -- ^ Standalone document with header
       , readerColumns               :: Int  -- ^ Number of columns in terminal
       , readerTabStop               :: Int  -- ^ Tab stop
       , readerIndentedCodeClasses   :: [String] -- ^ Default classes for
                                       -- indented code blocks
       , readerAbbreviations         :: Set.Set String -- ^ Strings to treat as abbreviations
       , readerDefaultImageExtension :: String -- ^ Default extension for images
       , readerTrackChanges          :: TrackChanges -- ^ Track changes setting for docx
       , readerStripComments         :: Bool -- ^ Strip HTML comments instead of parsing as raw HTML
} deriving (Show, Read, Data, Typeable, Generic)

instance HasSyntaxExtensions ReaderOptions where
  getExtensions opts = readerExtensions opts

instance Default ReaderOptions
  where def = ReaderOptions{
                 readerExtensions            = emptyExtensions
               , readerStandalone            = False
               , readerColumns               = 80
               , readerTabStop               = 4
               , readerIndentedCodeClasses   = []
               , readerAbbreviations         = defaultAbbrevs
               , readerDefaultImageExtension = ""
               , readerTrackChanges          = AcceptChanges
               , readerStripComments         = False
               }

defaultAbbrevs :: Set.Set String
defaultAbbrevs = Set.fromList
                 [ "Mr.", "Mrs.", "Ms.", "Capt.", "Dr.", "Prof.",
                   "Gen.", "Gov.", "e.g.", "i.e.", "Sgt.", "St.",
                   "vol.", "vs.", "Sen.", "Rep.", "Pres.", "Hon.",
                   "Rev.", "Ph.D.", "M.D.", "M.A.", "p.", "pp.",
                   "ch.", "sec.", "cf.", "cp."]

--
-- Writer options
--

data EPUBVersion = EPUB2 | EPUB3 deriving (Eq, Show, Read, Data, Typeable, Generic)

data HTMLMathMethod = PlainMath
                    | WebTeX String               -- url of TeX->image script.
                    | GladTeX
                    | MathML
                    | MathJax String              -- url of MathJax.js
                    | KaTeX String                -- url of KaTeX files
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

-- | Options defining the type of top-level headers.
data TopLevelDivision = TopLevelPart      -- ^ Top-level headers become parts
                      | TopLevelChapter   -- ^ Top-level headers become chapters
                      | TopLevelSection   -- ^ Top-level headers become sections
                      | TopLevelDefault   -- ^ Top-level type is determined via
                                          --   heuristics
                      deriving (Show, Read, Eq, Data, Typeable, Generic)

-- | Locations for footnotes and references in markdown output
data ReferenceLocation = EndOfBlock    -- ^ End of block
                       | EndOfSection  -- ^ prior to next section header (or end of document)
                       | EndOfDocument -- ^ at end of document
                       deriving (Show, Read, Eq, Data, Typeable, Generic)

-- | Options for writers
data WriterOptions = WriterOptions
  { writerTemplate          :: Maybe String -- ^ Template to use
  , writerVariables         :: [(String, String)] -- ^ Variables to set in template
  , writerTabStop           :: Int    -- ^ Tabstop for conversion btw spaces and tabs
  , writerTableOfContents   :: Bool   -- ^ Include table of contents
  , writerIncremental       :: Bool   -- ^ True if lists should be incremental
  , writerHTMLMathMethod    :: HTMLMathMethod  -- ^ How to print math in HTML
  , writerNumberSections    :: Bool   -- ^ Number sections in LaTeX
  , writerNumberOffset      :: [Int]  -- ^ Starting number for section, subsection, ...
  , writerSectionDivs       :: Bool   -- ^ Put sections in div tags in HTML
  , writerExtensions        :: Extensions -- ^ Markdown extensions that can be used
  , writerReferenceLinks    :: Bool   -- ^ Use reference links in writing markdown, rst
  , writerDpi               :: Int    -- ^ Dpi for pixel to/from inch/cm conversions
  , writerWrapText          :: WrapOption  -- ^ Option for wrapping text
  , writerColumns           :: Int    -- ^ Characters in a line (for text wrapping)
  , writerEmailObfuscation  :: ObfuscationMethod -- ^ How to obfuscate emails
  , writerIdentifierPrefix  :: String -- ^ Prefix for section & note ids in HTML
                                     -- and for footnote marks in markdown
  , writerCiteMethod        :: CiteMethod -- ^ How to print cites
  , writerHtmlQTags         :: Bool       -- ^ Use @<q>@ tags for quotes in HTML
  , writerSlideLevel        :: Maybe Int  -- ^ Force header level of slides
  , writerTopLevelDivision  :: TopLevelDivision -- ^ Type of top-level divisions
  , writerListings          :: Bool       -- ^ Use listings package for code
  , writerHighlightStyle    :: Maybe Style  -- ^ Style to use for highlighting
                                           -- (Nothing = no highlighting)
  , writerSetextHeaders     :: Bool       -- ^ Use setext headers for levels 1-2 in markdown
  , writerEpubSubdirectory  :: String       -- ^ Subdir for epub in OCF
  , writerEpubMetadata      :: Maybe String -- ^ Metadata to include in EPUB
  , writerEpubFonts         :: [FilePath] -- ^ Paths to fonts to embed
  , writerEpubChapterLevel  :: Int            -- ^ Header level for chapters (separate files)
  , writerTOCDepth          :: Int            -- ^ Number of levels to include in TOC
  , writerReferenceDoc      :: Maybe FilePath -- ^ Path to reference document if specified
  , writerReferenceLocation :: ReferenceLocation    -- ^ Location of footnotes and references for writing markdown
  , writerSyntaxMap         :: SyntaxMap
  } deriving (Show, Data, Typeable, Generic)

instance Default WriterOptions where
  def = WriterOptions { writerTemplate         = Nothing
                      , writerVariables        = []
                      , writerTabStop          = 4
                      , writerTableOfContents  = False
                      , writerIncremental      = False
                      , writerHTMLMathMethod   = PlainMath
                      , writerNumberSections   = False
                      , writerNumberOffset     = [0,0,0,0,0,0]
                      , writerSectionDivs      = False
                      , writerExtensions       = emptyExtensions
                      , writerReferenceLinks   = False
                      , writerDpi              = 96
                      , writerWrapText         = WrapAuto
                      , writerColumns          = 72
                      , writerEmailObfuscation = NoObfuscation
                      , writerIdentifierPrefix = ""
                      , writerCiteMethod       = Citeproc
                      , writerHtmlQTags        = False
                      , writerSlideLevel       = Nothing
                      , writerTopLevelDivision = TopLevelDefault
                      , writerListings         = False
                      , writerHighlightStyle   = Just pygments
                      , writerSetextHeaders    = True
                      , writerEpubSubdirectory = "EPUB"
                      , writerEpubMetadata     = Nothing
                      , writerEpubFonts        = []
                      , writerEpubChapterLevel = 1
                      , writerTOCDepth         = 3
                      , writerReferenceDoc     = Nothing
                      , writerReferenceLocation = EndOfDocument
                      , writerSyntaxMap        = defaultSyntaxMap
                      }

instance HasSyntaxExtensions WriterOptions where
  getExtensions opts = writerExtensions opts

-- | Returns True if the given extension is enabled.
isEnabled :: HasSyntaxExtensions a => Extension -> a -> Bool
isEnabled ext opts = ext `extensionEnabled` getExtensions opts

$(deriveJSON defaultOptions ''ReaderOptions)
$(deriveJSON defaultOptions ''HTMLMathMethod)
$(deriveJSON defaultOptions ''CiteMethod)
$(deriveJSON defaultOptions ''ObfuscationMethod)
$(deriveJSON defaultOptions ''HTMLSlideVariant)
$(deriveJSON defaultOptions ''TrackChanges)
$(deriveJSON defaultOptions ''WrapOption)
$(deriveJSON defaultOptions ''TopLevelDivision)
$(deriveJSON defaultOptions ''ReferenceLocation)
