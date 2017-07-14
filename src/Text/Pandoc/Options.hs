{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-
Copyright (C) 2012-2017 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2012-2017 John MacFarlane
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
import Data.Aeson (ToJSON(..), FromJSON(..),
                   genericToEncoding, defaultOptions)
import Data.Data (Data)
import Data.Default
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Skylighting (SyntaxMap, defaultSyntaxMap)
import Text.Pandoc.Extensions
import Text.Pandoc.Highlighting (Style, pygments)

data ReaderOptions = ReaderOptions{
         readerExtensions            :: Extensions  -- ^ Syntax extensions
       , readerStandalone            :: Bool -- ^ Standalone document with header
       , readerColumns               :: Int  -- ^ Number of columns in terminal
       , readerTabStop               :: Int  -- ^ Tab stop
       , readerIndentedCodeClasses   :: [String] -- ^ Default classes for
                                       -- indented code blocks
       , readerAbbreviations         :: Set.Set String -- ^ Strings to treat as abbreviations
       , readerDefaultImageExtension :: String -- ^ Default extension for images
       , readerTrackChanges          :: TrackChanges
} deriving (Show, Read, Data, Typeable, Generic)

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
                    | LaTeXMathML (Maybe String)  -- url of LaTeXMathML.js
                    | JsMath (Maybe String)       -- url of jsMath load script
                    | GladTeX
                    | WebTeX String               -- url of TeX->image script.
                    | MathML
                    | MathJax String              -- url of MathJax.js
                    | KaTeX String String -- url of stylesheet and katex.js
                    deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON HTMLMathMethod where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON HTMLMathMethod

data CiteMethod = Citeproc                        -- use citeproc to render them
                  | Natbib                        -- output natbib cite commands
                  | Biblatex                      -- output biblatex cite commands
                deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON CiteMethod where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON CiteMethod

-- | Methods for obfuscating email addresses in HTML.
data ObfuscationMethod = NoObfuscation
                       | ReferenceObfuscation
                       | JavascriptObfuscation
                       deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON ObfuscationMethod where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ObfuscationMethod

-- | Varieties of HTML slide shows.
data HTMLSlideVariant = S5Slides
                      | SlidySlides
                      | SlideousSlides
                      | DZSlides
                      | RevealJsSlides
                      | NoSlides
                      deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON HTMLSlideVariant where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON HTMLSlideVariant

-- | Options for accepting or rejecting MS Word track-changes.
data TrackChanges = AcceptChanges
                  | RejectChanges
                  | AllChanges
                  deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON TrackChanges where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON TrackChanges

-- | Options for wrapping text in the output.
data WrapOption = WrapAuto        -- ^ Automatically wrap to width
                | WrapNone        -- ^ No non-semantic newlines
                | WrapPreserve    -- ^ Preserve wrapping of input source
                deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON WrapOption where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON WrapOption

-- | Options defining the type of top-level headers.
data TopLevelDivision = TopLevelPart      -- ^ Top-level headers become parts
                      | TopLevelChapter   -- ^ Top-level headers become chapters
                      | TopLevelSection   -- ^ Top-level headers become sections
                      | TopLevelDefault   -- ^ Top-level type is determined via
                                          --   heuristics
                      deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON TopLevelDivision where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON TopLevelDivision

-- | Locations for footnotes and references in markdown output
data ReferenceLocation = EndOfBlock    -- ^ End of block
                       | EndOfSection  -- ^ prior to next section header (or end of document)
                       | EndOfDocument -- ^ at end of document
                       deriving (Show, Read, Eq, Data, Typeable, Generic)

instance ToJSON ReferenceLocation where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ReferenceLocation

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
  , writerSourceURL         :: Maybe String  -- ^ Absolute URL + directory of 1st source file
  , writerUserDataDir       :: Maybe FilePath -- ^ Path of user data directory
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
  , writerLaTeXArgs         :: [String]       -- ^ Flags to pass to latex-engine
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
                      , writerSourceURL        = Nothing
                      , writerUserDataDir      = Nothing
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
                      , writerLaTeXArgs        = []
                      , writerReferenceLocation = EndOfDocument
                      , writerSyntaxMap        = defaultSyntaxMap
                      }

-- | Returns True if the given extension is enabled.
isEnabled :: Extension -> WriterOptions -> Bool
isEnabled ext opts = ext `extensionEnabled` (writerExtensions opts)
