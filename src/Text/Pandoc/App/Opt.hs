{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveGeneric       #-}
#ifdef DERIVE_JSON_VIA_TH
{-# LANGUAGE TemplateHaskell     #-}
#endif
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
   Module      : Text.Pandoc.App.Opt
   Copyright   : Copyright (C) 2006-2018 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Options for pandoc when used as an app.
-}
module Text.Pandoc.App.Opt (
            Opt(..)
          , LineEnding (..)
          , defaultOpts
          ) where
import Prelude
import GHC.Generics
import Text.Pandoc.Filter (Filter (..))
import Text.Pandoc.Highlighting (Style, pygments)
import Text.Pandoc.Logging (Verbosity (WARNING))
import Text.Pandoc.Options (TopLevelDivision (TopLevelDefault),
                            TrackChanges (AcceptChanges),
                            WrapOption (WrapAuto), HTMLMathMethod (PlainMath),
                            ReferenceLocation (EndOfDocument),
                            ObfuscationMethod (NoObfuscation),
                            CiteMethod (Citeproc))

#ifdef DERIVE_JSON_VIA_TH
import Data.Aeson.TH (deriveJSON, defaultOptions)
#else
import Data.Aeson (FromJSON (..), ToJSON (..),
                   defaultOptions, genericToEncoding)
#endif

-- | The type of line-endings to be used when writing plain-text.
data LineEnding = LF | CRLF | Native deriving (Show, Generic)

-- | Data structure for command line options.
data Opt = Opt
    { optTabStop               :: Int     -- ^ Number of spaces per tab
    , optPreserveTabs          :: Bool    -- ^ Preserve tabs instead of converting to spaces
    , optStandalone            :: Bool    -- ^ Include header, footer
    , optReader                :: Maybe String  -- ^ Reader format
    , optWriter                :: Maybe String  -- ^ Writer format
    , optTableOfContents       :: Bool    -- ^ Include table of contents
    , optBaseHeaderLevel       :: Int     -- ^ Base header level
    , optTemplate              :: Maybe FilePath  -- ^ Custom template
    , optVariables             :: [(String,String)] -- ^ Template variables to set
    , optMetadata              :: [(String, String)] -- ^ Metadata fields to set
    , optMetadataFile          :: Maybe FilePath  -- ^ Name of YAML metadata file
    , optOutputFile            :: Maybe FilePath  -- ^ Name of output file
    , optInputFiles            :: [FilePath] -- ^ Names of input files
    , optNumberSections        :: Bool    -- ^ Number sections in LaTeX
    , optNumberOffset          :: [Int]   -- ^ Starting number for sections
    , optSectionDivs           :: Bool    -- ^ Put sections in div tags in HTML
    , optIncremental           :: Bool    -- ^ Use incremental lists in Slidy/Slideous/S5
    , optSelfContained         :: Bool    -- ^ Make HTML accessible offline
    , optHtmlQTags             :: Bool    -- ^ Use <q> tags in HTML
    , optHighlightStyle        :: Maybe Style -- ^ Style to use for highlighted code
    , optSyntaxDefinitions     :: [FilePath]  -- ^ xml syntax defs to load
    , optTopLevelDivision      :: TopLevelDivision -- ^ Type of the top-level divisions
    , optHTMLMathMethod        :: HTMLMathMethod -- ^ Method to print HTML math
    , optAbbreviations         :: Maybe FilePath -- ^ Path to abbrevs file
    , optReferenceDoc          :: Maybe FilePath -- ^ Path of reference doc
    , optEpubSubdirectory      :: String -- ^ EPUB subdir in OCF container
    , optEpubMetadata          :: Maybe FilePath   -- ^ EPUB metadata
    , optEpubFonts             :: [FilePath] -- ^ EPUB fonts to embed
    , optEpubChapterLevel      :: Int     -- ^ Header level at which to split chapters
    , optEpubCoverImage        :: Maybe FilePath -- ^ Cover image for epub
    , optTOCDepth              :: Int     -- ^ Number of levels to include in TOC
    , optDumpArgs              :: Bool    -- ^ Output command-line arguments
    , optIgnoreArgs            :: Bool    -- ^ Ignore command-line arguments
    , optVerbosity             :: Verbosity  -- ^ Verbosity of diagnostic output
    , optTrace                 :: Bool  -- ^ Enable tracing
    , optLogFile               :: Maybe FilePath -- ^ File to write JSON log output
    , optFailIfWarnings        :: Bool    -- ^ Fail on warnings
    , optReferenceLinks        :: Bool    -- ^ Use reference links in writing markdown, rst
    , optReferenceLocation     :: ReferenceLocation -- ^ location for footnotes and link references in markdown output
    , optDpi                   :: Int     -- ^ Dpi
    , optWrapText              :: WrapOption  -- ^ Options for wrapping text
    , optColumns               :: Int     -- ^ Line length in characters
    , optFilters               :: [Filter] -- ^ Filters to apply
    , optEmailObfuscation      :: ObfuscationMethod
    , optIdentifierPrefix      :: String
    , optStripEmptyParagraphs  :: Bool -- ^ Strip empty paragraphs
    , optIndentedCodeClasses   :: [String] -- ^ Default classes for indented code blocks
    , optDataDir               :: Maybe FilePath
    , optCiteMethod            :: CiteMethod -- ^ Method to output cites
    , optListings              :: Bool       -- ^ Use listings package for code blocks
    , optPdfEngine             :: Maybe String -- ^ Program to use for latex/html -> pdf
    , optPdfEngineArgs         :: [String]   -- ^ Flags to pass to the engine
    , optSlideLevel            :: Maybe Int  -- ^ Header level that creates slides
    , optSetextHeaders         :: Bool       -- ^ Use atx headers for markdown level 1-2
    , optAscii                 :: Bool       -- ^ Prefer ascii output
    , optDefaultImageExtension :: String -- ^ Default image extension
    , optExtractMedia          :: Maybe FilePath -- ^ Path to extract embedded media
    , optTrackChanges          :: TrackChanges -- ^ Accept or reject MS Word track-changes.
    , optFileScope             :: Bool         -- ^ Parse input files before combining
    , optTitlePrefix           :: Maybe String     -- ^ Prefix for title
    , optCss                   :: [FilePath]       -- ^ CSS files to link to
    , optIncludeBeforeBody     :: [FilePath]       -- ^ Files to include before
    , optIncludeAfterBody      :: [FilePath]       -- ^ Files to include after body
    , optIncludeInHeader       :: [FilePath]       -- ^ Files to include in header
    , optResourcePath          :: [FilePath] -- ^ Path to search for images etc
    , optRequestHeaders        :: [(String, String)] -- ^ Headers for HTTP requests
    , optEol                   :: LineEnding -- ^ Style of line-endings to use
    , optStripComments         :: Bool       -- ^ Skip HTML comments
    } deriving (Generic, Show)

-- | Defaults for command-line options.
defaultOpts :: Opt
defaultOpts = Opt
    { optTabStop               = 4
    , optPreserveTabs          = False
    , optStandalone            = False
    , optReader                = Nothing
    , optWriter                = Nothing
    , optTableOfContents       = False
    , optBaseHeaderLevel       = 1
    , optTemplate              = Nothing
    , optVariables             = []
    , optMetadata              = []
    , optMetadataFile          = Nothing
    , optOutputFile            = Nothing
    , optInputFiles            = []
    , optNumberSections        = False
    , optNumberOffset          = [0,0,0,0,0,0]
    , optSectionDivs           = False
    , optIncremental           = False
    , optSelfContained         = False
    , optHtmlQTags             = False
    , optHighlightStyle        = Just pygments
    , optSyntaxDefinitions     = []
    , optTopLevelDivision      = TopLevelDefault
    , optHTMLMathMethod        = PlainMath
    , optAbbreviations         = Nothing
    , optReferenceDoc          = Nothing
    , optEpubSubdirectory      = "EPUB"
    , optEpubMetadata          = Nothing
    , optEpubFonts             = []
    , optEpubChapterLevel      = 1
    , optEpubCoverImage        = Nothing
    , optTOCDepth              = 3
    , optDumpArgs              = False
    , optIgnoreArgs            = False
    , optVerbosity             = WARNING
    , optTrace                 = False
    , optLogFile               = Nothing
    , optFailIfWarnings        = False
    , optReferenceLinks        = False
    , optReferenceLocation     = EndOfDocument
    , optDpi                   = 96
    , optWrapText              = WrapAuto
    , optColumns               = 72
    , optFilters               = []
    , optEmailObfuscation      = NoObfuscation
    , optIdentifierPrefix      = ""
    , optStripEmptyParagraphs  = False
    , optIndentedCodeClasses   = []
    , optDataDir               = Nothing
    , optCiteMethod            = Citeproc
    , optListings              = False
    , optPdfEngine             = Nothing
    , optPdfEngineArgs         = []
    , optSlideLevel            = Nothing
    , optSetextHeaders         = True
    , optAscii                 = False
    , optDefaultImageExtension = ""
    , optExtractMedia          = Nothing
    , optTrackChanges          = AcceptChanges
    , optFileScope             = False
    , optTitlePrefix           = Nothing
    , optCss                   = []
    , optIncludeBeforeBody     = []
    , optIncludeAfterBody      = []
    , optIncludeInHeader       = []
    , optResourcePath          = ["."]
    , optRequestHeaders        = []
    , optEol                   = Native
    , optStripComments          = False
    }

#ifdef DERIVE_JSON_VIA_TH
-- see https://github.com/jgm/pandoc/pull/4083
-- using generic deriving caused long compilation times
$(deriveJSON defaultOptions ''LineEnding)
$(deriveJSON defaultOptions ''Opt)
#else
instance ToJSON LineEnding where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON LineEnding

instance ToJSON Opt where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Opt
#endif
