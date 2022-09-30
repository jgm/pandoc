{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{- |
   Module      : Text.Pandoc.Options
   Copyright   : Copyright (C) 2012-2022 John MacFarlane
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
                           , defaultMathJaxURL
                           , defaultKaTeXURL
                           ) where
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Data (Data)
import Data.Default
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Skylighting (SyntaxMap, defaultSyntaxMap)
import Text.DocTemplates (Context(..), Template)
import Text.Pandoc.Extensions
import Text.Pandoc.Highlighting (Style, pygments)
import Text.Pandoc.UTF8 (toStringLazy)
import Data.Aeson.TH (deriveJSON)
import Data.Aeson

class HasSyntaxExtensions a where
  getExtensions :: a -> Extensions

data ReaderOptions = ReaderOptions{
         readerExtensions            :: Extensions  -- ^ Syntax extensions
       , readerStandalone            :: Bool -- ^ Standalone document with header
       , readerColumns               :: Int  -- ^ Number of columns in terminal
       , readerTabStop               :: Int  -- ^ Tab stop
       , readerIndentedCodeClasses   :: [Text] -- ^ Default classes for
                                       -- indented code blocks
       , readerAbbreviations         :: Set.Set Text -- ^ Strings to treat as abbreviations
       , readerDefaultImageExtension :: Text -- ^ Default extension for images
       , readerTrackChanges          :: TrackChanges -- ^ Track changes setting for docx
       , readerStripComments         :: Bool -- ^ Strip HTML comments instead of parsing as raw HTML
                                             -- (only implemented in commonmark)
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

defaultAbbrevs :: Set.Set Text
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
                    | WebTeX Text               -- url of TeX->image script.
                    | GladTeX
                    | MathML
                    | MathJax Text              -- url of MathJax.js
                    | KaTeX Text                -- url of KaTeX files
                    deriving (Show, Read, Eq, Data, Typeable, Generic)

instance FromJSON HTMLMathMethod where
   parseJSON node =
     (withObject "HTMLMathMethod" $ \m -> do
        method <- m .: "method"
        mburl <- m .:? "url"
        case method :: Text of
          "plain" -> return PlainMath
          "webtex" -> return $ WebTeX $ fromMaybe "" mburl
          "gladtex" -> return GladTeX
          "mathml" -> return MathML
          "mathjax" -> return $ MathJax $
                         fromMaybe defaultMathJaxURL mburl
          "katex" -> return $ KaTeX $
                         fromMaybe defaultKaTeXURL mburl
          _ -> fail $ "Unknown HTML math method " ++ show method) node
       <|> (case node of
               String "plain" -> return PlainMath
               String "webtex" -> return $ WebTeX ""
               String "gladtex" -> return GladTeX
               String "mathml" -> return MathML
               String "mathjax" -> return $ MathJax defaultMathJaxURL
               String "katex" -> return $ KaTeX defaultKaTeXURL
               _ -> fail $ "Unknown HTML math method " <>
                             toStringLazy (encode node))

instance ToJSON HTMLMathMethod where
  toJSON PlainMath = String "plain"
  toJSON (WebTeX "") = String "webtex"
  toJSON (WebTeX url) = object ["method" .= String "webtex",
                                "url" .= String url]
  toJSON GladTeX = String "gladtex"
  toJSON MathML = String "mathml"
  toJSON (MathJax "") = String "mathjax"
  toJSON (MathJax url) = object ["method" .= String "mathjax",
                                 "url" .= String url]
  toJSON (KaTeX "") = String "katex"
  toJSON (KaTeX url) = object ["method" .= String "katex",
                               "url" .= String url]

data CiteMethod = Citeproc                        -- use citeproc to render them
                  | Natbib                        -- output natbib cite commands
                  | Biblatex                      -- output biblatex cite commands
                deriving (Show, Read, Eq, Data, Typeable, Generic)

instance FromJSON CiteMethod where
  parseJSON v =
    case v of
      String "citeproc" -> return Citeproc
      String "natbib"   -> return Natbib
      String "biblatex" -> return Biblatex
      _                 -> fail $ "Unknown citation method: " <>
                                    toStringLazy (encode v)

instance ToJSON CiteMethod where
  toJSON Citeproc = String "citeproc"
  toJSON Natbib = String "natbib"
  toJSON Biblatex = String "biblatex"

-- | Methods for obfuscating email addresses in HTML.
data ObfuscationMethod = NoObfuscation
                       | ReferenceObfuscation
                       | JavascriptObfuscation
                       deriving (Show, Read, Eq, Data, Typeable, Generic)

instance FromJSON ObfuscationMethod where
  parseJSON v =
    case v of
      String "none"       -> return NoObfuscation
      String "references" -> return ReferenceObfuscation
      String "javascript" -> return JavascriptObfuscation
      _ -> fail $ "Unknown obfuscation method " ++ toStringLazy (encode v)

instance ToJSON ObfuscationMethod where
   toJSON NoObfuscation = String "none"
   toJSON ReferenceObfuscation = String "references"
   toJSON JavascriptObfuscation = String "javascript"

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

-- update in doc/filters.md if this changes:
instance FromJSON TrackChanges where
  parseJSON v =
    case v of
      String "accept"     -> return AcceptChanges
      String "reject"     -> return RejectChanges
      String "all"        -> return AllChanges
      String "accept-changes" -> return AcceptChanges
      String "reject-changes" -> return RejectChanges
      String "all-changes"    -> return AllChanges
      _  -> fail $ "Unknown track changes method " <> toStringLazy (encode v)

instance ToJSON TrackChanges where
  toJSON AcceptChanges = String "accept-changes"
  toJSON RejectChanges = String "reject-changes"
  toJSON AllChanges = String "all-changes"

-- | Options for wrapping text in the output.
data WrapOption = WrapAuto        -- ^ Automatically wrap to width
                | WrapNone        -- ^ No non-semantic newlines
                | WrapPreserve    -- ^ Preserve wrapping of input source
                deriving (Show, Read, Eq, Data, Typeable, Generic)

instance FromJSON WrapOption where
  parseJSON v =
    case v of
      String "auto"      -> return WrapAuto
      String "wrap-auto" -> return WrapAuto
      String "none"      -> return WrapNone
      String "wrap-none" -> return WrapNone
      String "preserve"  -> return WrapPreserve
      String "wrap-preserve" -> return WrapPreserve
      _ -> fail $ "Unknown wrap method " <> toStringLazy (encode v)

instance ToJSON WrapOption where
  toJSON WrapAuto = "wrap-auto"
  toJSON WrapNone = "wrap-none"
  toJSON WrapPreserve = "wrap-preserve"

-- | Options defining the type of top-level headers.
data TopLevelDivision = TopLevelPart      -- ^ Top-level headers become parts
                      | TopLevelChapter   -- ^ Top-level headers become chapters
                      | TopLevelSection   -- ^ Top-level headers become sections
                      | TopLevelDefault   -- ^ Top-level type is determined via
                                          --   heuristics
                      deriving (Show, Read, Eq, Data, Typeable, Generic)

instance FromJSON TopLevelDivision where
  parseJSON v =
      case v of
        String "part"              -> return TopLevelPart
        String "top-level-part"    -> return TopLevelPart
        String "chapter"           -> return TopLevelChapter
        String "top-level-chapter" -> return TopLevelChapter
        String "section"           -> return TopLevelSection
        String "top-level-section" -> return TopLevelSection
        String "default"           -> return TopLevelDefault
        String "top-level-default" -> return TopLevelDefault
        _ -> fail $ "Unknown top level division " <> toStringLazy (encode v)

instance ToJSON TopLevelDivision where
  toJSON TopLevelPart = "top-level-part"
  toJSON TopLevelChapter = "top-level-chapter"
  toJSON TopLevelSection = "top-level-section"
  toJSON TopLevelDefault = "top-level-default"

-- | Locations for footnotes and references in markdown output
data ReferenceLocation = EndOfBlock    -- ^ End of block
                       | EndOfSection  -- ^ prior to next section header (or end of document)
                       | EndOfDocument -- ^ at end of document
                       deriving (Show, Read, Eq, Data, Typeable, Generic)

instance FromJSON ReferenceLocation where
  parseJSON v =
    case v of
      String "block"           -> return EndOfBlock
      String "end-of-block"    -> return EndOfBlock
      String "section"         -> return EndOfSection
      String "end-of-section"  -> return EndOfSection
      String "document"        -> return EndOfDocument
      String "end-of-document" -> return EndOfDocument
      _ -> fail $ "Unknown reference location " <> toStringLazy (encode v)

instance ToJSON ReferenceLocation where
   toJSON EndOfBlock = "end-of-block"
   toJSON EndOfSection = "end-of-section"
   toJSON EndOfDocument = "end-of-document"

-- | Options for writers
data WriterOptions = WriterOptions
  { writerTemplate          :: Maybe (Template Text) -- ^ Template to use
  , writerVariables         :: Context Text -- ^ Variables to set in template
  , writerTabStop           :: Int    -- ^ Tabstop for conversion btw spaces and tabs
  , writerTableOfContents   :: Bool   -- ^ Include table of contents
  , writerIncremental       :: Bool   -- ^ True if lists should be incremental
  , writerHTMLMathMethod    :: HTMLMathMethod  -- ^ How to print math in HTML
  , writerNumberSections    :: Bool   -- ^ Number sections in LaTeX
  , writerNumberOffset      :: [Int]  -- ^ Starting number for section, subsection, ...
  , writerSectionDivs       :: Bool   -- ^ Put sections in div tags in HTML
  , writerExtensions        :: Extensions -- ^ Markdown extensions that can be used
  , writerReferenceLinks    :: Bool   -- ^ Use reference links in writing markdown, rst
  , writerDpi               :: Int    -- ^ Dpi for pixel to\/from inch\/cm conversions
  , writerWrapText          :: WrapOption  -- ^ Option for wrapping text
  , writerColumns           :: Int    -- ^ Characters in a line (for text wrapping)
  , writerEmailObfuscation  :: ObfuscationMethod -- ^ How to obfuscate emails
  , writerIdentifierPrefix  :: Text -- ^ Prefix for section & note ids in HTML
                                     -- and for footnote marks in markdown
  , writerCiteMethod        :: CiteMethod -- ^ How to print cites
  , writerHtmlQTags         :: Bool       -- ^ Use @<q>@ tags for quotes in HTML
  , writerSlideLevel        :: Maybe Int  -- ^ Force header level of slides
  , writerTopLevelDivision  :: TopLevelDivision -- ^ Type of top-level divisions
  , writerListings          :: Bool       -- ^ Use listings package for code
  , writerHighlightStyle    :: Maybe Style  -- ^ Style to use for highlighting
                                           -- (Nothing = no highlighting)
  , writerSetextHeaders     :: Bool       -- ^ Use setext headers for levels 1-2 in markdown
  , writerListTables        :: Bool       -- ^ Use list tables for RST tables
  , writerEpubSubdirectory  :: Text       -- ^ Subdir for epub in OCF
  , writerEpubMetadata      :: Maybe Text -- ^ Metadata to include in EPUB
  , writerEpubFonts         :: [FilePath] -- ^ Paths to fonts to embed
  , writerEpubChapterLevel  :: Int            -- ^ Header level for chapters (separate files)
  , writerTOCDepth          :: Int            -- ^ Number of levels to include in TOC
  , writerReferenceDoc      :: Maybe FilePath -- ^ Path to reference document if specified
  , writerReferenceLocation :: ReferenceLocation    -- ^ Location of footnotes and references for writing markdown
  , writerSyntaxMap         :: SyntaxMap
  , writerPreferAscii       :: Bool           -- ^ Prefer ASCII representations of characters when possible
  } deriving (Show, Data, Typeable, Generic)

instance Default WriterOptions where
  def = WriterOptions { writerTemplate         = Nothing
                      , writerVariables        = mempty
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
                      , writerSetextHeaders    = False
                      , writerListTables       = False
                      , writerEpubSubdirectory = "EPUB"
                      , writerEpubMetadata     = Nothing
                      , writerEpubFonts        = []
                      , writerEpubChapterLevel = 1
                      , writerTOCDepth         = 3
                      , writerReferenceDoc     = Nothing
                      , writerReferenceLocation = EndOfDocument
                      , writerSyntaxMap        = defaultSyntaxMap
                      , writerPreferAscii      = False
                      }

instance HasSyntaxExtensions WriterOptions where
  getExtensions opts = writerExtensions opts

-- | Returns True if the given extension is enabled.
isEnabled :: HasSyntaxExtensions a => Extension -> a -> Bool
isEnabled ext opts = ext `extensionEnabled` getExtensions opts

defaultMathJaxURL :: Text
defaultMathJaxURL = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml-full.js"

defaultKaTeXURL :: Text
defaultKaTeXURL = "https://cdn.jsdelivr.net/npm/katex@0.15.1/dist/"

-- Update documentation in doc/filters.md if this is changed.
$(deriveJSON defaultOptions{ fieldLabelModifier =
                               camelTo2 '-' . drop 6 }
                            ''ReaderOptions)

$(deriveJSON defaultOptions{ constructorTagModifier = map toLower }
  ''HTMLSlideVariant)
