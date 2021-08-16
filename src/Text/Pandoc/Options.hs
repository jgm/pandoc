{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{- |
   Module      : Text.Pandoc.Options
   Copyright   : Copyright (C) 2012-2021 John MacFarlane
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
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Data (Data)
import Data.Default
import Data.Text (Text)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Skylighting (SyntaxMap, defaultSyntaxMap)
import Text.DocTemplates (Context(..), Template)
import Text.Pandoc.Extensions
import Text.Pandoc.Highlighting (Style, pygments)
import Text.Pandoc.Shared (camelCaseStrToHyphenated)
import Data.Aeson.TH (deriveJSON, defaultOptions, Options(..),
                      SumEncoding(..))
import Data.YAML

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

instance FromYAML HTMLMathMethod where
   parseYAML node =
     (withMap "HTMLMathMethod" $ \m -> do
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
       <|> (withStr "HTMLMathMethod" $ \method ->
             case method of
               "plain" -> return PlainMath
               "webtex" -> return $ WebTeX ""
               "gladtex" -> return GladTeX
               "mathml" -> return MathML
               "mathjax" -> return $ MathJax defaultMathJaxURL
               "katex" -> return $ KaTeX defaultKaTeXURL
               _  -> fail $ "Unknown HTML math method " ++ show method) node

data CiteMethod = Citeproc                        -- use citeproc to render them
                  | Natbib                        -- output natbib cite commands
                  | Biblatex                      -- output biblatex cite commands
                deriving (Show, Read, Eq, Data, Typeable, Generic)

instance FromYAML CiteMethod where
  parseYAML = withStr "Citeproc" $ \t ->
    case t of
      "citeproc" -> return Citeproc
      "natbib"   -> return Natbib
      "biblatex" -> return Biblatex
      _          -> fail $ "Unknown citation method " ++ show t

-- | Methods for obfuscating email addresses in HTML.
data ObfuscationMethod = NoObfuscation
                       | ReferenceObfuscation
                       | JavascriptObfuscation
                       deriving (Show, Read, Eq, Data, Typeable, Generic)

instance FromYAML ObfuscationMethod where
  parseYAML = withStr "Citeproc" $ \t ->
    case t of
      "none"       -> return NoObfuscation
      "references" -> return ReferenceObfuscation
      "javascript" -> return JavascriptObfuscation
      _            -> fail $ "Unknown obfuscation method " ++ show t

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

instance FromYAML TrackChanges where
  parseYAML = withStr "TrackChanges" $ \t ->
    case t of
      "accept"     -> return AcceptChanges
      "reject"     -> return RejectChanges
      "all"        -> return AllChanges
      _            -> fail $ "Unknown track changes method " ++ show t

-- | Options for wrapping text in the output.
data WrapOption = WrapAuto        -- ^ Automatically wrap to width
                | WrapNone        -- ^ No non-semantic newlines
                | WrapPreserve    -- ^ Preserve wrapping of input source
                deriving (Show, Read, Eq, Data, Typeable, Generic)

instance FromYAML WrapOption where
  parseYAML = withStr "WrapOption" $ \t ->
    case t of
      "auto"     -> return WrapAuto
      "none"     -> return WrapNone
      "preserve" -> return WrapPreserve
      _          -> fail $ "Unknown wrap method " ++ show t


-- | Options defining the type of top-level headers.
data TopLevelDivision = TopLevelPart      -- ^ Top-level headers become parts
                      | TopLevelChapter   -- ^ Top-level headers become chapters
                      | TopLevelSection   -- ^ Top-level headers become sections
                      | TopLevelDefault   -- ^ Top-level type is determined via
                                          --   heuristics
                      deriving (Show, Read, Eq, Data, Typeable, Generic)

instance FromYAML TopLevelDivision where
  parseYAML = withStr "TopLevelDivision" $ \t ->
    case t of
      "part"     -> return TopLevelPart
      "chapter"  -> return TopLevelChapter
      "section"  -> return TopLevelSection
      "default"  -> return TopLevelDefault
      _          -> fail $ "Unknown top level division " ++ show t


-- | Locations for footnotes and references in markdown output
data ReferenceLocation = EndOfBlock    -- ^ End of block
                       | EndOfSection  -- ^ prior to next section header (or end of document)
                       | EndOfDocument -- ^ at end of document
                       deriving (Show, Read, Eq, Data, Typeable, Generic)

instance FromYAML ReferenceLocation where
  parseYAML = withStr "ReferenceLocation" $ \t ->
    case t of
      "block"    -> return EndOfBlock
      "section"  -> return EndOfSection
      "document" -> return EndOfDocument
      _          -> fail $ "Unknown reference location " ++ show t


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
defaultKaTeXURL = "https://cdnjs.cloudflare.com/ajax/libs/KaTeX/0.11.1/"

-- Update documentation in doc/filters.md if this is changed.
$(deriveJSON defaultOptions{ constructorTagModifier =
                               camelCaseStrToHyphenated
                           } ''TrackChanges)

$(deriveJSON defaultOptions{ constructorTagModifier =
                               camelCaseStrToHyphenated
                           } ''WrapOption)

$(deriveJSON defaultOptions{ constructorTagModifier =
                               camelCaseStrToHyphenated . drop 8
                           } ''TopLevelDivision)

$(deriveJSON defaultOptions{ constructorTagModifier =
                               camelCaseStrToHyphenated
                           } ''ReferenceLocation)

-- Update documentation in doc/filters.md if this is changed.
$(deriveJSON defaultOptions ''ReaderOptions)

$(deriveJSON defaultOptions{
   constructorTagModifier = map toLower,
   sumEncoding = TaggedObject{
                    tagFieldName = "method",
                    contentsFieldName = "url" }
                           } ''HTMLMathMethod)

$(deriveJSON defaultOptions{ constructorTagModifier =
                               camelCaseStrToHyphenated
                           } ''CiteMethod)

$(deriveJSON defaultOptions{ constructorTagModifier =
                            \case
                                    "NoObfuscation"         -> "none"
                                    "ReferenceObfuscation"  -> "references"
                                    "JavascriptObfuscation" -> "javascript"
                                    _                       -> "none"
                           } ''ObfuscationMethod)

$(deriveJSON defaultOptions ''HTMLSlideVariant)
