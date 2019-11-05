{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Text.Pandoc.Legacy.Options
  ( module Text.Pandoc.Legacy.Extensions
  , TP.ReaderOptions
  , pattern ReaderOptions
  , readerExtensions
  , readerStandalone
  , readerColumns
  , readerTabStop
  , readerIndentedCodeClasses
  , readerAbbreviations
  , readerDefaultImageExtension
  , readerTrackChanges
  , readerStripComments
  , TP.HTMLMathMethod ( TP.PlainMath, TP.GladTeX, TP.MathML )
  , pattern WebTeX
  , pattern MathJax
  , pattern KaTeX
  , TP.CiteMethod(..)
  , TP.ObfuscationMethod(..)
  , TP.HTMLSlideVariant (..)
  , TP.EPUBVersion (..)
  , TP.WrapOption (..)
  , TP.TopLevelDivision (..)
  , TP.WriterOptions
  , pattern WriterOptions
  , writerTemplate
  , writerVariables
  , writerTabStop
  , writerTableOfContents
  , writerIncremental
  , writerHTMLMathMethod
  , writerNumberSections
  , writerNumberOffset
  , writerSectionDivs
  , writerExtensions
  , writerReferenceLinks
  , writerDpi
  , writerWrapText
  , writerColumns
  , writerEmailObfuscation
  , writerIdentifierPrefix
  , writerCiteMethod
  , writerHtmlQTags
  , writerSlideLevel
  , writerTopLevelDivision
  , writerListings
  , writerHighlightStyle
  , writerSetextHeaders
  , writerEpubSubdirectory
  , writerEpubMetadata
  , writerEpubFonts
  , writerEpubChapterLevel
  , writerTOCDepth
  , writerReferenceDoc
  , writerReferenceLocation
  , writerSyntaxMap
  , writerPreferAscii
  , TP.TrackChanges (..)
  , TP.ReferenceLocation (..)
  , TP.def
  , TP.isEnabled
  , defaultMathJaxURL
  , defaultKaTeXURL
  ) where

import Text.Pandoc.Highlighting (Style)
import Skylighting (SyntaxMap)
import Text.DocTemplates (Template, Context)
import Data.Text (Text)
import qualified Data.Set as Set
import qualified Text.Pandoc.Options as TP
import qualified Data.Text as T
import Text.Pandoc.Legacy.Extensions

pattern ReaderOptions :: TP.Extensions -> Bool -> Int -> Int -> [String] -> Set.Set String -> String -> TP.TrackChanges -> Bool -> TP.ReaderOptions
pattern ReaderOptions {
         readerExtensions
       , readerStandalone
       , readerColumns
       , readerTabStop
       , readerIndentedCodeClasses
       , readerAbbreviations
       , readerDefaultImageExtension
       , readerTrackChanges
       , readerStripComments } <- TP.ReaderOptions readerExtensions readerStandalone readerColumns readerTabStop (map T.unpack -> readerIndentedCodeClasses) (Set.map T.unpack -> readerAbbreviations) (T.unpack -> readerDefaultImageExtension) readerTrackChanges readerStripComments
  where
    ReaderOptions a b c d e f = TP.ReaderOptions a b c d (map T.pack e) (Set.map T.pack f) . T.pack

pattern WebTeX :: String -> TP.HTMLMathMethod
pattern WebTeX x <- TP.WebTeX (T.unpack -> x)
  where
    WebTeX = TP.WebTeX . T.pack

pattern MathJax :: String -> TP.HTMLMathMethod
pattern MathJax x <- TP.MathJax (T.unpack -> x)
  where
    MathJax = TP.MathJax . T.pack

pattern KaTeX :: String -> TP.HTMLMathMethod
pattern KaTeX x <- TP.KaTeX (T.unpack -> x)
  where
    KaTeX = TP.KaTeX . T.pack

pattern WriterOptions
  :: Maybe (Template Text)
  -> Context Text
  -> Int
  -> Bool
  -> Bool
  -> TP.HTMLMathMethod
  -> Bool
  -> [Int]
  -> Bool
  -> TP.Extensions
  -> Bool
  -> Int
  -> TP.WrapOption
  -> Int
  -> TP.ObfuscationMethod
  -> String
  -> TP.CiteMethod
  -> Bool
  -> Maybe Int
  -> TP.TopLevelDivision
  -> Bool
  -> Maybe Style
  -> Bool
  -> String
  -> Maybe String
  -> [FilePath]
  -> Int
  -> Int
  -> Maybe FilePath
  -> TP.ReferenceLocation
  -> SyntaxMap
  -> Bool
  -> TP.WriterOptions
pattern WriterOptions
  { writerTemplate
  , writerVariables
  , writerTabStop
  , writerTableOfContents
  , writerIncremental
  , writerHTMLMathMethod
  , writerNumberSections
  , writerNumberOffset
  , writerSectionDivs
  , writerExtensions
  , writerReferenceLinks
  , writerDpi
  , writerWrapText
  , writerColumns
  , writerEmailObfuscation
  , writerIdentifierPrefix
  , writerCiteMethod
  , writerHtmlQTags
  , writerSlideLevel
  , writerTopLevelDivision
  , writerListings
  , writerHighlightStyle
  , writerSetextHeaders
  , writerEpubSubdirectory
  , writerEpubMetadata
  , writerEpubFonts
  , writerEpubChapterLevel
  , writerTOCDepth
  , writerReferenceDoc
  , writerReferenceLocation
  , writerSyntaxMap
  , writerPreferAscii } <- TP.WriterOptions
                           writerTemplate
                           writerVariables
                           writerTabStop
                           writerTableOfContents
                           writerIncremental
                           writerHTMLMathMethod
                           writerNumberSections
                           writerNumberOffset
                           writerSectionDivs
                           writerExtensions
                           writerReferenceLinks
                           writerDpi
                           writerWrapText
                           writerColumns
                           writerEmailObfuscation
                           (T.unpack -> writerIdentifierPrefix)
                           writerCiteMethod
                           writerHtmlQTags
                           writerSlideLevel
                           writerTopLevelDivision
                           writerListings
                           writerHighlightStyle
                           writerSetextHeaders
                           (T.unpack -> writerEpubSubdirectory)
                           (fmap T.unpack -> writerEpubMetadata)
                           writerEpubFonts
                           writerEpubChapterLevel
                           writerTOCDepth
                           writerReferenceDoc
                           writerReferenceLocation
                           writerSyntaxMap
                           writerPreferAscii
  where
    WriterOptions a b c d e f g h i j k l m n o p q r s t u v w x
      = TP.WriterOptions a b c d e f g h i j k l m n o (T.pack p) q r s t u v w (T.pack x) . fmap T.pack

defaultMathJaxURL :: String
defaultMathJaxURL = T.unpack TP.defaultMathJaxURL

defaultKaTeXURL :: String
defaultKaTeXURL = T.unpack TP.defaultKaTeXURL


