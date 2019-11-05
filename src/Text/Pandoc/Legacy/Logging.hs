{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Text.Pandoc.Legacy.Logging (
    TP.Verbosity(..)
  , TP.LogMessage(TP.InlineNotRendered, TP.BlockNotRendered, TP.NoLangSpecified)
  , pattern SkippedContent
  , pattern IgnoredElement
  , pattern CouldNotParseYamlMetadata
  , pattern DuplicateLinkReference
  , pattern DuplicateNoteReference
  , pattern NoteDefinedButNotUsed
  , pattern DuplicateIdentifier
  , pattern ReferenceNotFound
  , pattern CircularReference
  , pattern UndefinedToggle
  , pattern ParsingUnescaped
  , pattern CouldNotLoadIncludeFile
  , pattern MacroAlreadyDefined
  , pattern DocxParserWarning
  , pattern IgnoredIOError
  , pattern CouldNotFetchResource
  , pattern CouldNotDetermineImageSize
  , pattern CouldNotConvertImage
  , pattern CouldNotDetermineMimeType
  , pattern CouldNotConvertTeXMath
  , pattern CouldNotParseCSS
  , pattern Fetching
  , pattern Extracting
  , pattern NoTitleElement
  , pattern InvalidLang
  , pattern CouldNotHighlight
  , pattern MissingCharacter
  , pattern Deprecated
  , pattern NoTranslation
  , pattern CouldNotLoadTranslations
  , pattern UnusualConversion
  , pattern UnexpectedXmlElement
  , pattern UnknownOrgExportOption
  , pattern CouldNotDeduceFormat
  , TP.encodeLogMessages
  , showLogMessage
  , TP.messageVerbosity
  ) where

import qualified Data.Text as T
import qualified Text.Pandoc.Logging as TP
import qualified Text.Parsec as P

on1 :: (T.Text -> a) -> String -> a
on1 f = f . T.pack

on2 :: (T.Text -> T.Text -> a) -> String -> String -> a
on2 f x = on1 $ on1 f x

pattern SkippedContent :: String -> P.SourcePos -> TP.LogMessage
pattern SkippedContent x y <- TP.SkippedContent (T.unpack -> x) y
  where
    SkippedContent = on1 TP.SkippedContent

pattern IgnoredElement :: String -> TP.LogMessage
pattern IgnoredElement x <- TP.IgnoredElement (T.unpack -> x)
  where
    IgnoredElement = on1 TP.IgnoredElement

pattern CouldNotParseYamlMetadata :: String -> P.SourcePos -> TP.LogMessage
pattern CouldNotParseYamlMetadata x y <- TP.CouldNotParseYamlMetadata (T.unpack -> x) y
  where
    CouldNotParseYamlMetadata = on1 TP.CouldNotParseYamlMetadata

pattern DuplicateLinkReference :: String -> P.SourcePos -> TP.LogMessage
pattern DuplicateLinkReference x y <- TP.DuplicateLinkReference (T.unpack -> x) y
  where
    DuplicateLinkReference = on1 TP.DuplicateLinkReference

pattern DuplicateNoteReference :: String -> P.SourcePos -> TP.LogMessage
pattern DuplicateNoteReference x y <- TP.DuplicateNoteReference (T.unpack -> x) y
  where
    DuplicateNoteReference = on1 TP.DuplicateNoteReference

pattern NoteDefinedButNotUsed :: String -> P.SourcePos -> TP.LogMessage
pattern NoteDefinedButNotUsed x y <- TP.NoteDefinedButNotUsed (T.unpack -> x) y
  where
    NoteDefinedButNotUsed = on1 TP.NoteDefinedButNotUsed

pattern DuplicateIdentifier :: String -> P.SourcePos -> TP.LogMessage
pattern DuplicateIdentifier x y <- TP.DuplicateIdentifier (T.unpack -> x) y
  where
    DuplicateIdentifier = on1 TP.DuplicateIdentifier

pattern ReferenceNotFound :: String -> P.SourcePos -> TP.LogMessage
pattern ReferenceNotFound x y <- TP.ReferenceNotFound (T.unpack -> x) y
  where
    ReferenceNotFound = on1 TP.ReferenceNotFound

pattern CircularReference :: String -> P.SourcePos -> TP.LogMessage
pattern CircularReference x y <- TP.CircularReference (T.unpack -> x) y
  where
    CircularReference = on1 TP.CircularReference

pattern UndefinedToggle :: String -> P.SourcePos -> TP.LogMessage
pattern UndefinedToggle x y <- TP.UndefinedToggle (T.unpack -> x) y
  where
    UndefinedToggle = on1 TP.UndefinedToggle

pattern ParsingUnescaped :: String -> P.SourcePos -> TP.LogMessage
pattern ParsingUnescaped x y <- TP.ParsingUnescaped (T.unpack -> x) y
  where
    ParsingUnescaped = on1 TP.ParsingUnescaped

pattern CouldNotLoadIncludeFile :: String -> P.SourcePos -> TP.LogMessage
pattern CouldNotLoadIncludeFile x y <- TP.CouldNotLoadIncludeFile (T.unpack -> x) y
  where
    CouldNotLoadIncludeFile = on1 TP.CouldNotLoadIncludeFile

pattern MacroAlreadyDefined :: String -> P.SourcePos -> TP.LogMessage
pattern MacroAlreadyDefined x y <- TP.MacroAlreadyDefined (T.unpack -> x) y
  where
    MacroAlreadyDefined = on1 TP.MacroAlreadyDefined

pattern DocxParserWarning :: String -> TP.LogMessage
pattern DocxParserWarning x <- TP.DocxParserWarning (T.unpack -> x)
  where
    DocxParserWarning = on1 TP.DocxParserWarning

pattern IgnoredIOError :: String -> TP.LogMessage
pattern IgnoredIOError x <- TP.IgnoredIOError (T.unpack -> x)
  where
    IgnoredIOError = on1 TP.IgnoredIOError

pattern CouldNotFetchResource :: String -> String -> TP.LogMessage
pattern CouldNotFetchResource x y <- TP.CouldNotFetchResource (T.unpack -> x) (T.unpack -> y)
  where
    CouldNotFetchResource = on2 TP.CouldNotFetchResource

pattern CouldNotDetermineImageSize :: String -> String -> TP.LogMessage
pattern CouldNotDetermineImageSize x y <- TP.CouldNotDetermineImageSize (T.unpack -> x) (T.unpack -> y)
  where
    CouldNotDetermineImageSize = on2 TP.CouldNotDetermineImageSize

pattern CouldNotConvertImage :: String -> String -> TP.LogMessage
pattern CouldNotConvertImage x y <- TP.CouldNotConvertImage (T.unpack -> x) (T.unpack -> y)
  where
    CouldNotConvertImage = on2 TP.CouldNotConvertImage

pattern CouldNotDetermineMimeType :: String -> TP.LogMessage
pattern CouldNotDetermineMimeType x <- TP.CouldNotDetermineMimeType (T.unpack -> x)
  where
    CouldNotDetermineMimeType = on1 TP.CouldNotDetermineMimeType

pattern CouldNotConvertTeXMath :: String -> String -> TP.LogMessage
pattern CouldNotConvertTeXMath x y <- TP.CouldNotConvertTeXMath (T.unpack -> x) (T.unpack -> y)
  where
    CouldNotConvertTeXMath = on2 TP.CouldNotConvertTeXMath

pattern CouldNotParseCSS :: String -> TP.LogMessage
pattern CouldNotParseCSS x <- TP.CouldNotParseCSS (T.unpack -> x)
  where
    CouldNotParseCSS = on1 TP.CouldNotParseCSS

pattern Fetching :: String -> TP.LogMessage
pattern Fetching x <- TP.Fetching (T.unpack -> x)
  where
    Fetching = on1 TP.Fetching

pattern Extracting :: String -> TP.LogMessage
pattern Extracting x <- TP.Extracting (T.unpack -> x)
  where
    Extracting = on1 TP.Extracting

pattern NoTitleElement :: String -> TP.LogMessage
pattern NoTitleElement x <- TP.NoTitleElement (T.unpack -> x)
  where
    NoTitleElement = on1 TP.NoTitleElement

pattern InvalidLang :: String -> TP.LogMessage
pattern InvalidLang x <- TP.InvalidLang (T.unpack -> x)
  where
    InvalidLang = on1 TP.InvalidLang

pattern CouldNotHighlight :: String -> TP.LogMessage
pattern CouldNotHighlight x <- TP.CouldNotHighlight (T.unpack -> x)
  where
    CouldNotHighlight = on1 TP.CouldNotHighlight

pattern MissingCharacter :: String -> TP.LogMessage
pattern MissingCharacter x <- TP.MissingCharacter (T.unpack -> x)
  where
    MissingCharacter = on1 TP.MissingCharacter

pattern Deprecated :: String -> String -> TP.LogMessage
pattern Deprecated x y <- TP.Deprecated (T.unpack -> x) (T.unpack -> y)
  where
    Deprecated = on2 TP.Deprecated

pattern NoTranslation :: String -> TP.LogMessage
pattern NoTranslation x <- TP.NoTranslation (T.unpack -> x)
  where
    NoTranslation = on1 TP.NoTranslation

pattern CouldNotLoadTranslations :: String -> String -> TP.LogMessage
pattern CouldNotLoadTranslations x y <- TP.CouldNotLoadTranslations (T.unpack -> x) (T.unpack -> y)
  where
    CouldNotLoadTranslations = on2 TP.CouldNotLoadTranslations

pattern UnusualConversion :: String -> TP.LogMessage
pattern UnusualConversion x <- TP.UnusualConversion (T.unpack -> x)
  where
    UnusualConversion = on1 TP.UnusualConversion

pattern UnexpectedXmlElement :: String -> String -> TP.LogMessage
pattern UnexpectedXmlElement x y <- TP.UnexpectedXmlElement (T.unpack -> x) (T.unpack -> y)
  where
    UnexpectedXmlElement = on2 TP.UnexpectedXmlElement

pattern UnknownOrgExportOption :: String -> TP.LogMessage
pattern UnknownOrgExportOption x <- TP.UnknownOrgExportOption (T.unpack -> x)
  where
    UnknownOrgExportOption = on1 TP.UnknownOrgExportOption

pattern CouldNotDeduceFormat :: [String] -> String -> TP.LogMessage
pattern CouldNotDeduceFormat x y <- TP.CouldNotDeduceFormat (map T.unpack -> x) (T.unpack -> y)
  where
    CouldNotDeduceFormat a = on1 $ TP.CouldNotDeduceFormat (map T.pack a)

showLogMessage :: TP.LogMessage -> String
showLogMessage = T.unpack . TP.showLogMessage
