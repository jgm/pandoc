{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{- |
   Module      : Text.Pandoc.Logging
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

This module provides data types and functions for warnings
and info messages.

-}
module Text.Pandoc.Logging (
    Verbosity(..)
  , LogMessage(..)
  , encodeLogMessages
  , showLogMessage
  , messageVerbosity
  ) where

import Control.Monad (mzero)
import Data.YAML (withStr, FromYAML(..))
import Data.Aeson
import Data.Aeson.Encode.Pretty (Config (..), defConfig, encodePretty',
                                 keyOrder)
import qualified Data.ByteString.Lazy as BL
import Data.Data (Data, toConstr)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Pandoc.Definition
import Text.Parsec.Pos

-- | Verbosity level.
data Verbosity = ERROR | WARNING | INFO
     deriving (Show, Read, Eq, Data, Enum, Ord, Bounded, Typeable, Generic)

instance ToJSON Verbosity where
  toJSON x = toJSON (show x)
instance FromJSON Verbosity where
  parseJSON (String t) =
    case t of
         "ERROR"   -> return ERROR
         "WARNING" -> return WARNING
         "INFO"    -> return INFO
         _         -> mzero
  parseJSON _      =  mzero

instance FromYAML Verbosity where
  parseYAML = withStr "Verbosity" $ \t ->
    case t of
         "ERROR"   -> return ERROR
         "WARNING" -> return WARNING
         "INFO"    -> return INFO
         _         -> mzero

data LogMessage =
    SkippedContent Text.Text SourcePos
  | IgnoredElement Text.Text
  | CouldNotParseYamlMetadata Text.Text SourcePos
  | DuplicateLinkReference Text.Text SourcePos
  | DuplicateNoteReference Text.Text SourcePos
  | NoteDefinedButNotUsed Text.Text SourcePos
  | DuplicateIdentifier Text.Text SourcePos
  | ReferenceNotFound Text.Text SourcePos
  | CircularReference Text.Text SourcePos
  | UndefinedToggle Text.Text SourcePos
  | ParsingUnescaped Text.Text SourcePos
  | CouldNotLoadIncludeFile Text.Text SourcePos
  | MacroAlreadyDefined Text.Text SourcePos
  | InlineNotRendered Inline
  | BlockNotRendered Block
  | DocxParserWarning Text.Text
  | IgnoredIOError Text.Text
  | CouldNotFetchResource Text.Text Text.Text
  | CouldNotDetermineImageSize Text.Text Text.Text
  | CouldNotConvertImage Text.Text Text.Text
  | CouldNotDetermineMimeType Text.Text
  | CouldNotConvertTeXMath Text.Text Text.Text
  | CouldNotParseCSS Text.Text
  | Fetching Text.Text
  | Extracting Text.Text
  | NoTitleElement Text.Text
  | NoLangSpecified
  | InvalidLang Text.Text
  | CouldNotHighlight Text.Text
  | MissingCharacter Text.Text
  | Deprecated Text.Text Text.Text
  | NoTranslation Text.Text
  | CouldNotLoadTranslations Text.Text Text.Text
  | UnusualConversion Text.Text
  | UnexpectedXmlElement Text.Text Text.Text
  | UnknownOrgExportOption Text.Text
  | CouldNotDeduceFormat [Text.Text] Text.Text
  | RunningFilter FilePath
  | FilterCompleted FilePath Integer
  deriving (Show, Eq, Data, Ord, Typeable, Generic)

instance ToJSON LogMessage where
  toJSON x = object $
    "verbosity" .= toJSON (messageVerbosity x) :
    "type" .= toJSON (show $ toConstr x) :
    case x of
      SkippedContent s pos ->
           ["contents" .= s,
            "source" .= sourceName pos,
            "line" .= sourceLine pos,
            "column" .= sourceColumn pos]
      IgnoredElement s ->
           ["contents" .= s]
      CouldNotParseYamlMetadata s pos ->
           ["message" .= s,
            "source" .= sourceName pos,
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      DuplicateLinkReference s pos ->
           ["contents" .= s,
            "source" .= sourceName pos,
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      NoteDefinedButNotUsed s pos ->
           ["key" .= s,
            "source" .= sourceName pos,
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      DuplicateNoteReference s pos ->
           ["contents" .= s,
            "source" .= sourceName pos,
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      DuplicateIdentifier s pos ->
           ["contents" .= s,
            "source" .= sourceName pos,
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      ReferenceNotFound s pos ->
           ["contents" .= s,
            "source" .= sourceName pos,
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      CircularReference s pos ->
           ["contents" .= s,
            "source" .= sourceName pos,
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      UndefinedToggle s pos ->
           ["contents" .= s,
            "source" .= sourceName pos,
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      ParsingUnescaped s pos ->
           ["contents" .= s,
            "source" .= sourceName pos,
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      CouldNotLoadIncludeFile fp pos ->
           ["path" .= fp,
            "source" .= sourceName pos,
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      MacroAlreadyDefined name pos ->
           ["name" .= name,
            "source" .= sourceName pos,
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      InlineNotRendered il ->
           ["contents" .= toJSON il]
      BlockNotRendered bl ->
           ["contents" .= toJSON bl]
      DocxParserWarning s ->
           ["contents" .= s]
      IgnoredIOError s ->
           ["contents" .= s]
      CouldNotFetchResource fp s ->
           ["path" .= fp,
            "message" .= s]
      CouldNotDetermineImageSize fp s ->
           ["path" .= fp,
            "message" .= s]
      CouldNotConvertImage fp s ->
           ["path" .= fp,
            "message" .= s]
      CouldNotDetermineMimeType fp ->
           ["path" .= fp]
      CouldNotConvertTeXMath s msg ->
           ["contents" .= s,
            "message" .= msg]
      CouldNotParseCSS msg ->
           ["message" .= msg]
      Fetching fp ->
           ["path" .= fp]
      Extracting fp ->
           ["path" .= fp]
      NoTitleElement fallback ->
           ["fallback" .= fallback]
      NoLangSpecified -> []
      InvalidLang s ->
           ["lang" .= s]
      CouldNotHighlight msg ->
           ["message" .= msg]
      MissingCharacter msg ->
           ["message" .= msg]
      Deprecated thing msg ->
           ["thing" .= thing,
            "message" .= msg]
      NoTranslation term ->
           ["term" .= term]
      CouldNotLoadTranslations lang msg ->
           ["lang" .= lang,
            "message" .= msg]
      UnusualConversion msg ->
           ["message" .= msg]
      UnexpectedXmlElement element parent ->
           ["element" .= element,
            "parent" .= parent]
      UnknownOrgExportOption option ->
           ["option" .= option]
      CouldNotDeduceFormat exts format ->
           ["extensions" .= exts
           ,"format" .= format]
      RunningFilter fp ->
           ["path" .= Text.pack fp ]
      FilterCompleted fp ms ->
           ["path" .= Text.pack fp
           ,"milliseconds" .= Text.pack (show ms) ]

showPos :: SourcePos -> Text.Text
showPos pos = Text.pack $ sn ++ "line " ++
     show (sourceLine pos) ++ " column " ++ show (sourceColumn pos)
  where sn = if sourceName pos == "source" || sourceName pos == ""
                then ""
                else sourceName pos ++ " "

encodeLogMessages :: [LogMessage] -> BL.ByteString
encodeLogMessages ms =
  encodePretty' defConfig{ confCompare =
      keyOrder [ "type", "verbosity", "contents", "message", "path",
                 "source", "line", "column" ] } ms

showLogMessage :: LogMessage -> Text.Text
showLogMessage msg =
  case msg of
       SkippedContent s pos ->
         "Skipped '" <> s <> "' at " <> showPos pos
       IgnoredElement s ->
         "Ignored element " <> s
       CouldNotParseYamlMetadata s pos ->
         "Could not parse YAML metadata at " <> showPos pos <>
           if Text.null s then "" else ": " <> s
       DuplicateLinkReference s pos ->
         "Duplicate link reference '" <> s <> "' at " <> showPos pos
       DuplicateNoteReference s pos ->
         "Duplicate note reference '" <> s <> "' at " <> showPos pos
       NoteDefinedButNotUsed s pos ->
         "Note with key '" <> s <> "' defined at " <> showPos pos <>
           " but not used."
       DuplicateIdentifier s pos ->
         "Duplicate identifier '" <> s <> "' at " <> showPos pos
       ReferenceNotFound s pos ->
         "Reference not found for '" <> s <> "' at " <> showPos pos
       CircularReference s pos ->
         "Circular reference '" <> s <> "' at " <> showPos pos
       UndefinedToggle s pos ->
         "Undefined toggle '" <> s <> "' at " <> showPos pos
       ParsingUnescaped s pos ->
         "Parsing unescaped '" <> s <> "' at " <> showPos pos
       CouldNotLoadIncludeFile fp pos ->
         "Could not load include file '" <> fp <> "' at " <> showPos pos
       MacroAlreadyDefined name pos ->
         "Macro '" <> name <> "' already defined, ignoring at " <> showPos pos
       InlineNotRendered il ->
         "Not rendering " <> Text.pack (show il)
       BlockNotRendered bl ->
         "Not rendering " <> Text.pack (show bl)
       DocxParserWarning s ->
         "Docx parser warning: " <> s
       IgnoredIOError s ->
         "IO Error (ignored): " <> s
       CouldNotFetchResource fp s ->
         "Could not fetch resource '" <> fp <> "'" <>
           if Text.null s then "" else ": " <> s
       CouldNotDetermineImageSize fp s ->
         "Could not determine image size for '" <> fp <> "'" <>
           if Text.null s then "" else ": " <> s
       CouldNotConvertImage fp s ->
         "Could not convert image '" <> fp <> "'" <>
           if Text.null s then "" else ": " <> s
       CouldNotDetermineMimeType fp ->
         "Could not determine mime type for '" <> fp <> "'"
       CouldNotConvertTeXMath s m ->
         "Could not convert TeX math '" <> s <> "', rendering as TeX" <>
           if Text.null m then "" else ":\n" <> m
       CouldNotParseCSS m ->
         "Could not parse CSS" <> if Text.null m then "" else ":\n" <> m
       Fetching fp ->
         "Fetching " <> fp <> "..."
       Extracting fp ->
         "Extracting " <> fp <> "..."
       NoTitleElement fallback ->
         "This document format requires a nonempty <title> element.\n" <>
         "Defaulting to '" <> fallback <> "' as the title.\n" <>
         "To specify a title, use 'title' in metadata or " <>
         "--metadata title=\"...\"."
       NoLangSpecified ->
         "No value for 'lang' was specified in the metadata.\n" <>
         "It is recommended that lang be specified for this format."
       InvalidLang s ->
         "Invalid 'lang' value '" <> s <> "'.\n" <>
         "Use an IETF language tag like 'en-US'."
       CouldNotHighlight m ->
         "Could not highlight code block:\n" <> m
       MissingCharacter m ->
         "Missing character: " <> m
       Deprecated t m ->
         "Deprecated: " <> t <>
         if Text.null m
            then ""
            else ". " <> m
       NoTranslation t ->
         "The term " <> t <> " has no translation defined."
       CouldNotLoadTranslations lang m ->
         "Could not load translations for " <> lang <>
           if Text.null m then "" else "\n" <> m
       UnusualConversion m ->
         "Unusual conversion: " <> m
       UnexpectedXmlElement element parent ->
         "Unexpected XML element " <> element <> " in " <> parent
       UnknownOrgExportOption option ->
         "Ignoring unknown Org export option: " <> option
       CouldNotDeduceFormat exts format ->
         "Could not deduce format from file extension " <>
         Text.intercalate " or " exts <> "\n" <>
         "Defaulting to " <> format
       RunningFilter fp -> "Running filter " <> Text.pack fp
       FilterCompleted fp ms -> "Completed filter " <> Text.pack fp <>
          " in " <> Text.pack (show ms) <> " ms"

messageVerbosity :: LogMessage -> Verbosity
messageVerbosity msg =
  case msg of
       SkippedContent{}              -> INFO
       IgnoredElement{}              -> INFO
       CouldNotParseYamlMetadata{}   -> WARNING
       DuplicateLinkReference{}      -> WARNING
       DuplicateNoteReference{}      -> WARNING
       NoteDefinedButNotUsed{}       -> WARNING
       DuplicateIdentifier{}         -> WARNING
       ReferenceNotFound{}           -> WARNING
       CircularReference{}           -> WARNING
       UndefinedToggle{}             -> WARNING
       CouldNotLoadIncludeFile f _
        | ".sty" `Text.isSuffixOf` f -> INFO
        | otherwise                  -> WARNING
       MacroAlreadyDefined{}         -> WARNING
       ParsingUnescaped{}            -> INFO
       InlineNotRendered{}           -> INFO
       BlockNotRendered{}            -> INFO
       DocxParserWarning{}           -> INFO
       IgnoredIOError{}              -> WARNING
       CouldNotFetchResource{}       -> WARNING
       CouldNotDetermineImageSize{}  -> WARNING
       CouldNotConvertImage{}        -> WARNING
       CouldNotDetermineMimeType{}   -> WARNING
       CouldNotConvertTeXMath{}      -> WARNING
       CouldNotParseCSS{}            -> WARNING
       Fetching{}                    -> INFO
       Extracting{}                  -> INFO
       NoTitleElement{}              -> WARNING
       NoLangSpecified               -> INFO
       InvalidLang{}                 -> WARNING
       CouldNotHighlight{}           -> WARNING
       MissingCharacter{}            -> WARNING
       Deprecated{}                  -> WARNING
       NoTranslation{}               -> WARNING
       CouldNotLoadTranslations{}    -> WARNING
       UnusualConversion {}          -> WARNING
       UnexpectedXmlElement {}       -> WARNING
       UnknownOrgExportOption {}     -> WARNING
       CouldNotDeduceFormat{}        -> WARNING
       RunningFilter{}               -> INFO
       FilterCompleted{}             -> INFO
