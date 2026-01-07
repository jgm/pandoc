{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{- |
   Module      : Text.Pandoc.Logging
   Copyright   : Copyright (C) 2006-2024 John MacFarlane
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
import Data.Aeson
import Data.Aeson.Encode.Pretty (Config (..), defConfig, encodePretty',
                                 keyOrder)
import qualified Data.ByteString.Lazy as BL
import Data.Data (Data, toConstr)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Pandoc.Definition
import Text.Parsec.Pos
import Text.Pandoc.Shared (tshow)

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

data LogMessage =
    SkippedContent Text SourcePos
  | IgnoredElement Text
  | DuplicateLinkReference Text SourcePos
  | DuplicateNoteReference Text SourcePos
  | NoteDefinedButNotUsed Text SourcePos
  | DuplicateIdentifier Text SourcePos
  | ReferenceNotFound Text SourcePos
  | CircularReference Text SourcePos
  | UndefinedToggle Text SourcePos
  | ParsingUnescaped Text SourcePos
  | CouldNotLoadIncludeFile Text SourcePos
  | CouldNotParseIncludeFile Text SourcePos
  | MacroAlreadyDefined Text SourcePos
  | InlineNotRendered Inline
  | BlockNotRendered Block
  | DocxParserWarning Text
  | PowerpointTemplateWarning Text
  | IgnoredIOError Text
  | CouldNotFetchResource Text Text
  | CouldNotDetermineImageSize Text Text
  | CouldNotConvertImage Text Text
  | CouldNotDetermineMimeType Text
  | CouldNotConvertTeXMath Text Text
  | CouldNotParseCSS Text
  | Fetching Text
  | Extracting Text
  | LoadedResource FilePath FilePath
  | ScriptingInfo Text (Maybe SourcePos)
  | ScriptingWarning Text (Maybe SourcePos)
  | NoTitleElement Text
  | NoLangSpecified
  | InvalidLang Text
  | CouldNotHighlight Text
  | MissingCharacter Text
  | Deprecated Text Text
  | NoTranslation Text
  | CouldNotLoadTranslations Text Text
  | UnusualConversion Text
  | UnexpectedXmlElement Text Text
  | UnknownOrgExportOption Text
  | CouldNotDeduceFormat [Text] Text
  | RunningFilter FilePath
  | FilterCompleted FilePath Integer
  | CiteprocWarning Text
  | ATXHeadingInLHS Int Text
  | EnvironmentVariableUndefined Text
  | DuplicateAttribute Text Text
  | NotUTF8Encoded FilePath
  | MakePDFInfo Text Text
  | MakePDFWarning Text
  | UnclosedDiv SourcePos SourcePos
  | UnsupportedCodePage Int
  | YamlWarning SourcePos Text
  deriving (Show, Eq, Data, Ord, Typeable, Generic)

instance ToJSON LogMessage where
  toJSON x = object $
    "verbosity" .= toJSON (messageVerbosity x) :
    "type" .= toJSON (show $ toConstr x) :
    "pretty" .= toJSON (showLogMessage x) :
    case x of
      SkippedContent s pos ->
           ["contents" .= s,
            "source" .= sourceName pos,
            "line" .= sourceLine pos,
            "column" .= sourceColumn pos]
      IgnoredElement s ->
           ["contents" .= s]
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
      CouldNotParseIncludeFile fp pos ->
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
      PowerpointTemplateWarning s ->
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
      LoadedResource orig found ->
           ["for"  .= orig
           ,"from" .= found]
      ScriptingInfo msg mbpos ->
           ["message" .= msg] <>
           case mbpos of
             Nothing  -> []
             Just pos -> ["source" .= sourceName pos
                         ,"line" .= toJSON (sourceLine pos)
                         ,"column" .= toJSON (sourceColumn pos)
                         ]
      ScriptingWarning msg mbpos ->
           ["message" .= msg] <>
           case mbpos of
             Nothing  -> []
             Just pos -> ["source" .= sourceName pos
                         ,"line" .= toJSON (sourceLine pos)
                         ,"column" .= toJSON (sourceColumn pos)
                         ]
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
      CiteprocWarning msg ->
           ["message" .= msg]
      ATXHeadingInLHS lvl contents ->
           ["level" .= lvl
           ,"contents" .= contents]
      EnvironmentVariableUndefined var ->
           ["variable" .= var ]
      DuplicateAttribute attr val ->
           ["attribute" .= attr
           ,"value" .= val]
      NotUTF8Encoded src ->
           ["source" .= src]
      MakePDFInfo description contents ->
           ["description" .= description
           ,"contents" .= contents]
      MakePDFWarning message ->
           ["message" .= message]
      UnclosedDiv openpos closepos ->
           ["openpos" .= object
             [ "source" .= sourceName openpos,
               "line" .= toJSON (sourceLine openpos),
               "column" .= toJSON (sourceColumn openpos)]
           ,"closepos" .= object
             [ "source" .= sourceName closepos,
               "line" .= toJSON (sourceLine closepos),
               "column" .= toJSON (sourceColumn closepos)]
           ]
      UnsupportedCodePage cpg ->
           ["codepage" .= cpg]
      YamlWarning pos msg ->
           [ "source" .= sourceName pos
           , "line" .= toJSON (sourceLine pos)
           , "column" .= toJSON (sourceColumn pos)
           , "message" .= msg
           ]

showPos :: SourcePos -> Text
showPos pos = Text.pack $ sn ++ "line " ++
     show (sourceLine pos) ++ " column " ++ show (sourceColumn pos)
  where
    sn' = sourceName pos
    sn = if sn' == "source" || sn' == "" || sn' == "-"
            then ""
            else sn' ++ " "

encodeLogMessages :: [LogMessage] -> BL.ByteString
encodeLogMessages ms =
  encodePretty' defConfig{ confCompare =
      keyOrder [ "type", "verbosity", "contents", "message", "path",
                 "source", "line", "column" ] } ms

showLogMessage :: LogMessage -> Text
showLogMessage msg =
  case msg of
       SkippedContent s pos ->
         "Skipped '" <> s <> "' at " <> showPos pos
       IgnoredElement s ->
         "Ignored element " <> s
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
         "Could not load include file " <> fp <> " at " <> showPos pos
       CouldNotParseIncludeFile fp pos ->
         "Parsing include file " <> fp <> " failed at " <> showPos pos
       MacroAlreadyDefined name pos ->
         "Macro '" <> name <> "' already defined, ignoring at " <> showPos pos
       InlineNotRendered il ->
         "Not rendering " <> Text.pack (show il)
       BlockNotRendered bl ->
         "Not rendering " <> Text.pack (show bl)
       DocxParserWarning s ->
         "Docx parser warning: " <> s
       PowerpointTemplateWarning s ->
         "Powerpoint template warning: " <> s
       IgnoredIOError s ->
         "IO Error (ignored): " <> s
       CouldNotFetchResource fp s ->
         "Could not fetch resource " <> fp <>
           if Text.null s then "" else ": " <> s
       CouldNotDetermineImageSize fp s ->
         "Could not determine image size for " <> fp <>
           if Text.null s then "" else ": " <> s
       CouldNotConvertImage fp s ->
         "Could not convert image " <> fp <>
           if Text.null s then "" else ": " <> s
       CouldNotDetermineMimeType fp ->
         "Could not determine mime type for " <> fp
       CouldNotConvertTeXMath s m ->
         "Could not convert TeX math " <> s <> ", rendering as TeX" <>
           if Text.null m then "" else ":\n" <> m
       CouldNotParseCSS m ->
         "Could not parse CSS" <> if Text.null m then "" else ":\n" <> m
       Fetching fp ->
         "Fetching " <> fp <> "..."
       Extracting fp ->
         "Extracting " <> fp <> "..."
       LoadedResource orig found ->
         "Loaded " <> Text.pack orig <> " from " <> Text.pack found
       ScriptingInfo s mbpos ->
         "Scripting info" <>
         maybe "" (\pos -> " at " <> showPos pos) mbpos  <> ": " <> s
       ScriptingWarning s mbpos ->
         "Scripting warning" <>
         maybe "" (\pos -> " at " <> showPos pos) mbpos  <> ": " <> s
       NoTitleElement fallback ->
         "This document format requires a nonempty <title> element.\n" <>
         "Defaulting to '" <> fallback <> "' as the title.\n" <>
         "To specify a title, use --variable pagetitle=\"...\"."
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
       CiteprocWarning ms -> "Citeproc: " <> ms
       ATXHeadingInLHS lvl contents ->
         "Rendering heading '" <> contents <> "' as a paragraph.\n" <>
         "ATX headings cannot be used in literate Haskell, because " <>
         "'#' is not\nallowed in column 1." <>
         if lvl < 3
            then " Consider using --markdown-headings=setext."
            else ""
       EnvironmentVariableUndefined var ->
         "Undefined environment variable " <> var <> " in defaults file."
       DuplicateAttribute attr val ->
         "Ignoring duplicate attribute " <> attr <> "=" <> tshow val <> "."
       NotUTF8Encoded src ->
         Text.pack src <>
           " is not UTF-8 encoded: falling back to latin1."
       MakePDFInfo description contents ->
         "[makePDF] " <> description <>
          if Text.null contents
             then mempty
             else "\n" <> contents
       MakePDFWarning message -> "[makePDF] " <> message
       UnclosedDiv openpos closepos -> "Div at " <> showPos openpos <>
          " unclosed at " <> showPos closepos <> ", closing implicitly."
       UnsupportedCodePage cpg -> "Unsupported code page " <> tshow cpg <>
          ". Text will likely be garbled."
       YamlWarning pos m -> "YAML warning (" <> showPos pos <> "): " <> m

messageVerbosity :: LogMessage -> Verbosity
messageVerbosity msg =
  case msg of
       SkippedContent{}              -> INFO
       IgnoredElement{}              -> INFO
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
       CouldNotParseIncludeFile{}    -> WARNING
       MacroAlreadyDefined{}         -> WARNING
       ParsingUnescaped{}            -> INFO
       InlineNotRendered{}           -> INFO
       BlockNotRendered{}            -> INFO
       DocxParserWarning{}           -> WARNING
       PowerpointTemplateWarning{}   -> WARNING
       IgnoredIOError{}              -> WARNING
       CouldNotFetchResource{}       -> WARNING
       CouldNotDetermineImageSize{}  -> WARNING
       CouldNotConvertImage{}        -> WARNING
       CouldNotDetermineMimeType{}   -> WARNING
       CouldNotConvertTeXMath{}      -> WARNING
       CouldNotParseCSS{}            -> WARNING
       Fetching{}                    -> INFO
       Extracting{}                  -> INFO
       LoadedResource{}              -> INFO
       ScriptingInfo{}               -> INFO
       ScriptingWarning{}            -> WARNING
       NoTitleElement{}              -> INFO
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
       CiteprocWarning{}             -> WARNING
       ATXHeadingInLHS{}             -> WARNING
       EnvironmentVariableUndefined{}-> WARNING
       DuplicateAttribute{}          -> WARNING
       NotUTF8Encoded{}              -> WARNING
       MakePDFInfo{}                 -> INFO
       MakePDFWarning{}              -> WARNING
       UnclosedDiv{}                 -> WARNING
       UnsupportedCodePage{}         -> WARNING
       YamlWarning{}                 -> WARNING
