{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-
Copyright (C) 2016-17 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Logging
   Copyright   : Copyright (C) 2006-2017 John MacFarlane
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
import Data.Generics (Typeable)
import qualified Data.Text as Text
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

data LogMessage =
    SkippedContent String SourcePos
  | CouldNotParseYamlMetadata String SourcePos
  | DuplicateLinkReference String SourcePos
  | DuplicateNoteReference String SourcePos
  | NoteDefinedButNotUsed String SourcePos
  | DuplicateIdentifier String SourcePos
  | ReferenceNotFound String SourcePos
  | CircularReference String SourcePos
  | ParsingUnescaped String SourcePos
  | CouldNotLoadIncludeFile String SourcePos
  | MacroAlreadyDefined String SourcePos
  | InlineNotRendered Inline
  | BlockNotRendered Block
  | DocxParserWarning String
  | CouldNotFetchResource String String
  | CouldNotDetermineImageSize String String
  | CouldNotConvertImage String String
  | CouldNotDetermineMimeType String
  | CouldNotConvertTeXMath String String
  | CouldNotParseCSS String
  | Fetching String
  | Extracting String
  | NoTitleElement String
  | NoLangSpecified
  | InvalidLang String
  | CouldNotHighlight String
  | MissingCharacter String
  deriving (Show, Eq, Data, Ord, Typeable, Generic)

instance ToJSON LogMessage where
  toJSON x = object $
    "verbosity" .= toJSON (messageVerbosity x) :
    "type" .= toJSON (show $ toConstr x) :
    case x of
      SkippedContent s pos ->
           ["contents" .= Text.pack s,
            "source" .= Text.pack (sourceName pos),
            "line" .= sourceLine pos,
            "column" .= sourceColumn pos]
      CouldNotParseYamlMetadata s pos ->
           ["message" .= Text.pack s,
            "source" .= Text.pack (sourceName pos),
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      DuplicateLinkReference s pos ->
           ["contents" .= Text.pack s,
            "source" .= Text.pack (sourceName pos),
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      NoteDefinedButNotUsed s pos ->
           ["key" .= Text.pack s,
            "source" .= Text.pack (sourceName pos),
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      DuplicateNoteReference s pos ->
           ["contents" .= Text.pack s,
            "source" .= Text.pack (sourceName pos),
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      DuplicateIdentifier s pos ->
           ["contents" .= Text.pack s,
            "source" .= Text.pack (sourceName pos),
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      ReferenceNotFound s pos ->
           ["contents" .= Text.pack s,
            "source" .= Text.pack (sourceName pos),
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      CircularReference s pos ->
           ["contents" .= Text.pack s,
            "source" .= Text.pack (sourceName pos),
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      ParsingUnescaped s pos ->
           ["contents" .= Text.pack s,
            "source" .= Text.pack (sourceName pos),
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      CouldNotLoadIncludeFile fp pos ->
           ["path" .= Text.pack fp,
            "source" .= Text.pack (sourceName pos),
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      MacroAlreadyDefined name pos ->
           ["name" .= Text.pack name,
            "source" .= Text.pack (sourceName pos),
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      InlineNotRendered il ->
           ["contents" .= toJSON il]
      BlockNotRendered bl ->
           ["contents" .= toJSON bl]
      DocxParserWarning s ->
           ["contents" .= Text.pack s]
      CouldNotFetchResource fp s ->
           ["path" .= Text.pack fp,
            "message" .= Text.pack s]
      CouldNotDetermineImageSize fp s ->
           ["path" .= Text.pack fp,
            "message" .= Text.pack s]
      CouldNotConvertImage fp s ->
           ["path" .= Text.pack fp,
            "message" .= Text.pack s]
      CouldNotDetermineMimeType fp ->
           ["path" .= Text.pack fp]
      CouldNotConvertTeXMath s msg ->
           ["contents" .= Text.pack s,
            "message" .= Text.pack msg]
      CouldNotParseCSS msg ->
           ["message" .= Text.pack msg]
      Fetching fp ->
           ["path" .= Text.pack fp]
      Extracting fp ->
           ["path" .= Text.pack fp]
      NoTitleElement fallback ->
           ["fallback" .= Text.pack fallback]
      NoLangSpecified -> []
      InvalidLang s ->
           ["lang" .= Text.pack s]
      CouldNotHighlight msg ->
           ["message" .= Text.pack msg]
      MissingCharacter msg ->
           ["message" .= Text.pack msg]

showPos :: SourcePos -> String
showPos pos = sn ++ "line " ++
     show (sourceLine pos) ++ " column " ++ show (sourceColumn pos)
  where sn = if sourceName pos == "source" || sourceName pos == ""
                then ""
                else sourceName pos ++ " "

encodeLogMessages :: [LogMessage] -> BL.ByteString
encodeLogMessages ms =
  encodePretty' defConfig{ confCompare =
      keyOrder [ "type", "verbosity", "contents", "message", "path",
                 "source", "line", "column" ] } ms

showLogMessage :: LogMessage -> String
showLogMessage msg =
  case msg of
       SkippedContent s pos ->
         "Skipped '" ++ s ++ "' at " ++ showPos pos
       CouldNotParseYamlMetadata s pos ->
         "Could not parse YAML metadata at " ++ showPos pos ++
           if null s then "" else (": " ++ s)
       DuplicateLinkReference s pos ->
         "Duplicate link reference '" ++ s ++ "' at " ++ showPos pos
       DuplicateNoteReference s pos ->
         "Duplicate note reference '" ++ s ++ "' at " ++ showPos pos
       NoteDefinedButNotUsed s pos ->
         "Note with key '" ++ s ++ "' defined at " ++ showPos pos ++
           " but not used."
       DuplicateIdentifier s pos ->
         "Duplicate identifier '" ++ s ++ "' at " ++ showPos pos
       ReferenceNotFound s pos ->
         "Reference not found for '" ++ s ++ "' at " ++ showPos pos
       CircularReference s pos ->
         "Circular reference '" ++ s ++ "' at " ++ showPos pos
       ParsingUnescaped s pos ->
         "Parsing unescaped '" ++ s ++ "' at " ++ showPos pos
       CouldNotLoadIncludeFile fp pos ->
         "Could not load include file '" ++ fp ++ "' at " ++ showPos pos
       MacroAlreadyDefined name pos ->
         "Macro '" ++ name ++ "' already defined, ignoring at " ++ showPos pos
       InlineNotRendered il ->
         "Not rendering " ++ show il
       BlockNotRendered bl ->
         "Not rendering " ++ show bl
       DocxParserWarning s ->
         "Docx parser warning: " ++ s
       CouldNotFetchResource fp s ->
         "Could not fetch resource '" ++ fp ++ "'" ++
           if null s then "" else (": " ++ s)
       CouldNotDetermineImageSize fp s ->
         "Could not determine image size for '" ++ fp ++ "'" ++
           if null s then "" else (": " ++ s)
       CouldNotConvertImage fp s ->
         "Could not convert image '" ++ fp ++ "'" ++
           if null s then "" else (": " ++ s)
       CouldNotDetermineMimeType fp ->
         "Could not determine mime type for '" ++ fp ++ "'"
       CouldNotConvertTeXMath s m ->
         "Could not convert TeX math '" ++ s ++ "', rendering as TeX" ++
           if null m then "" else (':':'\n':m)
       CouldNotParseCSS m ->
         "Could not parse CSS" ++ if null m then "" else (':':'\n':m)
       Fetching fp ->
         "Fetching " ++ fp ++ "..."
       Extracting fp ->
         "Extracting " ++ fp ++ "..."
       NoTitleElement fallback ->
         "This document format requires a nonempty <title> element.\n" ++
         "Please specify either 'title' or 'pagetitle' in the metadata.\n" ++
         "Falling back to '" ++ fallback ++ "'"
       NoLangSpecified ->
         "No value for 'lang' was specified in the metadata.\n" ++
         "It is recommended that lang be specified for this format."
       InvalidLang s ->
         "Invalid 'lang' value '" ++ s ++ "'.\n" ++
         "Use an IETF language tag like 'en-US'."
       CouldNotHighlight m ->
         "Could not highlight code block:\n" ++ m
       MissingCharacter m ->
         "Missing character: " ++ m

messageVerbosity:: LogMessage -> Verbosity
messageVerbosity msg =
  case msg of
       SkippedContent{}             -> WARNING
       CouldNotParseYamlMetadata{}  -> WARNING
       DuplicateLinkReference{}     -> WARNING
       DuplicateNoteReference{}     -> WARNING
       NoteDefinedButNotUsed{}      -> WARNING
       DuplicateIdentifier{}        -> WARNING
       ReferenceNotFound{}          -> WARNING
       CircularReference{}          -> WARNING
       CouldNotLoadIncludeFile{}    -> WARNING
       MacroAlreadyDefined{}        -> WARNING
       ParsingUnescaped{}           -> INFO
       InlineNotRendered{}          -> INFO
       BlockNotRendered{}           -> INFO
       DocxParserWarning{}          -> WARNING
       CouldNotFetchResource{}      -> WARNING
       CouldNotDetermineImageSize{} -> WARNING
       CouldNotConvertImage{}       -> WARNING
       CouldNotDetermineMimeType{}  -> WARNING
       CouldNotConvertTeXMath{}     -> WARNING
       CouldNotParseCSS{}           -> WARNING
       Fetching{}                   -> INFO
       Extracting{}                 -> INFO
       NoTitleElement{}             -> WARNING
       NoLangSpecified              -> INFO
       InvalidLang{}                -> WARNING
       CouldNotHighlight{}          -> WARNING
       MissingCharacter{}           -> WARNING
