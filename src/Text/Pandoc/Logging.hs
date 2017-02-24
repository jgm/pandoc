{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings #-}
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
   Copyright   : Copyright (C) 2006-2016 John MacFarlane
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

import Text.Parsec.Pos
import Data.Data (Data)
import Data.Generics (Typeable)
import GHC.Generics (Generic)
import qualified Data.Text as Text
import Data.Aeson
import Text.Pandoc.Definition
import Data.Aeson.Encode.Pretty (encodePretty', keyOrder,
                                 defConfig, Config(..))
import qualified Data.ByteString.Lazy as BL

-- | Verbosity level.
data Verbosity = ERROR | WARNING | INFO | DEBUG
     deriving (Show, Read, Eq, Data, Enum, Ord, Bounded, Typeable, Generic)

instance ToJSON Verbosity where
  toJSON x = toJSON (show x)

data LogMessage =
    SkippedContent String SourcePos
  | CouldNotParseYamlMetadata String SourcePos
  | DuplicateLinkReference String SourcePos
  | DuplicateNoteReference String SourcePos
  | ReferenceNotFound String SourcePos
  | CircularReference String SourcePos
  | ParsingUnescaped String SourcePos
  | CouldNotLoadIncludeFile String SourcePos
  | ParsingTrace String SourcePos
  | InlineNotRendered Inline
  | BlockNotRendered Block
  | DocxParserWarning String
  | CouldNotFetchResource String String
  | CouldNotDetermineImageSize String String
  | CouldNotDetermineMimeType String
  | CouldNotConvertTeXMath String String
  | CouldNotParseCSS String
  | Fetching String
  | UsingResourceFrom FilePath FilePath
  deriving (Show, Eq, Data, Ord, Typeable, Generic)

instance ToJSON LogMessage where
  toJSON x = object $ "verbosity" .= toJSON (messageVerbosity x) :
    case x of
      SkippedContent s pos ->
           ["type" .= String "SkippedContent",
            "contents" .= Text.pack s,
            "source" .= Text.pack (sourceName pos),
            "line" .= sourceLine pos,
            "column" .= sourceColumn pos]
      CouldNotParseYamlMetadata s pos ->
           ["type" .= String "YamlSectionNotAnObject",
            "message" .= Text.pack s,
            "source" .= Text.pack (sourceName pos),
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      DuplicateLinkReference s pos ->
           ["type" .= String "DuplicateLinkReference",
            "contents" .= Text.pack s,
            "source" .= Text.pack (sourceName pos),
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      DuplicateNoteReference s pos ->
           ["type" .= String "DuplicateNoteReference",
            "contents" .= Text.pack s,
            "source" .= Text.pack (sourceName pos),
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      ReferenceNotFound s pos ->
           ["type" .= String "ReferenceNotFound",
            "contents" .= Text.pack s,
            "source" .= Text.pack (sourceName pos),
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      CircularReference s pos ->
           ["type" .= String "CircularReference",
            "contents" .= Text.pack s,
            "source" .= Text.pack (sourceName pos),
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      ParsingUnescaped s pos ->
           ["type" .= String "ParsingUnescaped",
            "contents" .= Text.pack s,
            "source" .= Text.pack (sourceName pos),
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      CouldNotLoadIncludeFile fp pos ->
           ["type" .= String "CouldNotLoadIncludeFile",
            "path" .= Text.pack fp,
            "source" .= Text.pack (sourceName pos),
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      ParsingTrace s pos ->
           ["type" .= String "ParsingTrace",
            "contents" .= Text.pack s,
            "source" .= Text.pack (sourceName pos),
            "line" .= sourceLine pos,
            "column" .= sourceColumn pos]
      InlineNotRendered il ->
           ["type" .= String "InlineNotRendered",
            "contents" .= toJSON il]
      BlockNotRendered bl ->
           ["type" .= String "BlockNotRendered",
            "contents" .= toJSON bl]
      DocxParserWarning s ->
           ["type" .= String "DocxParserWarning",
            "contents" .= Text.pack s]
      CouldNotFetchResource fp s ->
           ["type" .= String "CouldNotFetchResource",
            "path" .= Text.pack fp,
            "message" .= Text.pack s]
      CouldNotDetermineImageSize fp s ->
           ["type" .= String "CouldNotDetermineImageSize",
            "path" .= Text.pack fp,
            "message" .= Text.pack s]
      CouldNotDetermineMimeType fp ->
           ["type" .= String "CouldNotDetermineMimeType",
            "path" .= Text.pack fp]
      CouldNotConvertTeXMath s msg ->
           ["type" .= String "CouldNotConvertTeXMath",
            "contents" .= Text.pack s,
            "message" .= Text.pack msg]
      CouldNotParseCSS msg ->
           ["type" .= String "CouldNotParseCSS",
            "message" .= Text.pack msg]
      Fetching fp ->
           ["type" .= String "CouldNotParseCSS",
            "path" .= Text.pack fp]
      UsingResourceFrom resource dir ->
           ["type" .= String "UsingResourceFrom",
            "resource" .= Text.pack resource,
            "path" .= Text.pack dir]

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
       ReferenceNotFound s pos ->
         "Reference not found for '" ++ s ++ "' at " ++ showPos pos
       CircularReference s pos ->
         "Circular reference '" ++ s ++ "' at " ++ showPos pos
       ParsingUnescaped s pos ->
         "Parsing unescaped '" ++ s ++ "' at " ++ showPos pos
       CouldNotLoadIncludeFile fp pos ->
         "Could not load include file '" ++ fp ++ "' at " ++ showPos pos
       ParsingTrace s pos ->
         "Parsing trace at " ++ showPos pos ++ ": " ++ s
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
       CouldNotDetermineMimeType fp ->
         "Could not determine mime type for '" ++ fp ++ "'"
       CouldNotConvertTeXMath s m ->
         "Could not convert TeX math '" ++ s ++ "', rendering as TeX" ++
           if null m then "" else (':':'\n':m)
       CouldNotParseCSS m ->
         "Could not parse CSS" ++ if null m then "" else (':':'\n':m)
       Fetching fp ->
         "Fetching " ++ fp ++ "..."
       UsingResourceFrom fp dir ->
         "Using " ++ fp ++ " from " ++ dir

messageVerbosity:: LogMessage -> Verbosity
messageVerbosity msg =
  case msg of
       SkippedContent{} -> INFO
       CouldNotParseYamlMetadata{} -> WARNING
       DuplicateLinkReference{} -> WARNING
       DuplicateNoteReference{} -> WARNING
       ReferenceNotFound{} -> WARNING
       CircularReference{} -> WARNING
       CouldNotLoadIncludeFile{} -> WARNING
       ParsingUnescaped{} -> INFO
       ParsingTrace{} -> DEBUG
       InlineNotRendered{} -> INFO
       BlockNotRendered{} -> INFO
       DocxParserWarning{} -> WARNING
       CouldNotFetchResource{} -> WARNING
       CouldNotDetermineImageSize{} -> WARNING
       CouldNotDetermineMimeType{} -> WARNING
       CouldNotConvertTeXMath{} -> WARNING
       CouldNotParseCSS{} -> WARNING
       Fetching{} -> INFO
       UsingResourceFrom{} -> INFO
