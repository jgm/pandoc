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

-- | Verbosity level.
data Verbosity = ERROR | WARNING | INFO | DEBUG
     deriving (Show, Read, Eq, Data, Enum, Ord, Bounded, Typeable, Generic)

instance ToJSON Verbosity where
  toJSON x = toJSON (show x)

data LogMessage =
    SkippedInput String SourcePos
  | YamlSectionNotAnObject SourcePos
  | DuplicateLinkReference String SourcePos
  | DuplicateNoteReference String SourcePos
  | ParsingUnescaped String SourcePos
  | InlineNotRendered Inline
  | BlockNotRendered Block
  | DocxCommentWillNotRetainFormatting String
  | CouldNotFetchResource String String
  | CouldNotDetermineImageSize String
  | CouldNotDetermineMimeType String
  | CouldNotConvertTeXMath String
  deriving (Show, Eq, Data, Ord, Typeable, Generic)

instance ToJSON LogMessage where
  toJSON x = object $ "verbosity" .= toJSON (messageVerbosity x) :
    case x of
      SkippedInput s pos ->
           ["type" .= String "SkippedInput",
            "contents" .= Text.pack s,
            "source" .= Text.pack (sourceName pos),
            "line" .= sourceLine pos,
            "column" .= sourceColumn pos]
      YamlSectionNotAnObject pos ->
           ["type" .= String "YamlSectionNotAnObject",
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
      ParsingUnescaped s pos ->
           ["type" .= String "ParsingUnescaped",
            "contents" .= Text.pack s,
            "source" .= Text.pack (sourceName pos),
            "line" .= toJSON (sourceLine pos),
            "column" .= toJSON (sourceColumn pos)]
      InlineNotRendered il ->
           ["type" .= String "InlineNotRendered",
            "contents" .= toJSON il]
      BlockNotRendered bl ->
           ["type" .= String "BlockNotRendered",
            "contents" .= toJSON bl]
      DocxCommentWillNotRetainFormatting s ->
           ["type" .= String "DocxCommentWillNotRetainFormatting",
            "commentId" .= Text.pack s]
      CouldNotFetchResource fp s ->
           ["type" .= String "CouldNotFetchResource",
            "path" .= Text.pack fp,
            "message" .= Text.pack s]
      CouldNotDetermineImageSize fp ->
           ["type" .= String "CouldNotDetermineImageSize",
            "path" .= Text.pack fp]
      CouldNotDetermineMimeType fp ->
           ["type" .= String "CouldNotDetermineMimeType",
            "path" .= Text.pack fp]
      CouldNotConvertTeXMath s ->
           ["type" .= String "CouldNotConvertTeXMath",
            "contents" .= Text.pack s]

showLogMessage :: LogMessage -> String
showLogMessage msg =
  case msg of
       SkippedInput s pos ->
         "Skipped '" ++ s ++ "' at " ++ show pos
       YamlSectionNotAnObject pos ->
         "YAML metadata section is not an object at " ++ show pos
       DuplicateLinkReference s pos ->
         "Duplicate link reference '" ++ s ++ "' at " ++ show pos
       DuplicateNoteReference s pos ->
         "Duplicate note reference '" ++ s ++ "' at " ++ show pos
       ParsingUnescaped s pos ->
         "Parsing unescaped '" ++ s ++ "' at " ++ show pos
       InlineNotRendered il ->
         "Not rendering " ++ show il
       BlockNotRendered bl ->
         "Not rendering " ++ show bl
       DocxCommentWillNotRetainFormatting s ->
         "Docx comment with id '" ++ s ++ "' will not retain formatting"
       CouldNotFetchResource fp s ->
         "Could not fetch resource '" ++ fp ++ "'" ++
           if null s then "" else (": " ++ s)
       CouldNotDetermineImageSize fp ->
         "Could not determine image size for '" ++ fp ++ "'"
       CouldNotDetermineMimeType fp ->
         "Could not determine mime type for '" ++ fp ++ "'"
       CouldNotConvertTeXMath s ->
         "Could not convert TeX math '" ++ s ++ "', rendering as TeX"

messageVerbosity:: LogMessage -> Verbosity
messageVerbosity msg =
  case msg of
       SkippedInput{} -> INFO
       YamlSectionNotAnObject{} -> WARNING
       DuplicateLinkReference{} -> WARNING
       DuplicateNoteReference{} -> WARNING
       ParsingUnescaped{} -> INFO
       InlineNotRendered{} -> INFO
       BlockNotRendered{} -> INFO
       DocxCommentWillNotRetainFormatting{} -> INFO
       CouldNotFetchResource{} -> WARNING
       CouldNotDetermineImageSize{} -> WARNING
       CouldNotDetermineMimeType{} -> WARNING
       CouldNotConvertTeXMath{} -> WARNING


