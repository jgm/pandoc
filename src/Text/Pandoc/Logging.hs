{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
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
  , messageVerbosity
  ) where

import Text.Parsec.Pos
import Data.Data (Data)
import Data.Generics (Typeable)
import GHC.Generics (Generic)

-- | Verbosity level.
data Verbosity = ERROR | WARNING | INFO | DEBUG
     deriving (Show, Read, Eq, Data, Enum, Ord, Bounded, Typeable, Generic)

data LogMessage =
    SkippedInput String SourcePos
  | NotRendered String
  | YamlSectionNotAnObject SourcePos
  | DuplicateLinkReference String SourcePos
  | DuplicateNoteReference String SourcePos
  | ParsingUnescaped String SourcePos
  | DocxCommentWillNotRetainFormatting String
  | CouldNotFetchResource String String
  | CouldNotDetermineImageSize String
  | CouldNotDetermineMimeType String
  | CouldNotConvertTeXMath String
  deriving (Show, Eq, Data, Ord, Typeable, Generic)

messageVerbosity:: LogMessage -> Verbosity
messageVerbosity msg =
  case msg of
       SkippedInput{} -> INFO
       NotRendered{} -> INFO
       YamlSectionNotAnObject{} -> WARNING
       DuplicateLinkReference{} -> WARNING
       DuplicateNoteReference{} -> WARNING
       ParsingUnescaped{} -> INFO
       DocxCommentWillNotRetainFormatting{} -> INFO
       CouldNotFetchResource{} -> WARNING
       CouldNotDetermineImageSize{} -> WARNING
       CouldNotDetermineMimeType{} -> WARNING
       CouldNotConvertTeXMath{} -> WARNING


