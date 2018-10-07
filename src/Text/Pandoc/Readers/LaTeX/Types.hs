{-# LANGUAGE NoImplicitPrelude #-}
{-
Copyright (C) 2017-2018 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Readers.LaTeX.Types
   Copyright   : Copyright (C) 2017-2018 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Types for LaTeX tokens and macros.
-}
module Text.Pandoc.Readers.LaTeX.Types ( Tok(..)
                                       , TokType(..)
                                       , Macro(..)
                                       , ArgSpec(..)
                                       , ExpansionPoint(..)
                                       , SourcePos
                                       )
where
import Prelude
import Data.Text (Text)
import Text.Parsec.Pos (SourcePos)

data TokType = CtrlSeq Text | Spaces | Newline | Symbol | Word | Comment |
               Esc1    | Esc2   | Arg Int
     deriving (Eq, Ord, Show)

data Tok = Tok SourcePos TokType Text
     deriving (Eq, Ord, Show)

data ExpansionPoint = ExpandWhenDefined | ExpandWhenUsed
     deriving (Eq, Ord, Show)

data Macro = Macro ExpansionPoint [ArgSpec] (Maybe [Tok]) [Tok]
     deriving Show

data ArgSpec = ArgNum Int | Pattern [Tok]
     deriving Show
