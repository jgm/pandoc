{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-
Copyright (C) 2006-2016 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Error
   Copyright   : Copyright (C) 2006-2016 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

This module provides a standard way to deal with possible errors encounted
during parsing.

-}
module Text.Pandoc.Error (PandocError(..), handleError) where

import Text.Parsec.Error
import Text.Parsec.Pos hiding (Line)
import GHC.Generics (Generic)
import Data.Generics (Typeable)
import Control.Exception (Exception)

type Input = String

data PandocError = -- | Generic parse failure
                   ParseFailure String
                 -- | Error thrown by a Parsec parser
                 | ParsecError Input ParseError
                 deriving (Show, Typeable, Generic)

instance Exception PandocError

-- | An unsafe method to handle `PandocError`s.
handleError :: Either PandocError a -> a
handleError (Right r) = r
handleError (Left err) =
  case err of
    ParseFailure string -> error string
    ParsecError input err' ->
        let errPos = errorPos err'
            errLine = sourceLine errPos
            errColumn = sourceColumn errPos
            ls = lines input ++ [""]
            errorInFile = if length ls > errLine - 1
                            then concat ["\n", (ls !! (errLine - 1))
                                        ,"\n", replicate (errColumn - 1) ' '
                                        ,"^"]
                        else ""
        in  error $ "\nError at " ++ show  err'
                ++ errorInFile

