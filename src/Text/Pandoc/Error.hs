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
import Data.Generics (Typeable)
import GHC.Generics (Generic)
import Control.Exception (Exception)
import Text.Pandoc.Shared (err)

type Input = String

data PandocError = PandocFileReadError FilePath
                 | PandocShouldNeverHappenError String
                 | PandocSomeError String
                 | PandocParseError String
                 | PandocParsecError Input ParseError
                 deriving (Show, Typeable, Generic)


-- data PandocError = -- | Generic parse failure
--                    ParseFailure String
--                  -- | Error thrown by a Parsec parser
--                  | ParsecError Input ParseError
--                  deriving (Show, Typeable, Generic)

instance Exception PandocError

-- | Handle PandocError by exiting with an error message.
handleError :: Either PandocError a -> IO a
handleError (Right r) = return r
handleError (Left e) =
  case e of
    PandocFileReadError fp -> err 61 $ "problem reading " ++ fp
    PandocShouldNeverHappenError s -> err 62 s
    PandocSomeError s -> err 63 s
    PandocParseError s -> err 64 s
    PandocParsecError input err' ->
        let errPos = errorPos err'
            errLine = sourceLine errPos
            errColumn = sourceColumn errPos
            ls = lines input ++ [""]
            errorInFile = if length ls > errLine - 1
                            then concat ["\n", (ls !! (errLine - 1))
                                        ,"\n", replicate (errColumn - 1) ' '
                                        ,"^"]
                        else ""
        in  err 65 $ "\nError at " ++ show  err' ++ errorInFile

