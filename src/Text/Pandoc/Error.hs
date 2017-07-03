{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-
Copyright (C) 2006-2017 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-2017 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

This module provides a standard way to deal with possible errors encounted
during parsing.

-}
module Text.Pandoc.Error (
  PandocError(..),
  handleError) where

import Control.Exception (Exception)
import Data.Generics (Typeable)
import GHC.Generics (Generic)
import Text.Parsec.Error
import Text.Parsec.Pos hiding (Line)
import qualified Text.Pandoc.UTF8 as UTF8
import System.Exit (exitWith, ExitCode(..))
import System.IO (stderr)
import Network.HTTP.Client (HttpException)

type Input = String

data PandocError = PandocIOError String IOError
                 | PandocHttpError String HttpException
                 | PandocShouldNeverHappenError String
                 | PandocSomeError String
                 | PandocParseError String
                 | PandocParsecError Input ParseError
                 | PandocMakePDFError String
                 | PandocOptionError String
                 | PandocSyntaxMapError String
                 | PandocFailOnWarningError
                 | PandocPDFProgramNotFoundError String
                 | PandocPDFError String
                 | PandocFilterError String String
                 | PandocCouldNotFindDataFileError String
                 | PandocResourceNotFound String
                 | PandocTemplateError String
                 | PandocAppError String
                 | PandocEpubSubdirectoryError String
                 | PandocMacroLoop String
                 deriving (Show, Typeable, Generic)

instance Exception PandocError

-- | Handle PandocError by exiting with an error message.
handleError :: Either PandocError a -> IO a
handleError (Right r) = return r
handleError (Left e) =
  case e of
    PandocIOError _ err' -> ioError err'
    PandocHttpError u err' -> err 61 $
      "Could not fetch " ++ u ++ "\n" ++ show err'
    PandocShouldNeverHappenError s -> err 62 s
    PandocSomeError s -> err 63 s
    PandocParseError s -> err 64 s
    PandocParsecError input err' ->
        let errPos = errorPos err'
            errLine = sourceLine errPos
            errColumn = sourceColumn errPos
            ls = lines input ++ [""]
            errorInFile = if length ls > errLine - 1
                            then concat ["\n", ls !! (errLine - 1)
                                        ,"\n", replicate (errColumn - 1) ' '
                                        ,"^"]
                        else ""
        in  err 65 $ "\nError at " ++ show  err' ++ errorInFile
    PandocMakePDFError s -> err 65 s
    PandocOptionError s -> err 2 s
    PandocSyntaxMapError s -> err 67 s
    PandocFailOnWarningError -> err 3 "Failing because there were warnings."
    PandocPDFProgramNotFoundError pdfprog -> err 47 $
        pdfprog ++ " not found. " ++ pdfprog ++ " is needed for pdf output."
    PandocPDFError logmsg -> err 43 $ "Error producing PDF.\n" ++ logmsg
    PandocFilterError filtername msg -> err 83 $ "Error running filter " ++
        filtername ++ ":\n" ++ msg
    PandocCouldNotFindDataFileError fn -> err 97 $
        "Could not find data file " ++ fn
    PandocResourceNotFound fn -> err 99 $
        "File " ++ fn ++ " not found in resource path"
    PandocTemplateError s -> err 5 s
    PandocAppError s -> err 1 s
    PandocEpubSubdirectoryError s -> err 31 $
      "EPUB subdirectory name '" ++ s ++ "' contains illegal characters"
    PandocMacroLoop s -> err 91 $
      "Loop encountered in expanding macro " ++ s

err :: Int -> String -> IO a
err exitCode msg = do
  UTF8.hPutStrLn stderr msg
  exitWith $ ExitFailure exitCode
  return undefined
