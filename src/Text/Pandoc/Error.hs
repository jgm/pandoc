{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{- |
   Module      : Text.Pandoc.Error
   Copyright   : Copyright (C) 2006-2019 John MacFarlane
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

import Prelude
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Network.HTTP.Client (HttpException)
import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr)
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Printf (printf)
import Text.Parsec.Error
import Text.Parsec.Pos hiding (Line)

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
                 | PandocUTF8DecodingError String Int Word8
                 | PandocIpynbDecodingError String
                 | PandocSandboxError String
                 deriving (Show, Typeable, Generic)

instance Exception PandocError

-- | Handle PandocError by exiting with an error message.
handleError :: Either PandocError a -> IO a
handleError (Right r) = return r
handleError (Left e) =
  case e of
    PandocIOError fp err' -> do
      UTF8.hPutStrLn stderr $ "IO Error (" ++ show fp ++ ")"
      ioError err'
    PandocHttpError u err' -> err 61 $
      "Could not fetch " ++ u ++ "\n" ++ show err'
    PandocShouldNeverHappenError s -> err 62 $
      "Something we thought was impossible happened!\n" ++
      "Please report this to pandoc's developers: " ++ s
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
        in  err 65 $ "\nError at " ++ show  err' ++
                     -- if error comes from a chunk or included file,
                     -- then we won't get the right text this way:
                     if sourceName errPos == "source"
                        then errorInFile
                        else ""
    PandocMakePDFError s -> err 66 s
    PandocOptionError s -> err 6 s
    PandocSyntaxMapError s -> err 67 s
    PandocFailOnWarningError -> err 3 "Failing because there were warnings."
    PandocPDFProgramNotFoundError pdfprog -> err 47 $
        pdfprog ++ " not found. Please select a different --pdf-engine or install " ++ pdfprog
    PandocPDFError logmsg -> err 43 $ "Error producing PDF.\n" ++ logmsg
    PandocFilterError filtername msg -> err 83 $ "Error running filter " ++
        filtername ++ ":\n" ++ msg
    PandocCouldNotFindDataFileError fn -> err 97 $
        "Could not find data file " ++ fn
    PandocResourceNotFound fn -> err 99 $
        "File " ++ fn ++ " not found in resource path"
    PandocTemplateError s -> err 5 $ "Error compiling template " ++ s
    PandocAppError s -> err 4 s
    PandocEpubSubdirectoryError s -> err 31 $
      "EPUB subdirectory name '" ++ s ++ "' contains illegal characters"
    PandocMacroLoop s -> err 91 $
      "Loop encountered in expanding macro " ++ s
    PandocUTF8DecodingError f offset w -> err 92 $
      "UTF-8 decoding error in " ++ f ++ " at byte offset " ++ show offset ++
      " (" ++ printf "%2x" w ++ ").\n" ++
      "The input must be a UTF-8 encoded text."
    PandocIpynbDecodingError w -> err 93 $
      "ipynb decoding error: " ++ w
    PandocSandboxError msg -> err 94 $
      "Attempted IO action in sandbox: " ++ msg

err :: Int -> String -> IO a
err exitCode msg = do
  UTF8.hPutStrLn stderr msg
  exitWith $ ExitFailure exitCode
  return undefined
