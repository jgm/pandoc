{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{- |
   Module      : Text.Pandoc.Error
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
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
import Data.Typeable (Typeable)
import Data.Word (Word8)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Client (HttpException)
import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr)
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Printf (printf)
import Text.Parsec.Error
import Text.Parsec.Pos hiding (Line)

type Input = Text

data PandocError = PandocIOError Text IOError
                 | PandocHttpError Text HttpException
                 | PandocShouldNeverHappenError Text
                 | PandocSomeError Text
                 | PandocParseError Text
                 | PandocParsecError Input ParseError
                 | PandocMakePDFError Text
                 | PandocOptionError Text
                 | PandocSyntaxMapError Text
                 | PandocFailOnWarningError
                 | PandocPDFProgramNotFoundError Text
                 | PandocPDFError Text
                 | PandocFilterError Text Text
                 | PandocLuaError Text
                 | PandocCouldNotFindDataFileError Text
                 | PandocResourceNotFound Text
                 | PandocTemplateError Text
                 | PandocAppError Text
                 | PandocEpubSubdirectoryError Text
                 | PandocMacroLoop Text
                 | PandocUTF8DecodingError Text Int Word8
                 | PandocIpynbDecodingError Text
                 | PandocUnknownReaderError Text
                 | PandocUnknownWriterError Text
                 | PandocUnsupportedExtensionError Text Text
                 deriving (Show, Typeable, Generic)

instance Exception PandocError

-- | Handle PandocError by exiting with an error message.
handleError :: Either PandocError a -> IO a
handleError (Right r) = return r
handleError (Left e) =
  case e of
    PandocIOError _ err' -> ioError err'
    PandocHttpError u err' -> err 61 $
      "Could not fetch " <> u <> "\n" <> tshow err'
    PandocShouldNeverHappenError s -> err 62 $
      "Something we thought was impossible happened!\n" <>
      "Please report this to pandoc's developers: " <> s
    PandocSomeError s -> err 63 s
    PandocParseError s -> err 64 s
    PandocParsecError input err' ->
        let errPos = errorPos err'
            errLine = sourceLine errPos
            errColumn = sourceColumn errPos
            ls = T.lines input <> [""]
            errorInFile = if length ls > errLine - 1
                            then T.concat ["\n", ls !! (errLine - 1)
                                          ,"\n", T.replicate (errColumn - 1) " "
                                          ,"^"]
                        else ""
        in  err 65 $ "\nError at " <> tshow  err' <>
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
        pdfprog <> " not found. Please select a different --pdf-engine or install " <> pdfprog
    PandocPDFError logmsg -> err 43 $ "Error producing PDF.\n" <> logmsg
    PandocFilterError filtername msg -> err 83 $ "Error running filter " <>
        filtername <> ":\n" <> msg
    PandocLuaError msg -> err 84 $ "Error running Lua:\n" <> msg
    PandocCouldNotFindDataFileError fn -> err 97 $
        "Could not find data file " <> fn
    PandocResourceNotFound fn -> err 99 $
        "File " <> fn <> " not found in resource path"
    PandocTemplateError s -> err 5 $ "Error compiling template " <> s
    PandocAppError s -> err 4 s
    PandocEpubSubdirectoryError s -> err 31 $
      "EPUB subdirectory name '" <> s <> "' contains illegal characters"
    PandocMacroLoop s -> err 91 $
      "Loop encountered in expanding macro " <> s
    PandocUTF8DecodingError f offset w -> err 92 $
      "UTF-8 decoding error in " <> f <> " at byte offset " <> tshow offset <>
      " (" <> T.pack (printf "%2x" w) <> ").\n" <>
      "The input must be a UTF-8 encoded text."
    PandocIpynbDecodingError w -> err 93 $
      "ipynb decoding error: " <> w
    PandocUnknownReaderError r -> err 21 $
      "Unknown input format " <> r <>
      case r of
        "doc" -> "\nPandoc can convert from DOCX, but not from DOC." <>
                 "\nTry using Word to save your DOC file as DOCX," <>
                 " and convert that with pandoc."
        "pdf" -> "\nPandoc can convert to PDF, but not from PDF."
        _     -> ""
    PandocUnknownWriterError w -> err 22 $
       "Unknown output format " <> w <>
       case w of
         "pdf" -> "To create a pdf using pandoc, use" <>
                  " -t latex|beamer|context|ms|html5" <>
                 "\nand specify an output file with " <>
                 ".pdf extension (-o filename.pdf)."
         "doc" -> "\nPandoc can convert to DOCX, but not from DOC."
         _     -> ""
    PandocUnsupportedExtensionError ext f -> err 23 $
      "The extension " <> ext <> " is not supported " <>
      "for " <> f

err :: Int -> Text -> IO a
err exitCode msg = do
  UTF8.hPutStrLn stderr (T.unpack msg)
  exitWith $ ExitFailure exitCode
  return undefined

tshow :: Show a => a -> Text
tshow = T.pack . show
