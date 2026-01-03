{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{- |
   Module      : Text.Pandoc.Error
   Copyright   : Copyright (C) 2006-2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

This module provides a standard way to deal with possible errors
encountered during parsing.

-}
module Text.Pandoc.Error (
  PandocError(..),
  renderError,
  handleError) where

import Control.Exception (Exception, displayException)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr)
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Printf (printf)
import Text.Pandoc.Shared (tshow)
import Citeproc (CiteprocError, prettyCiteprocError)

data PandocError = PandocIOError Text IOError
                 | PandocHttpError Text Text
                 | PandocShouldNeverHappenError Text
                 | PandocSomeError Text
                 | PandocParseError Text
                 | PandocMakePDFError Text
                 | PandocOptionError Text
                 | PandocSyntaxMapError Text
                 | PandocFailOnWarningError
                 | PandocPDFProgramNotFoundError Text
                 | PandocPDFError Text
                 | PandocXMLError Text Text
                 | PandocFilterError Text Text
                 | PandocLuaError Text
                 | PandocNoScriptingEngine
                 | PandocCouldNotFindDataFileError Text
                 | PandocCouldNotFindMetadataFileError Text
                 | PandocResourceNotFound Text
                 | PandocTemplateError Text
                 | PandocNoTemplateError Text
                 | PandocAppError Text
                 | PandocEpubSubdirectoryError Text
                 | PandocMacroLoop Text
                 | PandocUTF8DecodingError Text Int Word8
                 | PandocIpynbDecodingError Text
                 | PandocUnsupportedCharsetError Text
                 | PandocFormatError Text Text
                 | PandocUnknownReaderError Text
                 | PandocUnknownWriterError Text
                 | PandocUnsupportedExtensionError Text Text
                 | PandocCiteprocError CiteprocError
                 | PandocBibliographyError Text Text
                 | PandocInputNotTextError Text
                 deriving (Show, Typeable, Generic)

instance Exception PandocError

renderError :: PandocError -> Text
renderError e =
  case e of
    PandocIOError _ err' -> T.pack $ displayException err'
    PandocHttpError u err' -> "Could not fetch " <> u <> "\n" <> err'
    PandocShouldNeverHappenError s ->
      "Something we thought was impossible happened!\n" <>
      "Please report this to pandoc's developers: " <> s
    PandocSomeError s -> s
    PandocParseError s -> s
    PandocMakePDFError s -> s
    PandocOptionError s -> s
    PandocSyntaxMapError s -> s
    PandocFailOnWarningError -> "Failing because there were warnings."
    PandocPDFProgramNotFoundError pdfprog ->
        pdfprog <> " not found. Please select a different --pdf-engine or install " <> pdfprog
    PandocPDFError logmsg -> "Error producing PDF.\n" <> logmsg
    PandocXMLError fp logmsg -> "Invalid XML" <>
        (if T.null fp then "" else " in " <> fp) <> ":\n" <> logmsg
    PandocFilterError filtername msg -> "Error running filter " <>
        filtername <> ":\n" <> msg
    PandocLuaError msg -> "Error running Lua:\n" <> msg
    PandocNoScriptingEngine -> "This version of pandoc has been compiled " <>
                               "without Lua support."
    PandocCouldNotFindDataFileError fn ->
        "Could not find data file " <> fn
    PandocCouldNotFindMetadataFileError fn ->
        "Could not find metadata file " <> fn
    PandocResourceNotFound fn ->
        "File " <> fn <> " not found in resource path"
    PandocTemplateError s -> "Error compiling template " <> s
    PandocNoTemplateError fp -> "No template defined in " <> fp
    PandocAppError s -> s
    PandocEpubSubdirectoryError s ->
      "EPUB subdirectory name '" <> s <> "' contains illegal characters"
    PandocMacroLoop s ->
      "Loop encountered in expanding macro " <> s
    PandocUTF8DecodingError f offset w ->
      "UTF-8 decoding error in " <> f <> " at byte offset " <> tshow offset <>
      " (" <> T.pack (printf "%2x" w) <> ").\n" <>
      "The input must be a UTF-8 encoded text."
    PandocIpynbDecodingError w ->
      "ipynb decoding error: " <> w
    PandocUnsupportedCharsetError charset ->
      "Unsupported charset " <> charset
    PandocFormatError format s ->
      "Error parsing format " <> tshow format <> ": " <> s
    PandocUnknownReaderError r ->
      "Unknown input format " <> r <>
      case r of
        "doc" -> "\nPandoc can convert from DOCX, but not from DOC." <>
                 "\nTry using Word to save your DOC file as DOCX," <>
                 " and convert that with pandoc."
        "pdf" -> "\nPandoc can convert to PDF, but not from PDF."
        _     -> ""
    PandocUnknownWriterError w ->
       "Unknown output format " <> w <>
       case w of
         "pdf" -> "To create a pdf using pandoc, use" <>
                  " -t latex|beamer|context|ms|html5|typst" <>
                 "\nand specify an output file with " <>
                 ".pdf extension (-o filename.pdf)."
         "doc" -> "\nPandoc can convert to DOCX, but not to DOC."
         _     -> ""
    PandocUnsupportedExtensionError ext f ->
      "The extension " <> ext <> " is not supported " <>
      "for " <> f <> ".\nUse --list-extensions=" <> f <> " to " <>
      "list supported extensions."
    PandocCiteprocError e' ->
      prettyCiteprocError e'
    PandocBibliographyError fp msg ->
      "Error reading bibliography file " <> fp <> ":\n" <> msg
    PandocInputNotTextError fp ->
      "Expected text as an input, but received binary data from " <>
      (if T.null fp
        then "stdin"
        else "file " <> fp) <>
      ".\nIf you intended to convert from binary format, verify that it's " <>
      "supported and use\nexplicit -f FORMAT."


-- | Handle PandocError by exiting with an error message.
handleError :: Either PandocError a -> IO a
handleError (Right r) = return r
handleError (Left e) =
  case e of
    PandocIOError _ err' -> ioError err'
    _ -> err exitCode (renderError e)
 where
  exitCode =
    case e of
      PandocIOError{} -> 1
      PandocFailOnWarningError{} -> 3
      PandocAppError{} -> 4
      PandocTemplateError{} -> 5
      PandocOptionError{} -> 6
      PandocFormatError{} -> 20
      PandocUnknownReaderError{} -> 21
      PandocUnknownWriterError{} -> 22
      PandocUnsupportedExtensionError{} -> 23
      PandocCiteprocError{} -> 24
      PandocBibliographyError{} -> 25
      PandocEpubSubdirectoryError{} -> 31
      PandocPDFError{} -> 43
      PandocXMLError{} -> 44
      PandocPDFProgramNotFoundError{} -> 47
      PandocHttpError{} -> 61
      PandocShouldNeverHappenError{} -> 62
      PandocSomeError{} -> 63
      PandocParseError{} -> 64
      PandocMakePDFError{} -> 66
      PandocSyntaxMapError{} -> 67
      PandocFilterError{} -> 83
      PandocLuaError{} -> 84
      PandocNoTemplateError{} -> 87
      PandocNoScriptingEngine -> 89
      PandocMacroLoop{} -> 91
      PandocUTF8DecodingError{} -> 92
      PandocIpynbDecodingError{} -> 93
      PandocUnsupportedCharsetError{} -> 94
      PandocInputNotTextError{} -> 95
      PandocCouldNotFindDataFileError{} -> 97
      PandocCouldNotFindMetadataFileError{} -> 98
      PandocResourceNotFound{} -> 99

err :: Int -> Text -> IO a
err exitCode msg = do
  UTF8.hPutStrLn stderr msg
  exitWith $ ExitFailure exitCode
  return undefined
