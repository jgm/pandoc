{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{- |
   Module      : Text.Pandoc.Error
   Copyright   : Copyright (C) 2006-2021 John MacFarlane
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
import Data.List (sortOn)
import qualified Data.Text as T
import Data.Ord (Down(..))
import GHC.Generics (Generic)
import Network.HTTP.Client (HttpException)
import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr)
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Sources (Sources(..))
import Text.Printf (printf)
import Text.Parsec.Error
import Text.Parsec.Pos hiding (Line)
import Text.Pandoc.Shared (tshow)
import Citeproc (CiteprocError, prettyCiteprocError)

data PandocError = PandocIOError Text IOError
                 | PandocHttpError Text HttpException
                 | PandocShouldNeverHappenError Text
                 | PandocSomeError Text
                 | PandocParseError Text
                 | PandocParsecError Sources ParseError
                 | PandocMakePDFError Text
                 | PandocOptionError Text
                 | PandocSyntaxMapError Text
                 | PandocFailOnWarningError
                 | PandocPDFProgramNotFoundError Text
                 | PandocPDFError Text
                 | PandocXMLError Text Text
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
                 | PandocUnsupportedCharsetError Text
                 | PandocUnknownReaderError Text
                 | PandocUnknownWriterError Text
                 | PandocUnsupportedExtensionError Text Text
                 | PandocCiteprocError CiteprocError
                 | PandocBibliographyError Text Text
                 deriving (Show, Typeable, Generic)

instance Exception PandocError

renderError :: PandocError -> Text
renderError e =
  case e of
    PandocIOError _ err' -> T.pack $ displayException err'
    PandocHttpError u err' ->
      "Could not fetch " <> u <> "\n" <> tshow err'
    PandocShouldNeverHappenError s ->
      "Something we thought was impossible happened!\n" <>
      "Please report this to pandoc's developers: " <> s
    PandocSomeError s -> s
    PandocParseError s -> s
    PandocParsecError (Sources inputs) err' ->
        let errPos = errorPos err'
            errLine = sourceLine errPos
            errColumn = sourceColumn errPos
            errFile = sourceName errPos
            errorInFile =
              case sortOn (Down . sourceLine . fst)
                      [ (pos,t)
                        | (pos,t) <- inputs
                        , sourceName pos == errFile
                        , sourceLine pos <= errLine
                      ] of
                []  -> ""
                ((pos,txt):_) ->
                  let ls = T.lines txt <> [""]
                      ln = (errLine - sourceLine pos) + 1
                   in if length ls > ln && ln >= 1
                         then T.concat ["\n", ls !! (ln - 1)
                                       ,"\n", T.replicate (errColumn - 1) " "
                                       ,"^"]
                         else ""
        in  "Error at " <> tshow  err' <> errorInFile
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
    PandocCouldNotFindDataFileError fn ->
        "Could not find data file " <> fn
    PandocResourceNotFound fn ->
        "File " <> fn <> " not found in resource path"
    PandocTemplateError s -> "Error compiling template " <> s
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
                  " -t latex|beamer|context|ms|html5" <>
                 "\nand specify an output file with " <>
                 ".pdf extension (-o filename.pdf)."
         "doc" -> "\nPandoc can convert to DOCX, but not to DOC."
         _     -> ""
    PandocUnsupportedExtensionError ext f ->
      "The extension " <> ext <> " is not supported " <>
      "for " <> f
    PandocCiteprocError e' ->
      prettyCiteprocError e'
    PandocBibliographyError fp msg ->
      "Error reading bibliography file " <> fp <> ":\n" <> msg


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
      PandocHttpError{} -> 61
      PandocShouldNeverHappenError{} -> 62
      PandocSomeError{} -> 63
      PandocParseError{} -> 64
      PandocParsecError{} -> 65
      PandocMakePDFError{} -> 66
      PandocOptionError{} -> 6
      PandocSyntaxMapError{} -> 67
      PandocFailOnWarningError{} -> 3
      PandocPDFProgramNotFoundError{} -> 47
      PandocPDFError{} -> 43
      PandocXMLError{} -> 44
      PandocFilterError{} -> 83
      PandocLuaError{} -> 84
      PandocCouldNotFindDataFileError{} -> 97
      PandocResourceNotFound{} -> 99
      PandocTemplateError{} -> 5
      PandocAppError{} -> 4
      PandocEpubSubdirectoryError{} -> 31
      PandocMacroLoop{} -> 91
      PandocUTF8DecodingError{} -> 92
      PandocIpynbDecodingError{} -> 93
      PandocUnsupportedCharsetError{} -> 94
      PandocUnknownReaderError{} -> 21
      PandocUnknownWriterError{} -> 22
      PandocUnsupportedExtensionError{} -> 23
      PandocCiteprocError{} -> 24
      PandocBibliographyError{} -> 25

err :: Int -> Text -> IO a
err exitCode msg = do
  UTF8.hPutStrLn stderr msg
  exitWith $ ExitFailure exitCode
  return undefined
