{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Text.Pandoc.Legacy.Error
  ( TP.PandocError(TP.PandocFailOnWarningError)
  , pattern PandocIOError
  , pattern PandocHttpError
  , pattern PandocShouldNeverHappenError
  , pattern PandocSomeError
  , pattern PandocParseError
  , pattern PandocParsecError
  , pattern PandocMakePDFError
  , pattern PandocOptionError
  , pattern PandocSyntaxMapError
  , pattern PandocPDFProgramNotFoundError
  , pattern PandocPDFError
  , pattern PandocFilterError
  , pattern PandocCouldNotFindDataFileError
  , pattern PandocResourceNotFound
  , pattern PandocTemplateError
  , pattern PandocAppError
  , pattern PandocEpubSubdirectoryError
  , pattern PandocMacroLoop
  , pattern PandocUTF8DecodingError
  , pattern PandocIpynbDecodingError
  , pattern PandocUnknownReaderError
  , pattern PandocUnknownWriterError
  , pattern PandocUnsupportedExtensionError  
  , TP.handleError
  ) where

import qualified Data.Text as T
import qualified Text.Pandoc.Error as TP
import Text.Parsec.Error (ParseError)
import Data.Word (Word8)
import Network.HTTP.Client (HttpException)

type Input = String

on1 :: (T.Text -> a) -> String -> a
on1 f = f . T.pack

on2 :: (T.Text -> T.Text -> a) -> String -> String -> a
on2 f x = f (T.pack x) . T.pack

pattern PandocIOError :: String -> IOError -> TP.PandocError
pattern PandocIOError x y <- TP.PandocIOError (T.unpack -> x) y
  where
    PandocIOError = on1 TP.PandocIOError

pattern PandocHttpError :: String -> HttpException -> TP.PandocError
pattern PandocHttpError x y <- TP.PandocHttpError (T.unpack -> x) y
  where
    PandocHttpError = on1 TP.PandocHttpError

pattern PandocShouldNeverHappenError :: String -> TP.PandocError
pattern PandocShouldNeverHappenError x <- TP.PandocShouldNeverHappenError (T.unpack -> x)
  where
    PandocShouldNeverHappenError = on1 TP.PandocShouldNeverHappenError

pattern PandocSomeError :: String -> TP.PandocError
pattern PandocSomeError x <- TP.PandocSomeError (T.unpack -> x)
  where
    PandocSomeError = on1 TP.PandocSomeError

pattern PandocParseError :: String -> TP.PandocError
pattern PandocParseError x <- TP.PandocParseError (T.unpack -> x)
  where
    PandocParseError = on1 TP.PandocParseError

pattern PandocParsecError :: Input -> ParseError -> TP.PandocError
pattern PandocParsecError x y <- TP.PandocParsecError (T.unpack -> x) y
  where
    PandocParsecError = on1 TP.PandocParsecError

pattern PandocMakePDFError :: String -> TP.PandocError
pattern PandocMakePDFError x <- TP.PandocMakePDFError (T.unpack -> x)
  where
    PandocMakePDFError = on1 TP.PandocMakePDFError

pattern PandocOptionError :: String -> TP.PandocError
pattern PandocOptionError x <- TP.PandocOptionError (T.unpack -> x)
  where
    PandocOptionError = on1 TP.PandocOptionError

pattern PandocSyntaxMapError :: String -> TP.PandocError
pattern PandocSyntaxMapError x <- TP.PandocSyntaxMapError (T.unpack -> x)
  where
    PandocSyntaxMapError = on1 TP.PandocSyntaxMapError

pattern PandocPDFProgramNotFoundError :: String -> TP.PandocError
pattern PandocPDFProgramNotFoundError x <- TP.PandocPDFProgramNotFoundError (T.unpack -> x)
  where
    PandocPDFProgramNotFoundError = on1 TP.PandocPDFProgramNotFoundError

pattern PandocPDFError :: String -> TP.PandocError
pattern PandocPDFError x <- TP.PandocPDFError (T.unpack -> x)
  where
    PandocPDFError = on1 TP.PandocPDFError

pattern PandocFilterError :: String -> String -> TP.PandocError
pattern PandocFilterError x y <- TP.PandocFilterError (T.unpack -> x) (T.unpack -> y)
  where
    PandocFilterError = on2 TP.PandocFilterError

pattern PandocCouldNotFindDataFileError :: String -> TP.PandocError
pattern PandocCouldNotFindDataFileError x <- TP.PandocCouldNotFindDataFileError (T.unpack -> x)
  where
    PandocCouldNotFindDataFileError = on1 TP.PandocCouldNotFindDataFileError

pattern PandocResourceNotFound :: String -> TP.PandocError
pattern PandocResourceNotFound x <- TP.PandocResourceNotFound (T.unpack -> x)
  where
    PandocResourceNotFound = on1 TP.PandocResourceNotFound

pattern PandocTemplateError :: String -> TP.PandocError
pattern PandocTemplateError x <- TP.PandocTemplateError (T.unpack -> x)
  where
    PandocTemplateError = on1 TP.PandocTemplateError

pattern PandocAppError :: String -> TP.PandocError
pattern PandocAppError x <- TP.PandocAppError (T.unpack -> x)
  where
    PandocAppError = on1 TP.PandocAppError

pattern PandocEpubSubdirectoryError :: String -> TP.PandocError
pattern PandocEpubSubdirectoryError x <- TP.PandocEpubSubdirectoryError (T.unpack -> x)
  where
    PandocEpubSubdirectoryError = on1 TP.PandocEpubSubdirectoryError

pattern PandocMacroLoop :: String -> TP.PandocError
pattern PandocMacroLoop x <- TP.PandocMacroLoop (T.unpack -> x)
  where
    PandocMacroLoop = on1 TP.PandocMacroLoop

pattern PandocUTF8DecodingError :: String -> Int -> Word8 -> TP.PandocError
pattern PandocUTF8DecodingError x y z <- TP.PandocUTF8DecodingError (T.unpack -> x) y z
  where
    PandocUTF8DecodingError = on1 TP.PandocUTF8DecodingError

pattern PandocIpynbDecodingError :: String -> TP.PandocError
pattern PandocIpynbDecodingError x <- TP.PandocIpynbDecodingError (T.unpack -> x)
  where
    PandocIpynbDecodingError = on1 TP.PandocIpynbDecodingError

pattern PandocUnknownReaderError :: String -> TP.PandocError
pattern PandocUnknownReaderError x <- TP.PandocUnknownReaderError (T.unpack -> x)
  where
    PandocUnknownReaderError = on1 TP.PandocUnknownReaderError

pattern PandocUnknownWriterError :: String -> TP.PandocError
pattern PandocUnknownWriterError x <- TP.PandocUnknownWriterError (T.unpack -> x)
  where
    PandocUnknownWriterError = on1 TP.PandocUnknownWriterError

pattern PandocUnsupportedExtensionError :: String -> String -> TP.PandocError
pattern PandocUnsupportedExtensionError x y <- TP.PandocUnsupportedExtensionError (T.unpack -> x) (T.unpack -> y)
  where
    PandocUnsupportedExtensionError = on2 TP.PandocUnsupportedExtensionError
