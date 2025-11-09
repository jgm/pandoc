{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Xlsx
   Copyright   : © 2025 Anton Antic
   License     : GNU GPL, version 2 or above

   Maintainer  : Anton Antic <anton@everworker.ai>
   Stability   : alpha
   Portability : portable

Conversion of XLSX (Excel spreadsheet) documents to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Xlsx (readXlsx) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Codec.Archive.Zip (toArchiveOrFail)
import Control.Monad.Except (throwError)
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition (Pandoc(..))
import Text.Pandoc.Error (PandocError(..))
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Readers.Xlsx.Parse (archiveToXlsx)
import Text.Pandoc.Readers.Xlsx.Sheets (xlsxToOutput)

-- | Read XLSX file into Pandoc AST
readXlsx :: PandocMonad m => ReaderOptions -> B.ByteString -> m Pandoc
readXlsx opts bytes =
  case toArchiveOrFail bytes of
    Right archive ->
      case archiveToXlsx archive of
        Right xlsx -> do
          let (meta, blocks) = xlsxToOutput opts xlsx
          return $ Pandoc meta blocks
        Left err ->
          throwError $ PandocParseError $ "Failed to parse XLSX: " <> err

    Left err ->
      throwError $ PandocParseError $
        "Failed to unpack XLSX archive: " <> T.pack err
