{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Pptx
   Copyright   : © 2025 Anton Antic
   License     : GNU GPL, version 2 or above

   Maintainer  : Anton Antic <anton@everworker.ai>
   Stability   : alpha
   Portability : portable

Conversion of PPTX (PowerPoint) documents to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Pptx (readPptx) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Codec.Archive.Zip (toArchiveOrFail)
import Control.Monad.Except (throwError)
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition (Pandoc(..))
import Text.Pandoc.Error (PandocError(..))
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Readers.Pptx.Parse (archiveToPptx)
import Text.Pandoc.Readers.Pptx.Slides (pptxToOutput)

-- | Read PPTX file into Pandoc AST
readPptx :: PandocMonad m => ReaderOptions -> B.ByteString -> m Pandoc
readPptx opts bytes =
  case toArchiveOrFail bytes of
    Right archive ->
      case archiveToPptx archive of
        Right pptx -> do
          -- Convert Pptx intermediate to Pandoc AST
          (meta, blocks) <- pptxToOutput opts pptx
          return $ Pandoc meta blocks

        Left err ->
          throwError $ PandocParseError $
            "Failed to parse PPTX: " <> err

    Left err ->
      throwError $ PandocParseError $
        "Failed to unpack PPTX archive: " <> T.pack err
