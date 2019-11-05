module Text.Pandoc.Legacy.MIME
  ( MimeType
  , getMimeType
  , getMimeTypeDef
  , extensionFromMimeType
  , mediaCategory
  ) where

import qualified Data.Text as T
import qualified Text.Pandoc.MIME as TP

type MimeType = String

getMimeType :: FilePath -> Maybe MimeType
getMimeType = fmap T.unpack . TP.getMimeType

getMimeTypeDef :: FilePath -> MimeType
getMimeTypeDef = T.unpack . TP.getMimeTypeDef

extensionFromMimeType :: MimeType -> Maybe String
extensionFromMimeType = fmap T.unpack . TP.extensionFromMimeType . T.pack

mediaCategory :: FilePath -> Maybe String
mediaCategory = fmap T.unpack . TP.mediaCategory
