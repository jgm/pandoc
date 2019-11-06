{-# LANGUAGE ViewPatterns #-}
module Text.Pandoc.Legacy.MediaBag (
                     TP.MediaBag,
                     TP.deleteMedia,
                     lookupMedia,
                     insertMedia,
                     mediaDirectory,
                     mediaItems
                     ) where

import qualified Text.Pandoc.MediaBag as TP
import Text.Pandoc.Legacy.MIME (MimeType)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

insertMedia :: FilePath
            -> Maybe MimeType
            -> BL.ByteString
            -> TP.MediaBag
            -> TP.MediaBag
insertMedia fp = TP.insertMedia fp . fmap T.pack

lookupMedia :: FilePath
            -> TP.MediaBag
            -> Maybe (MimeType, BL.ByteString)
lookupMedia fp = fmap (\(x, y) -> (T.unpack x, y)) . TP.lookupMedia fp

-- | Get a list of the file paths stored in a 'MediaBag', with
-- their corresponding mime types and the lengths in bytes of the contents.
mediaDirectory :: TP.MediaBag -> [(FilePath, MimeType, Int)]
mediaDirectory = map go . TP.mediaDirectory
  where
    go (x, y, z) = (x, T.unpack y, z)

mediaItems :: TP.MediaBag -> [(FilePath, MimeType, BL.ByteString)]
mediaItems = map go . TP.mediaItems
  where
    go (x, y, z) = (x, T.unpack y, z)  
