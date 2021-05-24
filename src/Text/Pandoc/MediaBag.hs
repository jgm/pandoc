{-# LANGUAGE CPP                        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
   Module      : Text.Pandoc.MediaBag
   Copyright   : Copyright (C) 2014-2015, 2017-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Definition of a MediaBag object to hold binary resources, and an
interface for interacting with it.
-}
module Text.Pandoc.MediaBag (
                     MediaItem(..),
                     MediaBag,
                     deleteMedia,
                     lookupMedia,
                     insertMedia,
                     mediaDirectory,
                     mediaItems
                     ) where
import qualified Data.ByteString.Lazy as BL
import Data.Data (Data)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import System.FilePath
import Text.Pandoc.MIME (MimeType, getMimeTypeDef, extensionFromMimeType)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Digest.Pure.SHA (sha1, showDigest)

data MediaItem =
  MediaItem
  { mediaMimeType :: MimeType
  , mediaPath :: FilePath
  , mediaContents :: BL.ByteString
  } deriving (Eq, Ord, Show, Data, Typeable)

-- | A container for a collection of binary resources, with names and
-- mime types.  Note that a 'MediaBag' is a Monoid, so 'mempty'
-- can be used for an empty 'MediaBag', and '<>' can be used to append
-- two 'MediaBag's.
newtype MediaBag = MediaBag (M.Map Text MediaItem)
        deriving (Semigroup, Monoid, Data, Typeable)

instance Show MediaBag where
  show bag = "MediaBag " ++ show (mediaDirectory bag)

-- | We represent paths with /, in normalized form.
canonicalize :: FilePath -> Text
canonicalize = T.replace "\\" "/" . T.pack . normalise

-- | Delete a media item from a 'MediaBag', or do nothing if no item corresponds
-- to the given path.
deleteMedia :: FilePath       -- ^ relative path and canonical name of resource
            -> MediaBag
            -> MediaBag
deleteMedia fp (MediaBag mediamap) =
  MediaBag $ M.delete (canonicalize fp) mediamap

-- | Insert a media item into a 'MediaBag', replacing any existing
-- value with the same name.
insertMedia :: FilePath       -- ^ relative path and canonical name of resource
            -> Maybe MimeType -- ^ mime type (Nothing = determine from extension)
            -> BL.ByteString  -- ^ contents of resource
            -> MediaBag
            -> MediaBag
insertMedia fp mbMime contents (MediaBag mediamap) =
  MediaBag (M.insert (canonicalize fp) mediaItem mediamap)
  where mediaItem = MediaItem{ mediaPath = showDigest (sha1 contents) <>
                                 "." <> ext
                             , mediaContents = contents
                             , mediaMimeType = mt }
        fallback = case takeExtension fp of
                        ".gz" -> getMimeTypeDef $ dropExtension fp
                        _     -> getMimeTypeDef fp
        mt = fromMaybe fallback mbMime
        ext = maybe (takeExtension fp) T.unpack $ extensionFromMimeType mt


-- | Lookup a media item in a 'MediaBag', returning mime type and contents.
lookupMedia :: FilePath
            -> MediaBag
            -> Maybe MediaItem
lookupMedia fp (MediaBag mediamap) = M.lookup (canonicalize fp) mediamap

-- | Get a list of the file paths stored in a 'MediaBag', with
-- their corresponding mime types and the lengths in bytes of the contents.
mediaDirectory :: MediaBag -> [(FilePath, MimeType, Int)]
mediaDirectory (MediaBag mediamap) =
  M.foldrWithKey (\fp item ->
      ((T.unpack fp, mediaMimeType item,
        fromIntegral (BL.length (mediaContents item))):)) [] mediamap

mediaItems :: MediaBag -> [(FilePath, MimeType, BL.ByteString)]
mediaItems (MediaBag mediamap) =
  M.foldrWithKey (\fp item ->
      ((T.unpack fp, mediaMimeType item, mediaContents item):))
     [] mediamap
