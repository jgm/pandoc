{-# LANGUAGE CPP                        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
   Module      : Text.Pandoc.MediaBag
   Copyright   : Copyright (C) 2014-2015, 2017-2024 John MacFarlane
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
import Crypto.Hash (hashlazy, Digest, SHA1)
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Data.Data (Data)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Typeable (Typeable)
import System.FilePath
import qualified System.FilePath.Posix as Posix
import qualified System.FilePath.Windows as Windows
import Text.Pandoc.MIME (MimeType, getMimeTypeDef, extensionFromMimeType)
import Text.Pandoc.Shared (makeCanonical)
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI (URI (..), parseURI, unEscapeString)
import Text.Pandoc.URI (isURI)

data MediaItem =
  MediaItem
  { mediaMimeType :: !MimeType
  , mediaPath :: !FilePath
  , mediaContents :: BL.ByteString
    -- ^ Left lazy so that contents need not be forced at insert time.
  } deriving (Eq, Ord, Show, Data, Typeable)

-- | A container for a collection of binary resources, with names and
-- mime types.  Note that a 'MediaBag' is a Monoid, so 'mempty'
-- can be used for an empty 'MediaBag', and '<>' can be used to append
-- two 'MediaBag's.
newtype MediaBag = MediaBag (M.Map Text MediaItem)
        deriving (Semigroup, Monoid, Data, Typeable)

instance Show MediaBag where
  show bag = "MediaBag " ++ show (mediaDirectory bag)

-- | Check for the (case-insensitive) @data:@ URI scheme.
isDataURI :: FilePath -> Bool
isDataURI = (== "data:") . map toLower . take 5

-- | We represent paths with /, in canonical form (redundant @.@ and
-- @..@ components removed).  Percent-encoding is not resolved.
canonicalize :: FilePath -> Text
canonicalize fp
  -- avoid an expensive call to isURI for data URIs:
  | isDataURI fp = fp'
  | isURI fp' = fp'
  | otherwise = T.pack . makeCanonical . map slashify $ fp
 where
  fp' = T.pack fp
  slashify '\\' = '/'
  slashify c    = c

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
insertMedia fp mbMime contents (MediaBag mediamap)
 | isDataURI fp
 , Just mt' <- mbMime
   = MediaBag (M.insert fp'
               MediaItem{ mediaPath = hashpath
                        , mediaContents = contents
                        , mediaMimeType = mt' } mediamap)
 | otherwise = MediaBag (M.insert fp' mediaItem mediamap)
 where
  mediaItem = MediaItem{ mediaPath = newpath
                       , mediaContents = contents
                       , mediaMimeType = mt }
  fp' = canonicalize fp
  fp'' = unEscapeString $ T.unpack fp'
  uri = parseURI fp
  hashpath = show (hashlazy contents :: Digest SHA1) <> ext
  -- We only keep the original name if the key contains no
  -- percent-encoding, i.e., unescaping is the identity; otherwise
  -- distinct keys (e.g. "a%20b.png" and "a b.png") could unescape
  -- to the same mediaPath and clobber each other on extraction.
  newpath = if Posix.isRelative fp''
                 && Windows.isRelative fp''
                 && isNothing uri
                 && not containsParentRef
                 && not (T.any (== '%') fp')
               then fp''
               else hashpath
  -- Check for a ".." path component (treating both / and \ as
  -- separators, since the unescaped path may contain backslashes
  -- from percent-encoding); a mere ".." substring (as in
  -- "foo..bar.png") is harmless.
  containsParentRef = ".." `elem`
    Posix.splitDirectories (map (\c -> if c == '\\' then '/' else c) fp'')
  fallback = case takeExtension fp'' of
                  ".gz" -> getMimeTypeDef $ dropExtension fp''
                  _     -> getMimeTypeDef fp''
  mt = fromMaybe fallback mbMime
  path = maybe fp'' (unEscapeString . uriPath) uri
  ext = case extensionFromMimeType mt of
             Just e -> '.':T.unpack e
             Nothing -> case takeExtension path of
                             '.':e | '%' `notElem` e -> '.':e
                             _ -> ""

-- | Lookup a media item in a 'MediaBag', returning mime type and contents.
lookupMedia :: FilePath
            -> MediaBag
            -> Maybe MediaItem
lookupMedia fp (MediaBag mediamap) = M.lookup (canonicalize fp) mediamap

-- | Get a list of the file paths stored in a 'MediaBag', with
-- their corresponding mime types and the lengths in bytes of the contents.
mediaDirectory :: MediaBag -> [(FilePath, MimeType, Int)]
mediaDirectory mediabag =
  map (\(fp, mt, bs) -> (fp, mt, fromIntegral (BL.length bs)))
    (mediaItems mediabag)

mediaItems :: MediaBag -> [(FilePath, MimeType, BL.ByteString)]
mediaItems (MediaBag mediamap) =
  map (\item -> (mediaPath item, mediaMimeType item, mediaContents item))
      (M.elems mediamap)
