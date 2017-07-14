{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-
Copyright (C) 2014-2015, 2017 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.MediaBag
   Copyright   : Copyright (C) 2014-2015, 2017 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Definition of a MediaBag object to hold binary resources, and an
interface for interacting with it.
-}
module Text.Pandoc.MediaBag (
                     MediaBag,
                     lookupMedia,
                     insertMedia,
                     mediaDirectory,
                     ) where
import qualified Data.ByteString.Lazy as BL
import Data.Data (Data)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import System.FilePath
import qualified System.FilePath.Posix as Posix
import Text.Pandoc.MIME (MimeType, getMimeTypeDef)

-- | A container for a collection of binary resources, with names and
-- mime types.  Note that a 'MediaBag' is a Monoid, so 'mempty'
-- can be used for an empty 'MediaBag', and '<>' can be used to append
-- two 'MediaBag's.
newtype MediaBag = MediaBag (M.Map [String] (MimeType, BL.ByteString))
        deriving (Monoid, Data, Typeable)

instance Show MediaBag where
  show bag = "MediaBag " ++ show (mediaDirectory bag)

-- | Insert a media item into a 'MediaBag', replacing any existing
-- value with the same name.
insertMedia :: FilePath       -- ^ relative path and canonical name of resource
            -> Maybe MimeType -- ^ mime type (Nothing = determine from extension)
            -> BL.ByteString  -- ^ contents of resource
            -> MediaBag
            -> MediaBag
insertMedia fp mbMime contents (MediaBag mediamap) =
  MediaBag (M.insert (splitDirectories fp) (mime, contents) mediamap)
  where mime = fromMaybe fallback mbMime
        fallback = case takeExtension fp of
                        ".gz" -> getMimeTypeDef $ dropExtension fp
                        _     -> getMimeTypeDef fp

-- | Lookup a media item in a 'MediaBag', returning mime type and contents.
lookupMedia :: FilePath
            -> MediaBag
            -> Maybe (MimeType, BL.ByteString)
lookupMedia fp (MediaBag mediamap) = M.lookup (splitDirectories fp) mediamap

-- | Get a list of the file paths stored in a 'MediaBag', with
-- their corresponding mime types and the lengths in bytes of the contents.
mediaDirectory :: MediaBag -> [(String, MimeType, Int)]
mediaDirectory (MediaBag mediamap) =
  M.foldrWithKey (\fp (mime,contents) ->
      (((Posix.joinPath fp), mime, fromIntegral $ BL.length contents):)) [] mediamap
