{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, CPP #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-
  Copyright (C) 2011-2015 John MacFarlane <jgm@berkeley.edu>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
    more details.

    You should have received a copy of the GNU General Public License along
    with this program; if not, write to the Free Software Foundation, Inc., 59
    Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
Module      : Text.Pandoc.ImageSize
Copyright   : Copyright (C) 2011-2015 John MacFarlane
License     : GNU GPL, version 2 or above

Maintainer  : John MacFarlane <jgm@berkeley.edu>
Stability   : alpha
Portability : portable

Functions for determining the size of a PNG, JPEG, or GIF image.
-}
module Text.Pandoc.ImageSize ( ImageType(..), imageType, imageSize,
                    sizeInPixels, sizeInPoints ) where
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Control.Monad
import Text.Pandoc.Shared (safeRead)
import qualified Codec.Picture.Metadata as JM
import Codec.Picture( decodeImageWithMetadata )

-- quick and dirty functions to get image sizes
-- algorithms borrowed from wwwis.pl

data ImageType = Png | Gif | Jpeg | Pdf | Eps deriving Show

data ImageSize = ImageSize{
                     pxX   :: Integer
                   , pxY   :: Integer
                   , dpiX  :: Integer
                   , dpiY  :: Integer
                   } deriving (Read, Show, Eq)


imageType :: ByteString -> Maybe ImageType
imageType img = case B.take 4 img of
  "\xff\xd8\xff\xe0" -> return Jpeg  -- JFIF
  "\xff\xd8\xff\xe1" -> return Jpeg  -- Exif
  "%PDF"             -> return Pdf
  "%!PS"
    | (B.take 4 $ B.drop 1 $ B.dropWhile (/=' ') img) == "EPSF"
                     -> return Eps
  _                  -> mzero

imageSize :: ByteString -> Either String ImageSize
imageSize img = case (imageType img, decodeImageWithMetadata img) of
  (Just Pdf, _) -> maybe (Left "can't find EPS size") Right $ epsSize img
  (_, Left _err) -> Left "could not determine image size."
  (_, Right (_, metadatas)) -> Right $ ImageSize
      { pxX = extractSize metadatas JM.Width
      , pxY = extractSize metadatas JM.Height
      , dpiX = extractDpi metadatas JM.DpiX
      , dpiY = extractDpi metadatas JM.DpiY
      }
  where
    defaultDpi = 72
    extractDpi metas k =
      maybe defaultDpi fromIntegral $ JM.lookup k metas
    extractSize metas k =
      maybe 0 fromIntegral $ JM.lookup k metas

sizeInPixels :: ImageSize -> (Integer, Integer)
sizeInPixels s = (pxX s, pxY s)

sizeInPoints :: ImageSize -> (Integer, Integer)
sizeInPoints s = (pxX s * 72 `div` dpiX s, pxY s * 72 `div` dpiY s)

epsSize :: ByteString -> Maybe ImageSize
epsSize img = do
  let ls = takeWhile ("%" `B.isPrefixOf`) $ B.lines img
  let ls' = dropWhile (not . ("%%BoundingBox:" `B.isPrefixOf`)) ls
  case ls' of
       []    -> mzero
       (x:_) -> case B.words x of
                     (_:_:_:ux:uy:[]) -> do
                        ux' <- safeRead $ B.unpack ux
                        uy' <- safeRead $ B.unpack uy
                        return ImageSize{
                            pxX  = ux'
                          , pxY  = uy'
                          , dpiX = 72
                          , dpiY = 72 }
                     _ -> mzero

