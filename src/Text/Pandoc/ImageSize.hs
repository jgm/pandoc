{-# LANGUAGE OverloadedStrings, ScopedTypeVariables  #-}
{-
Copyright (C) 2011 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.ImageSize
   Copyright   : Copyright (C) 2011 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Functions for determining the size of a PNG, JPEG, or GIF image.

Algorithms borrowwed from wwwis.pl (c) 2005 Alex K, released
under the GPL.
-}
module Text.Pandoc.ImageSize ( imageType, imageSize, readImageSize ) where
import Data.ByteString.Lazy (ByteString, unpack)
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad
import Data.Bits

-- quick and dirty functions to get image sizes
-- algorithms borrowed from wwwis.pl

data ImageType = Png | Gif | Jpeg deriving Show

type Height = Integer
type Width = Integer

readImageSize :: FilePath -> IO (Maybe (Width,Height))
readImageSize fp = imageSize `fmap` B.readFile fp

imageType :: ByteString -> Maybe ImageType
imageType img = case B.take 4 img of
                     "\x89\x50\x4e\x47" -> return Png
                     "\x47\x49\x46\x38" -> return Gif
                     "\xff\xd8\xff\xe0" -> return Jpeg
                     _                  -> fail "Unknown image type"

imageSize :: ByteString -> Maybe (Width,Height)
imageSize img = do
  t <- imageType img
  case t of
       Png  -> pngSize img
       Gif  -> gifSize img
       Jpeg -> jpegSize img

pngSize :: ByteString -> Maybe (Width,Height)
pngSize img = do
  let (h, rest) = B.splitAt 8 img
  guard $ h == "\x8a\x4d\x4e\x47\x0d\x0a\x1a\x0a" ||
          h == "\x89\x50\x4e\x47\x0d\x0a\x1a\x0a"
  let (i, rest') = B.splitAt 4 $ B.drop 4 rest
  guard $ i == "MHDR" || i == "IHDR"
  case map fromIntegral $ unpack $ B.take 8 rest' of
       ([w1,w2,w3,w4,h1,h2,h3,h4] :: [Integer]) -> return
         ((shift w1 24) + (shift w2 16) + (shift w3 8) + w4,
          (shift h1 24) + (shift h2 16) + (shift h3 8) + h4)
       _ -> fail "PNG parse error"

gifSize :: ByteString -> Maybe (Width,Height)
gifSize img = do
  let (h, rest) = B.splitAt 6 img
  guard $ h == "GIF87a" || h == "GIF89a"
  case map fromIntegral $ unpack $ B.take 4 rest of
       [w2,w1,h2,h1] -> return (shift w1 8 + w2, shift h1 8 + h2)
       _             -> fail "GIF parse error"

jpegSize :: ByteString -> Maybe (Width,Height)
jpegSize img = do
  let (h, rest) = B.splitAt 2 img
  guard $ h == "\xff\xd8"
  findJpegLength rest

findJpegLength :: ByteString -> Maybe (Width,Height)
findJpegLength bs = do
  let bs' = B.dropWhile (=='\xff') $ B.dropWhile (/='\xff') bs
  case B.uncons bs' of
       Just (c,bs'') | c >= '\xc0' && c <= '\xc3' -> do
         case map fromIntegral $ unpack $ B.take 4 $ B.drop 3 bs'' of
              [h1,h2,w1,w2] -> return (shift w1 8 + w2, shift h1 8 + h2)
              _             -> fail "JPEG parse error"
       Just (_,bs'') ->  do
         case map fromIntegral $ unpack $ B.take 2 bs'' of
              [c1,c2] -> do
                let len = shift c1 8 + c2
                -- skip variables
                findJpegLength $ B.drop len bs''
              _       -> fail "JPEG parse error"
       Nothing -> fail "Did not find length record"


