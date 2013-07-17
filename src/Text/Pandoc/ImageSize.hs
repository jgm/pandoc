{-# LANGUAGE OverloadedStrings, ScopedTypeVariables  #-}
{-
  Copyright (C) 2011 John MacFarlane <jgm@berkeley.edu>

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
Copyright   : Copyright (C) 2011 John MacFarlane
License     : GNU GPL, version 2 or above

Maintainer  : John MacFarlane <jgm@berkeley.edu>
Stability   : alpha
Portability : portable

Functions for determining the size of a PNG, JPEG, or GIF image.
-}
module Text.Pandoc.ImageSize ( ImageType(..), imageType, imageSize,
                    sizeInPixels, sizeInPoints ) where
import Data.ByteString (ByteString, unpack)
import qualified Data.ByteString.Char8 as B
import Control.Monad
import Data.Bits
import Text.Pandoc.Shared (safeRead)

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
                     "\x89\x50\x4e\x47" -> return Png
                     "\x47\x49\x46\x38" -> return Gif
                     "\xff\xd8\xff\xe0" -> return Jpeg
                     "%PDF"             -> return Pdf
                     "%!PS"
                       | (B.take 4 $ B.drop 1 $ B.dropWhile (/=' ') img) == "EPSF"
                                        -> return Eps
                     _                  -> fail "Unknown image type"

imageSize :: ByteString -> Maybe ImageSize
imageSize img = do
  t <- imageType img
  case t of
       Png  -> pngSize img
       Gif  -> gifSize img
       Jpeg -> jpegSize img
       Eps  -> epsSize img
       Pdf  -> Nothing  -- TODO

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

pngSize :: ByteString -> Maybe ImageSize
pngSize img = do
  let (h, rest) = B.splitAt 8 img
  guard $ h == "\x8a\x4d\x4e\x47\x0d\x0a\x1a\x0a" ||
          h == "\x89\x50\x4e\x47\x0d\x0a\x1a\x0a"
  let (i, rest') = B.splitAt 4 $ B.drop 4 rest
  guard $ i == "MHDR" || i == "IHDR"
  let (sizes, rest'') = B.splitAt 8 rest'
  (x,y) <- case map fromIntegral $ unpack $ sizes of
                ([w1,w2,w3,w4,h1,h2,h3,h4] :: [Integer]) -> return
                    ((shift w1 24) + (shift w2 16) + (shift w3 8) + w4,
                     (shift h1 24) + (shift h2 16) + (shift h3 8) + h4)
                _ -> fail "PNG parse error"
  let (dpix, dpiy) = findpHYs rest''
  return $ ImageSize { pxX  = x, pxY = y, dpiX = dpix, dpiY = dpiy }

findpHYs :: ByteString -> (Integer, Integer)
findpHYs x =
  if B.null x || "IDAT" `B.isPrefixOf` x
     then (72,72) -- default, no pHYs
     else if "pHYs" `B.isPrefixOf` x
          then let [x1,x2,x3,x4,y1,y2,y3,y4,u] = map fromIntegral
                                               $ unpack $ B.take 9 $ B.drop 4 x
                   factor = if u == 1 -- dots per meter
                               then \z -> z * 254 `div` 10000
                               else const 72
               in  ( factor $ (shift x1 24) + (shift x2 16) + (shift x3 8) + x4,
                     factor $ (shift y1 24) + (shift y2 16) + (shift y3 8) + y4 )
          else findpHYs $ B.drop 1 x  -- read another byte

gifSize :: ByteString -> Maybe ImageSize
gifSize img = do
  let (h, rest) = B.splitAt 6 img
  guard $ h == "GIF87a" || h == "GIF89a"
  case map fromIntegral $ unpack $ B.take 4 rest of
       [w2,w1,h2,h1] -> return ImageSize {
                          pxX  = shift w1 8 + w2,
                          pxY  = shift h1 8 + h2,
                          dpiX = 72,
                          dpiY = 72
                          }
       _             -> fail "GIF parse error"

jpegSize :: ByteString -> Maybe ImageSize
jpegSize img = do
  let (hdr, rest) = B.splitAt 4 img
  guard $ hdr == "\xff\xd8\xff\xe0"
  guard $ B.length rest >= 14
  let [dpiDensity,dpix1,dpix2,dpiy1,dpiy2] = map fromIntegral
                                           $ unpack $ B.take 5 $ B.drop 9 $ rest
  let factor = case dpiDensity of
                    1 -> id
                    2 -> \x -> (x * 254 `div` 10)
                    _ -> const 72
  let dpix = factor (shift dpix1 8 + dpix2)
  let dpiy = factor (shift dpiy1 8 + dpiy2)
  (w,h) <- findJpegSize rest
  return $ ImageSize { pxX = w, pxY = h, dpiX = dpix, dpiY = dpiy }

findJpegSize :: ByteString -> Maybe (Integer,Integer)
findJpegSize bs = do
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
                findJpegSize $ B.drop len bs''
              _       -> fail "JPEG parse error"
       Nothing -> fail "Did not find length record"


