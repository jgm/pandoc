{-# LANGUAGE OverloadedStrings, ScopedTypeVariables  #-}
{-
  Copyright (C) 2011-2014 John MacFarlane <jgm@berkeley.edu>

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
Copyright   : Copyright (C) 2011-2014 John MacFarlane
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
import qualified Data.ByteString.Lazy as BL
import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Binary
import Data.Binary.Get
import Text.Pandoc.Shared (safeRead)
import qualified Data.Map as M

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
                     "\xff\xd8\xff\xe0" -> return Jpeg  -- JFIF
                     "\xff\xd8\xff\xe1" -> return Jpeg  -- Exif
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

defaultSize :: (Integer, Integer)
defaultSize = (72, 72)

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
  guard $ B.length rest >= 14
  case hdr of
       "\xff\xd8\xff\xe0" -> jfifSize rest
       "\xff\xd8\xff\xe1" -> exifSize $ B.takeWhile (/= '\xff') rest
       _                  -> mzero

jfifSize :: ByteString -> Maybe ImageSize
jfifSize rest = do
  let [dpiDensity,dpix1,dpix2,dpiy1,dpiy2] = map fromIntegral
                                           $ unpack $ B.take 5 $ B.drop 9 $ rest
  let factor = case dpiDensity of
                    1 -> id
                    2 -> \x -> (x * 254 `div` 10)
                    _ -> const 72
  let dpix = factor (shift dpix1 8 + dpix2)
  let dpiy = factor (shift dpiy1 8 + dpiy2)
  (w,h) <- findJfifSize rest
  return $ ImageSize { pxX = w, pxY = h, dpiX = dpix, dpiY = dpiy }

findJfifSize :: ByteString -> Maybe (Integer,Integer)
findJfifSize bs = do
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
                findJfifSize $ B.drop len bs''
              _       -> fail "JPEG parse error"
       Nothing -> fail "Did not find length record"

exifSize :: ByteString -> Maybe ImageSize
exifSize bs = runGet (Just <$> exifHeader bl) bl
  where bl = BL.fromChunks [bs]
-- NOTE:  It would be nicer to do
-- runGet ((Just <$> exifHeader) <|> return Nothing)
-- which would prevent pandoc from raising an error when an exif header can't
-- be parsed.  But we only get an Alternative instance for Get in binary 0.6,
-- and binary 0.5 ships with ghc 7.6.

exifHeader :: BL.ByteString -> Get ImageSize
exifHeader hdr = do
  _app1DataSize <- getWord16be
  exifHdr <- getWord32be
  unless (exifHdr == 0x45786966) $ fail "Did not find exif header"
  zeros <- getWord16be
  unless (zeros == 0) $ fail "Expected zeros after exif header"
  -- beginning of tiff header -- we read whole thing to use
  -- in getting data from offsets:
  let tiffHeader = BL.drop 8 hdr
  byteAlign <- getWord16be
  let bigEndian = byteAlign == 0x4d4d
  let (getWord16, getWord32, getWord64) =
        if bigEndian
           then (getWord16be, getWord32be, getWord64be)
           else (getWord16le, getWord32le, getWord64le)
  let getRational = do
        num <- getWord32
        den <- getWord32
        return $ fromIntegral num / fromIntegral den
  tagmark <- getWord16
  unless (tagmark == 0x002a) $ fail "Failed alignment sanity check"
  ifdOffset <- getWord32
  skip (fromIntegral ifdOffset - 8) -- skip to IDF
  numentries <- getWord16
  let ifdEntry = do
       tag <- getWord16 >>= \t ->
                maybe (return UnknownTagType) return
                (M.lookup t tagTypeTable)
       dataFormat <- getWord16
       numComponents <- getWord32
       (fmt, bytesPerComponent) <-
             case dataFormat of
                  1  -> return (UnsignedByte . runGet getWord8, 1)
                  2  -> return (AsciiString, 1)
                  3  -> return (UnsignedShort . runGet getWord16, 2)
                  4  -> return (UnsignedLong . runGet getWord32, 4)
                  5  -> return (UnsignedRational . runGet getRational, 8)
                  6  -> return (SignedByte . runGet getWord8, 1)
                  7  -> return (Undefined . runGet getWord8, 1)
                  8  -> return (SignedShort . runGet getWord16, 2)
                  9  -> return (SignedLong . runGet getWord32, 4)
                  10 -> return (SignedRational . runGet getRational, 8)
                  11 -> return (SingleFloat . runGet getWord32 {- TODO -}, 4)
                  12 -> return (DoubleFloat . runGet getWord64 {- TODO -}, 8)
                  _  -> fail $ "Unknown data format " ++ show dataFormat
       let totalBytes = fromIntegral $ numComponents * bytesPerComponent
       payload <- if totalBytes <= 4 -- data is right here
                     then fmt <$>
                          (getLazyByteString (fromIntegral totalBytes) <*
                          skip (4 - totalBytes))
                     else do  -- get data from offset
                          offs <- getWord32
                          return $ fmt $ BL.take (fromIntegral totalBytes) $
                                   BL.drop (fromIntegral offs) tiffHeader
       return (tag, payload)
  entries <- sequence $ replicate (fromIntegral numentries) ifdEntry
  subentries <- case lookup ExifOffset entries of
                      Just (UnsignedLong offset) -> do
                        pos <- bytesRead
                        skip (fromIntegral offset - (fromIntegral pos - 8))
                        numsubentries <- getWord16
                        sequence $
                           replicate (fromIntegral numsubentries) ifdEntry
                      _ -> return []
  let allentries = entries ++ subentries
  (width, height) <- case (lookup ExifImageWidth allentries,
                           lookup ExifImageHeight allentries) of
                          (Just (UnsignedLong w), Just (UnsignedLong h)) ->
                            return (fromIntegral w, fromIntegral h)
                          _ -> return defaultSize
                               -- we return a default width and height when
                               -- the exif header doesn't contain these
  let resfactor = case lookup ResolutionUnit allentries of
                        Just (UnsignedShort 1) -> (100 / 254)
                        _ -> 1
  let xres = maybe 72 (\(UnsignedRational x) -> floor $ x * resfactor)
             $ lookup XResolution allentries
  let yres = maybe 72 (\(UnsignedRational x) -> floor $ x * resfactor)
             $ lookup YResolution allentries
  return $ ImageSize{
                    pxX  = width
                  , pxY  = height
                  , dpiX = xres
                  , dpiY = yres }

data DataFormat = UnsignedByte Word8
                | AsciiString BL.ByteString
                | UnsignedShort Word16
                | UnsignedLong Word32
                | UnsignedRational Rational
                | SignedByte Word8
                | Undefined Word8
                | SignedShort Word16
                | SignedLong Word32
                | SignedRational Rational
                | SingleFloat Word32
                | DoubleFloat Word64
                deriving (Show)

data TagType = ImageDescription
             | Make
             | Model
             | Orientation
             | XResolution
             | YResolution
             | ResolutionUnit
             | Software
             | DateTime
             | WhitePoint
             | PrimaryChromaticities
             | YCbCrCoefficients
             | YCbCrPositioning
             | ReferenceBlackWhite
             | Copyright
             | ExifOffset
             | ExposureTime
             | FNumber
             | ExposureProgram
             | ISOSpeedRatings
             | ExifVersion
             | DateTimeOriginal
             | DateTimeDigitized
             | ComponentConfiguration
             | CompressedBitsPerPixel
             | ShutterSpeedValue
             | ApertureValue
             | BrightnessValue
             | ExposureBiasValue
             | MaxApertureValue
             | SubjectDistance
             | MeteringMode
             | LightSource
             | Flash
             | FocalLength
             | MakerNote
             | UserComment
             | FlashPixVersion
             | ColorSpace
             | ExifImageWidth
             | ExifImageHeight
             | RelatedSoundFile
             | ExifInteroperabilityOffset
             | FocalPlaneXResolution
             | FocalPlaneYResolution
             | FocalPlaneResolutionUnit
             | SensingMethod
             | FileSource
             | SceneType
             | UnknownTagType
             deriving (Show, Eq, Ord)

tagTypeTable :: M.Map Word16 TagType
tagTypeTable = M.fromList
  [ (0x010e, ImageDescription)
  , (0x010f, Make)
  , (0x0110, Model)
  , (0x0112, Orientation)
  , (0x011a, XResolution)
  , (0x011b, YResolution)
  , (0x0128, ResolutionUnit)
  , (0x0131, Software)
  , (0x0132, DateTime)
  , (0x013e, WhitePoint)
  , (0x013f, PrimaryChromaticities)
  , (0x0211, YCbCrCoefficients)
  , (0x0213, YCbCrPositioning)
  , (0x0214, ReferenceBlackWhite)
  , (0x8298, Copyright)
  , (0x8769, ExifOffset)
  , (0x829a, ExposureTime)
  , (0x829d, FNumber)
  , (0x8822, ExposureProgram)
  , (0x8827, ISOSpeedRatings)
  , (0x9000, ExifVersion)
  , (0x9003, DateTimeOriginal)
  , (0x9004, DateTimeDigitized)
  , (0x9101, ComponentConfiguration)
  , (0x9102, CompressedBitsPerPixel)
  , (0x9201, ShutterSpeedValue)
  , (0x9202, ApertureValue)
  , (0x9203, BrightnessValue)
  , (0x9204, ExposureBiasValue)
  , (0x9205, MaxApertureValue)
  , (0x9206, SubjectDistance)
  , (0x9207, MeteringMode)
  , (0x9208, LightSource)
  , (0x9209, Flash)
  , (0x920a, FocalLength)
  , (0x927c, MakerNote)
  , (0x9286, UserComment)
  , (0xa000, FlashPixVersion)
  , (0xa001, ColorSpace)
  , (0xa002, ExifImageWidth)
  , (0xa003, ExifImageHeight)
  , (0xa004, RelatedSoundFile)
  , (0xa005, ExifInteroperabilityOffset)
  , (0xa20e, FocalPlaneXResolution)
  , (0xa20f, FocalPlaneYResolution)
  , (0xa210, FocalPlaneResolutionUnit)
  , (0xa217, SensingMethod)
  , (0xa300, FileSource)
  , (0xa301, SceneType)
  ]
