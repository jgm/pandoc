{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, CPP #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-
  Copyright (C) 2011-2016 John MacFarlane <jgm@berkeley.edu>

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
Copyright   : Copyright (C) 2011-2016 John MacFarlane
License     : GNU GPL, version 2 or above

Maintainer  : John MacFarlane <jgm@berkeley.edu>
Stability   : alpha
Portability : portable

Functions for determining the size of a PNG, JPEG, or GIF image.
-}
module Text.Pandoc.ImageSize ( ImageType(..)
                             , imageType
                             , imageSize
                             , sizeInPixels
                             , sizeInPoints
                             , desiredSizeInPoints
                             , Dimension(..)
                             , Direction(..)
                             , dimension
                             , inInch
                             , inPoints
                             , numUnit
                             , showInInch
                             , showInPixel
                             , showFl
                             ) where
import Data.ByteString (ByteString, unpack)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (isDigit)
import Control.Monad
import Data.Bits
import Data.Binary
import Data.Binary.Get
import Text.Pandoc.Shared (safeRead, hush)
import Data.Default (Default)
import Numeric (showFFloat)
import Text.Pandoc.Definition
import Text.Pandoc.Options
import qualified Data.Map as M
import Control.Monad.Except
import Data.Maybe (fromMaybe)

-- quick and dirty functions to get image sizes
-- algorithms borrowed from wwwis.pl

data ImageType = Png | Gif | Jpeg | Pdf | Eps deriving Show
data Direction = Width | Height
instance Show Direction where
  show Width  = "width"
  show Height = "height"

data Dimension = Pixel Integer
               | Centimeter Double
               | Inch Double
               | Percent Double
instance Show Dimension where
  show (Pixel a)      = show   a ++ "px"
  show (Centimeter a) = showFl a ++ "cm"
  show (Inch a)       = showFl a ++ "in"
  show (Percent a)    = show   a ++ "%"

data ImageSize = ImageSize{
                     pxX   :: Integer
                   , pxY   :: Integer
                   , dpiX  :: Integer
                   , dpiY  :: Integer
                   } deriving (Read, Show, Eq)
instance Default ImageSize where
  def = ImageSize 300 200 72 72

showFl :: (RealFloat a) => a -> String
showFl a = showFFloat (Just 5) a ""

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
                     _                  -> mzero

imageSize :: ByteString -> Either String ImageSize
imageSize img =
  case imageType img of
       Just Png  -> mbToEither "could not determine PNG size" $ pngSize img
       Just Gif  -> mbToEither "could not determine GIF size" $ gifSize img
       Just Jpeg -> jpegSize img
       Just Eps  -> mbToEither "could not determine EPS size" $ epsSize img
       Just Pdf  -> Left "could not determine PDF size" -- TODO
       Nothing   -> Left "could not determine image type"
  where mbToEither msg Nothing  = Left msg
        mbToEither _   (Just x) = Right x

defaultSize :: (Integer, Integer)
defaultSize = (72, 72)

sizeInPixels :: ImageSize -> (Integer, Integer)
sizeInPixels s = (pxX s, pxY s)

-- | Calculate (height, width) in points using the image file's dpi metadata,
-- using 72 Points == 1 Inch.
sizeInPoints :: ImageSize -> (Double, Double)
sizeInPoints s = (pxXf * 72 / dpiXf, pxYf * 72 / dpiYf)
  where
    pxXf  = fromIntegral $ pxX s
    pxYf  = fromIntegral $ pxY s
    dpiXf = fromIntegral $ dpiX s
    dpiYf = fromIntegral $ dpiY s

-- | Calculate (height, width) in points, considering the desired dimensions in the
-- attribute, while falling back on the image file's dpi metadata if no dimensions
-- are specified in the attribute (or only dimensions in percentages).
desiredSizeInPoints :: WriterOptions -> Attr -> ImageSize -> (Double, Double)
desiredSizeInPoints opts attr s =
  case (getDim Width, getDim Height) of
    (Just w, Just h)   -> (w, h)
    (Just w, Nothing)  -> (w, w / ratio)
    (Nothing, Just h)  -> (h * ratio, h)
    (Nothing, Nothing) -> sizeInPoints s
  where
    ratio = fromIntegral (pxX s) / fromIntegral (pxY s)
    getDim dir = case (dimension dir attr) of
                   Just (Percent _) -> Nothing
                   Just dim         -> Just $ inPoints opts dim
                   Nothing          -> Nothing

inPoints :: WriterOptions -> Dimension -> Double
inPoints opts dim = 72 * inInch opts dim

inInch :: WriterOptions -> Dimension -> Double
inInch opts dim =
  case dim of
    (Pixel a)      -> fromIntegral a / (fromIntegral $ writerDpi opts)
    (Centimeter a) -> a * 0.3937007874
    (Inch a)       -> a
    (Percent _)    -> 0

-- | Convert a Dimension to a String denoting its equivalent in inches, for example "2.00000".
-- Note: Dimensions in percentages are converted to the empty string.
showInInch :: WriterOptions -> Dimension -> String
showInInch _ (Percent _) = ""
showInInch opts dim = showFl $ inInch opts dim

-- | Convert a Dimension to a String denoting its equivalent in pixels, for example "600".
-- Note: Dimensions in percentages are converted to the empty string.
showInPixel :: WriterOptions -> Dimension -> String
showInPixel opts dim =
  case dim of
    (Pixel a)      -> show a
    (Centimeter a) -> show (floor $ dpi * a * 0.3937007874 :: Int)
    (Inch a)       -> show (floor $ dpi * a :: Int)
    (Percent _)    -> ""
  where
    dpi = fromIntegral $ writerDpi opts

-- | Maybe split a string into a leading number and trailing unit, e.g. "3cm" to Just (3.0, "cm")
numUnit :: String -> Maybe (Double, String)
numUnit s =
  let (nums, unit) = span (\c -> isDigit c || ('.'==c)) s
  in  case safeRead nums of
        Just n  -> Just (n, unit)
        Nothing -> Nothing

-- | Read a Dimension from an Attr attribute.
-- `dimension Width attr` might return `Just (Pixel 3)` or for example `Just (Centimeter 2.0)`, etc.
dimension :: Direction -> Attr -> Maybe Dimension
dimension dir (_, _, kvs) =
  case dir of
    Width  -> extractDim "width"
    Height -> extractDim "height"
  where
    extractDim key =
      case lookup key kvs of
        Just str ->
          case numUnit str of
            Just (num, unit) -> toDim num unit
            Nothing -> Nothing
        Nothing  -> Nothing
    toDim a "cm"   = Just $ Centimeter a
    toDim a "mm"   = Just $ Centimeter (a / 10)
    toDim a "in"   = Just $ Inch a
    toDim a "inch" = Just $ Inch a
    toDim a "%"    = Just $ Percent a
    toDim a "px"   = Just $ Pixel (floor a::Integer)
    toDim a ""     = Just $ Pixel (floor a::Integer)
    toDim _ _      = Nothing

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
                _ -> (hush . Left) "PNG parse error"
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
       _             -> (hush . Left) "GIF parse error"

jpegSize :: ByteString -> Either String ImageSize
jpegSize img =
  let (hdr, rest) = B.splitAt 4 img
  in if B.length rest < 14
        then Left "unable to determine JPEG size"
        else case hdr of
               "\xff\xd8\xff\xe0" -> jfifSize rest
               "\xff\xd8\xff\xe1" -> exifSize rest
               _                  -> Left "unable to determine JPEG size"

jfifSize :: ByteString -> Either String ImageSize
jfifSize rest =
  let [dpiDensity,dpix1,dpix2,dpiy1,dpiy2] = map fromIntegral
                                           $ unpack $ B.take 5 $ B.drop 9 $ rest
      factor = case dpiDensity of
                    1 -> id
                    2 -> \x -> (x * 254 `div` 10)
                    _ -> const 72
      dpix = factor (shift dpix1 8 + dpix2)
      dpiy = factor (shift dpiy1 8 + dpiy2)
  in case findJfifSize rest of
       Left msg    -> Left msg
       Right (w,h) -> Right $ ImageSize { pxX = w
                                        , pxY = h
                                        , dpiX = dpix
                                        , dpiY = dpiy }

findJfifSize :: ByteString -> Either String (Integer,Integer)
findJfifSize bs =
  let bs' = B.dropWhile (=='\xff') $ B.dropWhile (/='\xff') bs
  in case B.uncons bs' of
       Just (c,bs'') | c >= '\xc0' && c <= '\xc3' ->
         case map fromIntegral $ unpack $ B.take 4 $ B.drop 3 bs'' of
              [h1,h2,w1,w2] -> Right (shift w1 8 + w2, shift h1 8 + h2)
              _             -> Left "JFIF parse error"
       Just (_,bs'') ->
         case map fromIntegral $ unpack $ B.take 2 bs'' of
              [c1,c2] ->
                let len = shift c1 8 + c2
                -- skip variables
                in  findJfifSize $ B.drop len bs''
              _       -> Left "JFIF parse error"
       Nothing -> Left "Did not find JFIF length record"

runGet' :: Get (Either String a) -> BL.ByteString -> Either String a
runGet' p bl =
#if MIN_VERSION_binary(0,7,0)
  case runGetOrFail p bl of
       Left (_,_,msg) -> Left msg
       Right (_,_,x)  -> x
#else
  runGet p bl
#endif


exifSize :: ByteString -> Either String ImageSize
exifSize bs = runGet' header $ bl
  where bl = BL.fromChunks [bs]
        header = runExceptT $ exifHeader bl
-- NOTE:  It would be nicer to do
-- runGet ((Just <$> exifHeader) <|> return Nothing)
-- which would prevent pandoc from raising an error when an exif header can't
-- be parsed.  But we only get an Alternative instance for Get in binary 0.6,
-- and binary 0.5 ships with ghc 7.6.

exifHeader :: BL.ByteString -> ExceptT String Get ImageSize
exifHeader hdr = do
  _app1DataSize <- lift getWord16be
  exifHdr <- lift getWord32be
  unless (exifHdr == 0x45786966) $ throwError "Did not find exif header"
  zeros <- lift getWord16be
  unless (zeros == 0) $ throwError "Expected zeros after exif header"
  -- beginning of tiff header -- we read whole thing to use
  -- in getting data from offsets:
  let tiffHeader = BL.drop 8 hdr
  byteAlign <- lift getWord16be
  let bigEndian = byteAlign == 0x4d4d
  let (getWord16, getWord32, getWord64) =
        if bigEndian
           then (getWord16be, getWord32be, getWord64be)
           else (getWord16le, getWord32le, getWord64le)
  let getRational = do
        num <- getWord32
        den <- getWord32
        return $ fromIntegral num / fromIntegral den
  tagmark <- lift getWord16
  unless (tagmark == 0x002a) $ throwError "Failed alignment sanity check"
  ifdOffset <- lift getWord32
  lift $ skip (fromIntegral ifdOffset - 8) -- skip to IDF
  numentries <- lift  getWord16
  let ifdEntry :: ExceptT String Get (TagType, DataFormat)
      ifdEntry = do
       tag <- fromMaybe UnknownTagType . flip M.lookup tagTypeTable
                <$> lift getWord16
       dataFormat <- lift getWord16
       numComponents <- lift getWord32
       (fmt, bytesPerComponent) <-
             case dataFormat of
                  1  -> return (UnsignedByte <$> getWord8, 1)
                  2  -> return (AsciiString <$>
                                getLazyByteString
                                (fromIntegral numComponents), 1)
                  3  -> return (UnsignedShort <$> getWord16, 2)
                  4  -> return (UnsignedLong <$> getWord32, 4)
                  5  -> return (UnsignedRational <$> getRational, 8)
                  6  -> return (SignedByte <$> getWord8, 1)
                  7  -> return (Undefined <$> getLazyByteString
                                (fromIntegral numComponents), 1)
                  8  -> return (SignedShort <$> getWord16, 2)
                  9  -> return (SignedLong <$> getWord32, 4)
                  10 -> return (SignedRational <$> getRational, 8)
                  11 -> return (SingleFloat <$> getWord32 {- TODO -}, 4)
                  12 -> return (DoubleFloat <$> getWord64 {- TODO -}, 8)
                  _  -> throwError $ "Unknown data format " ++ show dataFormat
       let totalBytes = fromIntegral $ numComponents * bytesPerComponent
       payload <- if totalBytes <= 4 -- data is right here
                     then lift $ fmt <* skip (4 - totalBytes)
                     else do  -- get data from offset
                          offs <- lift getWord32
                          let bytesAtOffset =
                                 BL.take (fromIntegral totalBytes)
                                 $ BL.drop (fromIntegral offs) tiffHeader
                          case runGet' (Right <$> fmt) bytesAtOffset of
                               Left msg -> throwError msg
                               Right x  -> return x
       return (tag, payload)
  entries <- sequence $ replicate (fromIntegral numentries) ifdEntry
  subentries <- case lookup ExifOffset entries of
                      Just (UnsignedLong offset') -> do
                        pos <- lift bytesRead
                        lift $ skip (fromIntegral offset' - (fromIntegral pos - 8))
                        numsubentries <- lift getWord16
                        sequence $
                           replicate (fromIntegral numsubentries) ifdEntry
                      _ -> return []
  let allentries = entries ++ subentries
  (wdth, hght) <- case (lookup ExifImageWidth allentries,
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
                    pxX  = wdth
                  , pxY  = hght
                  , dpiX = xres
                  , dpiY = yres }

data DataFormat = UnsignedByte Word8
                | AsciiString BL.ByteString
                | UnsignedShort Word16
                | UnsignedLong Word32
                | UnsignedRational Rational
                | SignedByte Word8
                | Undefined BL.ByteString
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
