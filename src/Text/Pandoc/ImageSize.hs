{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{- |
Module      : Text.Pandoc.ImageSize
Copyright   : Copyright (C) 2011-2021 John MacFarlane
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
                             , lengthToDim
                             , scaleDimension
                             , inInch
                             , inPixel
                             , inPoints
                             , inEm
                             , numUnit
                             , showInInch
                             , showInPixel
                             , showFl
                             ) where
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Data.Char (isDigit)
import Control.Monad
import Text.Pandoc.Shared (safeRead)
import Data.Default (Default)
import Numeric (showFFloat)
import Text.Pandoc.Definition
import Text.Pandoc.Options
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.XML.Light hiding (Attr)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Codec.Picture.Metadata as Metadata
import qualified Codec.Picture.Metadata.Exif as Exif
import Codec.Picture (decodeImageWithMetadata)

-- quick and dirty functions to get image sizes
-- algorithms borrowed from wwwis.pl

data ImageType = Png | Gif | Jpeg | Svg | Pdf | Eps | Emf | Tiff
                 deriving Show
data Direction = Width | Height
instance Show Direction where
  show Width  = "width"
  show Height = "height"

data Dimension = Pixel Integer
               | Centimeter Double
               | Millimeter Double
               | Inch Double
               | Percent Double
               | Em Double
               deriving Eq

instance Show Dimension where
  show (Pixel a)      = show a              ++ "px"
  show (Centimeter a) = T.unpack (showFl a) ++ "cm"
  show (Millimeter a) = T.unpack (showFl a) ++ "mm"
  show (Inch a)       = T.unpack (showFl a) ++ "in"
  show (Percent a)    = show a              ++ "%"
  show (Em a)         = T.unpack (showFl a) ++ "em"

data ImageSize = ImageSize{
                     pxX   :: Integer
                   , pxY   :: Integer
                   , dpiX  :: Integer
                   , dpiY  :: Integer
                   } deriving (Read, Show, Eq)
instance Default ImageSize where
  def = ImageSize 300 200 72 72

showFl :: (RealFloat a) => a -> T.Text
showFl a = removeExtra0s $ T.pack $ showFFloat (Just 5) a ""

removeExtra0s :: T.Text -> T.Text
removeExtra0s s = case T.dropWhileEnd (=='0') s of
  (T.unsnoc -> Just (xs, '.')) -> xs
  xs                           -> xs

imageType :: ByteString -> Maybe ImageType
imageType img = case B.take 4 img of
                     "\x89\x50\x4e\x47" -> return Png
                     "\x47\x49\x46\x38" -> return Gif
                     "\x49\x49\x2a\x00" -> return Tiff
                     "\x4D\x4D\x00\x2a" -> return Tiff
                     "\xff\xd8\xff\xe0" -> return Jpeg  -- JFIF
                     "\xff\xd8\xff\xe1" -> return Jpeg  -- Exif
                     "%PDF"             -> return Pdf
                     "<svg"             -> return Svg
                     "<?xm"
                       | findSvgTag img
                                        -> return Svg
                     "%!PS"
                       |  B.take 4 (B.drop 1 $ B.dropWhile (/=' ') img) == "EPSF"
                                        -> return Eps
                     "\x01\x00\x00\x00"
                       | B.take 4 (B.drop 40 img) == " EMF"
                                        -> return Emf
                     _                  -> mzero

findSvgTag :: ByteString -> Bool
findSvgTag img = "<svg" `B.isInfixOf` img || "<SVG" `B.isInfixOf` img

imageSize :: WriterOptions -> ByteString -> Either T.Text ImageSize
imageSize opts img = checkDpi <$>
  case imageType img of
       Just Png  -> getSize img
       Just Gif  -> getSize img
       Just Jpeg -> getSize img
       Just Tiff -> getSize img
       Just Svg  -> mbToEither "could not determine SVG size" $ svgSize opts img
       Just Eps  -> mbToEither "could not determine EPS size" $ epsSize img
       Just Pdf  -> mbToEither "could not determine PDF size" $ pdfSize img
       Just Emf  -> mbToEither "could not determine EMF size" $ emfSize img
       Nothing   -> Left "could not determine image type"
  where mbToEither msg Nothing  = Left msg
        mbToEither _   (Just x) = Right x
        -- see #6880, some defective JPEGs may encode dpi 0, so default to 72
        -- if that value is 0
        checkDpi size =
          size{ dpiX = if dpiX size == 0 then 72 else dpiX size
              , dpiY = if dpiY size == 0 then 72 else dpiY size }


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
    getDim dir = case dimension dir attr of
                   Just (Percent _) -> Nothing
                   Just dim         -> Just $ inPoints opts dim
                   Nothing          -> Nothing

inPoints :: WriterOptions -> Dimension -> Double
inPoints opts dim = 72 * inInch opts dim

inEm :: WriterOptions -> Dimension -> Double
inEm opts dim = (64/11) * inInch opts dim

inInch :: WriterOptions -> Dimension -> Double
inInch opts dim =
  case dim of
    (Pixel a)      -> fromIntegral a / fromIntegral (writerDpi opts)
    (Centimeter a) -> a * 0.3937007874
    (Millimeter a) -> a * 0.03937007874
    (Inch a)       -> a
    (Percent _)    -> 0
    (Em a)         -> a * (11/64)

inPixel :: WriterOptions -> Dimension -> Integer
inPixel opts dim =
  case dim of
    (Pixel a)      -> a
    (Centimeter a) -> floor $ dpi * a * 0.3937007874 :: Integer
    (Millimeter a) -> floor $ dpi * a * 0.03937007874 :: Integer
    (Inch a)       -> floor $ dpi * a :: Integer
    (Percent _)    -> 0
    (Em a)         -> floor $ dpi * a * (11/64) :: Integer
  where
    dpi = fromIntegral $ writerDpi opts

-- | Convert a Dimension to Text denoting its equivalent in inches, for example "2.00000".
-- Note: Dimensions in percentages are converted to the empty string.
showInInch :: WriterOptions -> Dimension -> T.Text
showInInch _ (Percent _) = ""
showInInch opts dim = showFl $ inInch opts dim

-- | Convert a Dimension to Text denoting its equivalent in pixels, for example "600".
-- Note: Dimensions in percentages are converted to the empty string.
showInPixel :: WriterOptions -> Dimension -> T.Text
showInPixel _ (Percent _) = ""
showInPixel opts dim = T.pack $ show $ inPixel opts dim

-- | Maybe split a string into a leading number and trailing unit, e.g. "3cm" to Just (3.0, "cm")
numUnit :: T.Text -> Maybe (Double, T.Text)
numUnit s =
  let (nums, unit) = T.span (\c -> isDigit c || ('.'==c)) s
  in (\n -> (n, unit)) <$> safeRead nums

-- | Scale a dimension by a factor.
scaleDimension :: Double -> Dimension -> Dimension
scaleDimension factor dim =
  case dim of
        Pixel x      -> Pixel (round $ factor * fromIntegral x)
        Centimeter x -> Centimeter (factor * x)
        Millimeter x -> Millimeter (factor * x)
        Inch x       -> Inch (factor * x)
        Percent x    -> Percent (factor * x)
        Em x         -> Em (factor * x)

-- | Read a Dimension from an Attr attribute.
-- `dimension Width attr` might return `Just (Pixel 3)` or for example `Just (Centimeter 2.0)`, etc.
dimension :: Direction -> Attr -> Maybe Dimension
dimension dir (_, _, kvs) =
  case dir of
    Width  -> extractDim "width"
    Height -> extractDim "height"
  where
    extractDim key = lookup key kvs >>= lengthToDim

lengthToDim :: T.Text -> Maybe Dimension
lengthToDim s = numUnit s >>= uncurry toDim
  where
    toDim a "cm"   = Just $ Centimeter a
    toDim a "mm"   = Just $ Millimeter a
    toDim a "in"   = Just $ Inch a
    toDim a "inch" = Just $ Inch a
    toDim a "%"    = Just $ Percent a
    toDim a "px"   = Just $ Pixel (floor a::Integer)
    toDim a ""     = Just $ Pixel (floor a::Integer)
    toDim a "pt"   = Just $ Inch (a / 72)
    toDim a "pc"   = Just $ Inch (a / 6)
    toDim a "em"   = Just $ Em a
    toDim _ _      = Nothing

epsSize :: ByteString -> Maybe ImageSize
epsSize img = do
  let ls = takeWhile ("%" `B.isPrefixOf`) $ B.lines img
  let ls' = dropWhile (not . ("%%BoundingBox:" `B.isPrefixOf`)) ls
  case ls' of
       []    -> mzero
       (x:_) -> case B.words x of
                     [_, _, _, ux, uy] -> do
                        ux' <- safeRead $ TE.decodeUtf8 ux
                        uy' <- safeRead $ TE.decodeUtf8 uy
                        return ImageSize{
                            pxX  = ux'
                          , pxY  = uy'
                          , dpiX = 72
                          , dpiY = 72 }
                     _ -> mzero

pdfSize :: ByteString -> Maybe ImageSize
pdfSize img =
  case A.parseOnly pPdfSize img of
    Left _   -> Nothing
    Right sz -> Just sz

pPdfSize :: A.Parser ImageSize
pPdfSize = do
  A.skipWhile (/='/')
  A.char8 '/'
  (do A.string "MediaBox"
      A.skipSpace
      A.char8 '['
      A.skipSpace
      [x1,y1,x2,y2] <- A.count 4 $ do
        A.skipSpace
        raw <- A.many1 $ A.satisfy (\c -> isDigit c || c == '.')
        case safeRead $ T.pack raw of
          Just (r :: Double) -> return $ floor r
          Nothing            -> mzero
      A.skipSpace
      A.char8 ']'
      return $ ImageSize{
              pxX  = x2 - x1
            , pxY  = y2 - y1
            , dpiX = 72
            , dpiY = 72 }
   ) <|> pPdfSize

getSize :: ByteString -> Either T.Text ImageSize
getSize img =
  case decodeImageWithMetadata img of
    Left e -> Left (T.pack e)
    Right (_, meta) -> do
      pxx <- maybe (Left "Could not determine width") Right $
                   -- first look for exif image width, then width
                   (Metadata.lookup
                     (Metadata.Exif (Exif.TagUnknown 0xA002)) meta >>=
                       exifDataToWord) <|>
                   Metadata.lookup Metadata.Width meta
      pxy <- maybe (Left "Could not determine height") Right $
                  -- first look for exif image height, then height
                  (Metadata.lookup
                     (Metadata.Exif (Exif.TagUnknown 0xA003)) meta >>=
                       exifDataToWord) <|>
                  Metadata.lookup Metadata.Height meta
      dpix <- maybe (Right 72) Right $ Metadata.lookup Metadata.DpiX meta
      dpiy <- maybe (Right 72) Right $ Metadata.lookup Metadata.DpiY meta
      return $ ImageSize
                { pxX = fromIntegral pxx
                , pxY = fromIntegral pxy
                , dpiX = fromIntegral dpix
                , dpiY = fromIntegral dpiy }
 where
  exifDataToWord (Exif.ExifLong x) = Just $ fromIntegral x
  exifDataToWord (Exif.ExifShort x) = Just $ fromIntegral x
  exifDataToWord _ = Nothing


svgSize :: WriterOptions -> ByteString -> Maybe ImageSize
svgSize opts img = do
  doc <- either (const mzero) return $ parseXMLElement
                                     $ TL.fromStrict $ UTF8.toText img
  let viewboxSize = do
        vb <- findAttrBy (== QName "viewBox" Nothing Nothing) doc
        [_,_,w,h] <- mapM safeRead (T.words vb)
        return (w,h)
  let dpi = fromIntegral $ writerDpi opts
  let dirToInt dir = do
        dim <- findAttrBy (== QName dir Nothing Nothing) doc >>= lengthToDim
        return $ inPixel opts dim
  w <- dirToInt "width" <|> (fst <$> viewboxSize)
  h <- dirToInt "height" <|> (snd <$> viewboxSize)
  return ImageSize {
    pxX  = w
  , pxY  = h
  , dpiX = dpi
  , dpiY = dpi
  }

emfSize :: ByteString -> Maybe ImageSize
emfSize img =
  let
    parseheader = runGetOrFail $ do
      skip 0x18             -- 0x00
      frameL <- getWord32le -- 0x18  measured in 1/100 of a millimetre
      frameT <- getWord32le -- 0x1C
      frameR <- getWord32le -- 0x20
      frameB <- getWord32le -- 0x24
      skip 0x20             -- 0x28
      deviceX <- getWord32le  -- 0x48 pixels of reference device
      deviceY <- getWord32le  -- 0x4C
      mmX <- getWord32le      -- 0x50 real mm of reference device (always 320*240?)
      mmY <- getWord32le      -- 0x58
      -- end of header
      let
        w = (deviceX * (frameR - frameL)) `quot` (mmX * 100)
        h = (deviceY * (frameB - frameT)) `quot` (mmY * 100)
        dpiW = (deviceX * 254) `quot` (mmX * 10)
        dpiH = (deviceY * 254) `quot` (mmY * 10)
      return $ ImageSize
        { pxX = fromIntegral w
        , pxY = fromIntegral h
        , dpiX = fromIntegral dpiW
        , dpiY = fromIntegral dpiH
        }
  in
    case parseheader . BL.fromStrict $ img of
      Left _ -> Nothing
      Right (_, _, size) -> Just size
