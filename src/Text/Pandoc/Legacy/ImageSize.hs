module Text.Pandoc.Legacy.ImageSize ( TP.ImageType(..)
                             , TP.imageType
                             , imageSize
                             , TP.sizeInPixels
                             , TP.sizeInPoints
                             , desiredSizeInPoints
                             , TP.Dimension(..)
                             , TP.Direction(..)
                             , dimension
                             , lengthToDim
                             , TP.scaleDimension
                             , TP.inInch
                             , TP.inPixel
                             , TP.inPoints
                             , TP.inEm
                             , numUnit
                             , showInInch
                             , showInPixel
                             , showFl
                             ) where

import qualified Text.Pandoc.Options as TP

import qualified Data.Text as T
import qualified Text.Pandoc.ImageSize as TP
import Data.ByteString (ByteString)
import qualified Text.Pandoc.Legacy.Definition as TP

showFl :: (RealFloat a) => a -> String
showFl = T.unpack . TP.showFl

imageSize :: TP.WriterOptions -> ByteString -> Either String TP.ImageSize
imageSize opts = either (Left . T.unpack) Right . TP.imageSize opts

desiredSizeInPoints :: TP.WriterOptions -> TP.Attr -> TP.ImageSize -> (Double, Double)
desiredSizeInPoints opts = TP.desiredSizeInPoints opts . go
  where
    go (x, y, z) = (T.pack x, map T.pack y, map (\(a, b) -> (T.pack a, T.pack b)) z)

dimension :: TP.Direction -> TP.Attr -> Maybe TP.Dimension
dimension d = TP.dimension d . go
  where
    go (x, y, z) = (T.pack x, map T.pack y, map (\(a, b) -> (T.pack a, T.pack b)) z)

showInInch :: TP.WriterOptions -> TP.Dimension -> String
showInInch opts = T.unpack . TP.showInInch opts

showInPixel :: TP.WriterOptions -> TP.Dimension -> String
showInPixel opts = T.unpack . TP.showInPixel opts

numUnit :: String -> Maybe (Double, String)
numUnit = fmap go . TP.numUnit . T.pack
  where
    go (x, y) = (x, T.unpack y)

lengthToDim :: String -> Maybe TP.Dimension
lengthToDim = TP.lengthToDim . T.pack 
