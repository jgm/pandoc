{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, CPP #-}
{- |
Module      : Text.Pandoc.Image
Copyright   : Copyright (C) 2020-2023 John MacFarlane
License     : GNU GPL, version 2 or above

Maintainer  : John MacFarlane <jgm@berkeley.edu>
Stability   : alpha
Portability : portable

Functions for converting images.
-}
module Text.Pandoc.Image ( createPngFallback, svgToPngIO ) where
import Text.Pandoc.Process (pipeProcess)
import qualified Data.ByteString.Lazy as L
import System.Exit
import Data.Text (Text)
import Text.Pandoc.Shared (tshow)
import qualified Control.Exception as E
import Control.Monad.IO.Class (MonadIO(liftIO))
import Text.Pandoc.Class.PandocMonad
import qualified Data.Text as T
import Text.Pandoc.Logging (LogMessage(CouldNotConvertImage))
import Data.ByteString.Lazy (ByteString)
import Text.Pandoc.MediaBag (MediaItem, lookupMedia)
import Text.Printf (printf)

-- | Convert svg image to png. rsvg-convert
-- is used and must be available on the path.
svgToPngIO :: (PandocMonad m, MonadIO m)
         => Int           -- ^ DPI
         -> Maybe Double        -- ^ width in Points
         -> Maybe Double        -- ^ height in Points
         -> L.ByteString  -- ^ Input image as bytestring
         -> m (Either Text L.ByteString)
svgToPngIO dpi widthPt heightPt bs = do
  let dpi' = show dpi
  let args = ["-f","png","-a","--dpi-x",dpi',"--dpi-y",dpi']
        ++ pt "width" widthPt ++ pt "height" heightPt
  trace (T.intercalate " " $ map T.pack $ "rsvg-convert" : args)
  liftIO $ E.catch
       (do (exit, out) <- pipeProcess Nothing "rsvg-convert"
                          args
                          bs
           return $ if exit == ExitSuccess
              then Right out
              else Left "conversion from SVG failed")
       (\(e :: E.SomeException) -> return $ Left $
           "check that rsvg-convert is in path.\n" <> tshow e)
  where pt name = maybe [] $ \points -> ["--" <> name, printf "%.6fpt" points]

createPngFallback :: (PandocMonad m) => Int -> (Double, Double) -> FilePath -> ByteString -> m (Maybe MediaItem)
createPngFallback dpi (xPt, yPt) fp bs = do
  -- create fallback pngs for svgs
  res <- svgToPng dpi (Just xPt) (Just yPt) bs
  case res of
    Right bs' -> do
      insertMedia fp (Just "image/png") bs'
      lookupMedia fp <$> getMediaBag
    Left e -> do
      report $ CouldNotConvertImage (T.pack fp) (tshow e)
      return Nothing
