{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, CPP #-}
{- |
Module      : Text.Pandoc.Image
Copyright   : Copyright (C) 2020-2022 John MacFarlane
License     : GNU GPL, version 2 or above

Maintainer  : John MacFarlane <jgm@berkeley.edu>
Stability   : alpha
Portability : portable

Functions for converting images.
-}
module Text.Pandoc.Image ( svgToPng ) where
import Text.Pandoc.Process (pipeProcess)
import qualified Data.ByteString.Lazy as L
import System.Exit
import Data.Text (Text)
import Text.Pandoc.Shared (tshow)
import qualified Control.Exception as E
import Control.Monad.IO.Class (MonadIO(liftIO))

-- | Convert svg image to png. rsvg-convert
-- is used and must be available on the path.
svgToPng :: MonadIO m
         => Int           -- ^ DPI
         -> L.ByteString  -- ^ Input image as bytestring
         -> m (Either Text L.ByteString)
svgToPng dpi bs = do
  let dpi' = show dpi
  liftIO $ E.catch
       (do (exit, out) <- pipeProcess Nothing "rsvg-convert"
                          ["-f","png","-a","--dpi-x",dpi',"--dpi-y",dpi']
                          bs
           return $ if exit == ExitSuccess
              then Right out
              else Left "conversion from SVG failed")
       (\(e :: E.SomeException) -> return $ Left $
           "check that rsvg-convert is in path.\n" <> tshow e)
