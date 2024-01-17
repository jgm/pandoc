{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{- |
   Module      : Text.Pandoc.Lua.Marshal.ImageSize
   Copyright   : © 2024 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>

Marshaling image properties.
-}
module Text.Pandoc.Lua.Marshal.ImageSize
  ( pushImageType
  , pushImageSize
  ) where

import Data.Char (toLower)
import HsLua
import Text.Pandoc.ImageSize

-- | Pushes an 'ImageType' as a string value.
pushImageType :: LuaError e => Pusher e ImageType
pushImageType = pushString . map toLower . show

-- | Pushes a dimensional value.
pushImageSize :: LuaError e => Pusher e ImageSize
pushImageSize = pushAsTable
  [ ("width", pushIntegral . pxX)
  , ("height", pushIntegral . pxY)
  , ("dpi_horz", pushIntegral . dpiX)
  , ("dpi_vert", pushIntegral . dpiY)
  ]
