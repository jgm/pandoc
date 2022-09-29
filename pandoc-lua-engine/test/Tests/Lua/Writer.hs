{- |
Module      : Tests.Lua.Writer
Copyright   : © 2019-2022 Albert Krewinkel
License     : GNU GPL, version 2 or above

Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
Stability   : alpha
Portability : portable

Tests for custom Lua writers.
-}
module Tests.Lua.Writer (tests) where

import Data.Default (Default (def))
import Text.Pandoc.Class (runIOorExplode, readFileStrict)
import Text.Pandoc.Lua (writeCustom)
import Text.Pandoc.Readers (readNative)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

import qualified Data.ByteString.Lazy as BL
import qualified Text.Pandoc.UTF8 as UTF8

tests :: [TestTree]
tests =
  [ goldenVsString "default testsuite"
    "writer.custom"
    (runIOorExplode $ do
        source <- UTF8.toText <$> readFileStrict "testsuite.native"
        doc <- readNative def source
        txt <- writeCustom "sample.lua" def doc
        pure $ BL.fromStrict (UTF8.fromText txt))

  , goldenVsString "tables testsuite"
    "tables.custom"
    (runIOorExplode $ do
        source <- UTF8.toText <$> readFileStrict "tables.native"
        doc <- readNative def source
        txt <- writeCustom "sample.lua" def doc
        pure $ BL.fromStrict (UTF8.fromText txt))
  ]
