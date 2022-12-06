{-# LANGUAGE LambdaCase        #-}
{- |
Module      : Tests.Lua.Reader
Copyright   : © 2022 Albert Krewinkel
License     : GPL-2.0-or-later
Maintainer  : Albert Krewinkel <pandoc@tarleb.com>

Tests for custom Lua readers.
-}
module Tests.Lua.Reader (tests) where

import Control.Arrow ((>>>))
import Data.Char (chr)
import Data.Default (Default (def))
import Text.Pandoc.Class (runIOorExplode)
import Text.Pandoc.Lua (loadCustom)
import Text.Pandoc.Readers (Reader (ByteStringReader))
import Text.Pandoc.Scripting (customReader)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit ((@?=), testCase)

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Text.Pandoc.Builder as B

tests :: [TestTree]
tests =
  [ testCase "read binary to code block" $ do
    input <- BL.readFile "bytestring.bin"
    doc <- runIOorExplode $
      loadCustom "bytestring-reader.lua" >>= (customReader >>> \case
        Just (ByteStringReader f) -> f def input
        _                         -> error "Expected a bytestring reader")
    let bytes = mconcat $ map (B.str . T.singleton . chr) [0..255]
    doc @?= B.doc (B.plain bytes)
  ]
