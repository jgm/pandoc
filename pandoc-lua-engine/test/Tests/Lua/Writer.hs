{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Module      : Tests.Lua.Writer
Copyright   : © 2019-2022 Albert Krewinkel
License     : GNU GPL, version 2 or above
Maintainer  : Albert Krewinkel <albert@zeitkraut.de>

Tests for custom Lua writers.
-}
module Tests.Lua.Writer (tests) where

import Data.Default (Default (def))
import Text.Pandoc.Class (runIOorExplode, readFileStrict)
import Text.Pandoc.Extensions (Extension (..), extensionsFromList)
import Text.Pandoc.Format (ExtensionsDiff (..), FlavoredFormat (..),
                           applyExtensionsDiff)
import Text.Pandoc.Lua (writeCustom)
import Text.Pandoc.Options (WriterOptions (..))
import Text.Pandoc.Readers (readNative)
import Text.Pandoc.Writers (Writer (ByteStringWriter, TextWriter))
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Data.ByteString.Lazy as BL
import qualified Text.Pandoc.Builder as B
import qualified Text.Pandoc.UTF8 as UTF8

tests :: [TestTree]
tests =
  [ goldenVsString "default testsuite"
    "writer.custom"
    (runIOorExplode $ do
        source <- UTF8.toText <$> readFileStrict "testsuite.native"
        doc <- readNative def source
        txt <- writeCustom "sample.lua" >>= \case
          (TextWriter f, _, _) -> f def doc
          _            -> error "Expected a text writer"
        pure $ BL.fromStrict (UTF8.fromText txt))

  , goldenVsString "tables testsuite"
    "tables.custom"
    (runIOorExplode $ do
        source <- UTF8.toText <$> readFileStrict "tables.native"
        doc <- readNative def source
        txt <- writeCustom "sample.lua" >>= \case
          (TextWriter f, _, _) -> f def doc
          _            -> error "Expected a text writer"
        pure $ BL.fromStrict (UTF8.fromText txt))

  , goldenVsString "bytestring writer"
    "bytestring.bin"
    (runIOorExplode $
        writeCustom "bytestring.lua" >>= \case
          (ByteStringWriter f, _, _) -> f def mempty
          _                       -> error "Expected a bytestring writer")

  , goldenVsString "template"
    "writer-template.out.txt"
    (runIOorExplode $ do
        txt <- writeCustom "writer-template.lua" >>= \case
          (TextWriter f, _, mt) -> do
            template <- mt
            let opts = def{ writerTemplate = Just template }
            f opts (B.doc (B.plain (B.str "body goes here")))
          _ -> error "Expected a text writer"
        pure $ BL.fromStrict (UTF8.fromText txt))

  , testCase "preset extensions" $ do
      let format = FlavoredFormat "extensions.lua" mempty
      result <- runIOorExplode $ writeCustom "extensions.lua" >>= \case
          (TextWriter write, extsConf, _) -> do
            exts <- applyExtensionsDiff extsConf format
            write def{writerExtensions = exts} (B.doc mempty)
          _                        -> error "Expected a text writer"
      result @?= "smart extension is enabled;\ncitations extension is disabled\n"
  , testCase "modified extensions" $ do
      let ediff = ExtensionsDiff
            { extsToEnable = extensionsFromList [Ext_citations]
            , extsToDisable = mempty
            }
      let format = FlavoredFormat "extensions.lua" ediff
      result <- runIOorExplode $ writeCustom "extensions.lua" >>= \case
          (TextWriter write, extsConf, _) -> do
            exts <- applyExtensionsDiff extsConf format
            write def{writerExtensions = exts} (B.doc mempty)
          _                        -> error "Expected a text writer"
      result @?= "smart extension is enabled;\ncitations extension is enabled\n"
  ]
