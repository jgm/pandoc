{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Reader.ODT
   Copyright   : Copyright (C) 2015 Martin Linnemann
   License     : GNU GPL, version 2 or above

   Maintainer  : Martin Linnemann <theCodingMarlin@googlemail.com>
   Stability   : alpha
   Portability : portable

Entry point to the odt reader.
-}

module Text.Pandoc.Readers.ODT ( readODT ) where

import Codec.Archive.Zip
import Text.Pandoc.XML.Light

import qualified Data.ByteString.Lazy as B

import System.FilePath

import Control.Monad.Except (throwError)

import qualified Data.Text as T

import Text.Pandoc.Class.PandocMonad (PandocMonad)
import qualified Text.Pandoc.Class.PandocMonad as P
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.MediaBag
import Text.Pandoc.Options
import qualified Text.Pandoc.UTF8 as UTF8

import Text.Pandoc.Readers.ODT.ContentReader
import Text.Pandoc.Readers.ODT.StyleReader

import Text.Pandoc.Readers.ODT.Generic.Fallible
import Text.Pandoc.Readers.ODT.Generic.XMLConverter
import Text.Pandoc.Shared (filteredFilesFromArchive)

readODT :: PandocMonad m
        => ReaderOptions
        -> B.ByteString
        -> m Pandoc
readODT opts bytes = case readODT' opts bytes of
  Right (doc, mb) -> do
    P.setMediaBag mb
    return doc
  Left e -> throwError e

--
readODT' :: ReaderOptions
         -> B.ByteString
         -> Either PandocError (Pandoc, MediaBag)
readODT' _ bytes = bytesToODT bytes-- of
--                    Right (pandoc, mediaBag) -> Right (pandoc , mediaBag)
--                    Left  err                -> Left err

--
bytesToODT :: B.ByteString -> Either PandocError (Pandoc, MediaBag)
bytesToODT bytes = case toArchiveOrFail bytes of
  Right archive -> archiveToODT archive
  Left err      -> Left $ PandocParseError
                        $ "Could not unzip ODT: " <> T.pack err

--
archiveToODT :: Archive -> Either PandocError (Pandoc, MediaBag)
archiveToODT archive = do
  let onFailure msg Nothing = Left $ PandocParseError msg
      onFailure _   (Just x) = Right x
  contentEntry <- onFailure "Could not find content.xml"
                   (findEntryByPath "content.xml" archive)
  stylesEntry <- onFailure "Could not find styles.xml"
                   (findEntryByPath "styles.xml" archive)
  contentElem <- entryToXmlElem contentEntry
  stylesElem <- entryToXmlElem stylesEntry
  styles <- either
               (\_ -> Left $ PandocParseError "Could not read styles")
               Right
               (chooseMax (readStylesAt stylesElem ) (readStylesAt contentElem))
  let filePathIsODTMedia :: FilePath -> Bool
      filePathIsODTMedia fp =
        let (dir, name) = splitFileName fp
        in  (dir == "Pictures/") || (dir /= "./" && name == "content.xml")
  let media = filteredFilesFromArchive archive filePathIsODTMedia
  let startState = readerState styles media
  either (\_ -> Left $ PandocParseError "Could not convert opendocument") Right
    (runConverter' read_body startState contentElem)


--
entryToXmlElem :: Entry -> Either PandocError Element
entryToXmlElem entry =
  case parseXMLElement . UTF8.toTextLazy . fromEntry $ entry of
    Right x  -> Right x
    Left msg -> Left $ PandocXMLError (T.pack $ eRelativePath entry) msg
