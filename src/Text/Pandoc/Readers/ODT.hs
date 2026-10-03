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

module Text.Pandoc.Readers.ODT ( readODT, readFODT ) where

import Codec.Archive.Zip
import Text.Pandoc.XML.Light
import qualified Text.Pandoc.XML.Light as XML
import Text.Pandoc.Walk

import Data.Char (isDigit)
import qualified Data.ByteString.Lazy as B

import System.FilePath

import Control.Monad (unless)
import Control.Monad.Except (throwError)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

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
import Text.Pandoc.Sources (ToSources(..), sourcesToText)

readODT :: PandocMonad m
        => ReaderOptions
        -> B.ByteString
        -> m Pandoc
readODT opts bytes = case readODT' opts bytes of
  Right (doc, mb) -> do
    P.setMediaBag mb
    return $ walk makeFigure doc
  Left e -> throwError e

-- | Read a flat OpenDocument text file, i.e. ODF's representation as a
-- single XML file rather than a zip archive.
readFODT :: (PandocMonad m, ToSources a)
         => ReaderOptions
         -> a
         -> m Pandoc
readFODT _ inp = do
  root <- either (throwError . PandocXMLError "") pure
            (parseXMLElement (TL.fromStrict (sourcesToText (toSources inp))))
  case elementToODT root of
    Right (doc, mb) -> do
      P.setMediaBag mb
      return $ walk makeFigure doc
    Left e -> throwError e

-- | A flat file has exactly one of each of the top-level elements that a
-- package distributes over @content.xml@ and @styles.xml@, and they are
-- all children of the root.  The converter addresses everything relative
-- to the element it is given, so it can simply be handed that root.
elementToODT :: Element -> Either PandocError (Pandoc, MediaBag)
elementToODT root = do
  -- the local name only, consistently with the URI-based namespace
  -- matching downstream; office:document-content is correctly rejected
  unless (XML.qName (XML.elName root) == "document") $
    Left $ PandocParseError
      "Expected office:document at the root of the flat OpenDocument file"
  styles <- either
               (\_ -> Left $ PandocParseError "Could not read styles")
               Right
               (readStylesAt root)
  -- the media list is empty: all media arrives in office:binary-data
  either (\_ -> Left $ PandocParseError "Could not convert opendocument") Right
    (runConverter read_body (readerState styles []) root)

-- the ODT parser uses old-style figures: an image with title beginning
-- "fig:" in a paragraph by itself.  Convert these to new Figure elements.
makeFigure :: Block -> Block
makeFigure (Para [ Image (ident, classes, kvs) capt (src, tit) ])
  | "fig:" `T.isPrefixOf` tit
  = Figure (ident, [], []) (Caption Nothing [Plain capt'])
      [Plain [Image ("", classes, kvs) capt (src, "")]]
   where
     capt' = case capt of -- strip "Figure 1:" for consistency
                 (Str _ : Space : Str t : Space : xs)
                    | T.all (\c -> isDigit c || c == ':') t
                    , ":" `T.isSuffixOf` t -> xs
                 xs -> xs
makeFigure x = x

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
    (runConverter read_body startState contentElem)


--
entryToXmlElem :: Entry -> Either PandocError Element
entryToXmlElem entry =
  case parseXMLElement . UTF8.toTextLazy . fromEntry $ entry of
    Right x  -> Right x
    Left msg -> Left $ PandocXMLError (T.pack $ eRelativePath entry) msg
