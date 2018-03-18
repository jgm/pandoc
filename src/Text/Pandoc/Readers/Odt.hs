{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}

{-
Copyright (C) 2015 Martin Linnemann <theCodingMarlin@googlemail.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Reader.Odt
   Copyright   : Copyright (C) 2015 Martin Linnemann
   License     : GNU GPL, version 2 or above

   Maintainer  : Martin Linnemann <theCodingMarlin@googlemail.com>
   Stability   : alpha
   Portability : portable

Entry point to the odt reader.
-}

module Text.Pandoc.Readers.Odt ( readOdt ) where

import Prelude
import Codec.Archive.Zip
import qualified Text.XML.Light as XML

import qualified Data.ByteString.Lazy as B

import System.FilePath

import Control.Monad.Except (throwError)

import Text.Pandoc.Class (PandocMonad)
import qualified Text.Pandoc.Class as P
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.MediaBag
import Text.Pandoc.Options
import qualified Text.Pandoc.UTF8 as UTF8

import Text.Pandoc.Readers.Odt.ContentReader
import Text.Pandoc.Readers.Odt.StyleReader

import Text.Pandoc.Readers.Odt.Generic.Fallible
import Text.Pandoc.Readers.Odt.Generic.XMLConverter
import Text.Pandoc.Shared (filteredFilesFromArchive)

readOdt :: PandocMonad m
        => ReaderOptions
        -> B.ByteString
        -> m Pandoc
readOdt opts bytes = case readOdt' opts bytes of
  Right (doc, mb) -> do
    P.setMediaBag mb
    return doc
  Left e -> throwError e

--
readOdt' :: ReaderOptions
         -> B.ByteString
         -> Either PandocError (Pandoc, MediaBag)
readOdt' _ bytes = bytesToOdt bytes-- of
--                    Right (pandoc, mediaBag) -> Right (pandoc , mediaBag)
--                    Left  err                -> Left err

--
bytesToOdt :: B.ByteString -> Either PandocError (Pandoc, MediaBag)
bytesToOdt bytes = case toArchiveOrFail bytes of
  Right archive -> archiveToOdt archive
  Left _        -> Left $ PandocParseError "Couldn't parse odt file."

--
archiveToOdt :: Archive -> Either PandocError (Pandoc, MediaBag)
archiveToOdt archive
  | Just contentEntry      <- findEntryByPath "content.xml" archive
  , Just stylesEntry       <- findEntryByPath "styles.xml"  archive
  , Just contentElem       <- entryToXmlElem contentEntry
  , Just stylesElem        <- entryToXmlElem stylesEntry
  , Right styles           <- chooseMax (readStylesAt stylesElem )
                                        (readStylesAt contentElem)
  , media                  <- filteredFilesFromArchive archive filePathIsOdtMedia
  , startState             <- readerState styles media
  , Right pandocWithMedia  <- runConverter' read_body
                                            startState
                                            contentElem

  = Right pandocWithMedia

  | otherwise
    -- Not very detailed, but I don't think more information would be helpful
  = Left $ PandocParseError "Couldn't parse odt file."
    where
      filePathIsOdtMedia :: FilePath -> Bool
      filePathIsOdtMedia fp =
        let (dir, _) = splitFileName fp
        in
         (dir == "Pictures/")


--
entryToXmlElem :: Entry -> Maybe XML.Element
entryToXmlElem = XML.parseXMLDoc . UTF8.toStringLazy . fromEntry
