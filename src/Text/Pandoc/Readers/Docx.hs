{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Readers.Docx (readDocx) where

import Text.Pandoc.Parsing
import Text.Pandoc.Definition
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.UTF8 (toString)
import Codec.Archive.Zip (toArchive, findEntryByPath, fromEntry)

-- | Convert OpenXML-formatted string to 'Pandoc' document.
readDocx :: ParserState   -- ^ Parser state
         -> ByteString    -- ^ ByteString to parse
         -> Pandoc
readDocx _ bs = Pandoc (Meta [] [] []) [Para [Str str]]
    where
      str      = (take 100 . toString) document
      document = readFromZip "word/document.xml" bs

readFromZip :: FilePath -> ByteString -> ByteString
readFromZip path bs = maybe "" fromEntry (findEntryByPath path $ toArchive bs)
