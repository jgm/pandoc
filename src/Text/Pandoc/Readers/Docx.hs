{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Readers.Docx (readDocx) where

import Text.Pandoc.Parsing
import Text.Pandoc.Definition
import Text.HTML.TagSoup (parseTags)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.UTF8 (toString)
import Codec.Archive.Zip (toArchive, findEntryByPath, fromEntry)

-- | Convert OpenXML-formatted string to 'Pandoc' document.
readDocx :: ParserState   -- ^ Parser state
         -> ByteString    -- ^ ByteString to parse
         -> Pandoc
readDocx st bs = Pandoc meta blocks
    where
      meta = Meta [] [] []
      blocks = readWith parseDocument st tags
      str = show $ take 100 tags
      tags = parseTags $ toString document
      document = readFromZip "word/document.xml" bs

readFromZip :: FilePath -> ByteString -> ByteString
readFromZip path bs = maybe "" fromEntry (findEntryByPath path $ toArchive bs)

type TagParser = GenParser (Tag String) ParserState

parseDocument :: TagParser [Block]
parseDocument = try $ do
  skipMany nonText
  contents <- many $ (pText >>~ skipMany nonText)
  return contents

nonText = pSatisfy isTagText

pText :: TagParser Block
pText = try $ do
  (TagText str) <- pSatisfy isTagText
  return (Str str)
