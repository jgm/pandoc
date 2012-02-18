module Text.Pandoc.Readers.Docx (readDocx) where

import Text.ParserCombinators.Parsec
import Text.Pandoc.Parsing
import Text.Pandoc.Definition
import Text.HTML.TagSoup
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.ByteString.Lazy.UTF8 (toString)
import Codec.Archive.Zip (toArchive, findEntryByPath, fromEntry)
import Control.Monad (liftM)

-- | Convert OpenXML-formatted string to 'Pandoc' document.
readDocx :: ParserState   -- ^ Parser state
         -> ByteString    -- ^ ByteString to parse
         -> Pandoc
readDocx st bs = Pandoc meta doc
    where
      meta = Meta [] [] []
      doc = readWith parseDocument st tags
      tags = parseTags $ toString document
      document = readFromZip "word/document.xml" bs

readFromZip :: FilePath -> ByteString -> ByteString
readFromZip path bs = maybe (pack "") fromEntry (findEntryByPath path $ toArchive bs)

type TagParser = GenParser (Tag String) ParserState

parseDocument :: TagParser [Block]
parseDocument = try $ do
  skipMany $ pSatisfy (not . (~== TagOpen "w:document" []))
  pInTags "w:document" parseBody

parseBody :: TagParser [Block]
parseBody = try $ do
  skipMany $ pSatisfy (not . (~== TagOpen "w:body" []))
  pInTags "w:body" blocks

blocks :: TagParser [Block]
blocks = liftM concat $ many1 block

block :: TagParser [Block]
block = choice
            [ pList
            , pPlain
            ]
pList :: TagParser [Block]
pList = try $ do
  items <- many1 item
  return $ [BulletList items]

item :: TagParser [Block]
item = try $ do
  pSatisfy (~== TagOpen "w:p" [])
  let non = pSatisfy (\t -> (t ~/= TagClose "w:p") && (t ~/= TagOpen "w:ilvl" []))
  skipMany non
  pSatisfy (~== TagOpen "w:ilvl" [])
  str <- liftM concat $ many1Till plainText (pSatisfy (~== TagClose "w:p"))
  return [Plain [Str str]]

pPlain :: TagParser [Block]
pPlain = do
  str <- plainText
  return [Plain [Str str]]

plainText :: TagParser String
plainText = try $ do
  tag <- lookAhead anyTag
  pPlain' tag
    where pPlain' (TagOpen n _) = do str <- pInTags n getText
                                     return str
          pPlain' (TagText str) = do anyTag
                                     return str
          pPlain' _             = do anyTag
                                     return ""

getText :: TagParser String
getText = do
  x <- anyTag
  return $ if isTagText x then innerText [x] else []

anyTag :: TagParser (Tag String)
anyTag = pSatisfy (const True)

pSatisfy :: (Tag String -> Bool) -> TagParser (Tag String)
pSatisfy f =  tokenPrim show
                        (\pos _ _ -> setSourceLine pos (1 + sourceLine pos))
                        (\t -> if f t then Just t else Nothing)

pInTags :: String -> TagParser [a]
        -> TagParser [a]
pInTags tagtype parser = try $ do
  pSatisfy (~== TagOpen tagtype [])
  liftM concat $ manyTill parser (pCloses tagtype <|> eof)

pCloses :: String -> TagParser ()
pCloses tagtype = try $ do
  pSatisfy (~== TagClose tagtype)
  return ()
