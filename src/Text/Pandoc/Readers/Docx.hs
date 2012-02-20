module Text.Pandoc.Readers.Docx (readDocx) where

import Text.ParserCombinators.Parsec
import Text.Pandoc.Parsing
import Text.Pandoc.Definition
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match (tagOpen)
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Maybe (isJust, fromJust, maybeToList)
import Data.List (isPrefixOf)
import Debug.Trace
import Codec.Archive.Zip (toArchive, findEntryByPath, fromEntry)
import Control.Monad (liftM, guard)
import Control.Applicative ((<$>), (<*>))

-- | Convert OpenXML-formatted string to 'Pandoc' document.
readDocx :: ParserState   -- ^ Parser state
         -> ByteString    -- ^ ByteString to parse
         -> Pandoc
readDocx st bs = Pandoc meta doc
    where
      meta = Meta [] [] []
      doc = readWith (parseDocument env) st tags
      tags = parseTags $ toString document
      document = readFromZip "word/document.xml" bs
      env = readWith parseEnv st envtags
      envtags = parseTags $ toString $ readFromZip "word/styles.xml" bs

readFromZip :: FilePath -> ByteString -> ByteString
readFromZip path bs = maybe (pack "") fromEntry (findEntryByPath path $ toArchive bs)

type TagParser = GenParser (Tag String) ParserState
type Env = [(String, String)]

parseEnv :: TagParser Env
parseEnv = do
  skipMany $ pSatisfy (~/= TagOpen "w:styles" [])
  env <- pInTags "w:styles" parseStyle
  let env' = map (\(k, v) -> if "heading " `isPrefixOf` v
                                  || "标题 " `isPrefixOf` v
                                then (k, dropWhile (not . (`elem` ['0'..'9'])) v)
                                else (k, v)) env
  return env'

parseStyle :: TagParser [(String, String)]
parseStyle = try $ do
  TagOpen sName sAttrs <- pSatisfy (~== TagOpen "w:style" [])
  let non = pSatisfy (\t -> (t ~/= TagClose "w:style") && (t ~/= TagOpen "w:name" []))
  skipMany non
  TagOpen nName nAttrs <- pSatisfy (~== TagOpen "w:name" [])
  manyTill anyTag (pCloses "w:style")
  return $ maybeToList $ (,) <$> lookup "w:styleId" sAttrs <*> lookup "w:val" nAttrs
  

parseDocument :: Env -> TagParser [Block]
parseDocument env = try $ do
  skipMany $ pSatisfy (~/= TagOpen "w:document" [])
  pInTags "w:document" (parseBody env)

parseBody :: Env -> TagParser [Block]
parseBody env = try $ do
  skipMany $ pSatisfy (~/= TagOpen "w:body" [])
  pInTags "w:body" (blocks env)

blocks :: Env -> TagParser [Block]
blocks env = liftM concat $ many1 (block env)

block :: Env -> TagParser [Block]
block env = choice
               [ pHead env
               , pList env
               , pTable env
               , pPlain env
               ]

pHead :: Env -> TagParser [Block]
pHead env = try $ do
  pSatisfy (~== TagOpen "w:p" [])
  let non = pSatisfy (\t -> (t ~/= TagClose "w:p") && (t ~/= TagOpen "w:pStyle" []))
  skipMany non
  TagOpen _ attrs <- pSatisfy (~== TagOpen "w:pStyle" [])
  let val = lookup "w:val" attrs >>= flip lookup env
  --                produce [Just "1"..Just "6"]
  guard $ val `elem` (map (Just . (\a-> [a])) ['1'..'6'])
  str <- liftM concat $ many1Till plainText (pSatisfy (~== TagClose "w:p"))
  return [Header (read (fromJust val)) [Str str]]

pList :: Env -> TagParser [Block]
pList env = try $ do
  items <- many1 (item env)
  return $ [BulletList items]

item :: Env -> TagParser [Block]
item env = try $ do
  pSatisfy (~== TagOpen "w:p" [])
  let non = pSatisfy (\t -> (t ~/= TagClose "w:p") && (t ~/= TagOpen "w:ilvl" []))
  skipMany non
  pSatisfy (~== TagOpen "w:ilvl" [])
  str <- liftM concat $ many1Till plainText (pSatisfy (~== TagClose "w:p"))
  return [Plain [Str str]]

pTable :: Env -> TagParser [Block]
pTable env = try $ do
  pSatisfy (~== TagOpen "w:tbl" [])
  let non = pSatisfy (\t -> (t ~/= TagClose "w:tbl") && (t ~/= TagOpen "w:tr" []))
  skipMany non
  rows <- manyTill (pTableRow env) (pCloses "w:tbl")
  let cols = maximum $ map length rows
  let aligns = replicate cols AlignLeft
  let widths = replicate cols 0
  return [Table [] aligns widths [] rows]

pTableRow :: Env -> TagParser [TableCell]
pTableRow env = try $ do
  pSatisfy (~== TagOpen "w:tr" [])
  let non = pSatisfy (\t -> (t ~/= TagClose "w:tr") && (t ~/= TagOpen "w:tc" []))
  skipMany non
  cells <- manyTill (pTableCell env) (pCloses "w:tr")
  return cells

pTableCell :: Env -> TagParser TableCell
pTableCell env = try $ do
  pSatisfy (~== TagOpen "w:tc" [])
  let non = pSatisfy (\t -> (t ~/= TagClose "w:tc") && (t ~/= TagOpen "w:p" []))
  skipMany non
  cell <- manyTill (block env) (pCloses "w:tc")
  return $ concat cell

pPlain :: Env -> TagParser [Block]
pPlain _ = do
  str <- plainText
  return [Plain [Str str]]

plainText :: TagParser String
plainText = try $ do
  tag <- lookAhead anyTag
  pPlain' tag
    where pPlain' (TagOpen n _) = pInTags n getText
          pPlain' (TagText str) = do anyTag
                                     return str
          pPlain' _             = do anyTag
                                     return ""

pPlainDebug :: TagParser [Block]
pPlainDebug = do
  str <- plainTextDebug
  return [Plain [Str str]]

plainTextDebug :: TagParser String
plainTextDebug = try $ do
  tag <- lookAhead anyTag
  txt <- pPlain' tag
  trace ("plainText: return " ++ txt) (return txt)
    where pPlain' (TagOpen n _) = trace ("plainText: open " ++ n) $ pInTags n getTextDebug
          pPlain' (TagText str) = do anyTag
                                     return $ trace ("plainText: text " ++ str) str
          pPlain' _             = do anyTag
                                     return $ trace ("plainText: other ") ""

getTextDebug :: TagParser String
getTextDebug = do
  x <- anyTag
  return $ if isTagText (trace ("getText: " ++ show x) x) then innerText [x] else []

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

