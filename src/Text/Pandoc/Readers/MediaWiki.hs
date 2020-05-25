{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.MediaWiki
   Copyright   : Copyright (C) 2012-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of mediawiki text to 'Pandoc' document.
-}
{-
TODO:
_ correctly handle tables within tables
_ parse templates?
-}
module Text.Pandoc.Readers.MediaWiki ( readMediaWiki ) where

import Control.Monad
import Control.Monad.Except (throwError)
import Data.Char (isDigit, isSpace)
import qualified Data.Foldable as F
import Data.List (intersperse)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Sequence (ViewL (..), viewl, (<|))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Text.HTML.TagSoup
import Text.Pandoc.Builder (Blocks, Inlines, trimInlines)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class.PandocMonad (PandocMonad (..))
import Text.Pandoc.Definition
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (nested)
import Text.Pandoc.Readers.HTML (htmlTag, isBlockTag, isCommentTag)
import Text.Pandoc.Shared (crFilter, safeRead, stringify, stripTrailingNewlines,
                           trim, splitTextBy, tshow)
import Text.Pandoc.Walk (walk)
import Text.Pandoc.XML (fromEntities)

-- | Read mediawiki from an input string and return a Pandoc document.
readMediaWiki :: PandocMonad m
              => ReaderOptions -- ^ Reader options
              -> Text          -- ^ String to parse (assuming @'\n'@ line endings)
              -> m Pandoc
readMediaWiki opts s = do
  parsed <- readWithM parseMediaWiki MWState{ mwOptions = opts
                                            , mwMaxNestingLevel = 4
                                            , mwNextLinkNumber  = 1
                                            , mwCategoryLinks = []
                                            , mwIdentifierList = Set.empty
                                            , mwLogMessages = []
                                            , mwInTT = False
                                            }
            (crFilter s <> "\n")
  case parsed of
    Right result -> return result
    Left e       -> throwError e

data MWState = MWState { mwOptions         :: ReaderOptions
                       , mwMaxNestingLevel :: Int
                       , mwNextLinkNumber  :: Int
                       , mwCategoryLinks   :: [Inlines]
                       , mwIdentifierList  :: Set.Set Text
                       , mwLogMessages     :: [LogMessage]
                       , mwInTT            :: Bool
                       }

type MWParser m = ParserT Text MWState m

instance HasReaderOptions MWState where
  extractReaderOptions = mwOptions

instance HasIdentifierList MWState where
  extractIdentifierList     = mwIdentifierList
  updateIdentifierList f st = st{ mwIdentifierList = f $ mwIdentifierList st }

instance HasLogMessages MWState where
  addLogMessage m s = s{ mwLogMessages = m : mwLogMessages s }
  getLogMessages = reverse . mwLogMessages

--
-- auxiliary functions
--

-- This is used to prevent exponential blowups for things like:
-- ''a'''a''a'''a''a'''a''a'''a
nested :: PandocMonad m => MWParser m a -> MWParser m a
nested p = do
  nestlevel <- mwMaxNestingLevel `fmap` getState
  guard $ nestlevel > 0
  updateState $ \st -> st{ mwMaxNestingLevel = mwMaxNestingLevel st - 1 }
  res <- p
  updateState $ \st -> st{ mwMaxNestingLevel = nestlevel }
  return res

specialChars :: [Char]
specialChars = "'[]<=&*{}|\":\\"

spaceChars :: [Char]
spaceChars = " \n\t"

sym :: PandocMonad m => Text -> MWParser m ()
sym s = () <$ try (string $ T.unpack s)

newBlockTags :: [Text]
newBlockTags = ["haskell","syntaxhighlight","source","gallery","references"]

isBlockTag' :: Tag Text -> Bool
isBlockTag' tag@(TagOpen t _) = (isBlockTag tag || t `elem` newBlockTags) &&
  t `notElem` eitherBlockOrInline
isBlockTag' tag@(TagClose t) = (isBlockTag tag || t `elem` newBlockTags) &&
  t `notElem` eitherBlockOrInline
isBlockTag' tag = isBlockTag tag

isInlineTag' :: Tag Text -> Bool
isInlineTag' (TagComment _) = True
isInlineTag' t              = not (isBlockTag' t)

eitherBlockOrInline :: [Text]
eitherBlockOrInline = ["applet", "button", "del", "iframe", "ins",
                               "map", "area", "object"]

htmlComment :: PandocMonad m => MWParser m ()
htmlComment = () <$ htmlTag isCommentTag

inlinesInTags :: PandocMonad m => Text -> MWParser m Inlines
inlinesInTags tag = try $ do
  (_,raw) <- htmlTag (~== TagOpen tag [])
  if T.any (== '/') raw   -- self-closing tag
     then return mempty
     else trimInlines . mconcat <$>
            manyTill inline (htmlTag (~== TagClose tag))

blocksInTags :: PandocMonad m => Text -> MWParser m Blocks
blocksInTags tag = try $ do
  (_,raw) <- htmlTag (~== TagOpen tag [])
  let closer = if tag == "li"
                  then htmlTag (~== TagClose ("li" :: Text))
                     <|> lookAhead (
                              htmlTag (~== TagOpen ("li" :: Text) [])
                          <|> htmlTag (~== TagClose ("ol" :: Text))
                          <|> htmlTag (~== TagClose ("ul" :: Text)))
                  else htmlTag (~== TagClose tag)
  if T.any (== '/') raw   -- self-closing tag
     then return mempty
     else mconcat <$> manyTill block closer

textInTags :: PandocMonad m => Text -> MWParser m Text
textInTags tag = try $ do
  (_,raw) <- htmlTag (~== TagOpen tag [])
  if T.any (== '/') raw   -- self-closing tag
     then return ""
     else T.pack <$> manyTill anyChar (htmlTag (~== TagClose tag))

--
-- main parser
--

parseMediaWiki :: PandocMonad m => MWParser m Pandoc
parseMediaWiki = do
  bs <- mconcat <$> many block
  spaces
  eof
  categoryLinks <- reverse . mwCategoryLinks <$> getState
  let categories = if null categoryLinks
                      then mempty
                      else B.para $ mconcat $ intersperse B.space categoryLinks
  reportLogMessages
  return $ B.doc $ bs <> categories

--
-- block parsers
--

block :: PandocMonad m => MWParser m Blocks
block = do
  res <- mempty <$ skipMany1 blankline
     <|> table
     <|> header
     <|> hrule
     <|> orderedList
     <|> bulletList
     <|> definitionList
     <|> mempty <$ try (spaces *> htmlComment)
     <|> preformatted
     <|> blockTag
     <|> (B.rawBlock "mediawiki" <$> template)
     <|> para
  trace (T.take 60 $ tshow $ B.toList res)
  return res

para :: PandocMonad m => MWParser m Blocks
para = do
  contents <- trimInlines . mconcat <$> many1 inline
  if F.all (==Space) contents
     then return mempty
     else return $ B.para contents

table :: PandocMonad m => MWParser m Blocks
table = do
  tableStart
  styles <- option [] $
               parseAttrs <* skipMany spaceChar <* optional (char '|')
  skipMany spaceChar
  optional $ template >> skipMany spaceChar
  optional blanklines
  let tableWidth = case lookup "width" styles of
                         Just w  -> fromMaybe 1.0 $ parseWidth w
                         Nothing -> 1.0
  caption <- option mempty tableCaption
  optional rowsep
  hasheader <- option False $ True <$ lookAhead (skipSpaces *> char '!')
  (cellspecs',hdr) <- unzip <$> tableRow
  let widths = map ((tableWidth *) . snd) cellspecs'
  let restwidth = tableWidth - sum widths
  let zerocols = length $ filter (==0.0) widths
  let defaultwidth = if zerocols == 0 || zerocols == length widths
                        then ColWidthDefault
                        else ColWidth $ restwidth / fromIntegral zerocols
  let widths' = map (\w -> if w > 0 then ColWidth w else defaultwidth) widths
  let cellspecs = zip (map fst cellspecs') widths'
  rows' <- many $ try $ rowsep *> (map snd <$> tableRow)
  optional blanklines
  tableEnd
  let cols = length hdr
  let (headers,rows) = if hasheader
                          then (hdr, rows')
                          else (replicate cols mempty, hdr:rows')
  let toRow = Row nullAttr . map B.simpleCell
      toHeaderRow l = if null l then [] else [toRow l]
  return $ B.table (B.simpleCaption $ B.plain caption)
                   cellspecs
                   (TableHead nullAttr $ toHeaderRow headers)
                   [TableBody nullAttr 0 [] $ map toRow rows]
                   (TableFoot nullAttr [])

parseAttrs :: PandocMonad m => MWParser m [(Text,Text)]
parseAttrs = many1 parseAttr

parseAttr :: PandocMonad m => MWParser m (Text, Text)
parseAttr = try $ do
  skipMany spaceChar
  k <- many1Char letter
  char '='
  v <- (char '"' >> many1TillChar (satisfy (/='\n')) (char '"'))
       <|> many1Char (satisfy $ \c -> not (isSpace c) && c /= '|')
  return (k,v)

tableStart :: PandocMonad m => MWParser m ()
tableStart = try $ guardColumnOne *> skipSpaces *> sym "{|"

tableEnd :: PandocMonad m => MWParser m ()
tableEnd = try $ guardColumnOne *> skipSpaces *> sym "|}"

rowsep :: PandocMonad m => MWParser m ()
rowsep = try $ guardColumnOne *> skipSpaces *> sym "|-" <*
               many (char '-') <* optional parseAttrs <* blanklines

cellsep :: PandocMonad m => MWParser m ()
cellsep = try $ do
  col <- sourceColumn <$> getPosition
  skipSpaces
  let pipeSep = do
        char '|'
        notFollowedBy (oneOf "-}+")
        if col == 1
           then optional (char '|')
           else void (char '|')
  let exclSep = do
        char '!'
        if col == 1
           then optional (char '!')
           else void (char '!')
  pipeSep <|> exclSep

tableCaption :: PandocMonad m => MWParser m Inlines
tableCaption = try $ do
  guardColumnOne
  skipSpaces
  sym "|+"
  optional (try $ parseAttrs *> skipSpaces *> char '|' *> blanklines)
  (trimInlines . mconcat) <$>
    many (notFollowedBy (cellsep <|> rowsep) *> inline)

tableRow :: PandocMonad m => MWParser m [((Alignment, Double), Blocks)]
tableRow = try $ skipMany htmlComment *> many tableCell

tableCell :: PandocMonad m => MWParser m ((Alignment, Double), Blocks)
tableCell = try $ do
  cellsep
  skipMany spaceChar
  attrs <- option [] $ try $ parseAttrs <* skipSpaces <* char '|' <*
                                 notFollowedBy (char '|')
  skipMany spaceChar
  pos' <- getPosition
  ls <- T.concat <$> many (notFollowedBy (cellsep <|> rowsep <|> tableEnd) *>
                            ((snd <$> withRaw table) <|> countChar 1 anyChar))
  bs <- parseFromString (do setPosition pos'
                            mconcat <$> many block) ls
  let align = case lookup "align" attrs of
                    Just "left"   -> AlignLeft
                    Just "right"  -> AlignRight
                    Just "center" -> AlignCenter
                    _             -> AlignDefault
  let width = case lookup "width" attrs of
                    Just xs -> fromMaybe 0.0 $ parseWidth xs
                    Nothing -> 0.0
  return ((align, width), bs)

parseWidth :: Text -> Maybe Double
parseWidth s =
  case T.unsnoc s of
    Just (ds, '%') | T.all isDigit ds -> safeRead $ "0." <> ds
    _ -> Nothing

template :: PandocMonad m => MWParser m Text
template = try $ do
  string "{{"
  notFollowedBy (char '{')
  lookAhead $ letter <|> digit <|> char ':'
  let chunk = template <|> variable <|> many1Char (noneOf "{}") <|> countChar 1 anyChar
  contents <- manyTill chunk (try $ string "}}")
  return $ "{{" <> T.concat contents <> "}}"

blockTag :: PandocMonad m => MWParser m Blocks
blockTag = do
  (tag, _) <- lookAhead $ htmlTag isBlockTag'
  case tag of
      TagOpen "blockquote" _ -> B.blockQuote <$> blocksInTags "blockquote"
      TagOpen "pre" _ -> B.codeBlock . trimCode <$> textInTags "pre"
      TagOpen "syntaxhighlight" attrs -> syntaxhighlight "syntaxhighlight" attrs
      TagOpen "source" attrs -> syntaxhighlight "source" attrs
      TagOpen "haskell" _ -> B.codeBlockWith ("",["haskell"],[]) . trimCode <$>
                                textInTags "haskell"
      TagOpen "gallery" _ -> blocksInTags "gallery"
      TagOpen "p" _ -> mempty <$ htmlTag (~== tag)
      TagClose "p"  -> mempty <$ htmlTag (~== tag)
      _ -> B.rawBlock "html" . snd <$> htmlTag (~== tag)

trimCode :: Text -> Text
trimCode t = case T.uncons t of
  Just ('\n', xs) -> stripTrailingNewlines xs
  _               -> stripTrailingNewlines t

syntaxhighlight :: PandocMonad m => Text -> [Attribute Text] -> MWParser m Blocks
syntaxhighlight tag attrs = try $ do
  let mblang = lookup "lang" attrs
  let mbstart = lookup "start" attrs
  let mbline = lookup "line" attrs
  let classes = maybeToList mblang ++ maybe [] (const ["numberLines"]) mbline
  let kvs = maybe [] (\x -> [("startFrom",x)]) mbstart
  contents <- textInTags tag
  return $ B.codeBlockWith ("",classes,kvs) $ trimCode contents

hrule :: PandocMonad m => MWParser m Blocks
hrule = B.horizontalRule <$ try (string "----" *> many (char '-') *> newline)

guardColumnOne :: PandocMonad m => MWParser m ()
guardColumnOne = getPosition >>= \pos -> guard (sourceColumn pos == 1)

preformatted :: PandocMonad m => MWParser m Blocks
preformatted = try $ do
  guardColumnOne
  char ' '
  let endline' = B.linebreak <$ try (newline <* char ' ')
  let whitespace' = B.str <$> many1Char ('\160' <$ spaceChar)
  let spToNbsp ' ' = '\160'
      spToNbsp x   = x
  let nowiki' = mconcat . intersperse B.linebreak . map B.str .
                T.lines . fromEntities . T.map spToNbsp <$> try
                  (htmlTag (~== TagOpen ("nowiki" :: Text) []) *>
                   manyTillChar anyChar (htmlTag (~== TagClose ("nowiki" :: Text))))
  let inline' = whitespace' <|> endline' <|> nowiki'
                  <|> try (notFollowedBy newline *> inline)
  contents <- mconcat <$> many1 inline'
  let spacesStr (Str xs) = T.all isSpace xs
      spacesStr _        = False
  if F.all spacesStr contents
     then return mempty
     else return $ B.para $ encode contents

encode :: Inlines -> Inlines
encode = B.fromList . normalizeCode . B.toList . walk strToCode
  where strToCode (Str s) = Code ("",[],[]) s
        strToCode Space   = Code ("",[],[]) " "
        strToCode  x      = x
        normalizeCode []  = []
        normalizeCode (Code a1 x : Code a2 y : zs) | a1 == a2 =
          normalizeCode $ Code a1 (x <> y) : zs
        normalizeCode (x:xs) = x : normalizeCode xs

header :: PandocMonad m => MWParser m Blocks
header = try $ do
  guardColumnOne
  lev <- length <$> many1 (char '=')
  guard $ lev <= 6
  contents <- trimInlines . mconcat <$> manyTill inline (count lev $ char '=')
  opts <- mwOptions <$> getState
  attr <- (if isEnabled Ext_gfm_auto_identifiers opts
              then id
              else modifyIdentifier) <$> registerHeader nullAttr contents
  return $ B.headerWith attr lev contents

-- See #4731:
modifyIdentifier :: Attr -> Attr
modifyIdentifier (ident,cl,kv) = (ident',cl,kv)
  where ident' = T.map (\c -> if c == '-' then '_' else c) ident

bulletList :: PandocMonad m => MWParser m Blocks
bulletList = B.bulletList <$>
   (   many1 (listItem '*')
   <|> (htmlTag (~== TagOpen ("ul" :: Text) []) *> spaces *> many (listItem '*' <|> li) <*
        optional (htmlTag (~== TagClose ("ul" :: Text)))) )

orderedList :: PandocMonad m => MWParser m Blocks
orderedList =
       (B.orderedList <$> many1 (listItem '#'))
   <|> try
       (do (tag,_) <- htmlTag (~== TagOpen ("ol" :: Text) [])
           spaces
           items <- many (listItem '#' <|> li)
           optional (htmlTag (~== TagClose ("ol" :: Text)))
           let start = fromMaybe 1 $ safeRead $ fromAttrib "start" tag
           return $ B.orderedListWith (start, DefaultStyle, DefaultDelim) items)

definitionList :: PandocMonad m => MWParser m Blocks
definitionList = B.definitionList <$> many1 defListItem

defListItem :: PandocMonad m => MWParser m (Inlines, [Blocks])
defListItem = try $ do
  terms <- mconcat . intersperse B.linebreak <$> many defListTerm
  -- we allow dd with no dt, or dt with no dd
  defs  <- if B.isNull terms
              then notFollowedBy
                    (try $ skipMany1 (char ':') >> string "<math>") *>
                       many1 (listItem ':')
              else many (listItem ':')
  return (terms, defs)

defListTerm  :: PandocMonad m => MWParser m Inlines
defListTerm = do
  guardColumnOne
  char ';'
  skipMany spaceChar
  pos' <- getPosition
  anyLine >>= parseFromString (do setPosition pos'
                                  trimInlines . mconcat <$> many inline)

listStart :: PandocMonad m => Char -> MWParser m ()
listStart c = char c *> notFollowedBy listStartChar

listStartChar :: PandocMonad m => MWParser m Char
listStartChar = oneOf "*#;:"

anyListStart :: PandocMonad m => MWParser m Char
anyListStart = guardColumnOne >> oneOf "*#:;"

li :: PandocMonad m => MWParser m Blocks
li = lookAhead (htmlTag (~== TagOpen ("li" :: Text) [])) *>
     (firstParaToPlain <$> blocksInTags "li") <* spaces

listItem :: PandocMonad m => Char -> MWParser m Blocks
listItem c = try $ do
  guardColumnOne
  extras <- many (try $ char c <* lookAhead listStartChar)
  if null extras
     then listItem' c
     else do
       skipMany spaceChar
       pos' <- getPosition
       first <- T.concat <$> manyTill listChunk newline
       rest <- many
                (try $ string extras *> lookAhead listStartChar *>
                       (T.concat <$> manyTill listChunk newline))
       contents <- parseFromString (do setPosition pos'
                                       many1 $ listItem' c)
                          (T.unlines (first : rest))
       case c of
           '*' -> return $ B.bulletList contents
           '#' -> return $ B.orderedList contents
           ':' -> return $ B.definitionList [(mempty, contents)]
           _   -> mzero

-- The point of this is to handle stuff like
-- * {{cite book
-- | blah
-- | blah
-- }}
-- * next list item
-- which seems to be valid mediawiki.
listChunk :: PandocMonad m => MWParser m Text
listChunk = template <|> countChar 1 anyChar

listItem' :: PandocMonad m => Char -> MWParser m Blocks
listItem' c = try $ do
  listStart c
  skipMany spaceChar
  pos' <- getPosition
  first <- T.concat <$> manyTill listChunk newline
  rest <- many (try $ char c *> lookAhead listStartChar *>
                   (T.concat <$> manyTill listChunk newline))
  parseFromString (do setPosition pos'
                      firstParaToPlain . mconcat <$> many1 block)
      $ T.unlines $ first : rest

firstParaToPlain :: Blocks -> Blocks
firstParaToPlain contents =
  case viewl (B.unMany contents) of
       Para xs :< ys -> B.Many $ Plain xs <| ys
       _             -> contents

--
-- inline parsers
--

inline :: PandocMonad m => MWParser m Inlines
inline =  whitespace
      <|> url
      <|> str
      <|> doubleQuotes
      <|> strong
      <|> emph
      <|> image
      <|> internalLink
      <|> externalLink
      <|> math
      <|> inlineTag
      <|> B.singleton <$> charRef
      <|> inlineHtml
      <|> (B.rawInline "mediawiki" <$> variable)
      <|> (B.rawInline "mediawiki" <$> template)
      <|> special

str :: PandocMonad m => MWParser m Inlines
str = B.str <$> many1Char (noneOf $ specialChars ++ spaceChars)

math :: PandocMonad m => MWParser m Inlines
math = (B.displayMath . trim <$> try (many1 (char ':') >> textInTags "math"))
   <|> (B.math . trim <$> textInTags "math")
   <|> (B.displayMath . trim <$> try (dmStart *> manyTillChar anyChar dmEnd))
   <|> (B.math . trim <$> try (mStart *> manyTillChar (satisfy (/='\n')) mEnd))
 where dmStart = string "\\["
       dmEnd   = try (string "\\]")
       mStart  = string "\\("
       mEnd    = try (string "\\)")

variable :: PandocMonad m => MWParser m Text
variable = try $ do
  string "{{{"
  contents <- manyTillChar anyChar (try $ string "}}}")
  return $ "{{{" <> contents <> "}}}"

inlineTag :: PandocMonad m => MWParser m Inlines
inlineTag = do
  (tag, _) <- lookAhead $ htmlTag isInlineTag'
  case tag of
       TagOpen "ref" _ -> B.note . B.plain <$> inlinesInTags "ref"
       TagOpen "nowiki" _ -> try $ do
          (_,raw) <- htmlTag (~== tag)
          if T.any (== '/') raw
             then return mempty
             else B.text . fromEntities <$>
                       manyTillChar anyChar (htmlTag (~== TagClose ("nowiki" :: Text)))
       TagOpen "br" _ -> B.linebreak <$ (htmlTag (~== TagOpen ("br" :: Text) []) -- will get /> too
                            *> optional blankline)
       TagOpen "strike" _ -> B.strikeout <$> inlinesInTags "strike"
       TagOpen "del" _ -> B.strikeout <$> inlinesInTags "del"
       TagOpen "sub" _ -> B.subscript <$> inlinesInTags "sub"
       TagOpen "sup" _ -> B.superscript <$> inlinesInTags "sup"
       TagOpen "code" _ -> encode <$> inlinesInTags "code"
       TagOpen "tt" _ -> do
         inTT <- mwInTT <$> getState
         updateState $ \st -> st{ mwInTT = True }
         result <- encode <$> inlinesInTags "tt"
         updateState $ \st -> st{ mwInTT = inTT }
         return result
       TagOpen "hask" _ -> B.codeWith ("",["haskell"],[]) <$> textInTags "hask"
       _ -> B.rawInline "html" . snd <$> htmlTag (~== tag)

special :: PandocMonad m => MWParser m Inlines
special = B.str <$> countChar 1 (notFollowedBy' (htmlTag isBlockTag') *>
                                  oneOf specialChars)

inlineHtml :: PandocMonad m => MWParser m Inlines
inlineHtml = B.rawInline "html" . snd <$> htmlTag isInlineTag'

whitespace :: PandocMonad m => MWParser m Inlines
whitespace = B.space <$ (skipMany1 spaceChar <|> htmlComment)
         <|> B.softbreak <$ endline

endline :: PandocMonad m => MWParser m ()
endline = () <$ try (newline <*
                     notFollowedBy spaceChar <*
                     notFollowedBy newline <*
                     notFollowedBy' hrule <*
                     notFollowedBy tableStart <*
                     notFollowedBy' header <*
                     notFollowedBy anyListStart)

imageIdentifiers :: PandocMonad m => [MWParser m ()]
imageIdentifiers = [sym (identifier <> ":") | identifier <- identifiers]
    where identifiers = ["File", "Image", "Archivo", "Datei", "Fichier",
                         "Bild"]

image :: PandocMonad m => MWParser m Inlines
image = try $ do
  sym "[["
  choice imageIdentifiers
  fname <- addUnderscores <$> many1Char (noneOf "|]")
  _ <- many imageOption
  dims <- try (char '|' *> sepBy (manyChar digit) (char 'x') <* string "px")
          <|> return []
  _ <- many imageOption
  let kvs = case dims of
              [w]    -> [("width", w)]
              [w, h] -> [("width", w), ("height", h)]
              _      -> []
  let attr = ("", [], kvs)
  caption <-   (B.str fname <$ sym "]]")
           <|> try (char '|' *> (mconcat <$> manyTill inline (sym "]]")))
  return $ B.imageWith attr fname ("fig:" <> stringify caption) caption

imageOption :: PandocMonad m => MWParser m Text
imageOption = try $ char '|' *> opt
  where
    opt = try (oneOfStrings [ "border", "thumbnail", "frameless"
                            , "thumb", "upright", "left", "right"
                            , "center", "none", "baseline", "sub"
                            , "super", "top", "text-top", "middle"
                            , "bottom", "text-bottom" ])
      <|> try (textStr "frame")
      <|> try (oneOfStrings ["link=","alt=","page=","class="] <* many (noneOf "|]"))

addUnderscores :: Text -> Text
addUnderscores = T.intercalate "_" . splitTextBy sep
  where
    sep c = isSpace c || c == '_'

internalLink :: PandocMonad m => MWParser m Inlines
internalLink = try $ do
  sym "[["
  pagename <- T.unwords . T.words <$> manyChar (noneOf "|]")
  label <- option (B.text pagename) $ char '|' *>
             (  (mconcat <$> many1 (notFollowedBy (char ']') *> inline))
             -- the "pipe trick"
             -- [[Help:Contents|] -> "Contents"
             <|> return (B.text $ T.drop 1 $ T.dropWhile (/=':') pagename) )
  sym "]]"
  linktrail <- B.text <$> manyChar letter
  let link = B.link (addUnderscores pagename) "wikilink" (label <> linktrail)
  if "Category:" `T.isPrefixOf` pagename
     then do
       updateState $ \st -> st{ mwCategoryLinks = link : mwCategoryLinks st }
       return mempty
     else return link

externalLink :: PandocMonad m => MWParser m Inlines
externalLink = try $ do
  char '['
  (_, src) <- uri
  lab <- try (trimInlines . mconcat <$>
              (skipMany1 spaceChar *> manyTill inline (char ']')))
       <|> do char ']'
              num <- mwNextLinkNumber <$> getState
              updateState $ \st -> st{ mwNextLinkNumber = num + 1 }
              return $ B.str $ tshow num
  return $ B.link src "" lab

url :: PandocMonad m => MWParser m Inlines
url = do
  (orig, src) <- uri
  return $ B.link src "" (B.str orig)

-- | Parses a list of inlines between start and end delimiters.
inlinesBetween :: (PandocMonad m, Show b) => MWParser m a -> MWParser m b -> MWParser m Inlines
inlinesBetween start end =
  (trimInlines . mconcat) <$> try (start >> many1Till inner end)
    where inner      = innerSpace <|> (notFollowedBy' (() <$ whitespace) >> inline)
          innerSpace = try $ whitespace <* notFollowedBy' end

emph :: PandocMonad m => MWParser m Inlines
emph = B.emph <$> nested (inlinesBetween start end)
    where start = sym "''" >> lookAhead nonspaceChar
          end   = try $ notFollowedBy' (() <$ strong) >> sym "''"

strong :: PandocMonad m => MWParser m Inlines
strong = B.strong <$> nested (inlinesBetween start end)
    where start = sym "'''" >> lookAhead nonspaceChar
          end   = try $ sym "'''"

doubleQuotes :: PandocMonad m => MWParser m Inlines
doubleQuotes = do
  guardEnabled Ext_smart
  inTT <- mwInTT <$> getState
  guard (not inTT)
  B.doubleQuoted <$> nested (inlinesBetween openDoubleQuote closeDoubleQuote)
    where openDoubleQuote = sym "\"" >> lookAhead nonspaceChar
          closeDoubleQuote = try $ sym "\""
