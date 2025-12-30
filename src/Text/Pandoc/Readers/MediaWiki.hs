{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.MediaWiki
   Copyright   : Copyright (C) 2012-2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of mediawiki text to 'Pandoc' document.
-}
{-
TODO:
_ correctly handle tables within tables
_ parse templates(?) and built-in magic words
-}
module Text.Pandoc.Readers.MediaWiki ( readMediaWiki ) where

import Control.Monad
import Control.Monad.Except (throwError)
import Data.Char (isDigit, isLetter, isSpace)
import qualified Data.Foldable as F
import Data.List (intersperse)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Sequence (ViewL (..), viewl, (<|))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Text.HTML.TagSoup
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Builder (Blocks, Inlines, trimInlines)
import Text.Pandoc.Char (isCJK)
import Text.Pandoc.Class.PandocMonad (PandocMonad (..))
import Text.Pandoc.Definition
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (tableCaption)
import Text.Pandoc.Readers.HTML (htmlTag, isCommentTag, toAttr)
import Text.Pandoc.Shared (formatCode, safeRead, splitTextBy, stringify,
                           stripTrailingNewlines, trim, tshow)
import Text.Pandoc.XML (fromEntities)

-- | Read mediawiki from an input string and return a Pandoc document.
readMediaWiki :: (PandocMonad m, ToSources a)
              => ReaderOptions
              -> a
              -> m Pandoc
readMediaWiki opts s = do
  let sources = toSources s
  parsed <- readWithM parseMediaWiki MWState{ mwOptions = opts
                                            , mwMaxNestingLevel = 4
                                            , mwNextLinkNumber  = 1
                                            , mwCategoryLinks = []
                                            , mwIdentifierList = Set.empty
                                            , mwLogMessages = []
                                            , mwInTT = False
                                            , mwAllowNewlines = True
                                            , mwMeta = nullMeta
                                            }
            sources
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
                       , mwAllowNewlines   :: Bool
                       , mwMeta            :: Meta
                       }

type MWParser m = ParsecT Sources MWState m

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

specialChars :: [Char]
specialChars = "'[]<=&*{}|\":\\_"

spaceChars :: [Char]
spaceChars = " \n\t"

sym :: PandocMonad m => Text -> MWParser m ()
sym s = () <$ try (string $ T.unpack s)

newBlockTags :: [Text]
newBlockTags = ["haskell","syntaxhighlight","source","gallery","references"]

isBlockTag' :: Tag Text -> Bool
isBlockTag' (TagOpen t _) = isBlockTagName t
isBlockTag' (TagClose t) = isBlockTagName t
isBlockTag' _ = False

isBlockTagName :: Text -> Bool
isBlockTagName t =
  t `elem` [ "blockquote"
           , "caption"
           , "col"
           , "colgroup"
           , "dd"
           , "div"
           , "dl"
           , "dt"
           , "h1"
           , "h2"
           , "h3"
           , "h4"
           , "h5"
           , "h6"
           , "hr"
           , "li"
           , "meta"
           , "ol"
           , "p"
           , "pre"
           , "rp"
           , "table"
           , "td"
           , "th"
           , "time"
           , "tr"
           , "ul"
           , "center"
           ] || t `elem` newBlockTags

isInlineTag' :: Tag Text -> Bool
isInlineTag' (TagComment _) = True
isInlineTag' (TagOpen t _) = isInlineTagName t
isInlineTag' (TagClose t) = isInlineTagName t
isInlineTag' _ = False

isInlineTagName :: Text -> Bool
isInlineTagName t =
  t `elem` [ "abbr"
           , "b"
           , "bdi"
           , "bdo"
           , "big"
           , "br"
           , "cite"
           , "code"
           , "data"
           , "del"
           , "dfn"
           , "em"
           , "i"
           , "ins"
           , "kbd"
           , "link"
           , "mark"
           , "q"
           , "rt"
           , "ruby"
           , "s"
           , "samp"
           , "small"
           , "span"
           , "strong"
           , "sub"
           , "sup"
           , "u"
           , "var"
           , "wbr"
           , "font"
           , "rb"
           , "rtc"
           , "strike"
           , "tt"
           ]

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
  meta <- mwMeta <$> getState
  let categories = if null categoryLinks
                      then mempty
                      else B.para $ mconcat $ intersperse B.space categoryLinks
  reportLogMessages
  return $ Pandoc meta (B.toList bs <> B.toList categories)

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
     else case B.toList contents of
         -- For the MediaWiki format all images are considered figures
         [Image attr figureCaption (src, title)] ->
             return $ B.simpleFigureWith
                 attr (B.fromList figureCaption) src title
         _ -> return $ B.para contents

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
  optional newline
  optional rowsep
  hasheader <- option False $ True <$ lookAhead (skipSpaces *> char '!')
  (cellwidths,hdr) <- unzip <$> tableRow
  let widths = map (tableWidth *) (concat cellwidths)
  let restwidth = tableWidth - sum widths
  let zerocols = length $ filter (==0.0) widths
  let defaultwidth = if zerocols == 0 || zerocols == length widths
                        then ColWidthDefault
                        else ColWidth $ restwidth / fromIntegral zerocols
  let widths' = map (\w -> if w > 0 then ColWidth w else defaultwidth) widths
  let cellspecs = zip (calculateAlignments hdr) widths'
  rows' <- many $ try $ rowsep *> (map snd <$> tableRow)
  optional blanklines
  tableEnd
  let (headers,rows) = if hasheader
                          then (hdr, rows')
                          else ([], hdr:rows')
  let toRow = Row nullAttr
      toHeaderRow l = [toRow l | not (null l)]
  return $ B.table (B.simpleCaption $ B.plain caption)
                   cellspecs
                   (TableHead nullAttr $ toHeaderRow headers)
                   [TableBody nullAttr 0 [] $ map toRow rows]
                   (TableFoot nullAttr [])

calculateAlignments :: [Cell] -> [Alignment]
calculateAlignments = concatMap cellAligns
  where
    cellAligns :: Cell -> [Alignment]
    cellAligns (Cell _ align _ (ColSpan colspan) _) = replicate colspan align

parseAttrs :: PandocMonad m => MWParser m [(Text,Text)]
parseAttrs = many1 parseAttr

parseAttr :: PandocMonad m => MWParser m (Text, Text)
parseAttr = try $ do
  skipMany spaceChar
  kFirst <- letter
  kRest <- many (alphaNum <|> oneOf "_-:.")
  let k = T.pack (kFirst : kRest)
  skipMany spaceChar
  char '='
  skipMany spaceChar
  v <- (char '"' >> manyTillChar (satisfy (/='\n')) (char '"'))
       <|> many1Char (satisfy $ \c -> not (isSpace c) && c /= '|')
  return (k,v)

tableStart :: PandocMonad m => MWParser m ()
tableStart = try $ guardColumnOne *> skipSpaces *> sym "{|"

tableEnd :: PandocMonad m => MWParser m ()
tableEnd = try $ guardColumnOne *> skipSpaces *> sym "|}"

rowsep :: PandocMonad m => MWParser m ()
rowsep = try $ guardColumnOne *> skipSpaces *> sym "|-" <*
               many (char '-') <* optional parseAttrs
                               <* skipSpaces
                               <* skipMany htmlComment
                               <* blanklines

cellsep :: PandocMonad m => MWParser m [(Text,Text)]
cellsep = try $ do
  col <- sourceColumn <$> getPosition
  skipMany spaceChar
  c <- oneOf "|!"
  when (col > 1) $ void $ char c
  notFollowedBy (oneOf "-}")
  attribs <- option [] (parseAttrs <* skipMany spaceChar <* char '|')
  skipMany spaceChar
  pure attribs

tableCaption :: PandocMonad m => MWParser m Inlines
tableCaption = try $ do
  optional rowsep
  guardColumnOne
  skipSpaces
  sym "|+"
  optional (try $ parseAttrs *> skipSpaces *> char '|' *> blanklines)
  trimInlines . mconcat <$>
    many (notFollowedBy (void cellsep <|> rowsep) *> inline)

tableRow :: PandocMonad m => MWParser m [([Double], Cell)]
tableRow = try $ skipMany htmlComment *> many tableCell

-- multiple widths because cell might have colspan
tableCell :: PandocMonad m => MWParser m ([Double], Cell)
tableCell = try $ do
  attribs <- cellsep
  pos' <- getPosition
  ls <- T.concat <$> many (notFollowedBy (void cellsep <|> rowsep <|> tableEnd) *>
                            ((snd <$> withRaw table) <|> countChar 1 anyChar))
  bs <- parseFromString (do setPosition pos'
                            mconcat <$> many block) ls
  let align = case lookup "align" attribs of
                    Just "left"   -> AlignLeft
                    Just "right"  -> AlignRight
                    Just "center" -> AlignCenter
                    _             -> AlignDefault
  let rowspan = RowSpan . fromMaybe 1 $
                safeRead =<< lookup "rowspan" attribs
  let colspan = ColSpan . fromMaybe 1 $
                safeRead =<< lookup "colspan" attribs
  let ColSpan rawcolspan = colspan
  let handledAttribs = ["align", "colspan", "rowspan"]
      attribs' = [ (k, v) | (k, v) <- attribs
                          , k `notElem` handledAttribs
                 ]
  let widths = case lookup "width" attribs of
                    Just xs -> maybe (replicate rawcolspan 0.0)
                                 (\w -> replicate rawcolspan
                                    (w / fromIntegral rawcolspan))
                                 (parseWidth xs)
                    Nothing -> replicate rawcolspan 0.0
  return (widths, B.cellWith (toAttr attribs') align rowspan colspan bs)

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
encode = formatCode nullAttr

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
  defs  <- if null terms
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
  trimInlines . mconcat <$> many (notFollowedBy (oneOf ":\r\n") *> inline) <*
    optional newline

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
  guardColumnOne <|> guard (c == ':') -- def can start on same line as term
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
-- Also multiline math: see #9293.
listChunk :: PandocMonad m => MWParser m Text
listChunk = template <|> (snd <$> withRaw math) <|> countChar 1 anyChar

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
      <|> behaviorSwitch
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

singleParaToPlain :: Blocks -> Blocks
singleParaToPlain bs =
  case B.toList bs of
    [Para ils] -> B.fromList [Plain ils]
    _ -> bs

inlineTag :: PandocMonad m => MWParser m Inlines
inlineTag = do
  (tag, _) <- lookAhead $ htmlTag (\tag -> case tag of
                                      TagOpen "hask" _ -> True
                                      TagOpen "ref" _ -> True
                                      TagOpen "nowiki" _ -> True
                                      _ -> isInlineTag' tag)
  case tag of
       TagOpen "ref" _ -> B.note . singleParaToPlain <$> blocksInTags "ref"
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
       TagOpen "var" _ -> B.codeWith ("",["variable"],[]) <$> textInTags "var"
       TagOpen "samp" _ -> B.codeWith ("",["sample"],[]) <$> textInTags "samp"
       TagOpen "kbd" _ -> B.spanWith ("",["kbd"],[]) <$> inlinesInTags "kbd"
       TagOpen "mark" _ -> B.spanWith ("",["mark"],[]) <$> inlinesInTags "mark"
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
special = B.str . T.singleton <$>
  (notFollowedBy' (htmlTag (\t -> isInlineTag' t ||
                                  isBlockTag' t ||
                                  case t of
                                    TagClose "ref" -> True
                                    TagClose "hask" -> True
                                    TagClose "nowiki" -> True
                                    _ -> False)
                                  ) *> oneOf specialChars)

inlineHtml :: PandocMonad m => MWParser m Inlines
inlineHtml = B.rawInline "html" . snd <$> htmlTag isInlineTag'

whitespace :: PandocMonad m => MWParser m Inlines
whitespace = B.space <$ (skipMany1 spaceChar <|> htmlComment)
         <|> B.softbreak <$ endline

endline :: PandocMonad m => MWParser m ()
endline = do
 getState >>= guard . mwAllowNewlines
 () <$ try (newline <* notFollowedBy spaceChar <*
                       notFollowedBy newline <*
                       notFollowedBy' hrule <*
                       notFollowedBy tableStart <*
                       notFollowedBy' header <*
                       notFollowedBy anyListStart)

imageIdentifier :: PandocMonad m => MWParser m ()
imageIdentifier = try $ do
  ident <- T.pack <$> many1Till letter (char ':')
  guard $ T.toLower ident `elem`
            ["file", "image", "archivo", "datei", "fichier", "bild"]

image :: PandocMonad m => MWParser m Inlines
image = try $ do
  sym "[["
  imageIdentifier
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
  return $ B.imageWith attr fname (stringify caption) caption

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
addUnderscores = T.intercalate "_" . splitTextBy sep . T.strip
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
  -- see #8525:
  linktrail <- B.text <$> manyChar (satisfy (\c -> isLetter c && not (isCJK c)))
  let link = B.linkWith (mempty, ["wikilink"], mempty) (addUnderscores pagename) (stringify label) (label <> linktrail)
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
  trimInlines . mconcat <$> try (start >> many1Till inline end)

emph :: PandocMonad m => MWParser m Inlines
emph = B.emph <$> inlinesBetween start end
    where start = sym "''"
          end   = try $ notFollowedBy' (() <$ strong) >> sym "''"

strong :: PandocMonad m => MWParser m Inlines
strong = B.strong <$> inlinesBetween start end
    where start = sym "'''"
          end   = sym "'''"

doubleQuotes :: PandocMonad m => MWParser m Inlines
doubleQuotes = do
  guardEnabled Ext_smart
  inTT <- mwInTT <$> getState
  guard (not inTT)
  B.doubleQuoted <$> inlinesBetween openDoubleQuote closeDoubleQuote
    where openDoubleQuote = sym "\"" >> lookAhead nonspaceChar
          closeDoubleQuote = try $ sym "\""

behaviorSwitch :: PandocMonad m => MWParser m Inlines
behaviorSwitch = try $ do
  let reservedMagicWords = [ "NOTOC"
              , "FORCETOC"
              , "TOC"
              , "NOEDITSECTION"
              , "NEWSECTIONLINK"
              , "NONEWSECTIONLINK"
              , "NOGALLERY"
              , "HIDDENCAT"
              , "EXPECTUNUSEDCATEGORY"
              , "NOCONTENTCONVERT"
              , "NOCC"
              , "NOTITLECONVERT"
              , "NOTC"
              , "INDEX"
              , "NOINDEX"
              , "STATICREDIRECT"
              , "EXPECTUNUSEDTEMPLATE"
              -- From popular extensions
              , "NOGLOBAL"
              , "DISAMBIG"
              , "ARCHIVEDTALK"
              , "NOTALK"
              ]
  string "__"
  name <- many1 alphaNum
  string "__"
  case name `elem` reservedMagicWords of
    True -> do
      updateState $ \st -> st{ mwMeta = B.setMeta (T.toLower $ T.pack name) True (mwMeta st) }
      return mempty
    False -> return $ B.str $ "__" <> T.pack name <> "__"
