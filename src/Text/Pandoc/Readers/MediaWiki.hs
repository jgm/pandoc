{-# LANGUAGE RelaxedPolyRec, FlexibleInstances, TypeSynonymInstances #-}
-- RelaxedPolyRec needed for inlinesBetween on GHC < 7
{-
  Copyright (C) 2012-2015 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Readers.MediaWiki
   Copyright   : Copyright (C) 2012-2015 John MacFarlane
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

import Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Builder (Inlines, Blocks, trimInlines)
import Data.Monoid ((<>))
import Text.Pandoc.Options
import Text.Pandoc.Readers.HTML ( htmlTag, isBlockTag, isCommentTag )
import Text.Pandoc.XML ( fromEntities )
import Text.Pandoc.Parsing hiding ( nested )
import Text.Pandoc.Walk ( walk )
import Text.Pandoc.Shared ( stripTrailingNewlines, safeRead, stringify, trim )
import Control.Monad
import Data.List (intersperse, intercalate, isPrefixOf )
import Text.HTML.TagSoup
import Data.Sequence (viewl, ViewL(..), (<|))
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Char (isDigit, isSpace)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import Debug.Trace (trace)

import Text.Pandoc.Error

-- | Read mediawiki from an input string and return a Pandoc document.
readMediaWiki :: ReaderOptions -- ^ Reader options
              -> String        -- ^ String to parse (assuming @'\n'@ line endings)
              -> Either PandocError Pandoc
readMediaWiki opts s =
  readWith parseMediaWiki MWState{ mwOptions = opts
                                       , mwMaxNestingLevel = 4
                                       , mwNextLinkNumber  = 1
                                       , mwCategoryLinks = []
                                       , mwHeaderMap = M.empty
                                       , mwIdentifierList = Set.empty
                                       }
           (s ++ "\n")

data MWState = MWState { mwOptions         :: ReaderOptions
                       , mwMaxNestingLevel :: Int
                       , mwNextLinkNumber  :: Int
                       , mwCategoryLinks   :: [Inlines]
                       , mwHeaderMap       :: M.Map Inlines String
                       , mwIdentifierList  :: Set.Set String
                       }

type MWParser = Parser [Char] MWState

instance HasReaderOptions MWState where
  extractReaderOptions = mwOptions

instance HasHeaderMap MWState where
  extractHeaderMap     = mwHeaderMap
  updateHeaderMap f st = st{ mwHeaderMap = f $ mwHeaderMap st }

instance HasIdentifierList MWState where
  extractIdentifierList     = mwIdentifierList
  updateIdentifierList f st = st{ mwIdentifierList = f $ mwIdentifierList st }

--
-- auxiliary functions
--

-- This is used to prevent exponential blowups for things like:
-- ''a'''a''a'''a''a'''a''a'''a
nested :: MWParser a -> MWParser a
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

sym :: String -> MWParser ()
sym s = () <$ try (string s)

newBlockTags :: [String]
newBlockTags = ["haskell","syntaxhighlight","source","gallery","references"]

isBlockTag' :: Tag String -> Bool
isBlockTag' tag@(TagOpen t _) = (isBlockTag tag || t `elem` newBlockTags) &&
  t `notElem` eitherBlockOrInline
isBlockTag' tag@(TagClose t) = (isBlockTag tag || t `elem` newBlockTags) &&
  t `notElem` eitherBlockOrInline
isBlockTag' tag = isBlockTag tag

isInlineTag' :: Tag String -> Bool
isInlineTag' (TagComment _) = True
isInlineTag' t = not (isBlockTag' t)

eitherBlockOrInline :: [String]
eitherBlockOrInline = ["applet", "button", "del", "iframe", "ins",
                               "map", "area", "object"]

htmlComment :: MWParser ()
htmlComment = () <$ htmlTag isCommentTag

inlinesInTags :: String -> MWParser Inlines
inlinesInTags tag = try $ do
  (_,raw) <- htmlTag (~== TagOpen tag [])
  if '/' `elem` raw   -- self-closing tag
     then return mempty
     else trimInlines . mconcat <$>
            manyTill inline (htmlTag (~== TagClose tag))

blocksInTags :: String -> MWParser Blocks
blocksInTags tag = try $ do
  (_,raw) <- htmlTag (~== TagOpen tag [])
  let closer = if tag == "li"
                  then htmlTag (~== TagClose "li")
                     <|> lookAhead (
                              htmlTag (~== TagOpen "li" [])
                          <|> htmlTag (~== TagClose "ol")
                          <|> htmlTag (~== TagClose "ul"))
                  else htmlTag (~== TagClose tag)
  if '/' `elem` raw   -- self-closing tag
     then return mempty
     else mconcat <$> manyTill block closer

charsInTags :: String -> MWParser [Char]
charsInTags tag = try $ do
  (_,raw) <- htmlTag (~== TagOpen tag [])
  if '/' `elem` raw   -- self-closing tag
     then return ""
     else manyTill anyChar (htmlTag (~== TagClose tag))

--
-- main parser
--

parseMediaWiki :: MWParser Pandoc
parseMediaWiki = do
  bs <- mconcat <$> many block
  spaces
  eof
  categoryLinks <- reverse . mwCategoryLinks <$> getState
  let categories = if null categoryLinks
                      then mempty
                      else B.para $ mconcat $ intersperse B.space categoryLinks
  return $ B.doc $ bs <> categories

--
-- block parsers
--

block :: MWParser Blocks
block = do
  tr <- getOption readerTrace
  pos <- getPosition
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
  when tr $
    trace (printf "line %d: %s" (sourceLine pos)
           (take 60 $ show $ B.toList res)) (return ())
  return res

para :: MWParser Blocks
para = do
  contents <- trimInlines . mconcat <$> many1 inline
  if F.all (==Space) contents
     then return mempty
     else return $ B.para contents

table :: MWParser Blocks
table = do
  tableStart
  styles <- option [] parseAttrs <* blankline
  let tableWidth = case lookup "width" styles of
                         Just w  -> fromMaybe 1.0 $ parseWidth w
                         Nothing -> 1.0
  caption <- option mempty tableCaption
  optional rowsep
  hasheader <- option False $ True <$ (lookAhead (skipSpaces *> char '!'))
  (cellspecs',hdr) <- unzip <$> tableRow
  let widths = map ((tableWidth *) . snd) cellspecs'
  let restwidth = tableWidth - sum widths
  let zerocols = length $ filter (==0.0) widths
  let defaultwidth = if zerocols == 0 || zerocols == length widths
                        then 0.0
                        else restwidth / fromIntegral zerocols
  let widths' = map (\w -> if w == 0 then defaultwidth else w) widths
  let cellspecs = zip (map fst cellspecs') widths'
  rows' <- many $ try $ rowsep *> (map snd <$> tableRow)
  optional blanklines
  tableEnd
  let cols = length hdr
  let (headers,rows) = if hasheader
                          then (hdr, rows')
                          else (replicate cols mempty, hdr:rows')
  return $ B.table caption cellspecs headers rows

parseAttrs :: MWParser [(String,String)]
parseAttrs = many1 parseAttr

parseAttr :: MWParser (String, String)
parseAttr = try $ do
  skipMany spaceChar
  k <- many1 letter
  char '='
  v <- (char '"' >> many1Till (satisfy (/='\n')) (char '"'))
       <|> many1 (satisfy $ \c -> not (isSpace c) && c /= '|')
  return (k,v)

tableStart :: MWParser ()
tableStart = try $ guardColumnOne *> skipSpaces *> sym "{|"

tableEnd :: MWParser ()
tableEnd = try $ guardColumnOne *> skipSpaces *> sym "|}"

rowsep :: MWParser ()
rowsep = try $ guardColumnOne *> skipSpaces *> sym "|-" <*
               optional parseAttr <* blanklines

cellsep :: MWParser ()
cellsep = try $
             (guardColumnOne *> skipSpaces <*
                 (  (char '|' <* notFollowedBy (oneOf "-}+"))
                <|> (char '!')
                 )
             )
            <|> (() <$ try (string "||"))
            <|> (() <$ try (string "!!"))

tableCaption :: MWParser Inlines
tableCaption = try $ do
  guardColumnOne
  skipSpaces
  sym "|+"
  optional (try $ parseAttr *> skipSpaces *> char '|' *> skipSpaces)
  (trimInlines . mconcat) <$> many (notFollowedBy (cellsep <|> rowsep) *> inline)

tableRow :: MWParser [((Alignment, Double), Blocks)]
tableRow = try $ skipMany htmlComment *> many tableCell

tableCell :: MWParser ((Alignment, Double), Blocks)
tableCell = try $ do
  cellsep
  skipMany spaceChar
  attrs <- option [] $ try $ parseAttrs <* skipSpaces <* char '|' <*
                                 notFollowedBy (char '|')
  skipMany spaceChar
  ls <- concat <$> many (notFollowedBy (cellsep <|> rowsep <|> tableEnd) *>
                     ((snd <$> withRaw table) <|> count 1 anyChar))
  bs <- parseFromString (mconcat <$> many block) ls
  let align = case lookup "align" attrs of
                    Just "left"   -> AlignLeft
                    Just "right"  -> AlignRight
                    Just "center" -> AlignCenter
                    _             -> AlignDefault
  let width = case lookup "width" attrs of
                    Just xs -> fromMaybe 0.0 $ parseWidth xs
                    Nothing -> 0.0
  return ((align, width), bs)

parseWidth :: String -> Maybe Double
parseWidth s =
  case reverse s of
       ('%':ds) | all isDigit ds -> safeRead ('0':'.':reverse ds)
       _ -> Nothing

template :: MWParser String
template = try $ do
  string "{{"
  notFollowedBy (char '{')
  lookAhead $ letter <|> digit <|> char ':'
  let chunk = template <|> variable <|> many1 (noneOf "{}") <|> count 1 anyChar
  contents <- manyTill chunk (try $ string "}}")
  return $ "{{" ++ concat contents ++ "}}"

blockTag :: MWParser Blocks
blockTag = do
  (tag, _) <- lookAhead $ htmlTag isBlockTag'
  case tag of
      TagOpen "blockquote" _ -> B.blockQuote <$> blocksInTags "blockquote"
      TagOpen "pre" _ -> B.codeBlock . trimCode <$> charsInTags "pre"
      TagOpen "syntaxhighlight" attrs -> syntaxhighlight "syntaxhighlight" attrs
      TagOpen "source" attrs -> syntaxhighlight "source" attrs
      TagOpen "haskell" _ -> B.codeBlockWith ("",["haskell"],[]) . trimCode <$>
                                charsInTags "haskell"
      TagOpen "gallery" _ -> blocksInTags "gallery"
      TagOpen "p" _ -> mempty <$ htmlTag (~== tag)
      TagClose "p"  -> mempty <$ htmlTag (~== tag)
      _ -> B.rawBlock "html" . snd <$> htmlTag (~== tag)

trimCode :: String -> String
trimCode ('\n':xs) = stripTrailingNewlines xs
trimCode xs        = stripTrailingNewlines xs

syntaxhighlight :: String -> [Attribute String] -> MWParser Blocks
syntaxhighlight tag attrs = try $ do
  let mblang = lookup "lang" attrs
  let mbstart = lookup "start" attrs
  let mbline = lookup "line" attrs
  let classes = maybe [] (:[]) mblang ++ maybe [] (const ["numberLines"]) mbline
  let kvs = maybe [] (\x -> [("startFrom",x)]) mbstart
  contents <- charsInTags tag
  return $ B.codeBlockWith ("",classes,kvs) $ trimCode contents

hrule :: MWParser Blocks
hrule = B.horizontalRule <$ try (string "----" *> many (char '-') *> newline)

guardColumnOne :: MWParser ()
guardColumnOne = getPosition >>= \pos -> guard (sourceColumn pos == 1)

preformatted :: MWParser Blocks
preformatted = try $ do
  guardColumnOne
  char ' '
  let endline' = B.linebreak <$ (try $ newline <* char ' ')
  let whitespace' = B.str <$> many1 ('\160' <$ spaceChar)
  let spToNbsp ' ' = '\160'
      spToNbsp x   = x
  let nowiki' = mconcat . intersperse B.linebreak . map B.str .
                lines . fromEntities . map spToNbsp <$> try
                  (htmlTag (~== TagOpen "nowiki" []) *>
                   manyTill anyChar (htmlTag (~== TagClose "nowiki")))
  let inline' = whitespace' <|> endline' <|> nowiki'
                  <|> (try $ notFollowedBy newline *> inline)
  contents <- mconcat <$> many1 inline'
  let spacesStr (Str xs) = all isSpace xs
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
          normalizeCode $ (Code a1 (x ++ y)) : zs
        normalizeCode (x:xs) = x : normalizeCode xs

header :: MWParser Blocks
header = try $ do
  guardColumnOne
  eqs <- many1 (char '=')
  let lev = length eqs
  guard $ lev <= 6
  contents <- trimInlines . mconcat <$> manyTill inline (count lev $ char '=')
  attr <- registerHeader nullAttr contents
  return $ B.headerWith attr lev contents

bulletList :: MWParser Blocks
bulletList = B.bulletList <$>
   (   many1 (listItem '*')
   <|> (htmlTag (~== TagOpen "ul" []) *> spaces *> many (listItem '*' <|> li) <*
        optional (htmlTag (~== TagClose "ul"))) )

orderedList :: MWParser Blocks
orderedList =
       (B.orderedList <$> many1 (listItem '#'))
   <|> try
       (do (tag,_) <- htmlTag (~== TagOpen "ol" [])
           spaces
           items <- many (listItem '#' <|> li)
           optional (htmlTag (~== TagClose "ol"))
           let start = fromMaybe 1 $ safeRead $ fromAttrib "start" tag
           return $ B.orderedListWith (start, DefaultStyle, DefaultDelim) items)

definitionList :: MWParser Blocks
definitionList = B.definitionList <$> many1 defListItem

defListItem :: MWParser (Inlines, [Blocks])
defListItem = try $ do
  terms <- mconcat . intersperse B.linebreak <$> many defListTerm
  -- we allow dd with no dt, or dt with no dd
  defs  <- if B.isNull terms
              then notFollowedBy (try $ string ":<math>") *>
                       many1 (listItem ':')
              else many (listItem ':')
  return (terms, defs)

defListTerm  :: MWParser Inlines
defListTerm = char ';' >> skipMany spaceChar >> anyLine >>=
  parseFromString (trimInlines . mconcat <$> many inline)

listStart :: Char -> MWParser ()
listStart c = char c *> notFollowedBy listStartChar

listStartChar :: MWParser Char
listStartChar = oneOf "*#;:"

anyListStart :: MWParser Char
anyListStart =  char '*'
            <|> char '#'
            <|> char ':'
            <|> char ';'

li :: MWParser Blocks
li = lookAhead (htmlTag (~== TagOpen "li" [])) *>
     (firstParaToPlain <$> blocksInTags "li") <* spaces

listItem :: Char -> MWParser Blocks
listItem c = try $ do
  extras <- many (try $ char c <* lookAhead listStartChar)
  if null extras
     then listItem' c
     else do
       skipMany spaceChar
       first <- concat <$> manyTill listChunk newline
       rest <- many
                (try $ string extras *> lookAhead listStartChar *>
                       (concat <$> manyTill listChunk newline))
       contents <- parseFromString (many1 $ listItem' c)
                          (unlines (first : rest))
       case c of
           '*'  -> return $ B.bulletList contents
           '#'  -> return $ B.orderedList contents
           ':'  -> return $ B.definitionList [(mempty, contents)]
           _    -> mzero

-- The point of this is to handle stuff like
-- * {{cite book
-- | blah
-- | blah
-- }}
-- * next list item
-- which seems to be valid mediawiki.
listChunk :: MWParser String
listChunk = template <|> count 1 anyChar

listItem' :: Char -> MWParser Blocks
listItem' c = try $ do
  listStart c
  skipMany spaceChar
  first <- concat <$> manyTill listChunk newline
  rest <- many (try $ char c *> lookAhead listStartChar *>
                   (concat <$> manyTill listChunk newline))
  parseFromString (firstParaToPlain . mconcat <$> many1 block)
      $ unlines $ first : rest

firstParaToPlain :: Blocks -> Blocks
firstParaToPlain contents =
  case viewl (B.unMany contents) of
       (Para xs) :< ys -> B.Many $ (Plain xs) <| ys
       _               -> contents

--
-- inline parsers
--

inline :: MWParser Inlines
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

str :: MWParser Inlines
str = B.str <$> many1 (noneOf $ specialChars ++ spaceChars)

math :: MWParser Inlines
math = (B.displayMath . trim <$> try (char ':' >> charsInTags "math"))
   <|> (B.math . trim <$> charsInTags "math")
   <|> (B.displayMath . trim <$> try (dmStart *> manyTill anyChar dmEnd))
   <|> (B.math . trim <$> try (mStart *> manyTill (satisfy (/='\n')) mEnd))
 where dmStart = string "\\["
       dmEnd   = try (string "\\]")
       mStart  = string "\\("
       mEnd    = try (string "\\)")

variable :: MWParser String
variable = try $ do
  string "{{{"
  contents <- manyTill anyChar (try $ string "}}}")
  return $ "{{{" ++ contents ++ "}}}"

inlineTag :: MWParser Inlines
inlineTag = do
  (tag, _) <- lookAhead $ htmlTag isInlineTag'
  case tag of
       TagOpen "ref" _ -> B.note . B.plain <$> inlinesInTags "ref"
       TagOpen "nowiki" _ -> try $ do
          (_,raw) <- htmlTag (~== tag)
          if '/' `elem` raw
             then return mempty
             else B.text . fromEntities <$>
                       manyTill anyChar (htmlTag (~== TagClose "nowiki"))
       TagOpen "br" _ -> B.linebreak <$ (htmlTag (~== TagOpen "br" []) -- will get /> too
                            *> optional blankline)
       TagOpen "strike" _ -> B.strikeout <$> inlinesInTags "strike"
       TagOpen "del" _ -> B.strikeout <$> inlinesInTags "del"
       TagOpen "sub" _ -> B.subscript <$> inlinesInTags "sub"
       TagOpen "sup" _ -> B.superscript <$> inlinesInTags "sup"
       TagOpen "code" _ -> encode <$> inlinesInTags "code"
       TagOpen "tt" _ -> encode <$> inlinesInTags "tt"
       TagOpen "hask" _ -> B.codeWith ("",["haskell"],[]) <$> charsInTags "hask"
       _ -> B.rawInline "html" . snd <$> htmlTag (~== tag)

special :: MWParser Inlines
special = B.str <$> count 1 (notFollowedBy' (htmlTag isBlockTag') *>
                             oneOf specialChars)

inlineHtml :: MWParser Inlines
inlineHtml = B.rawInline "html" . snd <$> htmlTag isInlineTag'

whitespace :: MWParser Inlines
whitespace = B.space <$ (skipMany1 spaceChar <|> htmlComment)
         <|> B.softbreak <$ endline

endline :: MWParser ()
endline = () <$ try (newline <*
                     notFollowedBy spaceChar <*
                     notFollowedBy newline <*
                     notFollowedBy' hrule <*
                     notFollowedBy tableStart <*
                     notFollowedBy' header <*
                     notFollowedBy anyListStart)

imageIdentifiers :: [MWParser ()]
imageIdentifiers = [sym (identifier ++ ":") | identifier <- identifiers]
    where identifiers = ["File", "Image", "Archivo", "Datei", "Fichier",
                         "Bild"]

image :: MWParser Inlines
image = try $ do
  sym "[["
  choice imageIdentifiers
  fname <- many1 (noneOf "|]")
  _ <- many imageOption
  dims <- try (char '|' *> (sepBy (many digit) (char 'x')) <* string "px")
          <|> return []
  _ <- many imageOption
  let kvs = case dims of
              w:[]     -> [("width", w)]
              w:(h:[]) -> [("width", w), ("height", h)]
              _        -> []
  let attr = ("", [], kvs)
  caption <-   (B.str fname <$ sym "]]")
           <|> try (char '|' *> (mconcat <$> manyTill inline (sym "]]")))
  return $ B.imageWith attr fname ("fig:" ++ stringify caption) caption

imageOption :: MWParser String
imageOption = try $ char '|' *> opt
  where
    opt = try (oneOfStrings [ "border", "thumbnail", "frameless"
                            , "thumb", "upright", "left", "right"
                            , "center", "none", "baseline", "sub"
                            , "super", "top", "text-top", "middle"
                            , "bottom", "text-bottom" ])
      <|> try (string "frame")
      <|> try (oneOfStrings ["link=","alt=","page=","class="] <* many (noneOf "|]"))

collapseUnderscores :: String -> String
collapseUnderscores [] = []
collapseUnderscores ('_':'_':xs) = collapseUnderscores ('_':xs)
collapseUnderscores (x:xs) = x : collapseUnderscores xs

addUnderscores :: String -> String
addUnderscores = collapseUnderscores . intercalate "_" . words

internalLink :: MWParser Inlines
internalLink = try $ do
  sym "[["
  pagename <- unwords . words <$> many (noneOf "|]")
  label <- option (B.text pagename) $ char '|' *>
             (  (mconcat <$> many1 (notFollowedBy (char ']') *> inline))
             -- the "pipe trick"
             -- [[Help:Contents|] -> "Contents"
             <|> (return $ B.text $ drop 1 $ dropWhile (/=':') pagename) )
  sym "]]"
  linktrail <- B.text <$> many letter
  let link = B.link (addUnderscores pagename) "wikilink" (label <> linktrail)
  if "Category:" `isPrefixOf` pagename
     then do
       updateState $ \st -> st{ mwCategoryLinks = link : mwCategoryLinks st }
       return mempty
     else return link

externalLink :: MWParser Inlines
externalLink = try $ do
  char '['
  (_, src) <- uri
  lab <- try (trimInlines . mconcat <$>
              (skipMany1 spaceChar *> manyTill inline (char ']')))
       <|> do char ']'
              num <- mwNextLinkNumber <$> getState
              updateState $ \st -> st{ mwNextLinkNumber = num + 1 }
              return $ B.str $ show num
  return $ B.link src "" lab

url :: MWParser Inlines
url = do
  (orig, src) <- uri
  return $ B.link src "" (B.str orig)

-- | Parses a list of inlines between start and end delimiters.
inlinesBetween :: (Show b) => MWParser a -> MWParser b -> MWParser Inlines
inlinesBetween start end =
  (trimInlines . mconcat) <$> try (start >> many1Till inner end)
    where inner      = innerSpace <|> (notFollowedBy' (() <$ whitespace) >> inline)
          innerSpace = try $ whitespace <* notFollowedBy' end

emph :: MWParser Inlines
emph = B.emph <$> nested (inlinesBetween start end)
    where start = sym "''" >> lookAhead nonspaceChar
          end   = try $ notFollowedBy' (() <$ strong) >> sym "''"

strong :: MWParser Inlines
strong = B.strong <$> nested (inlinesBetween start end)
    where start = sym "'''" >> lookAhead nonspaceChar
          end   = try $ sym "'''"

doubleQuotes :: MWParser Inlines
doubleQuotes = B.doubleQuoted . trimInlines . mconcat <$> try
 ((getState >>= guard . readerSmart . mwOptions) *>
   openDoubleQuote *> manyTill inline closeDoubleQuote )
    where openDoubleQuote = char '"' <* lookAhead alphaNum
          closeDoubleQuote = char '"' <* notFollowedBy alphaNum

