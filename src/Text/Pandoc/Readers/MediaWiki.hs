{-# LANGUAGE RelaxedPolyRec #-} -- needed for inlinesBetween on GHC < 7
{-
  Copyright (C) 2012 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2012 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of mediawiki text to 'Pandoc' document.
-}
{-
TODO:
_ support tables http://www.mediawiki.org/wiki/Help:Tables
- footnotes?
-}
module Text.Pandoc.Readers.MediaWiki ( readMediaWiki ) where

import Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Builder (Inlines, Blocks, trimInlines, (<>))
import Text.Pandoc.Options
import Text.Pandoc.Readers.HTML ( htmlTag, isInlineTag,
                                  isBlockTag, isCommentTag )
import Text.Pandoc.XML ( fromEntities )
import Text.Pandoc.Parsing hiding ( nested )
import Text.Pandoc.Generic ( bottomUp )
import Text.Pandoc.Shared ( stripTrailingNewlines, safeRead )
import Data.Monoid (mconcat, mempty)
import Control.Applicative ((<$>), (<*), (*>), (<$))
import Control.Monad
import Data.List (intersperse, intercalate )
import Text.HTML.TagSoup
import Data.Sequence (viewl, ViewL(..), (<|))

-- | Read mediawiki from an input string and return a Pandoc document.
readMediaWiki :: ReaderOptions -- ^ Reader options
              -> String        -- ^ String to parse (assuming @'\n'@ line endings)
              -> Pandoc
readMediaWiki opts s =
  case runParser parseMediaWiki MWState{ mwOptions = opts
                                       , mwMaxNestingLevel = 4
                                       , mwNextLinkNumber  = 1 }
       "source" (s ++ "\n") of
          Left err'    -> error $ "\nError:\n" ++ show err'
          Right result -> result

data MWState = MWState { mwOptions         :: ReaderOptions
                       , mwMaxNestingLevel :: Int
                       , mwNextLinkNumber  :: Int
                       }

type MWParser = Parser [Char] MWState

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
specialChars = "'[]<=&*{}|"

spaceChars :: [Char]
spaceChars = " \n\t"

sym :: String -> MWParser ()
sym s = () <$ try (string s)

newBlockTags :: [String]
newBlockTags = ["haskell","syntaxhighlight","gallery"]

isBlockTag' :: Tag String -> Bool
isBlockTag' tag@(TagOpen t _) = isBlockTag tag || t `elem` newBlockTags
isBlockTag' tag@(TagClose t) = isBlockTag tag || t `elem` newBlockTags
isBlockTag' tag = isBlockTag tag

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
  if '/' `elem` raw   -- self-closing tag
     then return mempty
     else mconcat <$> manyTill block (htmlTag (~== TagClose tag))

charsInTags :: String -> MWParser [Char]
charsInTags tag = try $ do
  (_,raw) <- htmlTag (~== TagOpen tag [])
  if '/' `elem` raw   -- self-closing tag
     then return ""
     else innerText . parseTags <$>
            manyTill anyChar (htmlTag (~== TagClose tag))

--
-- main parser
--

parseMediaWiki :: MWParser Pandoc
parseMediaWiki = do
  bs <- mconcat <$> many block
  spaces
  eof
  return $ B.doc bs

--
-- block parsers
--

block :: MWParser Blocks
block =  mempty <$ skipMany1 blankline
     <|> table
     <|> header
     <|> hrule
     <|> orderedList
     <|> bulletList
     <|> definitionList
     <|> mempty <$ try (spaces *> htmlComment)
     <|> preformatted
     <|> blockTag
     <|> template
     <|> para

para :: MWParser Blocks
para = B.para . trimInlines . mconcat <$> many1 inline

table :: MWParser Blocks
table = do
  tableStart
  caption <- option mempty tableCaption
  optional rowsep
  hasheader <- option False $ True <$ (lookAhead (char '!'))
  hdr <- tableRow
  rows' <- many $ try $ rowsep *> tableRow
  tableEnd
  -- TODO handle cellspecs from styles and aligns...
  let cols = length $ head rows'
  let (headers,rows) = if hasheader
                          then (hdr, rows')
                          else (replicate cols mempty, hdr:rows')
  let cellspecs = replicate cols (AlignDefault, 0.0)
  return $ B.table caption cellspecs headers rows

tableStart :: MWParser ()
tableStart = try $ guardColumnOne *> sym "{|" <* blanklines

tableEnd :: MWParser ()
tableEnd = try $ guardColumnOne *> sym "|}" <* blanklines

rowsep :: MWParser ()
rowsep = try $ guardColumnOne *> sym "|-" <* blanklines

-- TODO add something like 'guard inTable' since this is used in endline
cellsep :: MWParser ()
cellsep = try $ guardColumnOne <*
  (char '!' <|> (char '|' <* notFollowedBy (oneOf "-}+")))

tableCaption :: MWParser Inlines
tableCaption = try $ guardColumnOne *> sym "|+" *> skipMany spaceChar *>
  (trimInlines . mconcat <$> (many inline)) <* skipMany blankline

tableRow :: MWParser [Blocks]
tableRow = try $ many tableCell <* skipMany blankline

tableCell :: MWParser Blocks
tableCell =
  try $ cellsep *> skipMany spaceChar *> (mconcat <$> (many block))

template :: MWParser Blocks
template = B.rawBlock "mediawiki" <$> doublebrackets
  where doublebrackets = try $ do
           string "{{"
           notFollowedBy (char '{')
           contents <- manyTill anyChar (try $ string "}}")
           return $ "{{" ++ contents ++ "}}"

blockTag :: MWParser Blocks
blockTag = do
  (tag, _) <- lookAhead $ htmlTag isBlockTag'
  case tag of
      TagOpen "blockquote" _ -> B.blockQuote <$> blocksInTags "blockquote"
      TagOpen "pre" _ -> B.codeBlock . trimCode <$> charsInTags "pre"
      TagOpen "syntaxhighlight" attrs -> syntaxhighlight attrs
      TagOpen "haskell" _ -> B.codeBlockWith ("",["haskell"],[]) . trimCode <$>
                                charsInTags "haskell"
      TagOpen "gallery" _ -> blocksInTags "gallery"
      TagOpen "p" _ -> mempty <$ htmlTag (~== tag)
      TagClose "p"  -> mempty <$ htmlTag (~== tag)
      _ -> B.rawBlock "html" . snd <$> htmlTag (~== tag)

trimCode :: String -> String
trimCode ('\n':xs) = stripTrailingNewlines xs
trimCode xs        = stripTrailingNewlines xs

syntaxhighlight :: [Attribute String] -> MWParser Blocks
syntaxhighlight attrs = try $ do
  let mblang = lookup "lang" attrs
  let mbstart = lookup "start" attrs
  let mbline = lookup "line" attrs
  let classes = maybe [] (:[]) mblang ++ maybe [] (const ["numberLines"]) mbline
  let kvs = maybe [] (\x -> [("startFrom",x)]) mbstart
  contents <- charsInTags "syntaxhighlight"
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
  let inline' = whitespace' <|> endline' <|> nowiki' <|> inline
  let strToCode (Str s) = Code ("",[],[]) s
      strToCode  x      = x
  B.para . bottomUp strToCode . mconcat <$> many1 inline'

header :: MWParser Blocks
header = try $ do
  guardColumnOne
  eqs <- many1 (char '=')
  let lev = length eqs
  guard $ lev <= 6
  contents <- trimInlines . mconcat <$> manyTill inline (count lev $ char '=')
  return $ B.header lev contents

bulletList :: MWParser Blocks
bulletList = B.bulletList <$>
   (   many1 (listItem '*')
   <|> (htmlTag (~== TagOpen "ul" []) *> spaces *> many (listItem '*' <|> li) <*
        optional (htmlTag (~== TagClose "ul"))) )

orderedList :: MWParser Blocks
orderedList =
       (B.orderedList <$> many1 (listItem '#'))
   <|> (B.orderedList <$> (htmlTag (~== TagOpen "ul" []) *> spaces *>
        many (listItem '#' <|> li) <*
        optional (htmlTag (~== TagClose "ul"))))
   <|> do (tag,_) <- htmlTag (~== TagOpen "ol" [])
          spaces
          items <- many (listItem '#' <|> li)
          optional (htmlTag (~== TagClose "ol"))
          let start = maybe 1 id $ safeRead $ fromAttrib "start" tag
          return $ B.orderedListWith (start, DefaultStyle, DefaultDelim) items

definitionList :: MWParser Blocks
definitionList = B.definitionList <$> many1 defListItem

defListItem :: MWParser (Inlines, [Blocks])
defListItem = try $ do
  terms <- mconcat . intersperse B.linebreak <$> many defListTerm
  defs  <- many1 $ listItem ':'
  return (terms, defs)

defListTerm  :: MWParser Inlines
defListTerm = char ';' >> skipMany spaceChar >> manyTill anyChar newline >>=
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
       first <- manyTill anyChar newline
       rest <- many (try $ string extras *> manyTill anyChar newline)
       contents <- parseFromString (many1 $ listItem' c)
                          (unlines (first : rest))
       case c of
           '*'  -> return $ B.bulletList contents
           '#'  -> return $ B.orderedList contents
           ':'  -> return $ B.definitionList [(mempty, contents)]
           _    -> mzero

listItem' :: Char -> MWParser Blocks
listItem' c = try $ do
  listStart c
  skipMany spaceChar
  first <- manyTill anyChar newline
  rest <- many (try $ char c *> lookAhead listStartChar *>
                   manyTill anyChar newline)
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
      <|> strong
      <|> emph
      <|> image
      <|> internalLink
      <|> externalLink
      <|> inlineTag
      <|> B.singleton <$> charRef
      <|> inlineHtml
      <|> variable
      <|> special

str :: MWParser Inlines
str = B.str <$> many1 (noneOf $ specialChars ++ spaceChars)

variable :: MWParser Inlines
variable = B.rawInline "mediawiki" <$> triplebrackets
  where triplebrackets = try $ do
           string "{{{"
           contents <- manyTill anyChar (try $ string "}}}")
           return $ "{{{" ++ contents ++ "}}}"

inlineTag :: MWParser Inlines
inlineTag = do
  (tag, _) <- lookAhead $ htmlTag isInlineTag
  case tag of
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
       TagOpen "math" _ -> B.math <$> charsInTags "math"
       TagOpen "code" _ -> B.code <$> charsInTags "code"
       TagOpen "tt" _ -> B.code <$> charsInTags "tt"
       TagOpen "hask" _ -> B.codeWith ("",["haskell"],[]) <$> charsInTags "hask"
       _ -> B.rawInline "html" . snd <$> htmlTag (~== tag)

special :: MWParser Inlines
special = B.str <$> count 1 (notFollowedBy' (htmlTag isBlockTag') *>
                             notFollowedBy (char '|') *> oneOf specialChars)

inlineHtml :: MWParser Inlines
inlineHtml = B.rawInline "html" . snd <$> htmlTag isInlineTag

whitespace :: MWParser Inlines
whitespace = B.space <$ (skipMany1 spaceChar <|> endline <|> htmlComment)

endline :: MWParser ()
endline = () <$ try (newline <*
                     notFollowedBy blankline <*
                     notFollowedBy' hrule <*
                     notFollowedBy tableStart <*
                     notFollowedBy' template <*
                     notFollowedBy cellsep <*
                     notFollowedBy anyListStart)

image :: MWParser Inlines
image = try $ do
  sym "[["
  sym "File:"
  fname <- many1 (noneOf "|]")
  _ <- many (try $ char '|' *> imageOption)
  caption <-   (B.str fname <$ sym "]]")
           <|> try (char '|' *> (mconcat <$> manyTill inline (sym "]]")))
  return $ B.image fname "image" caption

imageOption :: MWParser String
imageOption =
      try (oneOfStrings [ "border", "thumbnail", "frameless"
                        , "thumb", "upright", "left", "right"
                        , "center", "none", "baseline", "sub"
                        , "super", "top", "text-top", "middle"
                        , "bottom", "text-bottom" ])
  <|> try (string "frame")
  <|> try (many1 (oneOf "x0123456789") <* string "px")
  <|> try (oneOfStrings ["link=","alt=","page=","class="] <* many (noneOf "|]"))

internalLink :: MWParser Inlines
internalLink = try $ do
  sym "[["
  let addUnderscores x = let (pref,suff) = break (=='#') x
                         in  pref ++ intercalate "_" (words suff)
  pagename <- unwords . words <$> many (noneOf "|]")
  label <- option (B.text pagename) $ char '|' *>
             (  (mconcat <$> many1 (notFollowedBy (char ']') *> inline))
             -- the "pipe trick"
             -- [[Help:Contents|] -> "Contents"
             <|> (return $ B.text $ drop 1 $ dropWhile (/=':') pagename) )
  sym "]]"
  linktrail <- B.text <$> many (char '\'' <|> letter)
  return $ B.link (addUnderscores pagename) "wikilink" (label <> linktrail)

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
          innerSpace = try $ whitespace >>~ notFollowedBy' end

emph :: MWParser Inlines
emph = B.emph <$> nested (inlinesBetween start end)
    where start = sym "''" >> lookAhead nonspaceChar
          end   = try $ notFollowedBy' (() <$ strong) >> sym "''"

strong :: MWParser Inlines
strong = B.strong <$> nested (inlinesBetween start end)
    where start = sym "'''" >> lookAhead nonspaceChar
          end   = try $ sym "'''"

