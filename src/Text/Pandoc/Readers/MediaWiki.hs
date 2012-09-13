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
_ support internal links http://www.mediawiki.org/wiki/Help:Links
_ support external links (partially implemented)
_ support images http://www.mediawiki.org/wiki/Help:Images
_ support tables http://www.mediawiki.org/wiki/Help:Tables
-}
module Text.Pandoc.Readers.MediaWiki ( readMediaWiki ) where

import Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Builder (Inlines, Blocks, trimInlines, (<>))
import Text.Pandoc.Options
import Text.Pandoc.Readers.HTML ( htmlTag, isInlineTag,
                                  isBlockTag, isCommentTag )
import Text.Pandoc.XML ( fromEntities )
import Text.Pandoc.Parsing
import Text.Pandoc.Generic ( bottomUp )
import Text.Pandoc.Shared ( stripTrailingNewlines, safeRead )
import Data.Monoid (mconcat, mempty)
import Control.Applicative ((<$>), (<*), (*>), (<$))
import Control.Monad
import Data.List (intersperse)
import Text.HTML.TagSoup
import Data.Sequence (viewl, ViewL(..), (<|))

-- | Read mediawiki from an input string and return a Pandoc document.
readMediaWiki :: ReaderOptions -- ^ Reader options
               -> String        -- ^ String to parse (assuming @'\n'@ line endings)
               -> Pandoc
readMediaWiki opts s =
  (readWith parseMediaWiki) def{ stateOptions = opts } (s ++ "\n\n")

type MWParser = Parser [Char] ParserState

--
-- auxiliary functions
--

specialChars :: [Char]
specialChars = "'[]<=&*{}"

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
inlinesInTags tag = trimInlines . mconcat <$> try
   (manyTill inline (htmlTag (~== TagClose tag)))

blocksInTags :: String -> MWParser Blocks
blocksInTags tag = mconcat <$> try
   (manyTill block (htmlTag (~== TagClose tag)))

charsInTags :: String -> MWParser [Char]
charsInTags tag = innerText . parseTags <$> try
   (manyTill anyChar (htmlTag (~== TagClose tag)))

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
     <|> header
     <|> hrule
     <|> orderedList
     <|> bulletList
     <|> definitionList
     <|> mempty <$ try (spaces *> htmlComment)
     <|> preformatted
     <|> blockTag
     <|> pTag
     <|> blockHtml
     <|> template
     <|> para

para :: MWParser Blocks
para = B.para . trimInlines . mconcat <$> many1 inline

template :: MWParser Blocks
template = B.rawBlock "mediawiki" <$> doublebrackets
  where doublebrackets = try $ do
           string "{{"
           notFollowedBy (char '{')
           contents <- manyTill anyChar (try $ string "}}")
           return $ "{{" ++ contents ++ "}}"

blockTag :: MWParser Blocks
blockTag = do
  (TagOpen t attrs, raw) <- htmlTag (\x -> isBlockTag' x && isTagOpen x)
  case t of
      "blockquote" -> B.blockQuote <$> blocksInTags "blockquote"
      "pre" -> B.codeBlock . trimCode <$> charsInTags "pre"
      "syntaxhighlight" -> syntaxhighlight attrs
      "haskell" -> B.codeBlockWith ("",["haskell"],[]) . trimCode <$>
                     charsInTags "haskell"
      "gallery" -> blocksInTags "gallery"
      "p" -> return mempty
      _ -> return $ B.rawBlock "html" raw

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

-- We can just skip pTags, as contents will be treated as paragraphs
pTag :: MWParser Blocks
pTag = mempty <$ (htmlTag (\t -> t ~== TagOpen "p" [] || t ~== TagClose "p"))

blockHtml :: MWParser Blocks
blockHtml = (B.rawBlock "html" . snd <$> htmlTag isBlockTag)

hrule :: MWParser Blocks
hrule = B.horizontalRule <$ try (string "----" *> many (char '-') *> newline)

preformatted :: MWParser Blocks
preformatted = try $ do
  getPosition >>= \pos -> guard (sourceColumn pos == 1)
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
  col <- sourceColumn <$> getPosition
  guard $ col == 1  -- header must be at beginning of line
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
li = htmlTag (~== TagOpen "li" []) *>
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
  (TagOpen t _, raw) <- htmlTag (\x -> isInlineTag x && isTagOpen x)
  case t of
       "nowiki" -> B.text . fromEntities <$> try
                     (manyTill anyChar (htmlTag (~== TagClose "nowiki")))
       "br" -> B.linebreak <$
                 (optional (htmlTag (~== TagClose "br")) *> optional blankline)
       "strike" -> B.strikeout <$> inlinesInTags "strike"
       "del" -> B.strikeout <$> inlinesInTags "del"
       "sub" -> B.subscript <$> inlinesInTags "sub"
       "sup" -> B.superscript <$> inlinesInTags "sup"
       "math" -> B.math <$> charsInTags "math"
       "code" -> B.code <$> charsInTags "code"
       "tt" -> B.code <$> charsInTags "tt"
       "hask" -> B.codeWith ("",["haskell"],[]) <$> charsInTags "hask"
       _ -> return $ B.rawInline "html" raw

special :: MWParser Inlines
special = B.str <$> count 1 (notFollowedBy' (htmlTag isBlockTag') *>
                             oneOf specialChars)

inlineHtml :: MWParser Inlines
inlineHtml = B.rawInline "html" . snd <$> htmlTag isInlineTag

whitespace :: MWParser Inlines
whitespace = B.space <$ (skipMany1 spaceChar <|> endline <|> htmlComment)

endline :: MWParser ()
endline = () <$ try (newline <*
                     notFollowedBy blankline <*
                     notFollowedBy' hrule <*
                     notFollowedBy anyListStart)

externalLink :: MWParser Inlines
externalLink = try $ do
  char '['
  (_, src) <- uri
  skipMany1 spaceChar
  lab <- manyTill inline (char ']')
  let lab' = if null lab
                then [B.str "1"] -- TODO generate sequentially from state
                else lab
  return $ B.link src "" $ trimInlines $ mconcat lab'

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

