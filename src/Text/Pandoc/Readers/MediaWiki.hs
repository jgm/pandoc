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
_ tests for lists
_ support HTML lists
_ support list style attributes and start values in ol lists, also
  value attribute on li
_ support :, ::, etc. for indent (treat as list continuation paras?)
_ support preformatted text (lines starting with space)
_ support preformatted text blocks
_ code highlighting: http://www.mediawiki.org/wiki/Extension:SyntaxHighlight_GeSHi <syntaxhighlight lang="php"> (alternativel, <source...>)
  if 'line' attribute present, number lines
  if 'start' present, set starting line number
_ support internal links http://www.mediawiki.org/wiki/Help:Links
_ support external links
_ support automatic linkification of URLs
_ support images http://www.mediawiki.org/wiki/Help:Images
_ ignore gallery tag?
_ support tables http://www.mediawiki.org/wiki/Help:Tables
_ support <math> tag for latex math
_ templates or anything in {{}} -> handle as raw wikimedia, can be dealt with in
  postprocessing
_ category links
_ tests for raw html inline
_ tests for sup, sub, del
_ tests for pre, haskell
_ tests for code, tt, hask
_ test for blockquote
-}
module Text.Pandoc.Readers.MediaWiki ( readMediaWiki ) where

import Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Builder (Inlines, Blocks, trimInlines, (<>))
import Text.Pandoc.Options
import Text.Pandoc.Readers.HTML ( htmlTag, htmlInBalanced,
           isInlineTag, isBlockTag, isTextTag, isCommentTag )
import Text.Pandoc.XML ( fromEntities )
import Text.Pandoc.Parsing
import Text.Pandoc.Shared ( stripTrailingNewlines )
import Data.Monoid (mconcat, mempty)
import qualified Data.Foldable as F
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
specialChars = "'[]<=&*"

spaceChars :: [Char]
spaceChars = " \n\t"

sym :: String -> MWParser ()
sym s = () <$ try (string s)

htmlComment :: MWParser ()
htmlComment = () <$ htmlTag isCommentTag

inlinesInTags :: String -> MWParser Inlines
inlinesInTags tag = trimInlines . mconcat <$> try
  (htmlTag (~== TagOpen tag []) *>
   manyTill inline (htmlTag (~== TagClose tag)))

blocksInTags :: String -> MWParser Blocks
blocksInTags tag = mconcat <$> try
  (htmlTag (~== TagOpen tag []) *>
   manyTill block (htmlTag (~== TagClose tag)))

charsInTags :: String -> MWParser [Char]
charsInTags tag = innerText . parseTags <$> try
  (htmlTag (~== TagOpen tag []) *>
   manyTill anyChar (htmlTag (~== TagClose tag)))

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
block = header
     <|> hrule
     <|> bulletList
     <|> orderedList
     <|> definitionList
     <|> blockquote
     <|> codeblock
     <|> haskell
     <|> mempty <$ skipMany1 blankline
     <|> mempty <$ try (spaces *> htmlComment)
     <|> pTag
     <|> blockHtml
     <|> para

para :: MWParser Blocks
para = B.para . trimInlines . mconcat <$> many1 inline

-- We can just skip pTags, as contents will be treated as paragraphs
pTag :: MWParser Blocks
pTag = mempty <$ (htmlTag (\t -> t ~== TagOpen "p" [] || t ~== TagClose "p"))

blockHtml :: MWParser Blocks
blockHtml = (B.rawBlock "html" . snd <$> htmlTag isBlockTag)

hrule :: MWParser Blocks
hrule = B.horizontalRule <$ try (string "----" *> many (char '-') *> newline)

blockquote :: MWParser Blocks
blockquote = B.blockQuote <$> blocksInTags "blockquote"

codeblock :: MWParser Blocks
codeblock = B.codeBlock . trimCode <$> charsInTags "pre"

trimCode :: String -> String
trimCode ('\n':xs) = stripTrailingNewlines xs
trimCode xs        = stripTrailingNewlines xs

haskell :: MWParser Blocks
haskell = B.codeBlockWith ("",["haskell"],[]) . trimCode <$>
             charsInTags "haskell"

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
bulletList = B.bulletList <$> many1 (listItem '*')

orderedList :: MWParser Blocks
orderedList = B.orderedList <$> many1 (listItem '#')

definitionList :: MWParser Blocks
definitionList = B.definitionList <$> many1 defListItem

defListItem :: MWParser (Inlines, [Blocks])
defListItem = try $ do
  terms <- mconcat . intersperse B.linebreak <$> many1 defListTerm
  defs  <- many1 $ listItem ':'
  return (terms, defs)

defListTerm  :: MWParser Inlines
defListTerm = char ';' >> skipMany spaceChar >> manyTill anyChar newline >>=
  parseFromString (trimInlines . mconcat <$> many inline)

listStart :: Char -> MWParser ()
listStart c = char c *> notFollowedBy listStartChar

listStartChar :: MWParser Char
listStartChar = oneOf "*#;:"

anyListStart :: MWParser ()
anyListStart =  skipMany1 (char '*')
            <|> skipMany1 (char '#')
            <|> (() <$ char ';')

listItem :: Char -> MWParser Blocks
listItem c = try $ do
  extras <- many (try $ char c <* lookAhead listStartChar)
  if null extras
     then listItem' c
     else do
       first <- manyTill anyChar newline
       rest <- many (try $ string extras *> manyTill anyChar newline)
       contents <- parseFromString (many1 $ listItem' c)
                          (unlines (first : rest))
       case c of
           '*'  -> return $ B.bulletList contents
           '#'  -> return $ B.orderedList contents
           _    -> mzero

listItem' :: Char -> MWParser Blocks
listItem' c = try $ do
  listStart c
  first <- manyTill anyChar newline
  rest <- many (try $ char c *> lookAhead listStartChar *>
                   manyTill anyChar newline)
  contents <- parseFromString (mconcat <$> many1 block)
               $ unlines $ first : rest
  case viewl (B.unMany contents) of
       (Para xs) :< rest -> return $ B.Many $ (Plain xs) <| rest
       _                 -> return contents

--
-- inline parsers
--

inline :: MWParser Inlines
inline =  whitespace
      <|> url
      <|> str
      <|> strong
      <|> emph
      <|> nowiki
      <|> linebreak
      <|> externalLink
      <|> strikeout
      <|> subscript
      <|> superscript
      <|> code
      <|> hask
      <|> B.singleton <$> charRef
      <|> inlineHtml
      <|> special

str :: MWParser Inlines
str = B.str <$> many1 (noneOf $ specialChars ++ spaceChars)

special :: MWParser Inlines
special = B.str <$> count 1 (notFollowedBy' (htmlTag isBlockTag) *>
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

linebreak :: MWParser Inlines
linebreak = B.linebreak <$
  (htmlTag (~== TagOpen "br" []) *>
   optional (htmlTag (~== TagClose "br")) *>
   optional blankline)

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

nowiki :: MWParser Inlines
nowiki = B.text <$> charsInTags "nowiki"

strikeout :: MWParser Inlines
strikeout = B.strikeout <$> (inlinesInTags "strike" <|> inlinesInTags "del")

superscript :: MWParser Inlines
superscript = B.superscript <$> inlinesInTags "sup"

subscript :: MWParser Inlines
subscript = B.subscript <$> inlinesInTags "sub"

code :: MWParser Inlines
code = B.code <$> (charsInTags "code" <|> charsInTags "tt")

hask :: MWParser Inlines
hask = B.codeWith ("",["haskell"],[]) <$> charsInTags "hask"

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

