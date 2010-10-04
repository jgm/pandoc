{-
Copyright (C) 2010 Paul Rivier <paul*rivier#demotera*com> | tr '*#' '.@'

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
   Module      : Text.Pandoc.Readers.Textile
   Copyright   : Copyright (C) 2010 Paul Rivier
   License     : GNU GPL, version 2 or above 

   Maintainer  : Paul Rivier <paul*rivier#demotera*com>
   Stability   : alpha
   Portability : portable

Conversion from Textile to 'Pandoc' document, based on the spec
available at http://redcloth.org/hobix.com/textile/

Implemented :
 - Paragraphs
 - Code blocks
 - Lists
 - blockquote
 - Inlines : strong, emph, cite, code, deleted, superscript,
   subscript, links

Not implemented :
 - HTML-specific and CSS-specific inlines
-}


module Text.Pandoc.Readers.Textile ( 
                                readTextile
                               ) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared 
import Text.Pandoc.Parsing
import Text.Pandoc.Readers.HTML ( htmlTag, htmlEndTag )
import Text.ParserCombinators.Parsec
import Data.Char ( digitToInt )

-- | Parse a Textile text and return a Pandoc document.
readTextile :: ParserState -- ^ Parser state, including options for parser
             -> String      -- ^ String to parse (assuming @'\n'@ line endings)
             -> Pandoc
readTextile state s = (readWith parseTextile) state (s ++ "\n\n")


--
-- Constants and data structure definitions
--

-- | Special chars border strings parsing
specialChars :: [Char]
specialChars = "\\[]*#_@~<>!?-+^&'\";:|"

-- | Generate a Pandoc ADT from a textile document
parseTextile :: GenParser Char ParserState Pandoc
parseTextile = do
  many blankline
  blocks <- parseBlocks 
  return $ Pandoc (Meta [Str ""] [[Str ""]] [Str ""]) blocks -- FIXME

-- | Parse document blocks
parseBlocks :: GenParser Char ParserState [Block]
parseBlocks = manyTill block eof

-- | Block parsers list tried in definition order
blockParsers :: [GenParser Char ParserState Block]
blockParsers = [ codeBlock
               , header
               , blockQuote
               , anyList
               , table
               , para
               , nullBlock ]

-- | Any block in the order of definition of blockParsers
block :: GenParser Char ParserState Block
block = choice blockParsers <?> "block"

-- | Code Blocks in Textile are between <pre> and </pre>
codeBlock :: GenParser Char ParserState Block
codeBlock = try $ do
  htmlTag False "pre"
  content <- manyTill anyChar (try $ htmlEndTag "pre" >> blockBreak)
  return $ CodeBlock ("",[],[]) content

-- | Header of the form "hN. content" with N in 1..6
header :: GenParser Char ParserState Block
header = try $ do
  char 'h'
  level <- oneOf "123456" >>= return . digitToInt
  char '.'
  whitespace
  name <- manyTill inline blockBreak
  return $ Header level (normalizeSpaces name)

-- | Blockquote of the form "bq. content"
blockQuote :: GenParser Char ParserState Block
blockQuote = try $ do
  string "bq."
  whitespace
  para >>= return . BlockQuote . (:[])

-- Lists handling

-- | Can be a bullet list or an ordered list. This implementation is
-- strict in the nesting, sublist must start at exactly "parent depth
-- plus one"
anyList :: GenParser Char ParserState Block
anyList = try $ do
  l <- anyListAtDepth 1
  blanklines
  return l

-- | This allow one type of list to be nested into an other type,
-- provided correct nesting
anyListAtDepth :: Int -> GenParser Char ParserState Block
anyListAtDepth depth = choice [ bulletListAtDepth depth,
                                orderedListAtDepth depth ]

-- | Bullet List of given depth, depth being the number of leading '*'
bulletListAtDepth :: Int -> GenParser Char ParserState Block
bulletListAtDepth depth = try $ do
  items <- many1 (bulletListItemAtDepth depth)
  return (BulletList items)

-- | Bullet List Item of given depth, depth being the number of
-- leading '*'
bulletListItemAtDepth :: Int -> GenParser Char ParserState [Block]
bulletListItemAtDepth depth = try $ do
  count depth (char '*')
  whitespace
  p <- inlines >>= return . Plain
  sublist <- option [] (anyListAtDepth (depth + 1) >>= return . (:[]))
  return (p:sublist)

-- | Ordered List of given depth, depth being the number of
-- leading '#'
orderedListAtDepth :: Int -> GenParser Char ParserState Block
orderedListAtDepth depth = try $ do
  items <- many1 (orderedListItemAtDepth depth)
  return (OrderedList (1, DefaultStyle, DefaultDelim) items)

-- | Ordered List Item of given depth, depth being the number of
-- leading '#'
orderedListItemAtDepth :: Int -> GenParser Char ParserState [Block]
orderedListItemAtDepth depth = try $ do
  count depth (char '#')
  whitespace
  p <- inlines >>= return . Plain
  sublist <- option [] (anyListAtDepth (depth + 1) >>= return . (:[]))
  return (p:sublist)

-- | This terminates a block such as a paragraph.
blockBreak :: GenParser Char ParserState ()
blockBreak = try $ newline >> blanklines >> return ()

-- | In textile, paragraphs are separated by blank lines.
para :: GenParser Char ParserState Block
para = try $ do
  content <- manyTill inline blockBreak
  return $ Para $ normalizeSpaces content
  

-- Tables
  
-- | A table cell spans until a pipe |
tableCell :: GenParser Char ParserState TableCell
tableCell = do
  c <- many1 (noneOf "|\n")
  content <- parseFromString (many1 inline) c
  return $ [ Plain $ normalizeSpaces content ]

-- | A table row is made of many table cells
tableRow :: GenParser Char ParserState [TableCell]
tableRow = try $ do
  char '|'
  cells <- endBy1 tableCell (char '|')
  newline
  return cells

-- | Many table rows
tableRows :: GenParser Char ParserState [[TableCell]]
tableRows = many1 tableRow

-- | Table headers are made of cells separated by a tag "|_."
tableHeaders :: GenParser Char ParserState [TableCell]
tableHeaders = try $ do
  let separator = (try $ string "|_.")
  separator
  headers <- sepBy1 tableCell separator
  char '|'
  newline
  return headers
  
-- | A table with an optional header. Current implementation can
-- handle tables with and without header, but will parse cells
-- alignment attributes as content.
table :: GenParser Char ParserState Block
table = try $ do
  headers <- option [] tableHeaders
  rows <- tableRows
  blanklines
  let nbOfCols = max (length headers) (length $ head rows)
  return $ Table [] 
    (replicate nbOfCols AlignDefault)
    (replicate nbOfCols 0.0)
    headers
    rows
  


----------
-- Inlines
----------


-- | Any inline element
inline :: GenParser Char ParserState Inline
inline = choice inlineParsers <?> "inline"

-- | List of consecutive inlines before a newline
inlines :: GenParser Char ParserState [Inline]
inlines = manyTill inline newline

-- | Inline parsers tried in order
inlineParsers :: [GenParser Char ParserState Inline]
inlineParsers = [ autoLink
                , str
                , whitespace
                , endline
                , code
                , simpleInline (string "??") (Cite [])
                , simpleInline (string "**") Strong
                , simpleInline (string "__") Emph
                , simpleInline (char '*') Strong
                , simpleInline (char '_') Emph
                , simpleInline (char '-') Strikeout
                , simpleInline (char '^') Superscript
                , simpleInline (char '~') Subscript
                , link
                , image
                , symbol
                ]

-- | Any string
str :: GenParser Char ParserState Inline
str = many1 (noneOf (specialChars ++ "\t\n ")) >>= return . Str

-- | Some number of space chars
whitespace :: GenParser Char ParserState Inline
whitespace = many1 spaceChar >> return Space <?> "whitespace"

-- | In Textile, an endline character that can be treated as a space,
-- not a structural break
endline :: GenParser Char ParserState Inline
endline = try $ do
  newline >> notFollowedBy blankline
  return Space

link :: GenParser Char ParserState Inline
link = try $ do
  name <- surrounded (char '"') inline
  char ':'
  url <- manyTill (anyChar) (lookAhead $ (space <|> try (oneOf ".;," >> (space <|> newline))))
  return $ Link name (url, "")
  
-- | Detect plain links to http or email.
autoLink :: GenParser Char ParserState Inline
autoLink = do
  (orig, src) <- (try uri <|> try emailAddress)
  return $ Link [Str orig] (src, "")

-- | image embedding
image :: GenParser Char ParserState Inline
image = try $ do
  char '!' >> notFollowedBy space
  src <- manyTill anyChar (lookAhead $ oneOf "!(")
  alt <- option "" (try $ (char '(' >> manyTill anyChar (char ')')))
  char '!'
  return $ Image [Str alt] (src, alt)

-- | Any special symbol defined in specialChars
symbol :: GenParser Char ParserState Inline
symbol = do 
  result <- oneOf specialChars
  return $ Str [result]

-- | Inline code
code :: GenParser Char ParserState Inline
code = surrounded (char '@') anyChar >>= 
       return . Code

-- | Parses material surrounded by a parser.
surrounded :: GenParser Char st t   -- ^ surrounding parser
	    -> GenParser Char st a    -- ^ content parser (to be used repeatedly)
	    -> GenParser Char st [a]
surrounded border = enclosed border border

-- | Inlines are most of the time of the same form
simpleInline :: GenParser Char ParserState t           -- ^ surrounding parser
                -> ([Inline] -> Inline)       -- ^ Inline constructor
                -> GenParser Char ParserState Inline   -- ^ content parser (to be used repeatedly)
simpleInline border construct = surrounded border inline >>=
                                return . construct . normalizeSpaces


-- TODO
-- 
--  - Pandoc Meta Information (title, author, date)
--  - footnotes
--  - should autolink be shared through Parsing.hs ?
