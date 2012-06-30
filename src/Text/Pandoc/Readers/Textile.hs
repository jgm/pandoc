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
   Copyright   : Copyright (C) 2010-2012 Paul Rivier and John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : Paul Rivier <paul*rivier#demotera*com>
   Stability   : alpha
   Portability : portable

Conversion from Textile to 'Pandoc' document, based on the spec
available at http://redcloth.org/textile.

Implemented and parsed:
 - Paragraphs
 - Code blocks
 - Lists
 - blockquote
 - Inlines : strong, emph, cite, code, deleted, superscript,
   subscript, links
 - footnotes

Implemented but discarded:
 - HTML-specific and CSS-specific attributes

Left to be implemented:
 - dimension sign
 - all caps
 - continued blocks (ex bq..)

TODO : refactor common patterns across readers :
 - autolink
 - smartPunctuation
 - more ...

-}


module Text.Pandoc.Readers.Textile ( readTextile, readTextile' ) where

import Text.Pandoc.Definition
import Text.Pandoc.Shared 
import Text.Pandoc.Parsing
import Text.Pandoc.Readers.HTML ( htmlTag, isInlineTag, isBlockTag )
import Text.Pandoc.Readers.LaTeX ( rawLaTeXInline, rawLaTeXBlock )
import Text.ParserCombinators.Parsec
import Text.HTML.TagSoup.Match
import Data.Char ( digitToInt, isUpper )
import Control.Monad ( guard, liftM )
import Control.Applicative ((<$>), (*>), (<*))

-- | Parse a Textile text and return a Pandoc document.
readTextile :: ParserState -- ^ Parser state, including options for parser
             -> String      -- ^ String to parse (assuming @'\n'@ line endings)
             -> Pandoc
readTextile st = dumpParseError . readTextile' st

readTextile' :: ParserState -- ^ Parser state, including options for parser
             -> String      -- ^ String to parse (assuming @'\n'@ line endings)
             -> Either ParseError Pandoc
readTextile' state s =
  (readWith parseTextile) state{ stateOldDashes = True } (s ++ "\n\n")


-- | Generate a Pandoc ADT from a textile document
parseTextile :: GenParser Char ParserState Pandoc
parseTextile = do
  -- textile allows raw HTML and does smart punctuation by default
  updateState (\state -> state { stateParseRaw = True, stateSmart = True })
  many blankline
  startPos <- getPosition
  -- go through once just to get list of reference keys and notes
  -- docMinusKeys is the raw document with blanks where the keys/notes were...
  let firstPassParser = noteBlock <|> lineClump
  manyTill firstPassParser eof >>= setInput . concat
  setPosition startPos
  st' <- getState
  let reversedNotes = stateNotes st'
  updateState $ \s -> s { stateNotes = reverse reversedNotes }
  -- now parse it for real...
  blocks <- parseBlocks
  return $ Pandoc (Meta [] [] []) blocks -- FIXME

noteMarker :: GenParser Char ParserState [Char]
noteMarker = skipMany spaceChar >> string "fn" >> manyTill digit (char '.')

noteBlock :: GenParser Char ParserState [Char]
noteBlock = try $ do
  startPos <- getPosition
  ref <- noteMarker
  optional blankline
  contents <- liftM unlines $ many1Till anyLine (blanklines <|> noteBlock)
  endPos <- getPosition
  let newnote = (ref, contents ++ "\n")
  st <- getState
  let oldnotes = stateNotes st
  updateState $ \s -> s { stateNotes = newnote : oldnotes }
  -- return blanks so line count isn't affected
  return $ replicate (sourceLine endPos - sourceLine startPos) '\n'

-- | Parse document blocks
parseBlocks :: GenParser Char ParserState [Block]
parseBlocks = manyTill block eof

-- | Block parsers list tried in definition order
blockParsers :: [GenParser Char ParserState Block]
blockParsers = [ codeBlock
               , header
               , blockQuote
               , hrule
               , anyList
               , rawHtmlBlock
               , rawLaTeXBlock'
               , maybeExplicitBlock "table" table
               , maybeExplicitBlock "p" para
               , nullBlock ]

-- | Any block in the order of definition of blockParsers
block :: GenParser Char ParserState Block
block = choice blockParsers <?> "block"

codeBlock :: GenParser Char ParserState Block
codeBlock = codeBlockBc <|> codeBlockPre

codeBlockBc :: GenParser Char ParserState Block
codeBlockBc = try $ do
  string "bc. "
  contents <- manyTill anyLine blanklines
  return $ CodeBlock ("",[],[]) $ unlines contents

-- | Code Blocks in Textile are between <pre> and </pre>
codeBlockPre :: GenParser Char ParserState Block
codeBlockPre = try $ do
  htmlTag (tagOpen (=="pre") null)
  result' <- manyTill anyChar (try $ htmlTag (tagClose (=="pre")) >> blockBreak)
  -- drop leading newline if any
  let result'' = case result' of
                      '\n':xs -> xs
                      _       -> result'
  -- drop trailing newline if any
  let result''' = case reverse result'' of
                       '\n':_ -> init result''
                       _      -> result''
  return $ CodeBlock ("",[],[]) result'''

-- | Header of the form "hN. content" with N in 1..6
header :: GenParser Char ParserState Block
header = try $ do
  char 'h'
  level <- digitToInt <$> oneOf "123456"
  optional attributes >> char '.' >> whitespace
  name <- normalizeSpaces <$> manyTill inline blockBreak
  return $ Header level name

-- | Blockquote of the form "bq. content"
blockQuote :: GenParser Char ParserState Block
blockQuote = try $ do
  string "bq" >> optional attributes >> char '.' >> whitespace
  BlockQuote . singleton <$> para

-- Horizontal rule

hrule :: GenParser Char st Block
hrule = try $ do
  skipSpaces
  start <- oneOf "-*"
  count 2 (skipSpaces >> char start)
  skipMany (spaceChar <|> char start)
  newline
  optional blanklines
  return HorizontalRule

-- Lists handling

-- | Can be a bullet list or an ordered list. This implementation is
-- strict in the nesting, sublist must start at exactly "parent depth
-- plus one"
anyList :: GenParser Char ParserState Block
anyList = try $ ( (anyListAtDepth 1) <* blanklines )

-- | This allow one type of list to be nested into an other type,
-- provided correct nesting
anyListAtDepth :: Int -> GenParser Char ParserState Block
anyListAtDepth depth = choice [ bulletListAtDepth depth,
                                orderedListAtDepth depth,
                                definitionList ]

-- | Bullet List of given depth, depth being the number of leading '*'
bulletListAtDepth :: Int -> GenParser Char ParserState Block
bulletListAtDepth depth = try $ BulletList <$> many1 (bulletListItemAtDepth depth)

-- | Bullet List Item of given depth, depth being the number of
-- leading '*'
bulletListItemAtDepth :: Int -> GenParser Char ParserState [Block]
bulletListItemAtDepth = genericListItemAtDepth '*'

-- | Ordered List of given depth, depth being the number of
-- leading '#'
orderedListAtDepth :: Int -> GenParser Char ParserState Block
orderedListAtDepth depth = try $ do
  items <- many1 (orderedListItemAtDepth depth)
  return (OrderedList (1, DefaultStyle, DefaultDelim) items)

-- | Ordered List Item of given depth, depth being the number of
-- leading '#'
orderedListItemAtDepth :: Int -> GenParser Char ParserState [Block]
orderedListItemAtDepth = genericListItemAtDepth '#'

-- | Common implementation of list items
genericListItemAtDepth :: Char -> Int -> GenParser Char ParserState [Block]
genericListItemAtDepth c depth = try $ do
  count depth (char c) >> optional attributes >> whitespace
  p <- inlines
  sublist <- option [] (singleton <$> anyListAtDepth (depth + 1))
  return ((Plain p):sublist)

-- | A definition list is a set of consecutive definition items
definitionList :: GenParser Char ParserState Block  
definitionList = try $ DefinitionList <$> many1 definitionListItem
  
-- | A definition list item in textile begins with '- ', followed by
-- the term defined, then spaces and ":=". The definition follows, on
-- the same single line, or spaned on multiple line, after a line
-- break.
definitionListItem :: GenParser Char ParserState ([Inline], [[Block]])
definitionListItem = try $ do
  string "- "
  term <- many1Till inline (try (whitespace >> string ":="))
  def <- inlineDef <|> multilineDef
  return (term, def)
  where inlineDef :: GenParser Char ParserState [[Block]]
        inlineDef = liftM (\d -> [[Plain d]]) $ try (whitespace >> inlines)
        multilineDef :: GenParser Char ParserState [[Block]]
        multilineDef = try $ do
          optional whitespace >> newline
          s <- many1Till anyChar (try (string "=:" >> newline))
          -- this ++ "\n\n" does not look very good
          ds <- parseFromString parseBlocks (s ++ "\n\n")
          return [ds]

-- | This terminates a block such as a paragraph. Because of raw html
-- blocks support, we have to lookAhead for a rawHtmlBlock.
blockBreak :: GenParser Char ParserState ()
blockBreak = try (newline >> blanklines >> return ()) <|>
              (lookAhead rawHtmlBlock >> return ())

-- raw content

-- | A raw Html Block, optionally followed by blanklines
rawHtmlBlock :: GenParser Char ParserState Block
rawHtmlBlock = try $ do
  (_,b) <- htmlTag isBlockTag
  optional blanklines
  return $ RawBlock "html" b

-- | Raw block of LaTeX content
rawLaTeXBlock' :: GenParser Char ParserState Block
rawLaTeXBlock' = do
  failIfStrict
  RawBlock "latex" <$> (rawLaTeXBlock <* spaces)


-- | In textile, paragraphs are separated by blank lines.
para :: GenParser Char ParserState Block
para = try $ Para . normalizeSpaces <$> manyTill inline blockBreak


-- Tables
  
-- | A table cell spans until a pipe |
tableCell :: GenParser Char ParserState TableCell
tableCell = do
  c <- many1 (noneOf "|\n")
  content <- parseFromString (many1 inline) c
  return $ [ Plain $ normalizeSpaces content ]

-- | A table row is made of many table cells
tableRow :: GenParser Char ParserState [TableCell]
tableRow = try $ ( char '|' *> (endBy1 tableCell (char '|')) <* newline)

-- | Many table rows
tableRows :: GenParser Char ParserState [[TableCell]]
tableRows = many1 tableRow

-- | Table headers are made of cells separated by a tag "|_."
tableHeaders :: GenParser Char ParserState [TableCell]
tableHeaders = let separator = (try $ string "|_.") in
  try $ ( separator *> (sepBy1 tableCell separator) <* char '|' <* newline )
  
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
  

-- | Blocks like 'p' and 'table' do not need explicit block tag.
-- However, they can be used to set HTML/CSS attributes when needed.
maybeExplicitBlock :: String  -- ^ block tag name
                    -> GenParser Char ParserState Block -- ^ implicit block
                    -> GenParser Char ParserState Block
maybeExplicitBlock name blk = try $ do
  optional $ try $ string name >> optional attributes >> char '.' >> 
    ((try whitespace) <|> endline)
  blk



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
                , escapedInline
                , htmlSpan
                , rawHtmlInline
                , rawLaTeXInline'
                , note
                , try $ (char '[' *> inlineMarkup <* char ']')
                , inlineMarkup
                , link
                , image
                , mark
                , smartPunctuation inline
                , symbol
                ]

-- | Inline markups
inlineMarkup :: GenParser Char ParserState Inline
inlineMarkup = choice [ simpleInline (string "??") (Cite [])
                      , simpleInline (string "**") Strong
                      , simpleInline (string "__") Emph
                      , simpleInline (char '*') Strong
                      , simpleInline (char '_') Emph
                      , simpleInline (char '+') Emph  -- approximates underline
                      , simpleInline (char '-') Strikeout
                      , simpleInline (char '^') Superscript
                      , simpleInline (char '~') Subscript
                      ]

-- | Trademark, registered, copyright
mark :: GenParser Char st Inline
mark = try $ char '(' >> (try tm <|> try reg <|> copy)

reg :: GenParser Char st Inline
reg = do
  oneOf "Rr"
  char ')'
  return $ Str "\174"

tm :: GenParser Char st Inline
tm = do
  oneOf "Tt"
  oneOf "Mm"
  char ')'
  return $ Str "\8482"

copy :: GenParser Char st Inline
copy = do
  oneOf "Cc"
  char ')'
  return $ Str "\169"

note :: GenParser Char ParserState Inline
note = try $ do
  ref <- (char '[' *> many1 digit <* char ']')
  notes <- stateNotes <$> getState
  case lookup ref notes of
    Nothing   -> fail "note not found"
    Just raw  -> liftM Note $ parseFromString parseBlocks raw

-- | Special chars 
markupChars :: [Char]
markupChars = "\\[]*#_@~-+^|%="

-- | Break strings on following chars. Space tab and newline break for
--  inlines breaking. Open paren breaks for mark. Quote, dash and dot
--  break for smart punctuation. Punctuation breaks for regular
--  punctuation. Double quote breaks for named links. > and < break
--  for inline html.
stringBreakers :: [Char]
stringBreakers = " \t\n('-.,:!?;\"<>"

wordBoundaries :: [Char]
wordBoundaries = markupChars ++ stringBreakers

-- | Parse a hyphened sequence of words
hyphenedWords :: GenParser Char ParserState String
hyphenedWords = try $ do
  hd <- noneOf wordBoundaries
  tl <- many ( (noneOf wordBoundaries) <|> 
               try (oneOf markupChars <* lookAhead (noneOf wordBoundaries) ) )
  let wd = hd:tl
  option wd $ try $ 
    (\r -> concat [wd, "-", r]) <$> (char '-' *> hyphenedWords)

-- | Any string
str :: GenParser Char ParserState Inline
str = do
  baseStr <- hyphenedWords
  -- RedCloth compliance : if parsed word is uppercase and immediatly
  -- followed by parens, parens content is unconditionally word acronym
  fullStr <- option baseStr $ try $ do
    guard $ all isUpper baseStr
    acro <- enclosed (char '(') (char ')') anyChar
    return $ concat [baseStr, " (", acro, ")"]
  updateLastStrPos
  return $ Str fullStr

-- | Textile allows HTML span infos, we discard them
htmlSpan :: GenParser Char ParserState Inline
htmlSpan = try $ Str <$> ( char '%' *> attributes *> manyTill anyChar (char '%') )

-- | Some number of space chars
whitespace :: GenParser Char ParserState Inline
whitespace = many1 spaceChar >> return Space <?> "whitespace"

-- | In Textile, an isolated endline character is a line break
endline :: GenParser Char ParserState Inline
endline = try $ do
  newline >> notFollowedBy blankline
  return LineBreak

rawHtmlInline :: GenParser Char ParserState Inline
rawHtmlInline = RawInline "html" . snd <$> htmlTag isInlineTag
                
-- | Raw LaTeX Inline 
rawLaTeXInline' :: GenParser Char ParserState Inline
rawLaTeXInline' = try $ do
  failIfStrict
  rawLaTeXInline

-- | Textile standard link syntax is "label":target
link :: GenParser Char ParserState Inline
link = try $ do
  name <- surrounded (char '"') inline
  char ':'
  url <- manyTill (anyChar) (lookAhead $ (space <|> try (oneOf ".;,:" >> (space <|> newline))))
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

escapedInline :: GenParser Char ParserState Inline
escapedInline = escapedEqs <|> escapedTag

escapedEqs :: GenParser Char ParserState Inline
escapedEqs = Str <$> (try $ string "==" *> manyTill anyChar (try $ string "=="))

-- | literal text escaped btw <notextile> tags
escapedTag :: GenParser Char ParserState Inline
escapedTag = Str <$>
  (try $ string "<notextile>" *> manyTill anyChar (try $ string "</notextile>"))

-- | Any special symbol defined in wordBoundaries
symbol :: GenParser Char ParserState Inline
symbol = Str . singleton <$> oneOf wordBoundaries

-- | Inline code
code :: GenParser Char ParserState Inline
code = code1 <|> code2

code1 :: GenParser Char ParserState Inline
code1 = Code nullAttr <$> surrounded (char '@') anyChar

code2 :: GenParser Char ParserState Inline
code2 = do
  htmlTag (tagOpen (=="tt") null)
  Code nullAttr <$> manyTill anyChar (try $ htmlTag $ tagClose (=="tt"))

-- | Html / CSS attributes
attributes :: GenParser Char ParserState String
attributes = choice [ enclosed (char '(') (char ')') anyChar,
                      enclosed (char '{') (char '}') anyChar,
                      enclosed (char '[') (char ']') anyChar]

-- | Parses material surrounded by a parser.
surrounded :: GenParser Char st t   -- ^ surrounding parser
	    -> GenParser Char st a    -- ^ content parser (to be used repeatedly)
	    -> GenParser Char st [a]
surrounded border = enclosed border (try border)

-- | Inlines are most of the time of the same form
simpleInline :: GenParser Char ParserState t           -- ^ surrounding parser
                -> ([Inline] -> Inline)       -- ^ Inline constructor
                -> GenParser Char ParserState Inline   -- ^ content parser (to be used repeatedly)
simpleInline border construct = surrounded border (inlineWithAttribute) >>=
                                return . construct . normalizeSpaces
  where inlineWithAttribute = (try $ optional attributes) >> inline

-- | Create a singleton list
singleton :: a -> [a]
singleton x = [x]
