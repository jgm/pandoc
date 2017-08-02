{-
Copyright (C) 2010-2012 Paul Rivier <paul*rivier#demotera*com> | tr '*#' '.@'
              2010-2017 John MacFarlane

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
   Copyright   : Copyright (C) 2010-2012 Paul Rivier
                               2010-2017 John MacFarlane
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
 - HTML-specific and CSS-specific attributes on headers

Left to be implemented:
 - dimension sign
 - all caps
 - continued blocks (ex bq..)

TODO : refactor common patterns across readers :
 - more ...

-}


module Text.Pandoc.Readers.Textile ( readTextile) where
import Control.Monad (guard, liftM)
import Control.Monad.Except (throwError)
import Data.Char (digitToInt, isUpper)
import Data.List (intercalate, intersperse, transpose)
import Data.Monoid ((<>))
import Text.HTML.TagSoup (Tag (..), fromAttrib)
import Text.HTML.TagSoup.Match
import Text.Pandoc.Builder (Blocks, Inlines, trimInlines)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class (PandocMonad(..))
import Text.Pandoc.CSS
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import Text.Pandoc.Readers.HTML (htmlTag, isBlockTag, isInlineTag)
import Text.Pandoc.Readers.LaTeX (rawLaTeXBlock, rawLaTeXInline)
import Text.Pandoc.Shared (trim, crFilter)
import Data.Text (Text)
import qualified Data.Text as T

-- | Parse a Textile text and return a Pandoc document.
readTextile :: PandocMonad m
            => ReaderOptions -- ^ Reader options
            -> Text          -- ^ String to parse (assuming @'\n'@ line endings)
            -> m Pandoc
readTextile opts s = do
  parsed <- readWithM parseTextile def{ stateOptions = opts }
                (T.unpack (crFilter s) ++ "\n\n")
  case parsed of
     Right result -> return result
     Left e       -> throwError e


-- | Generate a Pandoc ADT from a textile document
parseTextile :: PandocMonad m => ParserT [Char] ParserState m Pandoc
parseTextile = do
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
  return $ Pandoc nullMeta (B.toList blocks) -- FIXME

noteMarker :: PandocMonad m => ParserT [Char] ParserState m [Char]
noteMarker = skipMany spaceChar >> string "fn" >> manyTill digit (char '.')

noteBlock :: PandocMonad m => ParserT [Char] ParserState m [Char]
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
parseBlocks :: PandocMonad m => ParserT [Char] ParserState m Blocks
parseBlocks = mconcat <$> manyTill block eof

-- | Block parsers list tried in definition order
blockParsers :: PandocMonad m => [ParserT [Char] ParserState m Blocks]
blockParsers = [ codeBlock
               , header
               , blockQuote
               , hrule
               , commentBlock
               , anyList
               , rawHtmlBlock
               , rawLaTeXBlock'
               , table
               , maybeExplicitBlock "p" para
               , mempty <$ blanklines
               ]

-- | Any block in the order of definition of blockParsers
block :: PandocMonad m => ParserT [Char] ParserState m Blocks
block = do
  res <- choice blockParsers <?> "block"
  trace (take 60 $ show $ B.toList res)
  return res

commentBlock :: PandocMonad m => ParserT [Char] ParserState m Blocks
commentBlock = try $ do
  string "###."
  manyTill anyLine blanklines
  return mempty

codeBlock :: PandocMonad m => ParserT [Char] ParserState m Blocks
codeBlock = codeBlockBc <|> codeBlockPre

codeBlockBc :: PandocMonad m => ParserT [Char] ParserState m Blocks
codeBlockBc = try $ do
  string "bc."
  extended <- option False (True <$ char '.')
  char ' '
  let starts = ["p", "table", "bq", "bc", "h1", "h2", "h3",
                "h4", "h5", "h6", "pre", "###", "notextile"]
  let ender = choice $ map explicitBlockStart starts
  contents <- if extended
                 then do
                   f <- anyLine
                   rest <- many (notFollowedBy ender *> anyLine)
                   return (f:rest)
                 else manyTill anyLine blanklines
  return $ B.codeBlock (trimTrailingNewlines (unlines contents))

trimTrailingNewlines :: String -> String
trimTrailingNewlines = reverse . dropWhile (=='\n') . reverse

-- | Code Blocks in Textile are between <pre> and </pre>
codeBlockPre :: PandocMonad m => ParserT [Char] ParserState m Blocks
codeBlockPre = try $ do
  (t@(TagOpen _ attrs),_) <- htmlTag (tagOpen (=="pre") (const True))
  result' <- manyTill anyChar (htmlTag (tagClose (=="pre")))
  optional blanklines
  -- drop leading newline if any
  let result'' = case result' of
                      '\n':xs -> xs
                      _       -> result'
  -- drop trailing newline if any
  let result''' = case reverse result'' of
                       '\n':_ -> init result''
                       _      -> result''
  let classes = words $ fromAttrib "class" t
  let ident = fromAttrib "id" t
  let kvs = [(k,v) | (k,v) <- attrs, k /= "id" && k /= "class"]
  return $ B.codeBlockWith (ident,classes,kvs) result'''

-- | Header of the form "hN. content" with N in 1..6
header :: PandocMonad m => ParserT [Char] ParserState m Blocks
header = try $ do
  char 'h'
  level <- digitToInt <$> oneOf "123456"
  attr <- attributes
  char '.'
  lookAhead whitespace
  name <- trimInlines . mconcat <$> many inline
  attr' <- registerHeader attr name
  return $ B.headerWith attr' level name

-- | Blockquote of the form "bq. content"
blockQuote :: PandocMonad m => ParserT [Char] ParserState m Blocks
blockQuote = try $ do
  string "bq" >> attributes >> char '.' >> whitespace
  B.blockQuote <$> para

-- Horizontal rule

hrule :: PandocMonad m => ParserT [Char] st m Blocks
hrule = try $ do
  skipSpaces
  start <- oneOf "-*"
  count 2 (skipSpaces >> char start)
  skipMany (spaceChar <|> char start)
  newline
  optional blanklines
  return B.horizontalRule

-- Lists handling

-- | Can be a bullet list or an ordered list. This implementation is
-- strict in the nesting, sublist must start at exactly "parent depth
-- plus one"
anyList :: PandocMonad m => ParserT [Char] ParserState m Blocks
anyList = try $ anyListAtDepth 1 <* blanklines

-- | This allow one type of list to be nested into an other type,
-- provided correct nesting
anyListAtDepth :: PandocMonad m => Int -> ParserT [Char] ParserState m Blocks
anyListAtDepth depth = choice [ bulletListAtDepth depth,
                                orderedListAtDepth depth,
                                definitionList ]

-- | Bullet List of given depth, depth being the number of leading '*'
bulletListAtDepth :: PandocMonad m => Int -> ParserT [Char] ParserState m Blocks
bulletListAtDepth depth = try $ B.bulletList  <$> many1 (bulletListItemAtDepth depth)

-- | Bullet List Item of given depth, depth being the number of
-- leading '*'
bulletListItemAtDepth :: PandocMonad m => Int -> ParserT [Char] ParserState m Blocks
bulletListItemAtDepth = genericListItemAtDepth '*'

-- | Ordered List of given depth, depth being the number of
-- leading '#'
orderedListAtDepth :: PandocMonad m => Int -> ParserT [Char] ParserState m Blocks
orderedListAtDepth depth = try $ do
  items <- many1 (orderedListItemAtDepth depth)
  return $ B.orderedList items

-- | Ordered List Item of given depth, depth being the number of
-- leading '#'
orderedListItemAtDepth :: PandocMonad m => Int -> ParserT [Char] ParserState m Blocks
orderedListItemAtDepth = genericListItemAtDepth '#'

-- | Common implementation of list items
genericListItemAtDepth :: PandocMonad m => Char -> Int -> ParserT [Char] ParserState m Blocks
genericListItemAtDepth c depth = try $ do
  count depth (char c) >> attributes >> whitespace
  p <- mconcat <$> many listInline
  newline
  sublist <- option mempty (anyListAtDepth (depth + 1))
  return $ (B.plain p) <> sublist

-- | A definition list is a set of consecutive definition items
definitionList :: PandocMonad m => ParserT [Char] ParserState m Blocks
definitionList = try $ B.definitionList <$> many1 definitionListItem

-- | List start character.
listStart :: PandocMonad m => ParserT [Char] ParserState m ()
listStart = genericListStart '*'
        <|> () <$ genericListStart '#'
        <|> () <$ definitionListStart

genericListStart :: PandocMonad m => Char -> ParserT [Char] st m ()
genericListStart c = () <$ try (many1 (char c) >> whitespace)

basicDLStart :: PandocMonad m => ParserT [Char] ParserState m ()
basicDLStart = do
  char '-'
  whitespace
  notFollowedBy newline

definitionListStart :: PandocMonad m => ParserT [Char] ParserState m Inlines
definitionListStart = try $ do
  basicDLStart
  trimInlines . mconcat <$>
    many1Till inline
     (  try (newline *> lookAhead basicDLStart)
    <|> try (lookAhead (() <$ string ":="))
     )

listInline :: PandocMonad m => ParserT [Char] ParserState m Inlines
listInline = try (notFollowedBy newline >> inline)
         <|> try (endline <* notFollowedBy listStart)

-- | A definition list item in textile begins with '- ', followed by
-- the term defined, then spaces and ":=". The definition follows, on
-- the same single line, or spaned on multiple line, after a line
-- break.
definitionListItem :: PandocMonad m => ParserT [Char] ParserState m (Inlines, [Blocks])
definitionListItem = try $ do
  term <- (mconcat . intersperse B.linebreak) <$> many1 definitionListStart
  def' <- string ":=" *> optional whitespace *> (multilineDef <|> inlineDef)
  return (term, def')
  where inlineDef :: PandocMonad m => ParserT [Char] ParserState m [Blocks]
        inlineDef = liftM (\d -> [B.plain d])
                    $ optional whitespace >> (trimInlines . mconcat <$> many listInline) <* newline
        multilineDef :: PandocMonad m => ParserT [Char] ParserState m [Blocks]
        multilineDef = try $ do
          optional whitespace >> newline
          s <- many1Till anyChar (try (string "=:" >> newline))
          -- this ++ "\n\n" does not look very good
          ds <- parseFromString' parseBlocks (s ++ "\n\n")
          return [ds]

-- raw content

-- | A raw Html Block, optionally followed by blanklines
rawHtmlBlock :: PandocMonad m => ParserT [Char] ParserState m Blocks
rawHtmlBlock = try $ do
  skipMany spaceChar
  (_,b) <- htmlTag isBlockTag
  optional blanklines
  return $ B.rawBlock "html" b

-- | Raw block of LaTeX content
rawLaTeXBlock' :: PandocMonad m => ParserT [Char] ParserState m Blocks
rawLaTeXBlock' = do
  guardEnabled Ext_raw_tex
  B.rawBlock "latex" <$> (rawLaTeXBlock <* spaces)


-- | In textile, paragraphs are separated by blank lines.
para :: PandocMonad m => ParserT [Char] ParserState m Blocks
para = B.para . trimInlines . mconcat <$> many1 inline

-- Tables

toAlignment :: Char -> Alignment
toAlignment '<' = AlignLeft
toAlignment '>' = AlignRight
toAlignment '=' = AlignCenter
toAlignment _   = AlignDefault

cellAttributes :: PandocMonad m => ParserT [Char] ParserState m (Bool, Alignment)
cellAttributes = try $ do
  isHeader <- option False (True <$ char '_')
  -- we just ignore colspan and rowspan markers:
  optional $ try $ oneOf "/\\" >> many1 digit
  -- we pay attention to alignments:
  alignment <- option AlignDefault $ toAlignment <$> oneOf "<>="
  -- ignore other attributes for now:
  _ <- attributes
  char '.'
  return (isHeader, alignment)

-- | A table cell spans until a pipe |
tableCell :: PandocMonad m => ParserT [Char] ParserState m ((Bool, Alignment), Blocks)
tableCell = try $ do
  char '|'
  (isHeader, alignment) <- option (False, AlignDefault) $ cellAttributes
  notFollowedBy blankline
  raw <- trim <$>
         many (noneOf "|\n" <|> try (char '\n' <* notFollowedBy blankline))
  content <- mconcat <$> parseFromString' (many inline) raw
  return ((isHeader, alignment), B.plain content)

-- | A table row is made of many table cells
tableRow :: PandocMonad m => ParserT [Char] ParserState m [((Bool, Alignment), Blocks)]
tableRow = try $ do
  -- skip optional row attributes
  optional $ try $ do
    _ <- attributes
    char '.'
    many1 spaceChar
  many1 tableCell <* char '|' <* blankline

-- | A table with an optional header.
table :: PandocMonad m => ParserT [Char] ParserState m Blocks
table = try $ do
  -- ignore table attributes
  caption <- option mempty $ try $ do
    string "table"
    _ <- attributes
    char '.'
    rawcapt <- trim <$> anyLine
    parseFromString' (mconcat <$> many inline) rawcapt
  rawrows <- many1 $ (skipMany ignorableRow) >> tableRow
  skipMany ignorableRow
  blanklines
  let (headers, rows) = case rawrows of
                             (toprow:rest) | any (fst . fst) toprow ->
                                (toprow, rest)
                             _ -> (mempty, rawrows)
  let nbOfCols = max (length headers) (length $ head rows)
  let aligns = map minimum $ transpose $ map (map (snd . fst)) (headers:rows)
  return $ B.table caption
    (zip aligns (replicate nbOfCols 0.0))
    (map snd headers)
    (map (map snd) rows)

-- | Ignore markers for cols, thead, tfoot.
ignorableRow :: PandocMonad m => ParserT [Char] ParserState m ()
ignorableRow = try $ do
  char '|'
  oneOf ":^-~"
  _ <- attributes
  char '.'
  _ <- anyLine
  return ()

explicitBlockStart :: PandocMonad m => String -> ParserT [Char] ParserState m ()
explicitBlockStart name = try $ do
  string name
  attributes
  char '.'
  optional whitespace
  optional endline

-- | Blocks like 'p' and 'table' do not need explicit block tag.
-- However, they can be used to set HTML/CSS attributes when needed.
maybeExplicitBlock :: PandocMonad m
                   => String  -- ^ block tag name
                   -> ParserT [Char] ParserState m Blocks -- ^ implicit block
                   -> ParserT [Char] ParserState m Blocks
maybeExplicitBlock name blk = try $ do
  optional $ explicitBlockStart name
  blk



----------
-- Inlines
----------


-- | Any inline element
inline :: PandocMonad m => ParserT [Char] ParserState m Inlines
inline = do
    choice inlineParsers <?> "inline"

-- | Inline parsers tried in order
inlineParsers :: PandocMonad m => [ParserT [Char] ParserState m Inlines]
inlineParsers = [ str
                , whitespace
                , endline
                , code
                , escapedInline
                , inlineMarkup
                , groupedInlineMarkup
                , rawHtmlInline
                , rawLaTeXInline'
                , note
                , link
                , image
                , mark
                , (B.str . (:[])) <$> characterReference
                , smartPunctuation inline
                , symbol
                ]

-- | Inline markups
inlineMarkup :: PandocMonad m => ParserT [Char] ParserState m Inlines
inlineMarkup = choice [ simpleInline (string "??") (B.cite [])
                      , simpleInline (string "**") B.strong
                      , simpleInline (string "__") B.emph
                      , simpleInline (char '*') B.strong
                      , simpleInline (char '_') B.emph
                      , simpleInline (char '+') B.emph  -- approximates underline
                      , simpleInline (char '-' <* notFollowedBy (char '-')) B.strikeout
                      , simpleInline (char '^') B.superscript
                      , simpleInline (char '~') B.subscript
                      , simpleInline (char '%') id
                      ]

-- | Trademark, registered, copyright
mark :: PandocMonad m => ParserT [Char] st m Inlines
mark = try $ char '(' >> (try tm <|> try reg <|> copy)

reg :: PandocMonad m => ParserT [Char] st m Inlines
reg = do
  oneOf "Rr"
  char ')'
  return $ B.str "\174"

tm :: PandocMonad m => ParserT [Char] st m Inlines
tm = do
  oneOf "Tt"
  oneOf "Mm"
  char ')'
  return $ B.str "\8482"

copy :: PandocMonad m => ParserT [Char] st m Inlines
copy = do
  oneOf "Cc"
  char ')'
  return $ B.str "\169"

note :: PandocMonad m => ParserT [Char] ParserState m Inlines
note = try $ do
  ref <- (char '[' *> many1 digit <* char ']')
  notes <- stateNotes <$> getState
  case lookup ref notes of
    Nothing  -> fail "note not found"
    Just raw -> B.note <$> parseFromString' parseBlocks raw

-- | Special chars
markupChars :: [Char]
markupChars = "\\*#_@~-+^|%=[]&"

-- | Break strings on following chars. Space tab and newline break for
--  inlines breaking. Open paren breaks for mark. Quote, dash and dot
--  break for smart punctuation. Punctuation breaks for regular
--  punctuation. Double quote breaks for named links. > and < break
--  for inline html.
stringBreakers :: [Char]
stringBreakers = " \t\n\r.,\"'?!;:<>«»„“”‚‘’()[]"

wordBoundaries :: [Char]
wordBoundaries = markupChars ++ stringBreakers

-- | Parse a hyphened sequence of words
hyphenedWords :: PandocMonad m => ParserT [Char] ParserState m String
hyphenedWords = do
  x <- wordChunk
  xs <-  many (try $ char '-' >> wordChunk)
  return $ intercalate "-" (x:xs)

wordChunk :: PandocMonad m => ParserT [Char] ParserState m String
wordChunk = try $ do
  hd <- noneOf wordBoundaries
  tl <- many ( (noneOf wordBoundaries) <|>
               try (notFollowedBy' note *> oneOf markupChars
                     <* lookAhead (noneOf wordBoundaries) ) )
  return $ hd:tl

-- | Any string
str :: PandocMonad m => ParserT [Char] ParserState m Inlines
str = do
  baseStr <- hyphenedWords
  -- RedCloth compliance : if parsed word is uppercase and immediatly
  -- followed by parens, parens content is unconditionally word acronym
  fullStr <- option baseStr $ try $ do
    guard $ all isUpper baseStr
    acro <- enclosed (char '(') (char ')') anyChar'
    return $ concat [baseStr, " (", acro, ")"]
  updateLastStrPos
  return $ B.str fullStr

-- | Some number of space chars
whitespace :: PandocMonad m => ParserT [Char] st m Inlines
whitespace = many1 spaceChar >> return B.space <?> "whitespace"

-- | In Textile, an isolated endline character is a line break
endline :: PandocMonad m => ParserT [Char] ParserState m Inlines
endline = try $ do
  newline
  notFollowedBy blankline
  notFollowedBy listStart
  notFollowedBy rawHtmlBlock
  return B.linebreak

rawHtmlInline :: PandocMonad m => ParserT [Char] ParserState m Inlines
rawHtmlInline = B.rawInline "html" . snd <$> htmlTag isInlineTag

-- | Raw LaTeX Inline
rawLaTeXInline' :: PandocMonad m => ParserT [Char] ParserState m Inlines
rawLaTeXInline' = try $ do
  guardEnabled Ext_raw_tex
  B.rawInline "latex" <$> rawLaTeXInline

-- | Textile standard link syntax is "label":target. But we
-- can also have ["label":target].
link :: PandocMonad m => ParserT [Char] ParserState m Inlines
link = try $ do
  bracketed <- (True <$ char '[') <|> return False
  char '"' *> notFollowedBy (oneOf " \t\n\r")
  attr <- attributes
  name <- trimInlines . mconcat <$>
          withQuoteContext InDoubleQuote (many1Till inline (char '"'))
  char ':'
  let stop = if bracketed
                then char ']'
                else lookAhead $ space <|> eof' <|>
                       try (oneOf "!.,;:" *>
                              (space <|> newline <|> eof'))
  url <- many1Till nonspaceChar stop
  let name' = if B.toList name == [Str "$"] then B.str url else name
  return $ if attr == nullAttr
              then B.link url "" name'
              else B.spanWith attr $ B.link url "" name'

-- | image embedding
image :: PandocMonad m => ParserT [Char] ParserState m Inlines
image = try $ do
  char '!' >> notFollowedBy space
  (ident, cls, kvs) <- attributes
  let attr = case lookup "style" kvs of
               Just stls -> (ident, cls, pickStylesToKVs ["width", "height"] stls)
               Nothing   -> (ident, cls, kvs)
  src <- many1 (noneOf " \t\n\r!(")
  alt <- option "" $ try $ char '(' *> manyTill anyChar (char ')')
  char '!'
  return $ B.imageWith attr src alt (B.str alt)

escapedInline :: PandocMonad m => ParserT [Char] ParserState m Inlines
escapedInline = escapedEqs <|> escapedTag

escapedEqs :: PandocMonad m => ParserT [Char] ParserState m Inlines
escapedEqs = B.str <$>
  (try $ string "==" *> manyTill anyChar' (try $ string "=="))

-- | literal text escaped btw <notextile> tags
escapedTag :: PandocMonad m => ParserT [Char] ParserState m Inlines
escapedTag = B.str <$>
  (try $ string "<notextile>" *>
         manyTill anyChar' (try $ string "</notextile>"))

-- | Any special symbol defined in wordBoundaries
symbol :: PandocMonad m => ParserT [Char] ParserState m Inlines
symbol = B.str . singleton <$> (notFollowedBy newline *>
                                notFollowedBy rawHtmlBlock *>
                                oneOf wordBoundaries)

-- | Inline code
code :: PandocMonad m => ParserT [Char] ParserState m Inlines
code = code1 <|> code2

-- any character except a newline before a blank line
anyChar' :: PandocMonad m => ParserT [Char] ParserState m Char
anyChar' =
  satisfy (/='\n') <|> (try $ char '\n' <* notFollowedBy blankline)

code1 :: PandocMonad m => ParserT [Char] ParserState m Inlines
code1 = B.code <$> surrounded (char '@') anyChar'

code2 :: PandocMonad m => ParserT [Char] ParserState m Inlines
code2 = do
  htmlTag (tagOpen (=="tt") null)
  B.code <$> manyTill anyChar' (try $ htmlTag $ tagClose (=="tt"))

-- | Html / CSS attributes
attributes :: PandocMonad m => ParserT [Char] ParserState m Attr
attributes = (foldl (flip ($)) ("",[],[])) <$>
  try (do special <- option id specialAttribute
          attrs <- many attribute
          return (special : attrs))

specialAttribute :: PandocMonad m => ParserT [Char] ParserState m (Attr -> Attr)
specialAttribute = do
  alignStr <- ("center" <$ char '=') <|>
    ("justify" <$ try (string "<>")) <|>
    ("right" <$ char '>') <|>
    ("left" <$ char '<')
  notFollowedBy spaceChar
  return $ addStyle ("text-align:" ++ alignStr)

attribute :: PandocMonad m => ParserT [Char] ParserState m (Attr -> Attr)
attribute = try $
  (classIdAttr <|> styleAttr <|> langAttr) <* notFollowedBy spaceChar

classIdAttr :: PandocMonad m => ParserT [Char] ParserState m (Attr -> Attr)
classIdAttr = try $ do -- (class class #id)
  char '('
  ws <- words `fmap` manyTill anyChar' (char ')')
  case reverse ws of
       []                      -> return $ \(_,_,keyvals) -> ("",[],keyvals)
       (('#':ident'):classes') -> return $ \(_,_,keyvals) ->
                                             (ident',classes',keyvals)
       classes'                -> return $ \(_,_,keyvals) ->
                                             ("",classes',keyvals)

styleAttr :: PandocMonad m => ParserT [Char] ParserState m (Attr -> Attr)
styleAttr = do
  style <- try $ enclosed (char '{') (char '}') anyChar'
  return $ addStyle style

addStyle :: String -> Attr -> Attr
addStyle style (id',classes,keyvals) =
  (id',classes,keyvals')
  where keyvals' = ("style", style') : [(k,v) | (k,v) <- keyvals, k /= "style"]
        style' = style ++ ";" ++ concat [v | ("style",v) <- keyvals]

langAttr :: PandocMonad m => ParserT [Char] ParserState m (Attr -> Attr)
langAttr = do
  lang <- try $ enclosed (char '[') (char ']') alphaNum
  return $ \(id',classes,keyvals) -> (id',classes,("lang",lang):keyvals)

-- | Parses material surrounded by a parser.
surrounded :: (PandocMonad m, Show t)
           => ParserT [Char] st m t   -- ^ surrounding parser
           -> ParserT [Char] st m a   -- ^ content parser (to be used repeatedly)
           -> ParserT [Char] st m [a]
surrounded border =
  enclosed (border *> notFollowedBy (oneOf " \t\n\r")) (try border)

simpleInline :: PandocMonad m
             => ParserT [Char] ParserState m t        -- ^ surrounding parser
             -> (Inlines -> Inlines)                  -- ^ Inline constructor
             -> ParserT [Char] ParserState m Inlines  -- ^ content parser (to be used repeatedly)
simpleInline border construct = try $ do
  notAfterString
  border *> notFollowedBy (oneOf " \t\n\r")
  attr <- attributes
  body <- trimInlines . mconcat <$>
          withQuoteContext InSingleQuote
            (manyTill (notFollowedBy newline >> inline)
             (try border <* notFollowedBy alphaNum))
  return $ construct $
        if attr == nullAttr
           then body
           else B.spanWith attr body

groupedInlineMarkup :: PandocMonad m => ParserT [Char] ParserState m Inlines
groupedInlineMarkup = try $ do
    char '['
    sp1 <- option mempty $ B.space <$ whitespace
    result <- withQuoteContext InSingleQuote inlineMarkup
    sp2 <- option mempty $ B.space <$ whitespace
    char ']'
    return $ sp1 <> result <> sp2

-- | Create a singleton list
singleton :: a -> [a]
singleton x = [x]

eof' :: Monad m => ParserT [Char] s m Char
eof' = '\n' <$ eof
