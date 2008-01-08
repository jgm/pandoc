{-
Copyright (C) 2006-8 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Readers.Markdown
   Copyright   : Copyright (C) 2006-8 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of markdown-formatted plain text to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Markdown ( 
                                     readMarkdown 
                                    ) where

import Data.List ( transpose, isPrefixOf, isSuffixOf, lookup, sortBy, findIndex )
import Data.Ord ( comparing )
import Data.Char ( isAlphaNum )
import Data.Maybe ( fromMaybe )
import Text.Pandoc.Definition
import Text.Pandoc.Shared 
import Text.Pandoc.Readers.LaTeX ( rawLaTeXInline, rawLaTeXEnvironment )
import Text.Pandoc.Readers.HTML ( rawHtmlBlock, anyHtmlBlockTag, 
                                  anyHtmlInlineTag, anyHtmlTag,
                                  anyHtmlEndTag, htmlEndTag, extractTagType,
                                  htmlBlockElement )
import Text.Pandoc.CharacterReferences ( decodeCharacterReferences )
import Text.ParserCombinators.Parsec

-- | Read markdown from an input string and return a Pandoc document.
readMarkdown :: ParserState -> String -> Pandoc
readMarkdown state str = (readWith parseMarkdown) state (str ++ "\n\n")

--
-- Constants and data structure definitions
--

spaceChars = " \t"
bulletListMarkers = "*+-"
hruleChars = "*-_"
setextHChars = "=-"

-- treat these as potentially non-text when parsing inline:
specialChars = "\\[]*_~`<>$!^-.&'\"\8216\8217\8220\8221"

--
-- auxiliary functions
--

indentSpaces = try $ do
  state <- getState
  let tabStop = stateTabStop state
  try (count tabStop (char ' ')) <|> 
    (many (char ' ') >> string "\t") <?> "indentation"

nonindentSpaces = do
  state <- getState
  let tabStop = stateTabStop state
  sps <- many (char ' ')
  if length sps < tabStop 
     then return sps
     else unexpected "indented line"

-- | Fail unless we're at beginning of a line.
failUnlessBeginningOfLine = do
  pos <- getPosition
  if sourceColumn pos == 1 then return () else fail "not beginning of line"

-- | Fail unless we're in "smart typography" mode.
failUnlessSmart = do
  state <- getState
  if stateSmart state then return () else fail "Smart typography feature"

-- | Parse a sequence of inline elements between square brackets,
-- including inlines between balanced pairs of square brackets.
inlinesInBalancedBrackets :: GenParser Char ParserState Inline
                          -> GenParser Char ParserState [Inline]
inlinesInBalancedBrackets parser = try $ do
  char '['
  result <- manyTill ( (do lookAhead $ try $ do (Str res) <- parser
                                                if res == "["
                                                   then return ()
                                                   else pzero
                           bal <- inlinesInBalancedBrackets parser
                           return $ [Str "["] ++ bal ++ [Str "]"])
                       <|> (count 1 parser))
                     (char ']')
  return $ concat result

--
-- document structure
--

titleLine = try $ char '%' >> skipSpaces >> manyTill inline newline

authorsLine = try $ do 
  char '%'
  skipSpaces
  authors <- sepEndBy (many1 (noneOf ",;\n")) (oneOf ",;")
  newline
  return $ map (decodeCharacterReferences . removeLeadingTrailingSpace) authors

dateLine = try $ do
  char '%'
  skipSpaces
  date <- many (noneOf "\n")
  newline
  return $ decodeCharacterReferences $ removeTrailingSpace date

titleBlock = try $ do
  failIfStrict
  title <- option [] titleLine
  author <- option [] authorsLine
  date <- option "" dateLine
  optional blanklines
  return (title, author, date)

parseMarkdown = do
  -- markdown allows raw HTML
  updateState (\state -> state { stateParseRaw = True })
  startPos <- getPosition
  -- go through once just to get list of reference keys
  -- docMinusKeys is the raw document with blanks where the keys were...
  docMinusKeys <- manyTill (referenceKey <|> lineClump) eof >>= 
                  return . concat
  setInput docMinusKeys
  setPosition startPos
  st <- getState
  -- go through again for notes unless strict...
  if stateStrict st
     then return ()
     else do docMinusNotes <- manyTill (noteBlock <|> lineClump) eof >>= 
                              return . concat
             st <- getState
             let reversedNotes = stateNotes st
             updateState $ \st -> st { stateNotes = reverse reversedNotes }
             setInput docMinusNotes
             setPosition startPos
  -- now parse it for real...
  (title, author, date) <- option ([],[],"") titleBlock
  blocks <- parseBlocks 
  return $ Pandoc (Meta title author date) $ filter (/= Null) blocks

-- 
-- initial pass for references and notes
--

referenceKey = try $ do
  startPos <- getPosition
  nonindentSpaces
  label <- reference
  char ':'
  skipSpaces
  optional (char '<')
  src <- many (noneOf "> \n\t")
  optional (char '>')
  tit <- option "" referenceTitle
  blanklines
  endPos <- getPosition
  let newkey = (label, (removeTrailingSpace src,  tit))
  st <- getState
  let oldkeys = stateKeys st
  updateState $ \st -> st { stateKeys = newkey : oldkeys }
  -- return blanks so line count isn't affected
  return $ replicate (sourceLine endPos - sourceLine startPos) '\n'

referenceTitle = try $ do 
  (many1 spaceChar >> option '\n' newline) <|> newline
  skipSpaces
  tit <-    (charsInBalanced '(' ')' >>= return . unwords . words)
        <|> do delim <- char '\'' <|> char '"'
               manyTill anyChar (try (char delim >> skipSpaces >>
                                      notFollowedBy (noneOf ")\n")))
  return $ decodeCharacterReferences tit

noteMarker = string "[^" >> manyTill (noneOf " \t\n") (char ']')

rawLine = do
  notFollowedBy blankline
  notFollowedBy' noteMarker
  contents <- many1 nonEndline
  end <- option "" (newline >> optional indentSpaces >> return "\n") 
  return $ contents ++ end

rawLines = many1 rawLine >>= return . concat

noteBlock = try $ do
  startPos <- getPosition
  ref <- noteMarker
  char ':'
  optional blankline
  optional indentSpaces
  raw <- sepBy rawLines (try (blankline >> indentSpaces))
  optional blanklines
  endPos <- getPosition
  -- parse the extracted text, which may contain various block elements:
  contents <- parseFromString parseBlocks $ (joinWithSep "\n" raw) ++ "\n\n"
  let newnote = (ref, contents)
  st <- getState
  let oldnotes = stateNotes st
  updateState $ \st -> st { stateNotes = newnote : oldnotes }
  -- return blanks so line count isn't affected
  return $ replicate (sourceLine endPos - sourceLine startPos) '\n'

--
-- parsing blocks
--

parseBlocks = manyTill block eof

block = do
  st <- getState
  choice (if stateStrict st
              then [ header
                   , codeBlock
                   , blockQuote
                   , hrule
                   , bulletList
                   , orderedList
                   , htmlBlock
                   , para
                   , plain
                   , nullBlock ]
              else [ header 
                   , table
                   , codeBlock
                   , blockQuote
                   , hrule
                   , bulletList
                   , orderedList
                   , definitionList
                   , rawLaTeXEnvironment
                   , para
                   , rawHtmlBlocks
                   , plain
                   , nullBlock ]) <?> "block"

--
-- header blocks
--

header = atxHeader <|> setextHeader <?> "header"

atxHeader = try $ do
  level <- many1 (char '#') >>= return . length
  notFollowedBy (char '.' <|> char ')') -- this would be a list
  skipSpaces
  text <- manyTill inline atxClosing >>= return . normalizeSpaces
  return $ Header level text

atxClosing = try $ skipMany (char '#') >> blanklines

setextHeader = try $ do
  text <- many1Till inline newline
  underlineChar <- oneOf setextHChars
  many (char underlineChar)
  blanklines
  let level = (fromMaybe 0 $ findIndex (== underlineChar) setextHChars) + 1
  return $ Header level (normalizeSpaces text)

--
-- hrule block
--

hrule = try $ do
  skipSpaces
  start <- oneOf hruleChars
  count 2 (skipSpaces >> char start)
  skipMany (skipSpaces >> char start)
  newline
  optional blanklines
  return HorizontalRule

--
-- code blocks
--

indentedLine = indentSpaces >> manyTill anyChar newline >>= return . (++ "\n")

codeBlock = do
  contents <- many1 (indentedLine <|> 
                     try (do b <- blanklines
                             l <- indentedLine
                             return $ b ++ l))
  optional blanklines
  return $ CodeBlock $ stripTrailingNewlines $ concat contents

--
-- block quotes
--

emailBlockQuoteStart = try $ nonindentSpaces >> char '>' >>~ optional (char ' ')

emailBlockQuote = try $ do
  emailBlockQuoteStart
  raw <- sepBy (many (nonEndline <|> 
                      (try (endline >> notFollowedBy emailBlockQuoteStart >>
                       return '\n'))))
               (try (newline >> emailBlockQuoteStart))
  newline <|> (eof >> return '\n')
  optional blanklines
  return raw

blockQuote = do 
  raw <- emailBlockQuote
  -- parse the extracted block, which may contain various block elements:
  contents <- parseFromString parseBlocks $ (joinWithSep "\n" raw) ++ "\n\n"
  return $ BlockQuote contents
 
--
-- list blocks
--

bulletListStart = try $ do
  optional newline -- if preceded by a Plain block in a list context
  nonindentSpaces
  notFollowedBy' hrule     -- because hrules start out just like lists
  oneOf bulletListMarkers
  spaceChar
  skipSpaces

anyOrderedListStart = try $ do
  optional newline -- if preceded by a Plain block in a list context
  nonindentSpaces
  notFollowedBy $ string "p." >> spaceChar >> digit  -- page number
  state <- getState
  if stateStrict state
     then do many1 digit
             char '.'
             spaceChar
             return (1, DefaultStyle, DefaultDelim)
     else anyOrderedListMarker >>~ spaceChar

orderedListStart style delim = try $ do
  optional newline -- if preceded by a Plain block in a list context
  nonindentSpaces
  state <- getState
  num <- if stateStrict state
            then do many1 digit
                    char '.'
                    return 1
            else orderedListMarker style delim 
  if delim == Period && (style == UpperAlpha || (style == UpperRoman &&
     num `elem` [1, 5, 10, 50, 100, 500, 1000]))
     then char '\t' <|> (spaceChar >> spaceChar)
     else spaceChar
  skipSpaces

-- parse a line of a list item (start = parser for beginning of list item)
listLine start = try $ do
  notFollowedBy' start
  notFollowedBy blankline
  notFollowedBy' (do indentSpaces
                     many (spaceChar)
                     bulletListStart <|> (anyOrderedListStart >> return ()))
  line <- manyTill anyChar newline
  return $ line ++ "\n"

-- parse raw text for one list item, excluding start marker and continuations
rawListItem start = try $ do
  start
  result <- many1 (listLine start)
  blanks <- many blankline
  return $ concat result ++ blanks

-- continuation of a list item - indented and separated by blankline 
-- or (in compact lists) endline.
-- note: nested lists are parsed as continuations
listContinuation start = try $ do
  lookAhead indentSpaces
  result <- many1 (listContinuationLine start)
  blanks <- many blankline
  return $ concat result ++ blanks

listContinuationLine start = try $ do
  notFollowedBy blankline
  notFollowedBy' start
  optional indentSpaces
  result <- manyTill anyChar newline
  return $ result ++ "\n"

listItem start = try $ do 
  first <- rawListItem start
  continuations <- many (listContinuation start)
  -- parsing with ListItemState forces markers at beginning of lines to
  -- count as list item markers, even if not separated by blank space.
  -- see definition of "endline"
  state <- getState
  let oldContext = stateParserContext state
  setState $ state {stateParserContext = ListItemState}
  -- parse the extracted block, which may contain various block elements:
  let raw = concat (first:continuations)
  contents <- parseFromString parseBlocks raw
  updateState (\st -> st {stateParserContext = oldContext})
  return contents

orderedList = try $ do
  (start, style, delim) <- lookAhead anyOrderedListStart
  items <- many1 (listItem (orderedListStart style delim))
  return $ OrderedList (start, style, delim) $ compactify items

bulletList = many1 (listItem bulletListStart) >>= 
             return . BulletList . compactify

-- definition lists

definitionListItem = try $ do
  notFollowedBy blankline
  notFollowedBy' indentSpaces
  -- first, see if this has any chance of being a definition list:
  lookAhead (anyLine >> char ':')
  term <- manyTill inline newline
  raw <- many1 defRawBlock
  state <- getState
  let oldContext = stateParserContext state
  -- parse the extracted block, which may contain various block elements:
  contents <- parseFromString parseBlocks $ concat raw
  updateState (\st -> st {stateParserContext = oldContext})
  return ((normalizeSpaces term), contents)

defRawBlock = try $ do
  char ':'
  state <- getState
  let tabStop = stateTabStop state
  try (count (tabStop - 1) (char ' ')) <|> (many (char ' ') >> string "\t")
  firstline <- anyLine
  rawlines <- many (notFollowedBy blankline >> indentSpaces >> anyLine)
  trailing <- option "" blanklines
  return $ firstline ++ "\n" ++ unlines rawlines ++ trailing

definitionList = do
  items <- many1 definitionListItem
  let (terms, defs) = unzip items
  let defs' = compactify defs
  let items' = zip terms defs'
  return $ DefinitionList items'

--
-- paragraph block
--

isHtmlOrBlank (HtmlInline _) = True
isHtmlOrBlank (Space) = True
isHtmlOrBlank (LineBreak) = True
isHtmlOrBlank _ = False

para = try $ do 
  result <- many1 inline
  if all isHtmlOrBlank result
     then fail "treat as raw HTML"
     else return ()
  newline
  blanklines <|> do st <- getState
                    if stateStrict st
                       then lookAhead (blockQuote <|> header) >> return ""
                       else pzero
  return $ Para $ normalizeSpaces result

plain = many1 inline >>= return . Plain . normalizeSpaces 

-- 
-- raw html
--

htmlElement = strictHtmlBlock <|> htmlBlockElement <?> "html element"

htmlBlock = try $ do
    failUnlessBeginningOfLine
    first <- htmlElement
    finalSpace <- many (oneOf spaceChars)
    finalNewlines <- many newline
    return $ RawHtml $ first ++ finalSpace ++ finalNewlines

-- True if tag is self-closing
isSelfClosing tag = 
  isSuffixOf "/>" $ filter (not . (`elem` " \n\t")) tag

strictHtmlBlock = try $ do
  tag <- anyHtmlBlockTag 
  let tag' = extractTagType tag
  if isSelfClosing tag || tag' == "hr" 
     then return tag
     else do contents <- many (notFollowedBy' (htmlEndTag tag') >> 
                               (htmlElement <|> (count 1 anyChar)))
             end <- htmlEndTag tag'
             return $ tag ++ concat contents ++ end

rawHtmlBlocks = do
  htmlBlocks <- many1 $ do (RawHtml blk) <- rawHtmlBlock
                           sps <- do sp1 <- many spaceChar
                                     sp2 <- option "" (blankline >> return "\n")
                                     sp3 <- many spaceChar
                                     sp4 <- option "" blanklines
                                     return $ sp1 ++ sp2 ++ sp3 ++ sp4
                           -- note: we want raw html to be able to
                           -- precede a code block, when separated
                           -- by a blank line
                           return $ blk ++ sps
  let combined = concat htmlBlocks
  let combined' = if last combined == '\n' then init combined else combined
  return $ RawHtml combined'

--
-- Tables
-- 

-- Parse a dashed line with optional trailing spaces; return its length
-- and the length including trailing space.
dashedLine ch = do
  dashes <- many1 (char ch)
  sp     <- many spaceChar
  return $ (length dashes, length $ dashes ++ sp)

-- Parse a table header with dashed lines of '-' preceded by 
-- one line of text.
simpleTableHeader = try $ do
  rawContent  <- anyLine
  initSp      <- nonindentSpaces
  dashes      <- many1 (dashedLine '-')
  newline
  let (lengths, lines) = unzip dashes
  let indices  = scanl (+) (length initSp) lines
  let rawHeads = tail $ splitByIndices (init indices) rawContent
  let aligns   = zipWith alignType (map (\a -> [a]) rawHeads) lengths
  return (rawHeads, aligns, indices)

-- Parse a table footer - dashed lines followed by blank line.
tableFooter = try $ nonindentSpaces >> many1 (dashedLine '-') >> blanklines

-- Parse a table separator - dashed line.
tableSep = try $ nonindentSpaces >> many1 (dashedLine '-') >> string "\n"

-- Parse a raw line and split it into chunks by indices.
rawTableLine indices = do
  notFollowedBy' (blanklines <|> tableFooter)
  line <- many1Till anyChar newline
  return $ map removeLeadingTrailingSpace $ tail $ 
           splitByIndices (init indices) line

-- Parse a table line and return a list of lists of blocks (columns).
tableLine indices = rawTableLine indices >>= mapM (parseFromString (many plain))

-- Parse a multiline table row and return a list of blocks (columns).
multilineRow indices = do
  colLines <- many1 (rawTableLine indices)
  optional blanklines
  let cols = map unlines $ transpose colLines
  mapM (parseFromString (many plain)) cols

-- Calculate relative widths of table columns, based on indices
widthsFromIndices :: Int     -- Number of columns on terminal
                  -> [Int]   -- Indices
                  -> [Float] -- Fractional relative sizes of columns
widthsFromIndices _ [] = []  
widthsFromIndices numColumns indices = 
  let lengths = zipWith (-) indices (0:indices)
      totLength = sum lengths
      quotient = if totLength > numColumns
                   then fromIntegral totLength
                   else fromIntegral numColumns
      fracs = map (\l -> (fromIntegral l) / quotient) lengths in
  tail fracs

-- Parses a table caption:  inlines beginning with 'Table:'
-- and followed by blank lines.
tableCaption = try $ do
  nonindentSpaces
  string "Table:"
  result <- many1 inline
  blanklines
  return $ normalizeSpaces result

-- Parse a table using 'headerParser', 'lineParser', and 'footerParser'.
tableWith headerParser lineParser footerParser = try $ do
    (rawHeads, aligns, indices) <- headerParser
    lines <- many1Till (lineParser indices) footerParser
    caption <- option [] tableCaption
    heads <- mapM (parseFromString (many plain)) rawHeads
    state <- getState
    let numColumns = stateColumns state
    let widths = widthsFromIndices numColumns indices
    return $ Table caption aligns widths heads lines

-- Parse a simple table with '---' header and one line per row.
simpleTable = tableWith simpleTableHeader tableLine blanklines

-- Parse a multiline table:  starts with row of '-' on top, then header
-- (which may be multiline), then the rows,
-- which may be multiline, separated by blank lines, and
-- ending with a footer (dashed line followed by blank line).
multilineTable = tableWith multilineTableHeader multilineRow tableFooter

multilineTableHeader = try $ do
  tableSep 
  rawContent  <- many1 (notFollowedBy' tableSep >> many1Till anyChar newline)
  initSp      <- nonindentSpaces
  dashes      <- many1 (dashedLine '-')
  newline
  let (lengths, lines) = unzip dashes
  let indices  = scanl (+) (length initSp) lines
  let rawHeadsList = transpose $ map 
                     (\ln -> tail $ splitByIndices (init indices) ln)
                     rawContent
  let rawHeads = map (joinWithSep " ") rawHeadsList
  let aligns   = zipWith alignType rawHeadsList lengths
  return ((map removeLeadingTrailingSpace rawHeads), aligns, indices)

-- Returns an alignment type for a table, based on a list of strings
-- (the rows of the column header) and a number (the length of the
-- dashed line under the rows.
alignType :: [String] -> Int -> Alignment
alignType []  len = AlignDefault
alignType strLst len =
  let str        = head $ sortBy (comparing length) $ 
                          map removeTrailingSpace strLst
      leftSpace  = if null str then False else (str !! 0) `elem` " \t"
      rightSpace = length str < len || (str !! (len - 1)) `elem` " \t"
  in  case (leftSpace, rightSpace) of
        (True,  False)   -> AlignRight
        (False, True)    -> AlignLeft
        (True,  True)    -> AlignCenter
        (False, False)   -> AlignDefault

table = simpleTable <|> multilineTable <?> "table"

-- 
-- inline
--

inline = choice inlineParsers <?> "inline"

inlineParsers = [ str
                , smartPunctuation
                , whitespace
                , endline
                , code
                , charRef
                , strong
                , emph
                , note
                , inlineNote
                , link
                , image
                , math
                , strikeout
                , superscript
                , subscript
                , autoLink
                , rawHtmlInline'
                , rawLaTeXInline'
                , escapedChar
                , symbol
                , ltSign ]

inlineNonLink = (choice $
                 map (\parser -> try (parser >>= failIfLink)) inlineParsers)
                <?> "inline (non-link)"

failIfLink (Link _ _) = pzero
failIfLink elt = return elt

escapedChar = do
  char '\\'
  state <- getState
  result <- option '\\' $ if stateStrict state 
                             then oneOf "\\`*_{}[]()>#+-.!~"
                             else satisfy (not . isAlphaNum)
  return $ Str [result]

ltSign = do
  st <- getState
  if stateStrict st
     then char '<'
     else notFollowedBy' rawHtmlBlocks >> char '<' -- unless it starts html
  return $ Str ['<']

specialCharsMinusLt = filter (/= '<') specialChars

symbol = do 
  result <- oneOf specialCharsMinusLt
  return $ Str [result]

-- parses inline code, between n `s and n `s
code = try $ do 
  starts <- many1 (char '`')
  skipSpaces
  result <- many1Till (many1 (noneOf "`\n") <|> many1 (char '`') <|>
                       (char '\n' >> return " ")) 
                      (try (skipSpaces >> count (length starts) (char '`') >> 
                      notFollowedBy (char '`')))
  return $ Code $ removeLeadingTrailingSpace $ concat result

mathWord = many1 ((noneOf " \t\n\\$") <|>
                  (try (char '\\') >>~ notFollowedBy (char '$')))

math = try $ do
  failIfStrict
  char '$'
  notFollowedBy space
  words <- sepBy1 mathWord (many1 space)
  char '$'
  return $ Math $ joinWithSep " " words

emph = ((enclosed (char '*') (notFollowedBy' strong >> char '*') inline) <|>
        (enclosed (char '_') (notFollowedBy' strong >> char '_' >> 
                              notFollowedBy alphaNum) inline)) >>= 
        return . Emph . normalizeSpaces

strong = ((enclosed (string "**") (try $ string "**") inline) <|> 
          (enclosed (string "__") (try $ string "__") inline)) >>=
         return . Strong . normalizeSpaces

strikeout = failIfStrict >> enclosed (string "~~") (try $ string "~~") inline >>=
            return . Strikeout . normalizeSpaces

superscript = failIfStrict >> enclosed (char '^') (char '^') 
              (notFollowedBy' whitespace >> inline) >>= -- may not contain Space
              return . Superscript

subscript = failIfStrict >> enclosed (char '~') (char '~')
            (notFollowedBy' whitespace >> inline) >>=  -- may not contain Space
            return . Subscript 

smartPunctuation = failUnlessSmart >> 
                   choice [ quoted, apostrophe, dash, ellipses ]

apostrophe = (char '\'' <|> char '\8217') >> return Apostrophe

quoted = doubleQuoted <|> singleQuoted 

withQuoteContext context parser = do
  oldState <- getState
  let oldQuoteContext = stateQuoteContext oldState
  setState oldState { stateQuoteContext = context }
  result <- parser
  newState <- getState
  setState newState { stateQuoteContext = oldQuoteContext }
  return result

singleQuoted = try $ do
  singleQuoteStart
  withQuoteContext InSingleQuote $ many1Till inline singleQuoteEnd >>=
    return . Quoted SingleQuote . normalizeSpaces

doubleQuoted = try $ do 
  doubleQuoteStart
  withQuoteContext InDoubleQuote $ many1Till inline doubleQuoteEnd >>=
    return . Quoted DoubleQuote . normalizeSpaces

failIfInQuoteContext context = do
  st <- getState
  if stateQuoteContext st == context
     then fail "already inside quotes"
     else return ()

singleQuoteStart = do 
  failIfInQuoteContext InSingleQuote
  char '\8216' <|> 
     (try $ do char '\''  
               notFollowedBy (oneOf ")!],.;:-? \t\n")
               notFollowedBy (try (oneOfStrings ["s","t","m","ve","ll","re"] >>
                                   satisfy (not . isAlphaNum))) 
                                   -- possess/contraction
               return '\'')

singleQuoteEnd = try $ do
  char '\8217' <|> char '\''
  notFollowedBy alphaNum
  return '\''

doubleQuoteStart = do
  failIfInQuoteContext InDoubleQuote
  char '\8220' <|>
     (try $ do char '"'
               notFollowedBy (oneOf " \t\n")
               return '"')

doubleQuoteEnd = char '\8221' <|> char '"'

ellipses = oneOfStrings ["...", " . . . ", ". . .", " . . ."] >> return Ellipses

dash = enDash <|> emDash

enDash = try $ char '-' >> notFollowedBy (noneOf "0123456789") >> return EnDash

emDash = try $ skipSpaces >> oneOfStrings ["---", "--"] >>
               skipSpaces >> return EmDash

whitespace = do
  sps <- many1 (oneOf spaceChars)
  if length sps >= 2
     then option Space (endline >> return LineBreak)
     else return Space <?> "whitespace"

nonEndline = satisfy (/='\n')

strChar = noneOf (specialChars ++ spaceChars ++ "\n")

str = many1 strChar >>= return . Str

-- an endline character that can be treated as a space, not a structural break
endline = try $ do
  newline
  notFollowedBy blankline
  st <- getState
  if stateStrict st 
    then do notFollowedBy emailBlockQuoteStart
            notFollowedBy (char '#')  -- atx header
    else return () 
  -- parse potential list-starts differently if in a list:
  if stateParserContext st == ListItemState
     then notFollowedBy' (bulletListStart <|> 
                          (anyOrderedListStart >> return ()))
     else return ()
  return Space

--
-- links
--

-- a reference label for a link
reference = do notFollowedBy' (string "[^")   -- footnote reference
               result <- inlinesInBalancedBrackets inlineNonLink
               return $ normalizeSpaces result

-- source for a link, with optional title
source =
  (try $ charsInBalanced '(' ')' >>= parseFromString source') <|>
  -- the following is needed for cases like:  [ref](/url(a).
  (enclosed (char '(') (char ')') anyChar >>=
   parseFromString source')

-- auxiliary function for source
source' = do
  skipSpaces
  src <- try (char '<' >>
              many (optional (char '\\') >> noneOf "> \t\n") >>~
              char '>')
         <|> many (optional (char '\\') >> noneOf " \t\n")
  tit <- option "" linkTitle
  skipSpaces
  eof
  return (removeTrailingSpace src, tit)

linkTitle = try $ do 
  (many1 spaceChar >> option '\n' newline) <|> newline
  skipSpaces
  delim <- oneOf "'\""
  tit <-   manyTill (optional (char '\\') >> anyChar)
                    (try (char delim >> skipSpaces >> eof))
  return $ decodeCharacterReferences tit

link = try $ do
  label <- reference
  src <- source <|> referenceLink label
  return $ Link label src

-- a link like [this][ref] or [this][] or [this]
referenceLink label = do
  ref <- option [] (try (optional (char ' ') >> 
                         optional (newline >> skipSpaces) >> reference))
  let ref' = if null ref then label else ref
  state <- getState
  case lookupKeySrc (stateKeys state) ref' of
     Nothing     -> fail "no corresponding key" 
     Just target -> return target 

autoLink = try $ do
  char '<'
  src <- uri <|> (emailAddress >>= (return . ("mailto:" ++)))
  char '>'
  let src' = if "mailto:" `isPrefixOf` src
                then drop 7 src
                else src 
  st <- getState
  return $ if stateStrict st
              then Link [Str src'] (src, "")
              else Link [Code src'] (src, "")

image = try $ do
  char '!'
  (Link label src) <- link
  return $ Image label src

note = try $ do
  failIfStrict
  ref <- noteMarker
  state <- getState
  let notes = stateNotes state
  case lookup ref notes of
    Nothing       -> fail "note not found"
    Just contents -> return $ Note contents

inlineNote = try $ do
  failIfStrict
  char '^'
  contents <- inlinesInBalancedBrackets inline
  return $ Note [Para contents]

rawLaTeXInline' = failIfStrict >> rawLaTeXInline

rawHtmlInline' = do
  st <- getState
  result <- if stateStrict st
               then choice [htmlBlockElement, anyHtmlTag, anyHtmlEndTag] 
               else anyHtmlInlineTag
  return $ HtmlInline result

