{-# LANGUAGE CPP #-}
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

import Data.List ( transpose, isPrefixOf, isSuffixOf, lookup, sortBy, findIndex, intercalate )
import Data.Ord ( comparing )
import Data.Char ( isAlphaNum, isAlpha, isLower, isDigit, isUpper )
import Data.Maybe
import Text.Pandoc.Definition
import Text.Pandoc.Shared 
import Text.Pandoc.Readers.LaTeX ( rawLaTeXInline, rawLaTeXEnvironment' )
import Text.Pandoc.Readers.HTML ( rawHtmlBlock, anyHtmlBlockTag, 
                                  anyHtmlInlineTag, anyHtmlTag,
                                  anyHtmlEndTag, htmlEndTag, extractTagType,
                                  htmlBlockElement, unsanitaryURI )
import Text.Pandoc.CharacterReferences ( decodeCharacterReferences )
import Text.ParserCombinators.Parsec

-- | Read markdown from an input string and return a Pandoc document.
readMarkdown :: ParserState -> String -> Pandoc
readMarkdown state s = (readWith parseMarkdown) state (s ++ "\n\n")

--
-- Constants and data structure definitions
--

spaceChars :: [Char]
spaceChars = " \t"

bulletListMarkers :: [Char]
bulletListMarkers = "*+-"

hruleChars :: [Char]
hruleChars = "*-_"

setextHChars :: [Char]
setextHChars = "=-"

-- treat these as potentially non-text when parsing inline:
specialChars :: [Char]
specialChars = "\\[]*_~`<>$!^-.&'\"\8216\8217\8220\8221"

--
-- auxiliary functions
--

indentSpaces :: GenParser Char ParserState [Char]
indentSpaces = try $ do
  state <- getState
  let tabStop = stateTabStop state
  try (count tabStop (char ' ')) <|> 
    (many (char ' ') >> string "\t") <?> "indentation"

nonindentSpaces :: GenParser Char ParserState [Char]
nonindentSpaces = do
  state <- getState
  let tabStop = stateTabStop state
  sps <- many (char ' ')
  if length sps < tabStop 
     then return sps
     else unexpected "indented line"

-- | Fail unless we're at beginning of a line.
failUnlessBeginningOfLine :: GenParser tok st () 
failUnlessBeginningOfLine = do
  pos <- getPosition
  if sourceColumn pos == 1 then return () else fail "not beginning of line"

-- | Fail unless we're in "smart typography" mode.
failUnlessSmart :: GenParser tok ParserState ()
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

titleLine :: GenParser Char ParserState [Inline]
titleLine = try $ char '%' >> skipSpaces >> manyTill inline newline

authorsLine :: GenParser Char st [String]
authorsLine = try $ do 
  char '%'
  skipSpaces
  authors <- sepEndBy (many1 (noneOf ",;\n")) (oneOf ",;")
  newline
  return $ map (decodeCharacterReferences . removeLeadingTrailingSpace) authors

dateLine :: GenParser Char st String
dateLine = try $ do
  char '%'
  skipSpaces
  date <- many (noneOf "\n")
  newline
  return $ decodeCharacterReferences $ removeTrailingSpace date

titleBlock :: GenParser Char ParserState ([Inline], [String], [Char])
titleBlock = try $ do
  failIfStrict
  title <- option [] titleLine
  author <- option [] authorsLine
  date <- option "" dateLine
  optional blanklines
  return (title, author, date)

parseMarkdown :: GenParser Char ParserState Pandoc 
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
             st' <- getState
             let reversedNotes = stateNotes st'
             updateState $ \s -> s { stateNotes = reverse reversedNotes }
             setInput docMinusNotes
             setPosition startPos
  -- now parse it for real...
  (title, author, date) <- option ([],[],"") titleBlock
  blocks <- parseBlocks
  return $ Pandoc (Meta title author date) $ filter (/= Null) blocks

-- 
-- initial pass for references and notes
--

referenceKey :: GenParser Char ParserState [Char]
referenceKey = try $ do
  startPos <- getPosition
  nonindentSpaces
  lab <- reference
  char ':'
  skipSpaces >> optional newline >> skipSpaces >> notFollowedBy (char '[')
  let sourceURL excludes = many $
        optional (char '\\') >> (noneOf (' ':excludes) <|> (notFollowedBy' referenceTitle >> char ' '))
  src <- try (char '<' >> sourceURL ">\t\n" >>~ char '>') <|> sourceURL "\t\n"
  tit <- option "" referenceTitle
  blanklines
  endPos <- getPosition
  let newkey = (lab, (intercalate "%20" $ words $ removeTrailingSpace src,  tit))
  st <- getState
  let oldkeys = stateKeys st
  updateState $ \s -> s { stateKeys = newkey : oldkeys }
  -- return blanks so line count isn't affected
  return $ replicate (sourceLine endPos - sourceLine startPos) '\n'

referenceTitle :: GenParser Char st String
referenceTitle = try $ do 
  skipSpaces >> optional newline >> skipSpaces
  tit <-    (charsInBalanced '(' ')' >>= return . unwords . words)
        <|> do delim <- char '\'' <|> char '"'
               manyTill anyChar (try (char delim >> skipSpaces >>
                                      notFollowedBy (noneOf ")\n")))
  return $ decodeCharacterReferences tit

noteMarker :: GenParser Char st [Char]
noteMarker = string "[^" >> manyTill (noneOf " \t\n") (char ']')

rawLine :: GenParser Char ParserState [Char]
rawLine = do
  notFollowedBy blankline
  notFollowedBy' noteMarker
  contents <- many1 nonEndline
  end <- option "" (newline >> optional indentSpaces >> return "\n") 
  return $ contents ++ end

rawLines :: GenParser Char ParserState [Char]
rawLines = many1 rawLine >>= return . concat

noteBlock :: GenParser Char ParserState [Char]
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
  contents <- parseFromString parseBlocks $ (intercalate "\n" raw) ++ "\n\n"
  let newnote = (ref, contents)
  st <- getState
  let oldnotes = stateNotes st
  updateState $ \s -> s { stateNotes = newnote : oldnotes }
  -- return blanks so line count isn't affected
  return $ replicate (sourceLine endPos - sourceLine startPos) '\n'

--
-- parsing blocks
--

parseBlocks :: GenParser Char ParserState [Block]
parseBlocks = manyTill block eof

block :: GenParser Char ParserState Block
block = do
  st <- getState
  choice (if stateStrict st
              then [ header
                   , codeBlockIndented
                   , blockQuote
                   , hrule
                   , bulletList
                   , orderedList
                   , htmlBlock
                   , para
                   , plain
                   , nullBlock ]
              else [ codeBlockDelimited
                   , header 
                   , table
                   , codeBlockIndented
                   , blockQuote
                   , hrule
                   , bulletList
                   , orderedList
                   , definitionList
                   , para
                   , rawHtmlBlocks
                   , plain
                   , nullBlock ]) <?> "block"

--
-- header blocks
--

header :: GenParser Char ParserState Block
header = setextHeader <|> atxHeader <?> "header"

atxHeader :: GenParser Char ParserState Block
atxHeader = try $ do
  level <- many1 (char '#') >>= return . length
  notFollowedBy (char '.' <|> char ')') -- this would be a list
  skipSpaces
  text <- manyTill inline atxClosing >>= return . normalizeSpaces
  return $ Header level text

atxClosing :: GenParser Char st [Char]
atxClosing = try $ skipMany (char '#') >> blanklines

setextHeader :: GenParser Char ParserState Block
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

hrule :: GenParser Char st Block
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

indentedLine :: GenParser Char ParserState [Char]
indentedLine = indentSpaces >> manyTill anyChar newline >>= return . (++ "\n")

codeBlockDelimiter :: Maybe Int
                   -> GenParser Char st (Int, ([Char], [[Char]], [([Char], [Char])]))
codeBlockDelimiter len = try $ do
  size <- case len of
              Just l  -> count l (char '~') >> many (char '~') >> return l
              Nothing -> count 3 (char '~') >> many (char '~') >>= 
                         return . (+ 3) . length 
  many spaceChar
  attr <- option ([],[],[]) attributes
  blankline
  return (size, attr) 

attributes :: GenParser Char st ([Char], [[Char]], [([Char], [Char])])
attributes = try $ do
  char '{'
  many spaceChar
  attrs <- many (attribute >>~ many spaceChar)
  char '}'
  let (ids, classes, keyvals) = unzip3 attrs
  let id' = if null ids then "" else head ids
  return (id', concat classes, concat keyvals)  

attribute :: GenParser Char st ([Char], [[Char]], [([Char], [Char])])
attribute = identifierAttr <|> classAttr <|> keyValAttr

identifier :: GenParser Char st [Char]
identifier = do
  first <- letter
  rest <- many alphaNum
  return (first:rest)

identifierAttr :: GenParser Char st ([Char], [a], [a1])
identifierAttr = try $ do
  char '#'
  result <- identifier
  return (result,[],[])

classAttr :: GenParser Char st ([Char], [[Char]], [a])
classAttr = try $ do
  char '.'
  result <- identifier
  return ("",[result],[])

keyValAttr :: GenParser Char st ([Char], [a], [([Char], [Char])])
keyValAttr = try $ do
  key <- identifier
  char '='
  char '"'
  val <- manyTill (noneOf "\n") (char '"')
  return ("",[],[(key,val)])

codeBlockDelimited :: GenParser Char st Block
codeBlockDelimited = try $ do
  (size, attr) <- codeBlockDelimiter Nothing
  optional blankline   -- this helps make literate haskell possible; it requires
                       -- a blank line between comment and code
  contents <- manyTill anyLine (codeBlockDelimiter (Just size))
  blanklines
  return $ CodeBlock attr $ intercalate "\n" contents

codeBlockIndented :: GenParser Char ParserState Block
codeBlockIndented = do
  contents <- many1 (indentedLine <|> 
                     try (do b <- blanklines
                             l <- indentedLine
                             return $ b ++ l))
  optional blanklines
  return $ CodeBlock ("",[],[]) $ stripTrailingNewlines $ concat contents

--
-- block quotes
--

emailBlockQuoteStart :: GenParser Char ParserState Char
emailBlockQuoteStart = try $ nonindentSpaces >> char '>' >>~ optional (char ' ')

emailBlockQuote :: GenParser Char ParserState [[Char]]
emailBlockQuote = try $ do
  emailBlockQuoteStart
  raw <- sepBy (many (nonEndline <|> 
                      (try (endline >> notFollowedBy emailBlockQuoteStart >>
                       return '\n'))))
               (try (newline >> emailBlockQuoteStart))
  newline <|> (eof >> return '\n')
  optional blanklines
  return raw

blockQuote :: GenParser Char ParserState Block
blockQuote = do 
  raw <- emailBlockQuote
  -- parse the extracted block, which may contain various block elements:
  contents <- parseFromString parseBlocks $ (intercalate "\n" raw) ++ "\n\n"
  return $ BlockQuote contents
 
--
-- list blocks
--

bulletListStart :: GenParser Char ParserState ()
bulletListStart = try $ do
  optional newline -- if preceded by a Plain block in a list context
  nonindentSpaces
  notFollowedBy' hrule     -- because hrules start out just like lists
  oneOf bulletListMarkers
  spaceChar
  skipSpaces

anyOrderedListStart :: GenParser Char ParserState (Int, ListNumberStyle, ListNumberDelim) 
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
     else do (num, style, delim) <- anyOrderedListMarker
             -- if it could be an abbreviated first name, insist on more than one space
             if delim == Period && (style == UpperAlpha || (style == UpperRoman &&
                num `elem` [1, 5, 10, 50, 100, 500, 1000]))
                then char '\t' <|> (char ' ' >>~ notFollowedBy (satisfy isUpper))
                else spaceChar
             skipSpaces
             return (num, style, delim)

listStart :: GenParser Char ParserState ()
listStart = bulletListStart <|> (anyOrderedListStart >> return ())

-- parse a line of a list item (start = parser for beginning of list item)
listLine :: GenParser Char ParserState [Char]
listLine = try $ do
  notFollowedBy' listStart
  notFollowedBy blankline
  notFollowedBy' (do indentSpaces
                     many (spaceChar)
                     listStart)
  line <- manyTill anyChar newline
  return $ line ++ "\n"

-- parse raw text for one list item, excluding start marker and continuations
rawListItem :: GenParser Char ParserState [Char]
rawListItem = try $ do
  listStart
  result <- many1 listLine
  blanks <- many blankline
  return $ concat result ++ blanks

-- continuation of a list item - indented and separated by blankline 
-- or (in compact lists) endline.
-- note: nested lists are parsed as continuations
listContinuation :: GenParser Char ParserState [Char]
listContinuation = try $ do
  lookAhead indentSpaces
  result <- many1 listContinuationLine
  blanks <- many blankline
  return $ concat result ++ blanks

listContinuationLine :: GenParser Char ParserState [Char]
listContinuationLine = try $ do
  notFollowedBy blankline
  notFollowedBy' listStart
  optional indentSpaces
  result <- manyTill anyChar newline
  return $ result ++ "\n"

listItem :: GenParser Char ParserState [Block]
listItem = try $ do 
  first <- rawListItem
  continuations <- many listContinuation
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

orderedList :: GenParser Char ParserState Block
orderedList = try $ do
  (start, style, delim) <- lookAhead anyOrderedListStart
  items <- many1 listItem
  return $ OrderedList (start, style, delim) $ compactify items

bulletList :: GenParser Char ParserState Block
bulletList = try $ do
  lookAhead bulletListStart
  many1 listItem >>= return . BulletList . compactify

-- definition lists

definitionListItem :: GenParser Char ParserState ([Inline], [Block])
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

defRawBlock :: GenParser Char ParserState [Char]
defRawBlock = try $ do
  char ':'
  state <- getState
  let tabStop = stateTabStop state
  try (count (tabStop - 1) (char ' ')) <|> (many (char ' ') >> string "\t")
  firstline <- anyLine
  rawlines <- many (notFollowedBy blankline >> indentSpaces >> anyLine)
  trailing <- option "" blanklines
  return $ firstline ++ "\n" ++ unlines rawlines ++ trailing

definitionList :: GenParser Char ParserState Block
definitionList = do
  items <- many1 definitionListItem
  let (terms, defs) = unzip items
  let defs' = compactify defs
  let items' = zip terms defs'
  return $ DefinitionList items'

--
-- paragraph block
--

isHtmlOrBlank :: Inline -> Bool
isHtmlOrBlank (HtmlInline _) = True
isHtmlOrBlank (Space)        = True
isHtmlOrBlank (LineBreak)    = True
isHtmlOrBlank _              = False

para :: GenParser Char ParserState Block
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

plain :: GenParser Char ParserState Block
plain = many1 inline >>= return . Plain . normalizeSpaces 

-- 
-- raw html
--

htmlElement :: GenParser Char ParserState [Char]
htmlElement = strictHtmlBlock <|> htmlBlockElement <?> "html element"

htmlBlock :: GenParser Char ParserState Block
htmlBlock = try $ do
    failUnlessBeginningOfLine
    first <- htmlElement
    finalSpace <- many (oneOf spaceChars)
    finalNewlines <- many newline
    return $ RawHtml $ first ++ finalSpace ++ finalNewlines

-- True if tag is self-closing
isSelfClosing :: [Char] -> Bool
isSelfClosing tag = 
  isSuffixOf "/>" $ filter (not . (`elem` " \n\t")) tag

strictHtmlBlock :: GenParser Char ParserState [Char]
strictHtmlBlock = try $ do
  tag <- anyHtmlBlockTag 
  let tag' = extractTagType tag
  if isSelfClosing tag || tag' == "hr" 
     then return tag
     else do contents <- many (notFollowedBy' (htmlEndTag tag') >> 
                               (htmlElement <|> (count 1 anyChar)))
             end <- htmlEndTag tag'
             return $ tag ++ concat contents ++ end

rawHtmlBlocks :: GenParser Char ParserState Block
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
dashedLine :: Char 
           -> GenParser Char st (Int, Int)
dashedLine ch = do
  dashes <- many1 (char ch)
  sp     <- many spaceChar
  return $ (length dashes, length $ dashes ++ sp)

-- Parse a table header with dashed lines of '-' preceded by 
-- one line of text.
simpleTableHeader :: GenParser Char ParserState ([[Char]], [Alignment], [Int])
simpleTableHeader = try $ do
  rawContent  <- anyLine
  initSp      <- nonindentSpaces
  dashes      <- many1 (dashedLine '-')
  newline
  let (lengths, lines') = unzip dashes
  let indices  = scanl (+) (length initSp) lines'
  let rawHeads = tail $ splitByIndices (init indices) rawContent
  let aligns   = zipWith alignType (map (\a -> [a]) rawHeads) lengths
  return (rawHeads, aligns, indices)

-- Parse a table footer - dashed lines followed by blank line.
tableFooter :: GenParser Char ParserState [Char]
tableFooter = try $ nonindentSpaces >> many1 (dashedLine '-') >> blanklines

-- Parse a table separator - dashed line.
tableSep :: GenParser Char ParserState String
tableSep = try $ nonindentSpaces >> many1 (dashedLine '-') >> string "\n"

-- Parse a raw line and split it into chunks by indices.
rawTableLine :: [Int]
             -> GenParser Char ParserState [String]
rawTableLine indices = do
  notFollowedBy' (blanklines <|> tableFooter)
  line <- many1Till anyChar newline
  return $ map removeLeadingTrailingSpace $ tail $ 
           splitByIndices (init indices) line

-- Parse a table line and return a list of lists of blocks (columns).
tableLine :: [Int]
          -> GenParser Char ParserState [[Block]]
tableLine indices = rawTableLine indices >>= mapM (parseFromString (many plain))

-- Parse a multiline table row and return a list of blocks (columns).
multilineRow :: [Int]
             -> GenParser Char ParserState [[Block]]
multilineRow indices = do
  colLines <- many1 (rawTableLine indices)
  optional blanklines
  let cols = map unlines $ transpose colLines
  mapM (parseFromString (many plain)) cols

-- Calculate relative widths of table columns, based on indices
widthsFromIndices :: Int      -- Number of columns on terminal
                  -> [Int]    -- Indices
                  -> [Double] -- Fractional relative sizes of columns
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
tableCaption :: GenParser Char ParserState [Inline]
tableCaption = try $ do
  nonindentSpaces
  string "Table:"
  result <- many1 inline
  blanklines
  return $ normalizeSpaces result

-- Parse a table using 'headerParser', 'lineParser', and 'footerParser'.
tableWith :: GenParser Char ParserState ([[Char]], [Alignment], [Int])
          -> ([Int] -> GenParser Char ParserState [[Block]])
          -> GenParser Char ParserState end
          -> GenParser Char ParserState Block
tableWith headerParser lineParser footerParser = try $ do
    (rawHeads, aligns, indices) <- headerParser
    lines' <- many1Till (lineParser indices) footerParser
    caption <- option [] tableCaption
    heads <- mapM (parseFromString (many plain)) rawHeads
    state <- getState
    let numColumns = stateColumns state
    let widths = widthsFromIndices numColumns indices
    return $ Table caption aligns widths heads lines'

-- Parse a simple table with '---' header and one line per row.
simpleTable :: GenParser Char ParserState Block
simpleTable = tableWith simpleTableHeader tableLine blanklines

-- Parse a multiline table:  starts with row of '-' on top, then header
-- (which may be multiline), then the rows,
-- which may be multiline, separated by blank lines, and
-- ending with a footer (dashed line followed by blank line).
multilineTable :: GenParser Char ParserState Block
multilineTable = tableWith multilineTableHeader multilineRow tableFooter

multilineTableHeader :: GenParser Char ParserState ([String], [Alignment], [Int])
multilineTableHeader = try $ do
  tableSep 
  rawContent  <- many1 (notFollowedBy' tableSep >> many1Till anyChar newline)
  initSp      <- nonindentSpaces
  dashes      <- many1 (dashedLine '-')
  newline
  let (lengths, lines') = unzip dashes
  let indices  = scanl (+) (length initSp) lines'
  let rawHeadsList = transpose $ map 
                     (\ln -> tail $ splitByIndices (init indices) ln)
                     rawContent
  let rawHeads = map (intercalate " ") rawHeadsList
  let aligns   = zipWith alignType rawHeadsList lengths
  return ((map removeLeadingTrailingSpace rawHeads), aligns, indices)

-- Returns an alignment type for a table, based on a list of strings
-- (the rows of the column header) and a number (the length of the
-- dashed line under the rows.
alignType :: [String]
          -> Int
          -> Alignment
alignType [] _ = AlignDefault
alignType strLst len =
  let s          = head $ sortBy (comparing length) $ 
                          map removeTrailingSpace strLst
      leftSpace  = if null s then False else (s !! 0) `elem` " \t"
      rightSpace = length s < len || (s !! (len - 1)) `elem` " \t"
  in  case (leftSpace, rightSpace) of
        (True,  False)   -> AlignRight
        (False, True)    -> AlignLeft
        (True,  True)    -> AlignCenter
        (False, False)   -> AlignDefault

table :: GenParser Char ParserState Block
table = simpleTable <|> multilineTable <?> "table"

-- 
-- inline
--

inline :: GenParser Char ParserState Inline
inline = choice inlineParsers <?> "inline"

inlineParsers :: [GenParser Char ParserState Inline]
inlineParsers = [ abbrev
                , str
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
#ifdef _CITEPROC
                , inlineCitation
#endif
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

inlineNonLink :: GenParser Char ParserState Inline
inlineNonLink = (choice $
                 map (\parser -> try (parser >>= failIfLink)) inlineParsers)
                <?> "inline (non-link)"

failIfLink :: Inline -> GenParser tok st Inline
failIfLink (Link _ _) = pzero
failIfLink elt        = return elt

escapedChar :: GenParser Char ParserState Inline
escapedChar = do
  char '\\'
  state <- getState
  result <- option '\\' $ if stateStrict state 
                             then oneOf "\\`*_{}[]()>#+-.!~"
                             else satisfy (not . isAlphaNum)
  let result' = if result == ' '
                   then '\160'  -- '\ ' is a nonbreaking space
                   else result
  return $ Str [result']

ltSign :: GenParser Char ParserState Inline
ltSign = do
  st <- getState
  if stateStrict st
     then char '<'
     else notFollowedBy' rawHtmlBlocks >> char '<' -- unless it starts html
  return $ Str ['<']

specialCharsMinusLt :: [Char]
specialCharsMinusLt = filter (/= '<') specialChars

symbol :: GenParser Char ParserState Inline
symbol = do 
  result <- oneOf specialCharsMinusLt
  return $ Str [result]

-- parses inline code, between n `s and n `s
code :: GenParser Char ParserState Inline
code = try $ do 
  starts <- many1 (char '`')
  skipSpaces
  result <- many1Till (many1 (noneOf "`\n") <|> many1 (char '`') <|>
                       (char '\n' >> return " ")) 
                      (try (skipSpaces >> count (length starts) (char '`') >> 
                      notFollowedBy (char '`')))
  return $ Code $ removeLeadingTrailingSpace $ concat result

mathWord :: GenParser Char st [Char]
mathWord = many1 ((noneOf " \t\n\\$") <|>
                  (try (char '\\') >>~ notFollowedBy (char '$')))

math :: GenParser Char ParserState Inline
math = (mathDisplay >>= return . Math DisplayMath)
     <|> (mathInline >>= return . Math InlineMath)

mathDisplay :: GenParser Char ParserState String 
mathDisplay = try $ char '$' >> mathInline >>~ char '$' >>~ notFollowedBy digit

mathInline :: GenParser Char ParserState String
mathInline = try $ do
  failIfStrict
  char '$'
  notFollowedBy space
  words' <- sepBy1 mathWord (many1 (spaceChar <|> (newline >>~ notFollowedBy' blankline)))
  char '$'
  notFollowedBy digit
  return $ intercalate " " words'

emph :: GenParser Char ParserState Inline
emph = ((enclosed (char '*') (notFollowedBy' strong >> char '*') inline) <|>
        (enclosed (char '_') (notFollowedBy' strong >> char '_' >> 
                              notFollowedBy alphaNum) inline)) >>= 
        return . Emph . normalizeSpaces

strong :: GenParser Char ParserState Inline
strong = ((enclosed (string "**") (try $ string "**") inline) <|> 
          (enclosed (string "__") (try $ string "__") inline)) >>=
         return . Strong . normalizeSpaces

strikeout :: GenParser Char ParserState Inline
strikeout = failIfStrict >> enclosed (string "~~") (try $ string "~~") inline >>=
            return . Strikeout . normalizeSpaces

superscript :: GenParser Char ParserState Inline
superscript = failIfStrict >> enclosed (char '^') (char '^') 
              (notFollowedBy' whitespace >> inline) >>= -- may not contain Space
              return . Superscript

subscript :: GenParser Char ParserState Inline
subscript = failIfStrict >> enclosed (char '~') (char '~')
            (notFollowedBy' whitespace >> inline) >>=  -- may not contain Space
            return . Subscript 

abbrev :: GenParser Char ParserState Inline
abbrev = failUnlessSmart >>
         (assumedAbbrev <|> knownAbbrev) >>= return . Str . (++ ".\160")

-- an string of letters followed by a period that does not end a sentence
-- is assumed to be an abbreviation.  It is assumed that sentences don't
-- start with lowercase letters or numerals.
assumedAbbrev :: GenParser Char ParserState [Char]
assumedAbbrev = try $ do
  result <- many1 $ satisfy isAlpha
  string ". "
  lookAhead $ satisfy (\x -> isLower x || isDigit x)
  return result

-- these strings are treated as abbreviations even if they are followed
-- by a capital letter (such as a name).
knownAbbrev :: GenParser Char ParserState [Char]
knownAbbrev = try $ do
  result <- oneOfStrings [ "Mr", "Mrs", "Ms", "Capt", "Dr", "Prof", "Gen",
                           "Gov", "e.g", "i.e", "Sgt", "St", "vol", "vs",
                           "Sen", "Rep", "Pres", "Hon", "Rev" ]
  string ". "
  return result

smartPunctuation :: GenParser Char ParserState Inline
smartPunctuation = failUnlessSmart >> 
                   choice [ quoted, apostrophe, dash, ellipses ]

apostrophe :: GenParser Char ParserState Inline
apostrophe = (char '\'' <|> char '\8217') >> return Apostrophe

quoted :: GenParser Char ParserState Inline
quoted = doubleQuoted <|> singleQuoted 

withQuoteContext :: QuoteContext
                 -> (GenParser Char ParserState Inline)
                 -> GenParser Char ParserState Inline
withQuoteContext context parser = do
  oldState <- getState
  let oldQuoteContext = stateQuoteContext oldState
  setState oldState { stateQuoteContext = context }
  result <- parser
  newState <- getState
  setState newState { stateQuoteContext = oldQuoteContext }
  return result

singleQuoted :: GenParser Char ParserState Inline
singleQuoted = try $ do
  singleQuoteStart
  withQuoteContext InSingleQuote $ many1Till inline singleQuoteEnd >>=
    return . Quoted SingleQuote . normalizeSpaces

doubleQuoted :: GenParser Char ParserState Inline
doubleQuoted = try $ do 
  doubleQuoteStart
  withQuoteContext InDoubleQuote $ many1Till inline doubleQuoteEnd >>=
    return . Quoted DoubleQuote . normalizeSpaces

failIfInQuoteContext :: QuoteContext -> GenParser tok ParserState ()
failIfInQuoteContext context = do
  st <- getState
  if stateQuoteContext st == context
     then fail "already inside quotes"
     else return ()

singleQuoteStart :: GenParser Char ParserState Char
singleQuoteStart = do 
  failIfInQuoteContext InSingleQuote
  char '\8216' <|> 
     (try $ do char '\''  
               notFollowedBy (oneOf ")!],.;:-? \t\n")
               notFollowedBy (try (oneOfStrings ["s","t","m","ve","ll","re"] >>
                                   satisfy (not . isAlphaNum))) 
                                   -- possess/contraction
               return '\'')

singleQuoteEnd :: GenParser Char st Char
singleQuoteEnd = try $ do
  char '\8217' <|> char '\''
  notFollowedBy alphaNum
  return '\''

doubleQuoteStart :: GenParser Char ParserState Char
doubleQuoteStart = do
  failIfInQuoteContext InDoubleQuote
  char '\8220' <|>
     (try $ do char '"'
               notFollowedBy (oneOf " \t\n")
               return '"')

doubleQuoteEnd :: GenParser Char st Char
doubleQuoteEnd = char '\8221' <|> char '"'

ellipses :: GenParser Char st Inline
ellipses = oneOfStrings ["...", " . . . ", ". . .", " . . ."] >> return Ellipses

dash :: GenParser Char st Inline
dash = enDash <|> emDash

enDash :: GenParser Char st Inline
enDash = try $ char '-' >> notFollowedBy (noneOf "0123456789") >> return EnDash

emDash :: GenParser Char st Inline
emDash = oneOfStrings ["---", "--"] >> return EmDash

whitespace :: GenParser Char ParserState Inline
whitespace = do
  sps <- many1 (oneOf spaceChars)
  if length sps >= 2
     then option Space (endline >> return LineBreak)
     else return Space <?> "whitespace"

nonEndline :: GenParser Char st Char
nonEndline = satisfy (/='\n')

strChar :: GenParser Char st Char
strChar = noneOf (specialChars ++ spaceChars ++ "\n")

str :: GenParser Char st Inline
str = many1 strChar >>= return . Str

-- an endline character that can be treated as a space, not a structural break
endline :: GenParser Char ParserState Inline
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
reference :: GenParser Char ParserState [Inline]
reference = do notFollowedBy' (string "[^")   -- footnote reference
               result <- inlinesInBalancedBrackets inlineNonLink
               return $ normalizeSpaces result

-- source for a link, with optional title
source :: GenParser Char st (String, [Char])
source =
  (try $ charsInBalanced '(' ')' >>= parseFromString source') <|>
  -- the following is needed for cases like:  [ref](/url(a).
  (enclosed (char '(') (char ')') anyChar >>=
   parseFromString source')

-- auxiliary function for source
source' :: GenParser Char st (String, [Char])
source' = do
  skipSpaces
  let sourceURL excludes = many $
        optional (char '\\') >> (noneOf (' ':excludes) <|> (notFollowedBy' linkTitle >> char ' '))
  src <- try (char '<' >> sourceURL ">\t\n" >>~ char '>') <|> sourceURL "\t\n"
  tit <- option "" linkTitle
  skipSpaces
  eof
  return (intercalate "%20" $ words $ removeTrailingSpace src, tit)

linkTitle :: GenParser Char st String
linkTitle = try $ do 
  (many1 spaceChar >> option '\n' newline) <|> newline
  skipSpaces
  delim <- oneOf "'\""
  tit <-   manyTill (optional (char '\\') >> anyChar)
                    (try (char delim >> skipSpaces >> eof))
  return $ decodeCharacterReferences tit

link :: GenParser Char ParserState Inline
link = try $ do
  lab <- reference
  src <- source <|> referenceLink lab
  sanitize <- getState >>= return . stateSanitizeHTML
  if sanitize && unsanitaryURI (fst src)
     then fail "Unsanitary URI"
     else return $ Link lab src

-- a link like [this][ref] or [this][] or [this]
referenceLink :: [Inline]
              -> GenParser Char ParserState (String, [Char])
referenceLink lab = do
  ref <- option [] (try (optional (char ' ') >> 
                         optional (newline >> skipSpaces) >> reference))
  let ref' = if null ref then lab else ref
  state <- getState
  case lookupKeySrc (stateKeys state) ref' of
     Nothing     -> fail "no corresponding key" 
     Just target -> return target 

autoLink :: GenParser Char ParserState Inline
autoLink = try $ do
  char '<'
  src <- uri <|> (emailAddress >>= (return . ("mailto:" ++)))
  char '>'
  let src' = if "mailto:" `isPrefixOf` src
                then drop 7 src
                else src 
  st <- getState
  let sanitize = stateSanitizeHTML st
  if sanitize && unsanitaryURI src
     then fail "Unsanitary URI"
     else return $ if stateStrict st
                      then Link [Str src'] (src, "")
                      else Link [Code src'] (src, "")

image :: GenParser Char ParserState Inline
image = try $ do
  char '!'
  (Link lab src) <- link
  return $ Image lab src

note :: GenParser Char ParserState Inline
note = try $ do
  failIfStrict
  ref <- noteMarker
  state <- getState
  let notes = stateNotes state
  case lookup ref notes of
    Nothing       -> fail "note not found"
    Just contents -> return $ Note contents

inlineNote :: GenParser Char ParserState Inline
inlineNote = try $ do
  failIfStrict
  char '^'
  contents <- inlinesInBalancedBrackets inline
  return $ Note [Para contents]

rawLaTeXInline' :: GenParser Char ParserState Inline
rawLaTeXInline' = do
  failIfStrict
  (rawConTeXtEnvironment' >>= return . TeX)
    <|> (rawLaTeXEnvironment' >>= return . TeX)
    <|> rawLaTeXInline

rawConTeXtEnvironment' :: GenParser Char st String
rawConTeXtEnvironment' = try $ do
  string "\\start"
  completion <- inBrackets (letter <|> digit <|> spaceChar)
               <|> (many1 letter)
  contents <- manyTill (rawConTeXtEnvironment' <|> (count 1 anyChar))
                       (try $ string "\\stop" >> string completion)
  return $ "\\start" ++ completion ++ concat contents ++ "\\stop" ++ completion

inBrackets :: (GenParser Char st Char) -> GenParser Char st String
inBrackets parser = do
  char '['
  contents <- many parser
  char ']'
  return $ "[" ++ contents ++ "]"

rawHtmlInline' :: GenParser Char ParserState Inline
rawHtmlInline' = do
  st <- getState
  result <- if stateStrict st
               then choice [htmlBlockElement, anyHtmlTag, anyHtmlEndTag] 
               else anyHtmlInlineTag
  return $ HtmlInline result

#ifdef _CITEPROC
inlineCitation :: GenParser Char ParserState Inline
inlineCitation = try $ do
  failIfStrict
  cit <- citeMarker
  let citations = readWith parseCitation defaultParserState cit
  mr <- mapM chkCit citations
  if catMaybes mr /= []
     then return $ Cite citations []
     else fail "no citation found"

chkCit :: Target -> GenParser Char ParserState (Maybe Target)
chkCit t = do
  st <- getState
  case lookupKeySrc (stateKeys st) [Str $ fst t] of
     Just  _ -> fail "This is a link"
     Nothing -> if elem (fst t) $ stateCitations st
                   then return $ Just t
                   else return $ Nothing

citeMarker :: GenParser Char ParserState String
citeMarker = char '[' >> manyTill ( noneOf "\n" <|> (newline >>~ notFollowedBy blankline) ) (char ']')

parseCitation :: GenParser Char ParserState [(String,String)]
parseCitation = try $ sepBy (parseLabel) (oneOf ";")

parseLabel :: GenParser Char ParserState (String,String)
parseLabel = try $ do
  res <- sepBy (skipSpaces >> optional newline >> skipSpaces >> many1 (noneOf "@;")) (oneOf "@")
  case res of
    [lab,loc] -> return (lab, loc)
    [lab]     -> return (lab, "" )
    _         -> return ("" , "" )

#endif
