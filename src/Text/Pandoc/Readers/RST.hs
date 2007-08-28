{-
Copyright (C) 2006-7 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Readers.RST 
   Copyright   : Copyright (C) 2006-7 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion from reStructuredText to 'Pandoc' document.
-}
module Text.Pandoc.Readers.RST ( 
                                readRST
                               ) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared 
import Text.ParserCombinators.Parsec
import Data.List ( findIndex, delete )

-- | Parse reStructuredText string and return Pandoc document.
readRST :: ParserState -> String -> Pandoc
readRST state str = (readWith parseRST) state (str ++ "\n\n")

--
-- Constants and data structure definitions
---

bulletListMarkers = "*+-"
underlineChars = "!\"#$&'()*+,-./:;<=>?@[\\]^_`{|}~"

-- treat these as potentially non-text when parsing inline:
specialChars = "\\`|*_<>$:[-"

--
-- parsing documents
--

isAnonKey (ref, src) = ref == [Str "_"]

isHeader :: Int -> Block -> Bool
isHeader n (Header x _) = x == n
isHeader _ _ = False

-- | Promote all headers in a list of blocks.  (Part of
-- title transformation for RST.)
promoteHeaders :: Int -> [Block] -> [Block]
promoteHeaders num ((Header level text):rest) = 
    (Header (level - num) text):(promoteHeaders num rest)
promoteHeaders num (other:rest) = other:(promoteHeaders num rest)
promoteHeaders num [] = []

-- | If list of blocks starts with a header (or a header and subheader)
-- of level that are not found elsewhere, return it as a title and
-- promote all the other headers. 
titleTransform :: [Block]              -- ^ list of blocks
               -> ([Block], [Inline])  -- ^ modified list of blocks, title
titleTransform ((Header 1 head1):(Header 2 head2):rest) =  -- title subtitle
    if (any (isHeader 1) rest) || (any (isHeader 2) rest)
       then ((Header 1 head1):(Header 2 head2):rest, [])
       else ((promoteHeaders 2 rest), head1 ++ [Str ":", Space] ++ head2)
titleTransform ((Header 1 head1):rest) =       -- title, no subtitle
    if (any (isHeader 1) rest)
       then ((Header 1 head1):rest, [])
       else ((promoteHeaders 1 rest), head1)
titleTransform blocks = (blocks, [])

parseRST = do
  -- first pass: get keys
  refs <- manyTill (referenceKey <|> (lineClump >>= return . LineClump)) eof
  let keys = map (\(KeyBlock label target) -> (label, target)) $
                 filter isKeyBlock refs
  -- second pass, with keys stripped out
  let rawlines = map (\(LineClump ln) -> ln) $ filter isLineClump refs
  setInput $ concat rawlines 
  updateState (\state -> state { stateKeys = keys })
  blocks <- parseBlocks
  let blocks' = filter (/= Null) blocks
  state <- getState
  let (blocks'', title) = if stateStandalone state
                              then titleTransform blocks'
                              else (blocks', [])
  let authors = stateAuthors state
  let date = stateDate state
  let title' = if (null title) then (stateTitle state) else title
  return $ Pandoc (Meta title' authors date) blocks''

--
-- parsing blocks
--

parseBlocks = manyTill block eof

block = choice [ codeBlock
               , rawHtmlBlock
               , rawLaTeXBlock
               , blockQuote
               , imageBlock
               , unknownDirective
               , header
               , hrule
               , list
               , fieldList
               , lineBlock
               , para
               , plain
               , nullBlock ] <?> "block"

--
-- field list
--

fieldListItem = try $ do
  char ':'
  name <- many1 alphaNum
  string ": "
  skipSpaces
  first <- manyTill anyChar newline
  rest <- many (notFollowedBy ((char ':') <|> blankline)  >>
                skipSpaces >> manyTill anyChar newline)
  return $ (name, (joinWithSep " " (first:rest)))

fieldList = try $ do
  items <- many1 fieldListItem
  blanklines
  let authors = case lookup "Authors" items of
                  Just auth -> [auth]
                  Nothing  -> map snd (filter (\(x,y) -> x == "Author") items)
  let date = case (lookup "Date" items) of
                  Just dat -> dat
                  Nothing  -> ""
  let title = case (lookup "Title" items) of
                  Just tit -> [Str tit]
                  Nothing  -> []
  let remaining = filter (\(x,y) -> (x /= "Authors") && (x /= "Author") && 
                  (x /= "Date") && (x /= "Title")) items
  let result = map (\(x,y) -> 
               Para [Strong [Str x], Str ":", Space, Str y]) remaining
  updateState (\st -> st { stateAuthors = authors, 
                           stateDate = date, 
                           stateTitle = title })
  return $ BlockQuote result

--
-- line block
--

lineBlockLine = try $ do
  string "| "
  white <- many (oneOf " \t")
  line <- manyTill inline newline
  return $ (if null white then [] else [Str white]) ++ line ++ [LineBreak]

lineBlock = try $ do
  lines <- many1 lineBlockLine
  blanklines
  return $ Para (concat lines)

--
-- paragraph block
--

para = paraBeforeCodeBlock <|> paraNormal <?> "paragraph"

codeBlockStart = try $ string "::" >> blankline >> blankline

-- paragraph that ends in a :: starting a code block
paraBeforeCodeBlock = try $ do
  result <- many1 (notFollowedBy' codeBlockStart >> inline)
  lookAhead (string "::")
  return $ Para $ if last result == Space
                     then normalizeSpaces result
                     else (normalizeSpaces result) ++ [Str ":"]

-- regular paragraph
paraNormal = try $ do 
  result <- many1 inline
  newline
  blanklines
  return $ Para $ normalizeSpaces result

plain = many1 inline >>= return . Plain . normalizeSpaces 

--
-- image block
--

imageBlock = try $ do
  string ".. image:: "
  src <- manyTill anyChar newline
  return $ Plain [Image [Str "image"] (src, "")]

--
-- header blocks
--

header = doubleHeader <|> singleHeader <?> "header"

-- a header with lines on top and bottom
doubleHeader = try $ do
  c <- oneOf underlineChars
  rest <- many (char c)  -- the top line
  let lenTop = length (c:rest)
  skipSpaces
  newline
  txt <- many1 (notFollowedBy blankline >> inline)
  pos <- getPosition
  let len = (sourceColumn pos) - 1
  if (len > lenTop) then fail "title longer than border" else return ()
  blankline              -- spaces and newline
  count lenTop (char c)  -- the bottom line
  blanklines
  -- check to see if we've had this kind of header before.  
  -- if so, get appropriate level.  if not, add to list.
  state <- getState
  let headerTable = stateHeaderTable state
  let (headerTable',level) = case findIndex (== DoubleHeader c) headerTable of
        Just ind -> (headerTable, ind + 1)
        Nothing -> (headerTable ++ [DoubleHeader c], (length headerTable) + 1)
  setState (state { stateHeaderTable = headerTable' })
  return $ Header level (normalizeSpaces txt)

-- a header with line on the bottom only
singleHeader = try $ do 
  notFollowedBy' whitespace
  txt <- many1 (do {notFollowedBy blankline; inline})
  pos <- getPosition
  let len = (sourceColumn pos) - 1
  blankline
  c <- oneOf underlineChars
  rest <- count (len - 1) (char c)
  many (char c)
  blanklines
  state <- getState
  let headerTable = stateHeaderTable state
  let (headerTable',level) = case findIndex (== SingleHeader c) headerTable of
        Just ind -> (headerTable, ind + 1)
        Nothing -> (headerTable ++ [SingleHeader c], (length headerTable) + 1)
  setState (state { stateHeaderTable = headerTable' })
  return $ Header level (normalizeSpaces txt)

--
-- hrule block
--

hruleWith chr = try $ do
  count 4 (char chr)
  skipMany (char chr)
  skipSpaces
  newline
  blanklines
  return HorizontalRule

hrule = choice (map hruleWith underlineChars) <?> "hrule"

--
-- code blocks
--

-- read a line indented by a given string
indentedLine indents = try $ do
  string indents
  result <- manyTill anyChar newline
  return $ result ++ "\n"

-- two or more indented lines, possibly separated by blank lines.
-- if variable = True, then any indent will work, but it must be
-- consistent through the block.
-- if variable = False, indent should be one tab or equivalent in spaces.
indentedBlock variable = try $ do 
  state <- getState
  let tabStop = stateTabStop state
  indents <- if variable
                then many1 (oneOf " \t")
                else oneOfStrings ["\t", (replicate tabStop ' ')]
  firstline <- manyTill anyChar newline
  rest <- many (choice [ indentedLine indents, 
                         try (do b <- blanklines
                                 l <- indentedLine indents
                                 return (b ++ l))])
  optional blanklines
  return $ firstline ++ "\n" ++ concat rest

codeBlock = try $ do
  codeBlockStart
  result <- indentedBlock False
  -- the False means we want one tab stop indent on each line
  return $ CodeBlock $ stripTrailingNewlines result

--
-- raw html
--

rawHtmlBlock = try $ string ".. raw:: html" >> blanklines >>
                     indentedBlock True >>= return . RawHtml

--
-- raw latex
--

rawLaTeXBlock = try $ do
  string ".. raw:: latex"
  blanklines
  result <- indentedBlock True
  return $ Para [(TeX result)]

--
-- block quotes
--

blockQuote = try $ do
  raw <- indentedBlock True
  -- parse the extracted block, which may contain various block elements:
  contents <- parseFromString parseBlocks $ raw ++ "\n\n"
  return $ BlockQuote contents

--
-- list blocks
--

list = choice [ bulletList, orderedList, definitionList ] <?> "list"

definitionListItem = try $ do
  term <- many1Till inline endline
  raw <- indentedBlock True
  -- parse the extracted block, which may contain various block elements:
  contents <- parseFromString parseBlocks $ raw ++ "\n\n"
  return (normalizeSpaces term, contents)

definitionList = try $ do
  items <- many1 definitionListItem 
  return $ DefinitionList items

-- parses bullet list start and returns its length (inc. following whitespace)
bulletListStart = try $ do
  notFollowedBy' hrule  -- because hrules start out just like lists
  marker <- oneOf bulletListMarkers
  white <- many1 spaceChar
  return $ length (marker:white)

-- parses ordered list start and returns its length (inc following whitespace)
orderedListStart style delim = try $ do
  (_, markerLen) <- withHorizDisplacement (orderedListMarker style delim)
  white <- many1 spaceChar
  return $ markerLen + length white

-- parse a line of a list item
listLine markerLength = try $ do
  notFollowedBy blankline
  indentWith markerLength
  line <- manyTill anyChar newline
  return $ line ++ "\n"

-- indent by specified number of spaces (or equiv. tabs)
indentWith num = do
  state <- getState
  let tabStop = stateTabStop state
  if (num < tabStop)
     then count num  (char ' ')
     else choice [ try (count num (char ' ')), 
                   (try (char '\t' >> count (num - tabStop) (char ' '))) ] 

-- parse raw text for one list item, excluding start marker and continuations
rawListItem start = try $ do
  markerLength <- start
  firstLine <- manyTill anyChar newline
  restLines <- many (listLine markerLength)
  return (markerLength, (firstLine ++ "\n" ++ (concat restLines)))

-- continuation of a list item - indented and separated by blankline or 
-- (in compact lists) endline.  
-- Note: nested lists are parsed as continuations.
listContinuation markerLength = try $ do
  blanks <- many1 blankline
  result <- many1 (listLine markerLength)
  return $ blanks ++ concat result

listItem start = try $ do 
  (markerLength, first) <- rawListItem start
  rest <- many (listContinuation markerLength)
  blanks <- choice [ try (many blankline >>~ lookAhead start),
                     many1 blankline ]  -- whole list must end with blank.
  -- parsing with ListItemState forces markers at beginning of lines to
  -- count as list item markers, even if not separated by blank space.
  -- see definition of "endline"
  state <- getState
  let oldContext = stateParserContext state
  setState $ state {stateParserContext = ListItemState}
  -- parse the extracted block, which may itself contain block elements
  parsed <- parseFromString parseBlocks $ concat (first:rest) ++ blanks
  updateState (\st -> st {stateParserContext = oldContext})
  return parsed

orderedList = try $ do
  (start, style, delim) <- lookAhead (anyOrderedListMarker >>~ spaceChar)
  items <- many1 (listItem (orderedListStart style delim))
  let items' = compactify items
  return $ OrderedList (start, style, delim) items'

bulletList = try $ do
  items <- many1 (listItem bulletListStart)
  let items' = compactify items
  return $ BulletList items'

--
-- unknown directive (e.g. comment)
--

unknownDirective = try $ do
  string ".. "
  manyTill anyChar newline
  many (string "   :" >> many1 (noneOf "\n:") >> char ':' >>
        many1 (noneOf "\n") >> newline)
  optional blanklines
  return Null

-- 
-- reference key
--

referenceKey = 
  choice [imageKey, anonymousKey, regularKeyQuoted, regularKey] >>~ 
  optional blanklines

targetURI = try $ do
  skipSpaces
  optional newline
  contents <- many1 (try (many spaceChar >> newline >> 
                          many1 spaceChar >> noneOf " \t\n") <|> noneOf "\n")
  blanklines
  return contents

imageKey = try $ do
  string ".. |"
  ref <- manyTill inline (char '|')
  skipSpaces
  string "image::"
  src <- targetURI
  return $ KeyBlock (normalizeSpaces ref) (removeLeadingTrailingSpace src, "")

anonymousKey = try $ do
  oneOfStrings [".. __:", "__"]
  src <- targetURI
  state <- getState
  return $ KeyBlock [Str "_"] (removeLeadingTrailingSpace src, "")

regularKeyQuoted = try $ do
  string ".. _`"
  ref <- manyTill inline (char '`')
  char ':'
  src <- targetURI
  return $ KeyBlock (normalizeSpaces ref) (removeLeadingTrailingSpace src, "")

regularKey = try $ do
  string ".. _"
  ref <- manyTill inline (char ':')
  src <- targetURI
  return $ KeyBlock (normalizeSpaces ref) (removeLeadingTrailingSpace src, "")

 -- 
 -- inline
 --

inline = choice [ superscript
                , subscript
                , escapedChar
                , link
                , image
                , hyphens
                , strong
                , emph
                , code
                , str
                , tabchar
                , whitespace
                , endline
                , symbol ] <?> "inline"

hyphens = try $ do
  result <- many1 (char '-')
  option Space endline 
  -- don't want to treat endline after hyphen or dash as a space
  return $ Str result

escapedChar = escaped anyChar

symbol = do 
  result <- oneOf specialChars
  return $ Str [result]

-- parses inline code, between codeStart and codeEnd
code = try $ do 
  string "``"
  result <- manyTill anyChar (try (string "``"))
  return $ Code $ removeLeadingTrailingSpace $ joinWithSep " " $ lines result

emph = enclosed (char '*') (char '*') inline >>= 
       return . Emph . normalizeSpaces

strong = enclosed (string "**") (try $ string "**") inline >>= 
         return . Strong . normalizeSpaces

interpreted role = try $ do
  optional $ try $ string "\\ "
  result <- enclosed (string $ ":" ++ role ++ ":`") (char '`') anyChar
  nextChar <- lookAhead anyChar
  try (string "\\ ") <|> lookAhead (count 1 $ oneOf " \t\n") <|> (eof >> return "")
  return [Str result]

superscript = interpreted "sup" >>= (return . Superscript)

subscript = interpreted "sub" >>= (return . Subscript)

whitespace = many1 spaceChar >> return Space <?> "whitespace"

tabchar = tab >> return (Str "\t")

str = notFollowedBy' oneWordReference >> 
      many1 (noneOf (specialChars ++ "\t\n ")) >>= return . Str

-- an endline character that can be treated as a space, not a structural break
endline = try $ do
  newline
  notFollowedBy blankline
  -- parse potential list-starts at beginning of line differently in a list:
  st <- getState
  if ((stateParserContext st) == ListItemState)
     then notFollowedBy (anyOrderedListMarker >> spaceChar) >>
          notFollowedBy' bulletListStart
     else return ()
  return Space

--
-- links
--

link = choice [explicitLink, referenceLink, autoLink]  <?> "link"

explicitLink = try $ do
  char '`'
  notFollowedBy (char '`') -- `` is marks start of inline code
  label <- manyTill inline (try (do {spaces; char '<'}))
  src <- manyTill (noneOf ">\n ") (char '>')
  skipSpaces
  string "`_"
  return $ Link (normalizeSpaces label) (removeLeadingTrailingSpace src, "")

reference = try $ do
  char '`'
  notFollowedBy (char '`')
  label <- many1Till inline (char '`') 
  char '_'
  return label

oneWordReference = do
  raw <- many1 alphaNum
  char '_'
  notFollowedBy alphaNum    -- because this_is_not a link
  return [Str raw]

referenceLink = try $ do
  label <- reference <|> oneWordReference
  key <- option label (do{char '_'; return [Str "_"]}) -- anonymous link
  state <- getState
  let keyTable = stateKeys state
  src <- case lookupKeySrc keyTable key of
           Nothing     -> fail "no corresponding key"
           Just target -> return target
  -- if anonymous link, remove first anon key so it won't be used again
  let keyTable' = if (key == [Str "_"]) -- anonymous link? 
                    then delete ([Str "_"], src) keyTable -- remove first anon key 
                    else keyTable                    
  setState $ state { stateKeys = keyTable' }
  return $ Link (normalizeSpaces label) src 

uriScheme = oneOfStrings [ "http://", "https://", "ftp://", "file://", 
                           "mailto:", "news:", "telnet:" ]

uri = try $ do
  scheme <- uriScheme
  identifier <- many1 (noneOf " \t\n")
  return $ scheme ++ identifier

autoURI = try $ do
  src <- uri
  return $ Link [Str src] (src, "")

emailChar = alphaNum <|> oneOf "-+_."

emailAddress = try $ do
  firstLetter <- alphaNum
  restAddr <- many emailChar
  let addr = firstLetter:restAddr
  char '@'
  dom <- domain
  return $ addr ++ '@':dom

domainChar = alphaNum <|> char '-'

domain = try $ do
  first <- many1 domainChar
  dom <- many1 (try (do{ char '.'; many1 domainChar }))
  return $ joinWithSep "." (first:dom)

autoEmail = try $ do
  src <- emailAddress
  return $ Link [Str src] ("mailto:" ++ src, "")

autoLink = autoURI <|> autoEmail

-- For now, we assume that all substitution references are for images.
image = try $ do
  char '|'
  ref <- manyTill inline (char '|')
  state <- getState
  let keyTable = stateKeys state
  src <- case lookupKeySrc keyTable ref of
           Nothing     -> fail "no corresponding key"
           Just target -> return target
  return $ Image (normalizeSpaces ref) src

