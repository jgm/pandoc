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
   Module      : Text.Pandoc.Readers.RST 
   Copyright   : Copyright (C) 2006-8 John MacFarlane
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
import Control.Monad ( when )
import Data.List ( findIndex, delete, intercalate )

-- | Parse reStructuredText string and return Pandoc document.
readRST :: ParserState -- ^ Parser state, including options for parser
        -> String      -- ^ String to parse (assuming @'\n'@ line endings)
        -> Pandoc
readRST state s = (readWith parseRST) state (s ++ "\n\n")

--
-- Constants and data structure definitions
---

bulletListMarkers :: [Char]
bulletListMarkers = "*+-"

underlineChars :: [Char]
underlineChars = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

-- treat these as potentially non-text when parsing inline:
specialChars :: [Char]
specialChars = "\\`|*_<>$:[-"

--
-- parsing documents
--

isHeader :: Int -> Block -> Bool
isHeader n (Header x _) = x == n
isHeader _ _            = False

-- | Promote all headers in a list of blocks.  (Part of
-- title transformation for RST.)
promoteHeaders :: Int -> [Block] -> [Block]
promoteHeaders num ((Header level text):rest) = 
    (Header (level - num) text):(promoteHeaders num rest)
promoteHeaders num (other:rest) = other:(promoteHeaders num rest)
promoteHeaders _   [] = []

-- | If list of blocks starts with a header (or a header and subheader)
-- of level that are not found elsewhere, return it as a title and
-- promote all the other headers. 
titleTransform :: [Block]              -- ^ list of blocks
               -> ([Block], [Inline])  -- ^ modified list of blocks, title
titleTransform ((Header 1 head1):(Header 2 head2):rest) |
   not (any (isHeader 1) rest || any (isHeader 2) rest) =  -- both title & subtitle
   (promoteHeaders 2 rest, head1 ++ [Str ":", Space] ++ head2)
titleTransform ((Header 1 head1):rest) |
   not (any (isHeader 1) rest) =  -- title, no subtitle
   (promoteHeaders 1 rest, head1)
titleTransform blocks = (blocks, [])

parseRST :: GenParser Char ParserState Pandoc
parseRST = do
  startPos <- getPosition
  -- go through once just to get list of reference keys
  -- docMinusKeys is the raw document with blanks where the keys were...
  docMinusKeys <- manyTill (referenceKey <|> lineClump) eof >>= return . concat
  setInput docMinusKeys
  setPosition startPos
  st <- getState
  let reversedKeys = stateKeys st
  updateState $ \s -> s { stateKeys = reverse reversedKeys }
  -- now parse it for real...
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

parseBlocks :: GenParser Char ParserState [Block]
parseBlocks = manyTill block eof

block :: GenParser Char ParserState Block
block = choice [ codeBlock
               , rawHtmlBlock
               , rawLaTeXBlock
               , fieldList
               , blockQuote
               , imageBlock
               , customCodeBlock
               , unknownDirective
               , header
               , hrule
               , lineBlock     -- must go before definitionList
               , list
               , lhsCodeBlock
               , para
               , plain
               , nullBlock ] <?> "block"

--
-- field list
--

fieldListItem :: String -> GenParser Char st ([Char], [Char])
fieldListItem indent = try $ do
  string indent
  char ':'
  name <- many1 alphaNum
  string ": "
  skipSpaces
  first <- manyTill anyChar newline
  rest <- option "" $ try $ lookAhead (string indent >> oneOf " \t") >> 
                            indentedBlock
  return (name, intercalate " " (first:(lines rest)))

fieldList :: GenParser Char ParserState Block
fieldList = try $ do
  indent <- lookAhead $ many (oneOf " \t")
  items <- many1 $ fieldListItem indent
  blanklines
  let authors = case lookup "Authors" items of
                  Just auth -> [auth]
                  Nothing  -> map snd (filter (\(x,_) -> x == "Author") items)
  if null authors 
     then return () 
     else updateState $ \st -> st {stateAuthors = authors}
  case (lookup "Date" items) of
           Just dat -> updateState $ \st -> st {stateDate = dat}
           Nothing  -> return ()
  case (lookup "Title" items) of
           Just tit -> parseFromString (many inline) tit >>=
                       \t -> updateState $ \st -> st {stateTitle = t}
           Nothing  -> return ()
  let remaining = filter (\(x,_) -> (x /= "Authors") && (x /= "Author") && 
                  (x /= "Date") && (x /= "Title")) items
  if null remaining
              then return Null
              else do terms <- mapM (return . (:[]) . Str . fst) remaining
                      defs  <- mapM (parseFromString (many block) . snd) 
                                    remaining
                      return $ DefinitionList $ zip terms $ map (:[]) defs

--
-- line block
--

lineBlockLine :: GenParser Char ParserState [Inline]
lineBlockLine = try $ do
  string "| "
  white <- many (oneOf " \t")
  line <- many $ (notFollowedBy newline >> inline) <|> (try $ endline >>~ char ' ')
  optional endline
  return $ normalizeSpaces $ (if null white then [] else [Str white]) ++ line

lineBlock :: GenParser Char ParserState Block
lineBlock = try $ do
  lines' <- many1 lineBlockLine
  blanklines
  return $ Para (intercalate [LineBreak] lines')

--
-- paragraph block
--

para :: GenParser Char ParserState Block
para = paraBeforeCodeBlock <|> paraNormal <?> "paragraph"

codeBlockStart :: GenParser Char st Char
codeBlockStart = string "::" >> blankline >> blankline

-- paragraph that ends in a :: starting a code block
paraBeforeCodeBlock :: GenParser Char ParserState Block
paraBeforeCodeBlock = try $ do
  result <- many1 (notFollowedBy' codeBlockStart >> inline)
  lookAhead (string "::")
  return $ Para $ if last result == Space
                     then normalizeSpaces result
                     else (normalizeSpaces result) ++ [Str ":"]

-- regular paragraph
paraNormal :: GenParser Char ParserState Block
paraNormal = try $ do 
  result <- many1 inline
  newline
  blanklines
  return $ Para $ normalizeSpaces result

plain :: GenParser Char ParserState Block
plain = many1 inline >>= return . Plain . normalizeSpaces 

--
-- image block
--

imageBlock :: GenParser Char st Block
imageBlock = try $ do
  string ".. image:: "
  src <- manyTill anyChar newline
  fields <- option [] $ do indent <- lookAhead $ many (oneOf " /t")
                           many1 $ fieldListItem indent
  optional blanklines
  case lookup "alt" fields of
        Just alt -> return $ Plain [Image [Str alt] (src, alt)]
        Nothing  -> return $ Plain [Image [Str "image"] (src, "")]
--
-- header blocks
--

header :: GenParser Char ParserState Block
header = doubleHeader <|> singleHeader <?> "header"

-- a header with lines on top and bottom
doubleHeader :: GenParser Char ParserState Block
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
singleHeader :: GenParser Char ParserState Block
singleHeader = try $ do 
  notFollowedBy' whitespace
  txt <- many1 (do {notFollowedBy blankline; inline})
  pos <- getPosition
  let len = (sourceColumn pos) - 1
  blankline
  c <- oneOf underlineChars
  count (len - 1) (char c)
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

hrule :: GenParser Char st Block
hrule = try $ do
  chr <- oneOf underlineChars
  count 3 (char chr)
  skipMany (char chr)
  blankline
  blanklines
  return HorizontalRule

--
-- code blocks
--

-- read a line indented by a given string
indentedLine :: String -> GenParser Char st [Char]
indentedLine indents = try $ do
  string indents
  result <- manyTill anyChar newline
  return $ result ++ "\n"

-- two or more indented lines, possibly separated by blank lines.
-- any amount of indentation will work.
indentedBlock :: GenParser Char st [Char]
indentedBlock = do 
  indents <- lookAhead $ many1 (oneOf " \t")
  lns <- many $ choice $ [ indentedLine indents,
                           try $ do b <- blanklines
                                    l <- indentedLine indents
                                    return (b ++ l) ]
  optional blanklines 
  return $ concat lns

codeBlock :: GenParser Char st Block
codeBlock = try $ do
  codeBlockStart
  result <- indentedBlock
  return $ CodeBlock ("",[],[]) $ stripTrailingNewlines result

-- | The 'code-block' directive (from Sphinx) that allows a language to be
-- specified.
customCodeBlock :: GenParser Char st Block
customCodeBlock = try $ do
  string ".. code-block:: "
  language <- manyTill anyChar newline
  blanklines
  result <- indentedBlock
  return $ CodeBlock ("", ["sourceCode", language], []) $ stripTrailingNewlines result

lhsCodeBlock :: GenParser Char ParserState Block
lhsCodeBlock = try $ do
  failUnlessLHS
  pos <- getPosition
  when (sourceColumn pos /= 1) $ fail "Not in first column"
  lns <- many1 birdTrackLine
  -- if (as is normal) there is always a space after >, drop it
  let lns' = if all (\ln -> null ln || take 1 ln == " ") lns
                then map (drop 1) lns
                else lns
  blanklines
  return $ CodeBlock ("", ["sourceCode", "literate", "haskell"], []) $ intercalate "\n" lns'

birdTrackLine :: GenParser Char st [Char]
birdTrackLine = do
  char '>'
  manyTill anyChar newline

--
-- raw html
--

rawHtmlBlock :: GenParser Char st Block
rawHtmlBlock = try $ string ".. raw:: html" >> blanklines >>
                     indentedBlock >>= return . RawHtml

--
-- raw latex
--

rawLaTeXBlock :: GenParser Char st Block
rawLaTeXBlock = try $ do
  string ".. raw:: latex"
  blanklines
  result <- indentedBlock
  return $ Para [(TeX result)]

--
-- block quotes
--

blockQuote :: GenParser Char ParserState Block
blockQuote = do
  raw <- indentedBlock
  -- parse the extracted block, which may contain various block elements:
  contents <- parseFromString parseBlocks $ raw ++ "\n\n"
  return $ BlockQuote contents

--
-- list blocks
--

list :: GenParser Char ParserState Block
list = choice [ bulletList, orderedList, definitionList ] <?> "list"

definitionListItem :: GenParser Char ParserState ([Inline], [[Block]])
definitionListItem = try $ do
  -- avoid capturing a directive or comment
  notFollowedBy (try $ char '.' >> char '.')
  term <- many1Till inline endline
  raw <- indentedBlock
  -- parse the extracted block, which may contain various block elements:
  contents <- parseFromString parseBlocks $ raw ++ "\n\n"
  return (normalizeSpaces term, [contents])

definitionList :: GenParser Char ParserState Block
definitionList = many1 definitionListItem >>= return . DefinitionList

-- parses bullet list start and returns its length (inc. following whitespace)
bulletListStart :: GenParser Char st Int
bulletListStart = try $ do
  notFollowedBy' hrule  -- because hrules start out just like lists
  marker <- oneOf bulletListMarkers
  white <- many1 spaceChar
  return $ length (marker:white)

-- parses ordered list start and returns its length (inc following whitespace)
orderedListStart :: ListNumberStyle
                 -> ListNumberDelim
                 -> GenParser Char st Int
orderedListStart style delim = try $ do
  (_, markerLen) <- withHorizDisplacement (orderedListMarker style delim)
  white <- many1 spaceChar
  return $ markerLen + length white

-- parse a line of a list item
listLine :: Int -> GenParser Char ParserState [Char]
listLine markerLength = try $ do
  notFollowedBy blankline
  indentWith markerLength
  line <- manyTill anyChar newline
  return $ line ++ "\n"

-- indent by specified number of spaces (or equiv. tabs)
indentWith :: Int -> GenParser Char ParserState [Char]
indentWith num = do
  state <- getState
  let tabStop = stateTabStop state
  if (num < tabStop)
     then count num  (char ' ')
     else choice [ try (count num (char ' ')), 
                   (try (char '\t' >> count (num - tabStop) (char ' '))) ] 

-- parse raw text for one list item, excluding start marker and continuations
rawListItem :: GenParser Char ParserState Int
            -> GenParser Char ParserState (Int, [Char])
rawListItem start = try $ do
  markerLength <- start
  firstLine <- manyTill anyChar newline
  restLines <- many (listLine markerLength)
  return (markerLength, (firstLine ++ "\n" ++ (concat restLines)))

-- continuation of a list item - indented and separated by blankline or 
-- (in compact lists) endline.  
-- Note: nested lists are parsed as continuations.
listContinuation :: Int -> GenParser Char ParserState [Char]
listContinuation markerLength = try $ do
  blanks <- many1 blankline
  result <- many1 (listLine markerLength)
  return $ blanks ++ concat result

listItem :: GenParser Char ParserState Int
         -> GenParser Char ParserState [Block]
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

orderedList :: GenParser Char ParserState Block
orderedList = try $ do
  (start, style, delim) <- lookAhead (anyOrderedListMarker >>~ spaceChar)
  items <- many1 (listItem (orderedListStart style delim))
  let items' = compactify items
  return $ OrderedList (start, style, delim) items'

bulletList :: GenParser Char ParserState Block
bulletList = many1 (listItem bulletListStart) >>= 
             return . BulletList . compactify

--
-- unknown directive (e.g. comment)
--

unknownDirective :: GenParser Char st Block
unknownDirective = try $ do
  string ".."
  notFollowedBy (noneOf " \t\n")
  manyTill anyChar newline
  many $ blanklines <|> (oneOf " \t" >> manyTill anyChar newline)
  return Null

-- 
-- reference key
--

quotedReferenceName :: GenParser Char ParserState [Inline]
quotedReferenceName = try $ do
  char '`' >> notFollowedBy (char '`') -- `` means inline code!
  label' <- many1Till inline (char '`') 
  return label'

unquotedReferenceName :: GenParser Char ParserState [Inline]
unquotedReferenceName = try $ do
  label' <- many1Till inline (lookAhead $ char ':')
  return label'

isolated :: Char -> GenParser Char st Char
isolated ch = try $ char ch >>~ notFollowedBy (char ch)

simpleReferenceName :: GenParser Char st [Inline]
simpleReferenceName = do
  raw <- many1 (alphaNum <|> isolated '-' <|> isolated '.' <|>
                (try $ char '_' >>~ lookAhead alphaNum))
  return [Str raw]

referenceName :: GenParser Char ParserState [Inline]
referenceName = quotedReferenceName <|>
                (try $ simpleReferenceName >>~ lookAhead (char ':')) <|>
                unquotedReferenceName

referenceKey :: GenParser Char ParserState [Char]
referenceKey = do
  startPos <- getPosition
  key <- choice [imageKey, anonymousKey, regularKey]
  st <- getState
  let oldkeys = stateKeys st
  updateState $ \s -> s { stateKeys = key : oldkeys }
  optional blanklines
  endPos <- getPosition
  -- return enough blanks to replace key
  return $ replicate (sourceLine endPos - sourceLine startPos) '\n'

targetURI :: GenParser Char st [Char]
targetURI = do
  skipSpaces
  optional newline
  contents <- many1 (try (many spaceChar >> newline >> 
                          many1 spaceChar >> noneOf " \t\n") <|> noneOf "\n")
  blanklines
  return contents

imageKey :: GenParser Char ParserState ([Inline], (String, [Char]))
imageKey = try $ do
  string ".. |"
  ref <- manyTill inline (char '|')
  skipSpaces
  string "image::"
  src <- targetURI
  return (normalizeSpaces ref, (removeLeadingTrailingSpace src, ""))

anonymousKey :: GenParser Char st ([Inline], (String, [Char]))
anonymousKey = try $ do
  oneOfStrings [".. __:", "__"]
  src <- targetURI
  return ([Str "_"], (removeLeadingTrailingSpace src, ""))

regularKey :: GenParser Char ParserState ([Inline], (String, [Char]))
regularKey = try $ do
  string ".. _"
  ref <- referenceName
  char ':'
  src <- targetURI
  return (normalizeSpaces ref, (removeLeadingTrailingSpace src, ""))

 -- 
 -- inline
 --

inline :: GenParser Char ParserState Inline
inline = choice [ link
                , str
                , whitespace
                , endline
                , strong
                , emph
                , code
                , image
                , hyphens
                , superscript
                , subscript
                , escapedChar
                , symbol ] <?> "inline"

hyphens :: GenParser Char ParserState Inline
hyphens = do
  result <- many1 (char '-')
  option Space endline 
  -- don't want to treat endline after hyphen or dash as a space
  return $ Str result

escapedChar :: GenParser Char st Inline
escapedChar = escaped anyChar

symbol :: GenParser Char ParserState Inline
symbol = do 
  result <- oneOf specialChars
  return $ Str [result]

-- parses inline code, between codeStart and codeEnd
code :: GenParser Char ParserState Inline
code = try $ do 
  string "``"
  result <- manyTill anyChar (try (string "``"))
  return $ Code $ removeLeadingTrailingSpace $ intercalate " " $ lines result

emph :: GenParser Char ParserState Inline
emph = enclosed (char '*') (char '*') inline >>= 
       return . Emph . normalizeSpaces

strong :: GenParser Char ParserState Inline
strong = enclosed (string "**") (try $ string "**") inline >>= 
         return . Strong . normalizeSpaces

interpreted :: [Char] -> GenParser Char st [Inline]
interpreted role = try $ do
  optional $ try $ string "\\ "
  result <- enclosed (string $ ":" ++ role ++ ":`") (char '`') anyChar
  try (string "\\ ") <|> lookAhead (count 1 $ oneOf " \t\n") <|> (eof >> return "")
  return [Str result]

superscript :: GenParser Char ParserState Inline
superscript = interpreted "sup" >>= (return . Superscript)

subscript :: GenParser Char ParserState Inline
subscript = interpreted "sub" >>= (return . Subscript)

whitespace :: GenParser Char ParserState Inline
whitespace = many1 spaceChar >> return Space <?> "whitespace"

str :: GenParser Char ParserState Inline
str = many1 (noneOf (specialChars ++ "\t\n ")) >>= return . Str

-- an endline character that can be treated as a space, not a structural break
endline :: GenParser Char ParserState Inline
endline = try $ do
  newline
  notFollowedBy blankline
  -- parse potential list-starts at beginning of line differently in a list:
  st <- getState
  if (stateParserContext st) == ListItemState
     then notFollowedBy (anyOrderedListMarker >> spaceChar) >>
          notFollowedBy' bulletListStart
     else return ()
  return Space

--
-- links
--

link :: GenParser Char ParserState Inline
link = choice [explicitLink, referenceLink, autoLink]  <?> "link"

explicitLink :: GenParser Char ParserState Inline
explicitLink = try $ do
  char '`'
  notFollowedBy (char '`') -- `` marks start of inline code
  label' <- manyTill (notFollowedBy (char '`') >> inline) 
                    (try (spaces >> char '<'))
  src <- manyTill (noneOf ">\n") (char '>')
  skipSpaces
  string "`_"
  return $ Link (normalizeSpaces label') (removeLeadingTrailingSpace src, "")

referenceLink :: GenParser Char ParserState Inline
referenceLink = try $ do
  label' <- (quotedReferenceName <|> simpleReferenceName) >>~ char '_'
  key <- option label' (do{char '_'; return [Str "_"]}) -- anonymous link
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
  return $ Link (normalizeSpaces label') src 

autoURI :: GenParser Char ParserState Inline
autoURI = do
  src <- uri
  return $ Link [Str src] (src, "")

autoEmail :: GenParser Char ParserState Inline
autoEmail = do
  src <- emailAddress
  return $ Link [Str src] ("mailto:" ++ src, "")

autoLink :: GenParser Char ParserState Inline
autoLink = autoURI <|> autoEmail

-- For now, we assume that all substitution references are for images.
image :: GenParser Char ParserState Inline
image = try $ do
  char '|'
  ref <- manyTill inline (char '|')
  state <- getState
  let keyTable = stateKeys state
  src <- case lookupKeySrc keyTable ref of
           Nothing     -> fail "no corresponding key"
           Just target -> return target
  return $ Image (normalizeSpaces ref) src

