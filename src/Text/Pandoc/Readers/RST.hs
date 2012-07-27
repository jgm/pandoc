{-
Copyright (C) 2006-2010 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-2010 John MacFarlane
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
import Text.Pandoc.Parsing
import Text.Pandoc.Options
import Control.Monad ( when, liftM, guard, mzero )
import Data.List ( findIndex, intercalate, transpose, sort, deleteFirstsBy )
import qualified Data.Map as M
import Text.Printf ( printf )
import Data.Maybe ( catMaybes )

-- | Parse reStructuredText string and return Pandoc document.
readRST :: ReaderOptions -- ^ Reader options
        -> String        -- ^ String to parse (assuming @'\n'@ line endings)
        -> Pandoc
readRST opts s = (readWith parseRST) def{ stateOptions = opts } (s ++ "\n\n")

--
-- Constants and data structure definitions
---

bulletListMarkers :: [Char]
bulletListMarkers = "*+-"

underlineChars :: [Char]
underlineChars = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

-- treat these as potentially non-text when parsing inline:
specialChars :: [Char]
specialChars = "\\`|*_<>$:/[]{}()-.\"'\8216\8217\8220\8221"

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

parseRST :: Parser [Char] ParserState Pandoc
parseRST = do
  optional blanklines -- skip blank lines at beginning of file
  startPos <- getPosition
  -- go through once just to get list of reference keys and notes
  -- docMinusKeys is the raw document with blanks where the keys were...
  docMinusKeys <- manyTill (referenceKey <|> noteBlock <|> lineClump) eof >>=
                   return . concat
  setInput docMinusKeys
  setPosition startPos
  st' <- getState
  let reversedNotes = stateNotes st'
  updateState $ \s -> s { stateNotes = reverse reversedNotes }
  -- now parse it for real...
  blocks <- parseBlocks
  let blocks' = filter (/= Null) blocks
  standalone <- getOption readerStandalone
  let (blocks'', title) = if standalone
                              then titleTransform blocks'
                              else (blocks', [])
  state <- getState
  let authors = stateAuthors state
  let date = stateDate state
  let title' = if (null title) then (stateTitle state) else title
  return $ Pandoc (Meta title' authors date) blocks''

--
-- parsing blocks
--

parseBlocks :: Parser [Char] ParserState [Block]
parseBlocks = manyTill block eof

block :: Parser [Char] ParserState Block
block = choice [ codeBlock
               , rawBlock
               , blockQuote
               , fieldList
               , imageBlock
               , figureBlock
               , customCodeBlock
               , mathBlock
               , defaultRoleBlock
               , unknownDirective
               , header
               , hrule
               , lineBlock     -- must go before definitionList
               , table
               , list
               , lhsCodeBlock
               , para
               , plain
               , nullBlock ] <?> "block"

--
-- field list
--

rawFieldListItem :: String -> Parser [Char] ParserState (String, String)
rawFieldListItem indent = try $ do
  string indent
  char ':'
  name <- many1 $ alphaNum <|> spaceChar
  string ": "
  skipSpaces
  first <- manyTill anyChar newline
  rest <- option "" $ try $ do lookAhead (string indent >> spaceChar)
                               indentedBlock
  let raw = first ++ "\n" ++ rest ++ "\n"
  return (name, raw)

fieldListItem :: String
              -> Parser [Char] ParserState (Maybe ([Inline], [[Block]]))
fieldListItem indent = try $ do
  (name, raw) <- rawFieldListItem indent
  let term = [Str name]
  contents <- parseFromString (many block) raw
  optional blanklines
  case (name, contents) of
       ("Author", x) -> do
           updateState $ \st ->
             st{ stateAuthors = stateAuthors st ++ [extractContents x] }
           return Nothing
       ("Authors", [BulletList auths]) -> do
           updateState $ \st -> st{ stateAuthors = map extractContents auths }
           return Nothing
       ("Date", x) -> do
           updateState $ \st -> st{ stateDate = extractContents x }
           return Nothing
       ("Title", x) -> do
           updateState $ \st -> st{ stateTitle = extractContents x }
           return Nothing
       _            -> return $ Just (term, [contents])

extractContents :: [Block] -> [Inline]
extractContents [Plain auth] = auth
extractContents [Para auth]  = auth
extractContents _            = []

fieldList :: Parser [Char] ParserState Block
fieldList = try $ do
  indent <- lookAhead $ many spaceChar
  items <- many1 $ fieldListItem indent
  if null items
     then return Null
     else return $ DefinitionList $ catMaybes items

--
-- line block
--

lineBlockLine :: Parser [Char] ParserState [Inline]
lineBlockLine = try $ do
  char '|'
  char ' ' <|> lookAhead (char '\n')
  white <- many spaceChar
  line <- many $ (notFollowedBy newline >> inline) <|> (try $ endline >>~ char ' ')
  optional endline
  return $ if null white
              then normalizeSpaces line
              else Str white : normalizeSpaces line

lineBlock :: Parser [Char] ParserState Block
lineBlock = try $ do
  lines' <- many1 lineBlockLine
  blanklines
  return $ Para (intercalate [LineBreak] lines')

--
-- paragraph block
--

para :: Parser [Char] ParserState Block
para = paraBeforeCodeBlock <|> paraNormal <?> "paragraph"

codeBlockStart :: Parser [Char] st Char
codeBlockStart = string "::" >> blankline >> blankline

-- paragraph that ends in a :: starting a code block
paraBeforeCodeBlock :: Parser [Char] ParserState Block
paraBeforeCodeBlock = try $ do
  result <- many1 (notFollowedBy' codeBlockStart >> inline)
  lookAhead (string "::")
  return $ Para $ if last result == Space
                     then normalizeSpaces result
                     else (normalizeSpaces result) ++ [Str ":"]

-- regular paragraph
paraNormal :: Parser [Char] ParserState Block
paraNormal = try $ do
  result <- many1 inline
  newline
  blanklines
  return $ Para $ normalizeSpaces result

plain :: Parser [Char] ParserState Block
plain = many1 inline >>= return . Plain . normalizeSpaces

--
-- image block
--

imageBlock :: Parser [Char] ParserState Block
imageBlock = try $ do
  string ".. image:: "
  src <- manyTill anyChar newline
  fields <- try $ do indent <- lookAhead $ many (oneOf " /t")
                     many $ rawFieldListItem indent
  optional blanklines
  case lookup "alt" fields of
        Just alt -> return $ Plain [Image [Str $ removeTrailingSpace alt]
                             (src, "")]
        Nothing  -> return $ Plain [Image [Str "image"] (src, "")]
--
-- header blocks
--

header :: Parser [Char] ParserState Block
header = doubleHeader <|> singleHeader <?> "header"

-- a header with lines on top and bottom
doubleHeader :: Parser [Char] ParserState Block
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
singleHeader :: Parser [Char] ParserState Block
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

hrule :: Parser [Char] st Block
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
indentedLine :: String -> Parser [Char] st [Char]
indentedLine indents = try $ do
  string indents
  manyTill anyChar newline

-- one or more indented lines, possibly separated by blank lines.
-- any amount of indentation will work.
indentedBlock :: Parser [Char] st [Char]
indentedBlock = try $ do
  indents <- lookAhead $ many1 spaceChar
  lns <- many1 $ try $ do b <- option "" blanklines
                          l <- indentedLine indents
                          return (b ++ l)
  optional blanklines
  return $ unlines lns

codeBlock :: Parser [Char] st Block
codeBlock = try $ do
  codeBlockStart
  result <- indentedBlock
  return $ CodeBlock ("",[],[]) $ stripTrailingNewlines result

-- | The 'code-block' directive (from Sphinx) that allows a language to be
-- specified.
customCodeBlock :: Parser [Char] st Block
customCodeBlock = try $ do
  string ".. code-block:: "
  language <- manyTill anyChar newline
  blanklines
  result <- indentedBlock
  return $ CodeBlock ("", ["sourceCode", language], []) $ stripTrailingNewlines result


figureBlock :: Parser [Char] ParserState Block
figureBlock = try $ do
  string ".. figure::"
  src <- removeLeadingTrailingSpace `fmap` manyTill anyChar newline
  body <- indentedBlock
  caption <- parseFromString extractCaption body
  return $ Para [Image caption (src,"")]

extractCaption :: Parser [Char] ParserState [Inline]
extractCaption = try $ do
  manyTill anyLine blanklines
  many inline

-- | The 'math' directive (from Sphinx) for display math.
mathBlock :: Parser [Char] st Block
mathBlock = try $ do
  string ".. math::"
  mathBlockMultiline <|> mathBlockOneLine

mathBlockOneLine :: Parser [Char] st Block
mathBlockOneLine = try $ do
  result <- manyTill anyChar newline
  blanklines
  return $ Para [Math DisplayMath $ removeLeadingTrailingSpace result]

mathBlockMultiline :: Parser [Char] st Block
mathBlockMultiline = try $ do
  blanklines
  result <- indentedBlock
  -- a single block can contain multiple equations, which need to go
  -- in separate Pandoc math elements
  let lns = map removeLeadingTrailingSpace $ lines result
  -- drop :label, :nowrap, etc.
  let startsWithColon (':':_) = True
      startsWithColon _       = False
  let lns' = dropWhile startsWithColon lns
  let eqs = map (removeLeadingTrailingSpace . unlines)
            $ filter (not . null) $ splitBy null lns'
  return $ Para $ map (Math DisplayMath) eqs

lhsCodeBlock :: Parser [Char] ParserState Block
lhsCodeBlock = try $ do
  failUnlessLHS
  optional codeBlockStart
  pos <- getPosition
  when (sourceColumn pos /= 1) $ fail "Not in first column"
  lns <- many1 birdTrackLine
  -- if (as is normal) there is always a space after >, drop it
  let lns' = if all (\ln -> null ln || take 1 ln == " ") lns
                then map (drop 1) lns
                else lns
  blanklines
  return $ CodeBlock ("", ["sourceCode", "literate", "haskell"], []) $ intercalate "\n" lns'

birdTrackLine :: Parser [Char] st [Char]
birdTrackLine = do
  char '>'
  manyTill anyChar newline

--
-- raw html/latex/etc
--

rawBlock :: Parser [Char] st Block
rawBlock = try $ do
  string ".. raw:: "
  lang <- many1 (letter <|> digit)
  blanklines
  result <- indentedBlock
  return $ RawBlock lang result

--
-- block quotes
--

blockQuote :: Parser [Char] ParserState Block
blockQuote = do
  raw <- indentedBlock
  -- parse the extracted block, which may contain various block elements:
  contents <- parseFromString parseBlocks $ raw ++ "\n\n"
  return $ BlockQuote contents

--
-- list blocks
--

list :: Parser [Char] ParserState Block
list = choice [ bulletList, orderedList, definitionList ] <?> "list"

definitionListItem :: Parser [Char] ParserState ([Inline], [[Block]])
definitionListItem = try $ do
  -- avoid capturing a directive or comment
  notFollowedBy (try $ char '.' >> char '.')
  term <- many1Till inline endline
  raw <- indentedBlock
  -- parse the extracted block, which may contain various block elements:
  contents <- parseFromString parseBlocks $ raw ++ "\n"
  return (normalizeSpaces term, [contents])

definitionList :: Parser [Char] ParserState Block
definitionList = many1 definitionListItem >>= return . DefinitionList

-- parses bullet list start and returns its length (inc. following whitespace)
bulletListStart :: Parser [Char] st Int
bulletListStart = try $ do
  notFollowedBy' hrule  -- because hrules start out just like lists
  marker <- oneOf bulletListMarkers
  white <- many1 spaceChar
  return $ length (marker:white)

-- parses ordered list start and returns its length (inc following whitespace)
orderedListStart :: ListNumberStyle
                 -> ListNumberDelim
                 -> Parser [Char] ParserState Int
orderedListStart style delim = try $ do
  (_, markerLen) <- withHorizDisplacement (orderedListMarker style delim)
  white <- many1 spaceChar
  return $ markerLen + length white

-- parse a line of a list item
listLine :: Int -> Parser [Char] ParserState [Char]
listLine markerLength = try $ do
  notFollowedBy blankline
  indentWith markerLength
  line <- manyTill anyChar newline
  return $ line ++ "\n"

-- indent by specified number of spaces (or equiv. tabs)
indentWith :: Int -> Parser [Char] ParserState [Char]
indentWith num = do
  tabStop <- getOption readerTabStop
  if (num < tabStop)
     then count num  (char ' ')
     else choice [ try (count num (char ' ')),
                   (try (char '\t' >> count (num - tabStop) (char ' '))) ]

-- parse raw text for one list item, excluding start marker and continuations
rawListItem :: Parser [Char] ParserState Int
            -> Parser [Char] ParserState (Int, [Char])
rawListItem start = try $ do
  markerLength <- start
  firstLine <- manyTill anyChar newline
  restLines <- many (listLine markerLength)
  return (markerLength, (firstLine ++ "\n" ++ (concat restLines)))

-- continuation of a list item - indented and separated by blankline or
-- (in compact lists) endline.
-- Note: nested lists are parsed as continuations.
listContinuation :: Int -> Parser [Char] ParserState [Char]
listContinuation markerLength = try $ do
  blanks <- many1 blankline
  result <- many1 (listLine markerLength)
  return $ blanks ++ concat result

listItem :: Parser [Char] ParserState Int
         -> Parser [Char] ParserState [Block]
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

orderedList :: Parser [Char] ParserState Block
orderedList = try $ do
  (start, style, delim) <- lookAhead (anyOrderedListMarker >>~ spaceChar)
  items <- many1 (listItem (orderedListStart style delim))
  let items' = compactify items
  return $ OrderedList (start, style, delim) items'

bulletList :: Parser [Char] ParserState Block
bulletList = many1 (listItem bulletListStart) >>=
             return . BulletList . compactify

--
-- default-role block
--

defaultRoleBlock :: Parser [Char] ParserState Block
defaultRoleBlock = try $ do
    string ".. default-role::"
    -- doesn't enforce any restrictions on the role name; embedded spaces shouldn't be allowed, for one
    role <- manyTill anyChar newline >>= return . removeLeadingTrailingSpace
    updateState $ \s -> s { stateRstDefaultRole =
        if null role
           then stateRstDefaultRole defaultParserState
           else role
        }
    -- skip body of the directive if it exists
    many $ blanklines <|> (spaceChar >> manyTill anyChar newline)
    return Null

--
-- unknown directive (e.g. comment)
--

unknownDirective :: Parser [Char] st Block
unknownDirective = try $ do
  string ".."
  notFollowedBy (noneOf " \t\n")
  manyTill anyChar newline
  many $ blanklines <|> (spaceChar >> manyTill anyChar newline)
  return Null

---
--- note block
---

noteBlock :: Parser [Char] ParserState [Char]
noteBlock = try $ do
  startPos <- getPosition
  string ".."
  spaceChar >> skipMany spaceChar
  ref <- noteMarker
  first <- (spaceChar >> skipMany spaceChar >> anyLine)
        <|> (newline >> return "")
  blanks <- option "" blanklines
  rest <- option "" indentedBlock
  endPos <- getPosition
  let raw = first ++ "\n" ++ blanks ++ rest ++ "\n"
  let newnote = (ref, raw)
  st <- getState
  let oldnotes = stateNotes st
  updateState $ \s -> s { stateNotes = newnote : oldnotes }
  -- return blanks so line count isn't affected
  return $ replicate (sourceLine endPos - sourceLine startPos) '\n'

noteMarker :: Parser [Char] ParserState [Char]
noteMarker = do
  char '['
  res <- many1 digit
      <|> (try $ char '#' >> liftM ('#':) simpleReferenceName')
      <|> count 1 (oneOf "#*")
  char ']'
  return res

--
-- reference key
--

quotedReferenceName :: Parser [Char] ParserState [Inline]
quotedReferenceName = try $ do
  char '`' >> notFollowedBy (char '`') -- `` means inline code!
  label' <- many1Till inline (char '`')
  return label'

unquotedReferenceName :: Parser [Char] ParserState [Inline]
unquotedReferenceName = try $ do
  label' <- many1Till inline (lookAhead $ char ':')
  return label'

-- Simple reference names are single words consisting of alphanumerics
-- plus isolated (no two adjacent) internal hyphens, underscores,
-- periods, colons and plus signs; no whitespace or other characters
-- are allowed.
simpleReferenceName' :: Parser [Char] st String
simpleReferenceName' = do
  x <- alphaNum
  xs <- many $  alphaNum
            <|> (try $ oneOf "-_:+." >> lookAhead alphaNum)
  return (x:xs)

simpleReferenceName :: Parser [Char] st [Inline]
simpleReferenceName = do
  raw <- simpleReferenceName'
  return [Str raw]

referenceName :: Parser [Char] ParserState [Inline]
referenceName = quotedReferenceName <|>
                (try $ simpleReferenceName >>~ lookAhead (char ':')) <|>
                unquotedReferenceName

referenceKey :: Parser [Char] ParserState [Char]
referenceKey = do
  startPos <- getPosition
  (key, target) <- choice [imageKey, anonymousKey, regularKey]
  st <- getState
  let oldkeys = stateKeys st
  updateState $ \s -> s { stateKeys = M.insert key target oldkeys }
  optional blanklines
  endPos <- getPosition
  -- return enough blanks to replace key
  return $ replicate (sourceLine endPos - sourceLine startPos) '\n'

targetURI :: Parser [Char] st [Char]
targetURI = do
  skipSpaces
  optional newline
  contents <- many1 (try (many spaceChar >> newline >>
                          many1 spaceChar >> noneOf " \t\n") <|> noneOf "\n")
  blanklines
  return $ escapeURI $ removeLeadingTrailingSpace $ contents

imageKey :: Parser [Char] ParserState (Key, Target)
imageKey = try $ do
  string ".. |"
  ref <- manyTill inline (char '|')
  skipSpaces
  string "image::"
  src <- targetURI
  return (toKey (normalizeSpaces ref), (src, ""))

anonymousKey :: Parser [Char] st (Key, Target)
anonymousKey = try $ do
  oneOfStrings [".. __:", "__"]
  src <- targetURI
  pos <- getPosition
  return (toKey [Str $ "_" ++ printf "%09d" (sourceLine pos)], (src, ""))

regularKey :: Parser [Char] ParserState (Key, Target)
regularKey = try $ do
  string ".. _"
  ref <- referenceName
  char ':'
  src <- targetURI
  return (toKey (normalizeSpaces ref), (src, ""))

--
-- tables
--

-- General tables TODO:
--  - figure out if leading spaces are acceptable and if so, add
--    support for them
--
-- Simple tables TODO:
--  - column spans
--  - multiline support
--  - ensure that rightmost column span does not need to reach end
--  - require at least 2 columns
--
-- Grid tables TODO:
--  - column spans

dashedLine :: Char -> Parser [Char] st (Int, Int)
dashedLine ch = do
  dashes <- many1 (char ch)
  sp     <- many (char ' ')
  return (length dashes, length $ dashes ++ sp)

simpleDashedLines :: Char -> Parser [Char] st [(Int,Int)]
simpleDashedLines ch = try $ many1 (dashedLine ch)

-- Parse a table row separator
simpleTableSep :: Char -> Parser [Char] ParserState Char
simpleTableSep ch = try $ simpleDashedLines ch >> newline

-- Parse a table footer
simpleTableFooter :: Parser [Char] ParserState [Char]
simpleTableFooter = try $ simpleTableSep '=' >> blanklines

-- Parse a raw line and split it into chunks by indices.
simpleTableRawLine :: [Int] -> Parser [Char] ParserState [String]
simpleTableRawLine indices = do
  line <- many1Till anyChar newline
  return (simpleTableSplitLine indices line)

-- Parse a table row and return a list of blocks (columns).
simpleTableRow :: [Int] -> Parser [Char] ParserState [[Block]]
simpleTableRow indices = do
  notFollowedBy' simpleTableFooter
  firstLine <- simpleTableRawLine indices
  colLines  <- return [] -- TODO
  let cols = map unlines . transpose $ firstLine : colLines
  mapM (parseFromString (many plain)) cols

simpleTableSplitLine :: [Int] -> String -> [String]
simpleTableSplitLine indices line =
  map removeLeadingTrailingSpace
  $ tail $ splitByIndices (init indices) line

simpleTableHeader :: Bool  -- ^ Headerless table
                  -> Parser [Char] ParserState ([[Block]], [Alignment], [Int])
simpleTableHeader headless = try $ do
  optional blanklines
  rawContent  <- if headless
                    then return ""
                    else simpleTableSep '=' >> anyLine
  dashes      <- simpleDashedLines '='
  newline
  let lines'   = map snd dashes
  let indices  = scanl (+) 0 lines'
  let aligns   = replicate (length lines') AlignDefault
  let rawHeads = if headless
                    then replicate (length dashes) ""
                    else simpleTableSplitLine indices rawContent
  heads <- mapM (parseFromString (many plain)) $
             map removeLeadingTrailingSpace rawHeads
  return (heads, aligns, indices)

-- Parse a simple table.
simpleTable :: Bool  -- ^ Headerless table
            -> Parser [Char] ParserState Block
simpleTable headless = do
  Table c a _w h l <- tableWith (simpleTableHeader headless) simpleTableRow sep simpleTableFooter
  -- Simple tables get 0s for relative column widths (i.e., use default)
  return $ Table c a (replicate (length a) 0) h l
 where
  sep = return () -- optional (simpleTableSep '-')

gridTable :: Bool -- ^ Headerless table
          -> Parser [Char] ParserState Block
gridTable = gridTableWith block

table :: Parser [Char] ParserState Block
table = gridTable False <|> simpleTable False <|>
        gridTable True  <|> simpleTable True <?> "table"


 --
 -- inline
 --

inline :: Parser [Char] ParserState Inline
inline = choice [ whitespace
                , link
                , str
                , endline
                , strong
                , emph
                , code
                , image
                , superscript
                , subscript
                , math
                , note
                , smartPunctuation inline
                , hyphens
                , escapedChar
                , symbol ] <?> "inline"

hyphens :: Parser [Char] ParserState Inline
hyphens = do
  result <- many1 (char '-')
  option Space endline
  -- don't want to treat endline after hyphen or dash as a space
  return $ Str result

escapedChar :: Parser [Char] st Inline
escapedChar = do c <- escaped anyChar
                 return $ if c == ' '  -- '\ ' is null in RST
                             then Str ""
                             else Str [c]

symbol :: Parser [Char] ParserState Inline
symbol = do
  result <- oneOf specialChars
  return $ Str [result]

-- parses inline code, between codeStart and codeEnd
code :: Parser [Char] ParserState Inline
code = try $ do
  string "``"
  result <- manyTill anyChar (try (string "``"))
  return $ Code nullAttr
         $ removeLeadingTrailingSpace $ intercalate " " $ lines result

-- succeeds only if we're not right after a str (ie. in middle of word)
atStart :: Parser [Char] ParserState a -> Parser [Char] ParserState a
atStart p = do
  pos <- getPosition
  st <- getState
  -- single quote start can't be right after str
  guard $ stateLastStrPos st /= Just pos
  p

emph :: Parser [Char] ParserState Inline
emph = enclosed (atStart $ char '*') (char '*') inline >>=
       return . Emph . normalizeSpaces

strong :: Parser [Char] ParserState Inline
strong = enclosed (atStart $ string "**") (try $ string "**") inline >>=
         return . Strong . normalizeSpaces

-- Parses inline interpreted text which is required to have the given role.
-- This decision is based on the role marker (if present),
-- and the current default interpreted text role.
interpreted :: [Char] -> Parser [Char] ParserState [Char]
interpreted role = try $ do
  state <- getState
  if role == stateRstDefaultRole state
     then try markedInterpretedText <|> unmarkedInterpretedText
     else     markedInterpretedText
 where
  markedInterpretedText = try (roleMarker >> unmarkedInterpretedText)
                          <|> (unmarkedInterpretedText >>= (\txt -> roleMarker >> return txt))
  roleMarker = string $ ":" ++ role ++ ":"
  -- Note, this doesn't precisely implement the complex rule in
  -- http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#inline-markup-recognition-rules
  -- but it should be good enough for most purposes
  unmarkedInterpretedText = do
      result <- enclosed (atStart $ char '`') (char '`') anyChar
      return result

superscript :: Parser [Char] ParserState Inline
superscript = interpreted "sup" >>= \x -> return (Superscript [Str x])

subscript :: Parser [Char] ParserState Inline
subscript = interpreted "sub" >>= \x -> return (Subscript [Str x])

math :: Parser [Char] ParserState Inline
math = interpreted "math" >>= \x -> return (Math InlineMath x)

whitespace :: Parser [Char] ParserState Inline
whitespace = many1 spaceChar >> return Space <?> "whitespace"

str :: Parser [Char] ParserState Inline
str = do
  let strChar = noneOf ("\t\n " ++ specialChars)
  result <- many1 strChar
  updateLastStrPos
  return $ Str result

-- an endline character that can be treated as a space, not a structural break
endline :: Parser [Char] ParserState Inline
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

link :: Parser [Char] ParserState Inline
link = choice [explicitLink, referenceLink, autoLink]  <?> "link"

explicitLink :: Parser [Char] ParserState Inline
explicitLink = try $ do
  char '`'
  notFollowedBy (char '`') -- `` marks start of inline code
  label' <- manyTill (notFollowedBy (char '`') >> inline)
                    (try (spaces >> char '<'))
  src <- manyTill (noneOf ">\n") (char '>')
  skipSpaces
  string "`_"
  return $ Link (normalizeSpaces label')
            (escapeURI $ removeLeadingTrailingSpace src, "")

referenceLink :: Parser [Char] ParserState Inline
referenceLink = try $ do
  label' <- (quotedReferenceName <|> simpleReferenceName) >>~ char '_'
  state <- getState
  let keyTable = stateKeys state
  let isAnonKey x = case fromKey x of
                         [Str ('_':_)] -> True
                         _             -> False
  key <- option (toKey label') $
                do char '_'
                   let anonKeys = sort $ filter isAnonKey $ M.keys keyTable
                   if null anonKeys
                      then mzero
                      else return (head anonKeys)
  (src,tit) <- case lookupKeySrc keyTable key of
                    Nothing     -> fail "no corresponding key"
                    Just target -> return target
  -- if anonymous link, remove key so it won't be used again
  when (isAnonKey key) $ updateState $ \s -> s{ stateKeys = M.delete key keyTable }
  return $ Link (normalizeSpaces label') (src, tit)

autoURI :: Parser [Char] ParserState Inline
autoURI = do
  (orig, src) <- uri
  return $ Link [Str orig] (src, "")

autoEmail :: Parser [Char] ParserState Inline
autoEmail = do
  (orig, src) <- emailAddress
  return $ Link [Str orig] (src, "")

autoLink :: Parser [Char] ParserState Inline
autoLink = autoURI <|> autoEmail

-- For now, we assume that all substitution references are for images.
image :: Parser [Char] ParserState Inline
image = try $ do
  char '|'
  ref <- manyTill inline (char '|')
  state <- getState
  let keyTable = stateKeys state
  (src,tit) <- case lookupKeySrc keyTable (toKey ref) of
                     Nothing     -> fail "no corresponding key"
                     Just target -> return target
  return $ Image (normalizeSpaces ref) (src, tit)

note :: Parser [Char] ParserState Inline
note = try $ do
  ref <- noteMarker
  char '_'
  state <- getState
  let notes = stateNotes state
  case lookup ref notes of
    Nothing   -> fail "note not found"
    Just raw  -> do
      -- We temporarily empty the note list while parsing the note,
      -- so that we don't get infinite loops with notes inside notes...
      -- Note references inside other notes are allowed in reST, but
      -- not yet in this implementation.
      updateState $ \st -> st{ stateNotes = [] }
      contents <- parseFromString parseBlocks raw
      let newnotes = if (ref == "*" || ref == "#") -- auto-numbered
                        -- delete the note so the next auto-numbered note
                        -- doesn't get the same contents:
                        then deleteFirstsBy (==) notes [(ref,raw)]
                        else notes
      updateState $ \st -> st{ stateNotes = newnotes }
      return $ Note contents
