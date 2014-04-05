{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2014 Albert Krewinkel <tarleb@moltkeplatz.de>

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
   Module      : Text.Pandoc.Readers.Org
   Copyright   : Copyright (C) 2014 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb@moltkeplatz.de>

Conversion of Org-Mode to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Org ( readOrg ) where

import qualified Text.Pandoc.Builder as B
import           Text.Pandoc.Builder (Inlines, Blocks, trimInlines, (<>), HasMeta(..))
import           Text.Pandoc.Definition
import           Text.Pandoc.Options
import           Text.Pandoc.Parsing hiding (orderedListMarker, updateLastStrPos)
import           Text.Pandoc.Shared (compactify')

import           Control.Applicative (pure, (<$>), (<$), (<*>), (<*), (*>), (<**>))
import           Control.Monad (guard, mzero)
import           Data.Char (toLower)
import           Data.Default
import           Data.List (foldl')
import           Data.Maybe (listToMaybe, fromMaybe)
import           Data.Monoid (mconcat, mempty, mappend)

-- | Parse org-mode string and return a Pandoc document.
readOrg :: ReaderOptions -- ^ Reader options
        -> String        -- ^ String to parse (assuming @'\n'@ line endings)
        -> Pandoc
readOrg opts s = (readWith parseOrg) def{ orgOptions = opts } (s ++ "\n\n")

type OrgParser = Parser [Char] OrgParserState

-- | Org-mode parser state
data OrgParserState = OrgParserState
                      { orgOptions          :: ReaderOptions
                      , orgInlineCharStack  :: [Char]
                      , orgLastStrPos       :: Maybe SourcePos
                      , orgMeta             :: Meta
                      } deriving (Show)

instance HasReaderOptions OrgParserState where
  extractReaderOptions = orgOptions

instance HasMeta OrgParserState where
  setMeta field val st =
    st{ orgMeta = setMeta field val $ orgMeta st }
  deleteMeta field st =
    st{ orgMeta = deleteMeta field $ orgMeta st }

instance Default OrgParserState where
  def = defaultOrgParserState

defaultOrgParserState :: OrgParserState
defaultOrgParserState = OrgParserState
                        { orgOptions = def
                        , orgInlineCharStack = []
                        , orgLastStrPos = Nothing
                        , orgMeta = nullMeta
                        }

updateLastStrPos :: OrgParser ()
updateLastStrPos = getPosition >>= \p ->
  updateState $ \s -> s{ orgLastStrPos = Just p }


parseOrg:: OrgParser Pandoc
parseOrg = do
  blocks' <- B.toList <$> parseBlocks
  st <- getState
  let meta = orgMeta st
  return $ Pandoc meta $ filter (/= Null) blocks'

--
-- parsing blocks
--

parseBlocks :: OrgParser Blocks
parseBlocks = mconcat <$> manyTill block eof

block :: OrgParser Blocks
block = choice [ mempty <$ blanklines
               , orgBlock
               , example
               , drawer
               , specialLine
               , header
               , hline
               , list
               , table
               , paraOrPlain
               ] <?> "block"

--
-- Org Blocks (#+BEGIN_... / #+END_...)
--

orgBlock :: OrgParser Blocks
orgBlock = try $ do
  (indent, blockType, args) <- blockHeader
  blockStr <- rawBlockContent indent blockType
  let classArgs = [ translateLang . fromMaybe [] $ listToMaybe args ]
  case blockType of
    "comment" -> return mempty
    "src"     -> return $ B.codeBlockWith ("", classArgs, []) blockStr
    _         -> B.divWith ("", [blockType], [])
                            <$> (parseFromString parseBlocks blockStr)

blockHeader :: OrgParser (Int, String, [String])
blockHeader = (,,) <$> blockIndent
                   <*> blockType
                   <*> (skipSpaces *> blockArgs)
 where blockIndent = length <$> many spaceChar
       blockType = map toLower <$> (stringAnyCase "#+begin_" *> many letter)
       blockArgs = manyTill (many nonspaceChar <* skipSpaces) newline

rawBlockContent :: Int -> String -> OrgParser String
rawBlockContent indent blockType =
  unlines . map commaEscaped <$> manyTill indentedLine blockEnder
 where
   indentedLine = try $ choice [ blankline         *> pure "\n"
                               , indentWith indent *> anyLine
                               ]
   blockEnder = try $ indentWith indent *> stringAnyCase ("#+end_" <> blockType)

-- indent by specified number of spaces (or equiv. tabs)
indentWith :: Int -> OrgParser String
indentWith num = do
  tabStop <- getOption readerTabStop
  if (num < tabStop)
     then count num (char ' ')
     else choice [ try (count num (char ' '))
                 , try (char '\t' >> count (num - tabStop) (char ' ')) ]

translateLang :: String -> String
translateLang "C"          = "c"
translateLang "C++"        = "cpp"
translateLang "emacs-lisp" = "commonlisp" -- emacs lisp is not supported
translateLang "js"         = "javascript"
translateLang "lisp"       = "commonlisp"
translateLang "R"          = "r"
translateLang "sh"         = "bash"
translateLang "sqlite"     = "sql"
translateLang cs = cs

commaEscaped :: String -> String
commaEscaped (',':cs@('*':_))     = cs
commaEscaped (',':cs@('#':'+':_)) = cs
commaEscaped cs                   = cs

example :: OrgParser Blocks
example = try $
  B.codeBlockWith ("", ["example"], []) . unlines <$> many1 exampleLine

exampleLine :: OrgParser String
exampleLine = try $ string ": " *> anyLine

-- Drawers for properties or a logbook
drawer :: OrgParser Blocks
drawer = try $ do
  drawerStart
  manyTill drawerLine (try drawerEnd)
  return mempty

drawerStart :: OrgParser String
drawerStart = try $
  skipSpaces *> drawerName <* skipSpaces <* newline
 where drawerName = try $  char ':' *> validDrawerName <* char ':'
       validDrawerName =  stringAnyCase "PROPERTIES"
                          <|> stringAnyCase "LOGBOOK"

drawerLine :: OrgParser String
drawerLine = try $ anyLine

drawerEnd :: OrgParser String
drawerEnd = try $
  skipSpaces *> stringAnyCase ":END:" <* skipSpaces <* newline


-- Comments, Options and Metadata
specialLine :: OrgParser Blocks
specialLine = try $ metaLine <|> commentLine

metaLine :: OrgParser Blocks
metaLine = try $ metaLineStart *> declarationLine

commentLine :: OrgParser Blocks
commentLine = try $ commentLineStart *> anyLine *> pure mempty

-- The order, in which blocks are tried, makes sure that we're not looking at
-- the beginning of a block, so we don't need to check for it
metaLineStart :: OrgParser String
metaLineStart = try $ mappend <$> many spaceChar <*> string "#+"

commentLineStart :: OrgParser String
commentLineStart = try $ mappend <$> many spaceChar <*> string "# "

declarationLine :: OrgParser Blocks
declarationLine = try $ do
  meta' <- B.setMeta <$> metaKey <*> metaValue <*> pure nullMeta
  updateState $ \st -> st { orgMeta  = orgMeta st <> meta' }
  return mempty

metaValue :: OrgParser MetaValue
metaValue = MetaInlines . B.toList . trimInlines <$> restOfLine

metaKey :: OrgParser [Char]
metaKey = map toLower <$> many1 (noneOf ": \n\r")
                      <*  char ':'
                      <*  skipSpaces

-- | Headers
header :: OrgParser Blocks
header = try $
  B.header <$> headerStart
           <*> (trimInlines <$> restOfLine)

headerStart :: OrgParser Int
headerStart = try $
  (length <$> many1 (char '*')) <* many1 (char ' ')

-- Horizontal Line (five dashes or more)
hline :: OrgParser Blocks
hline = try $ do
  skipSpaces
  string "-----"
  many (char '-')
  skipSpaces
  newline
  return B.horizontalRule

--
-- Tables
--

data OrgTableRow = OrgContentRow [Blocks]
                 | OrgAlignRow [Alignment]
                 | OrgHlineRow
 deriving (Eq, Show)

data OrgTable = OrgTable
  { orgTableColumns    :: Int
  , orgTableAlignments :: [Alignment]
  , orgTableHeader     :: [Blocks]
  , orgTableRows       :: [[Blocks]]
  } deriving (Eq, Show)

table :: OrgParser Blocks
table = try $ do
  lookAhead tableStart
  OrgTable _ aligns heads lns <- normalizeTable . rowsToTable <$> tableRows
  return $ B.table "" (zip aligns $ repeat 0) heads lns

tableStart :: OrgParser Char
tableStart = try $ skipSpaces *> char '|'

tableRows :: OrgParser [OrgTableRow]
tableRows = try $ many (tableAlignRow <|> tableHline <|> tableContentRow)

tableContentRow :: OrgParser OrgTableRow
tableContentRow = try $
  OrgContentRow <$> (tableStart *> manyTill tableContentCell newline)

tableContentCell :: OrgParser Blocks
tableContentCell = try $
  B.plain . trimInlines . mconcat <$> many1Till inline endOfCell

endOfCell :: OrgParser Char
endOfCell = try $ char '|' <|> lookAhead newline

tableAlignRow :: OrgParser OrgTableRow
tableAlignRow = try $
  OrgAlignRow <$> (tableStart *> manyTill tableAlignCell newline)

tableAlignCell :: OrgParser Alignment
tableAlignCell =
  choice [ try $ emptyCell *> return (AlignDefault)
         , try $ skipSpaces
                   *> char '<'
                   *> tableAlignFromChar
                   <* many digit
                   <* char '>'
                   <* emptyCell
         ] <?> "alignment info"
    where emptyCell = try $ skipSpaces *> endOfCell

tableAlignFromChar :: OrgParser Alignment
tableAlignFromChar = try $ choice [ char 'l' *> return AlignLeft
                                  , char 'c' *> return AlignCenter
                                  , char 'r' *> return AlignRight
                                  ]

tableHline :: OrgParser OrgTableRow
tableHline = try $
  OrgHlineRow <$ (tableStart *> char '-' *> anyLine)

rowsToTable :: [OrgTableRow]
            -> OrgTable
rowsToTable = foldl' (flip rowToContent) zeroTable
  where zeroTable = OrgTable 0 mempty mempty mempty

normalizeTable :: OrgTable
               -> OrgTable
normalizeTable (OrgTable cols aligns heads lns) =
  let aligns' = fillColumns aligns AlignDefault
      heads'  = if heads == mempty
                then mempty
                else fillColumns heads  (B.plain mempty)
      lns'    = map (flip fillColumns (B.plain mempty)) lns
      fillColumns base padding = take cols $ base ++ repeat padding
  in OrgTable cols aligns' heads' lns'


-- One or more horizontal rules after the first content line mark the previous
-- line as a header.  All other horizontal lines are discarded.
rowToContent :: OrgTableRow
             -> OrgTable
             -> OrgTable
rowToContent OrgHlineRow        = maybeBodyToHeader
rowToContent (OrgContentRow rs) = setLongestRow rs . appendToBody rs
rowToContent (OrgAlignRow as)   = setLongestRow as . setAligns as

setLongestRow :: [a]
              -> OrgTable
              -> OrgTable
setLongestRow rs t = t{ orgTableColumns = max (length rs) (orgTableColumns t) }

maybeBodyToHeader :: OrgTable
                  -> OrgTable
maybeBodyToHeader t = case t of
  OrgTable{ orgTableHeader = [], orgTableRows = b:[] } ->
         t{ orgTableHeader = b , orgTableRows = [] }
  _   -> t

appendToBody :: [Blocks]
             -> OrgTable
             -> OrgTable
appendToBody r t = t{ orgTableRows = orgTableRows t ++ [r] }

setAligns :: [Alignment]
          -> OrgTable
          -> OrgTable
setAligns aligns t = t{ orgTableAlignments = aligns }

-- Paragraphs or Plain text
paraOrPlain :: OrgParser Blocks
paraOrPlain = try $
  trimInlines . mconcat
    <$> many1 inline
    <**> option B.plain
                (try $ newline *> pure B.para)

restOfLine :: OrgParser Inlines
restOfLine = mconcat <$> manyTill inline newline


--
-- list blocks
--

list :: OrgParser Blocks
list = choice [ bulletList, orderedList ] <?> "list"

bulletList :: OrgParser Blocks
bulletList = B.bulletList . compactify' <$> many1 (listItem bulletListStart)

orderedList :: OrgParser Blocks
orderedList = B.orderedList . compactify' <$> many1 (listItem orderedListStart)

genericListStart :: OrgParser String
                 -> OrgParser Int
genericListStart listMarker = try $
  (+) <$> (length <$> many spaceChar)
      <*> (length <$> listMarker <* many1 spaceChar)

-- parses bullet list start and returns its length (excl. following whitespace)
bulletListStart :: OrgParser Int
bulletListStart = genericListStart bulletListMarker
  where bulletListMarker = pure <$> oneOf "*-+"

orderedListStart :: OrgParser Int
orderedListStart = genericListStart orderedListMarker
  -- Ordered list markers allowed in org-mode
  where orderedListMarker = mappend <$> many1 digit <*> (pure <$> oneOf ".)")

listItem :: OrgParser Int
         -> OrgParser Blocks
listItem start = try $ do
  (markerLength, first) <- try (start >>= rawListItem)
  rest <- many (listContinuation markerLength)
  parseFromString parseBlocks $ concat (first:rest)

-- parse raw text for one list item, excluding start marker and continuations
rawListItem :: Int
            -> OrgParser (Int, String)
rawListItem markerLength = try $ do
  firstLine <- anyLine
  restLines <- many (listLine markerLength)
  return (markerLength, (firstLine ++ "\n" ++ (concat restLines)))

-- continuation of a list item - indented and separated by blankline or endline.
-- Note: nested lists are parsed as continuations.
listContinuation :: Int
                 -> OrgParser String
listContinuation markerLength = try $
  mappend <$> many blankline
          <*> (concat <$> many1 (listLine markerLength))

-- parse a line of a list item
listLine :: Int
         -> OrgParser String
listLine markerLength = try $
  indentWith markerLength *> anyLine
    <**> pure (++ "\n")


--
-- inline
--

inline :: OrgParser Inlines
inline = choice inlineParsers <?> "inline"
  where inlineParsers = [ whitespace
                        , link
                        , str
                        , endline
                        , emph
                        , strong
                        , strikeout
                        , underline
                        , code
                        , verbatim
                        , subscript
                        , superscript
                        , symbol
                        ]

-- treat these as potentially non-text when parsing inline:
specialChars :: [Char]
specialChars = "\"$'()*+-./:<=>[\\]^_{|}~"


whitespace :: OrgParser Inlines
whitespace = B.space <$ skipMany1 spaceChar <?> "whitespace"

str :: OrgParser Inlines
str = B.str <$> many1 (noneOf $ specialChars ++ "\n\r ")
            <* updateLastStrPos

-- an endline character that can be treated as a space, not a structural break
endline :: OrgParser Inlines
endline = try $ do
  newline
  notFollowedBy blankline
  notFollowedBy' exampleLine
  notFollowedBy' hline
  notFollowedBy' tableStart
  notFollowedBy' drawerStart
  notFollowedBy' headerStart
  notFollowedBy' metaLineStart
  notFollowedBy' commentLineStart
  notFollowedBy' bulletListStart
  notFollowedBy' orderedListStart
  return B.space

link :: OrgParser Inlines
link = explicitLink <|> selfLink <?> "link"

explicitLink :: OrgParser Inlines
explicitLink = try $ do
  char '['
  src   <- enclosedRaw     (char '[') (char ']')
  title <- enclosedInlines (char '[') (char ']')
  char ']'
  return $ B.link src "" title

selfLink :: OrgParser Inlines
selfLink = try $ do
  src <- enclosedRaw (string "[[") (string "]]")
  return $ B.link src "" (B.str src)

emph      :: OrgParser Inlines
emph      = B.emph         <$> inlinesEnclosedBy '/'

strong    :: OrgParser Inlines
strong    = B.strong       <$> inlinesEnclosedBy '*'

strikeout :: OrgParser Inlines
strikeout = B.strikeout    <$> inlinesEnclosedBy '+'

-- There is no underline, so we use strong instead.
underline :: OrgParser Inlines
underline = B.strong       <$> inlinesEnclosedBy '_'

code      :: OrgParser Inlines
code      = B.code         <$> rawEnclosedBy '='

verbatim  ::  OrgParser Inlines
verbatim  = B.rawInline "" <$> rawEnclosedBy '~'

subscript ::  OrgParser Inlines
subscript = B.subscript    <$> (try $ char '_' *> maybeGroupedByBraces)

superscript ::  OrgParser Inlines
superscript = B.superscript <$> (try $ char '^' *> maybeGroupedByBraces)

maybeGroupedByBraces :: OrgParser Inlines
maybeGroupedByBraces = try $
  choice [ try $ enclosedInlines (char '{') (char '}')
         , B.str . (:"") <$> anyChar
         ]

symbol :: OrgParser Inlines
symbol = B.str . (: "") <$> oneOf specialChars

enclosedInlines :: OrgParser a
                -> OrgParser b
                -> OrgParser Inlines
enclosedInlines start end = try $
  trimInlines . mconcat <$> enclosed start end inline

-- FIXME: This is a hack
inlinesEnclosedBy :: Char
                  -> OrgParser Inlines
inlinesEnclosedBy c = try $ do
  updateState $ \st -> st { orgInlineCharStack = c:(orgInlineCharStack st) }
  res <- enclosedInlines (atStart (char c) <* endsOnThisOrNextLine c)
                         (atEnd $ char c)
  updateState $ \st -> st { orgInlineCharStack = shift . orgInlineCharStack $ st }
  return res
 where shift xs
           | null xs   = []
           | otherwise = tail xs

enclosedRaw :: OrgParser a
            -> OrgParser b
            -> OrgParser String
enclosedRaw start end = try $
  start *> (onSingleLine <|> spanningTwoLines)
 where onSingleLine = try $ many1Till (noneOf "\n\r") end
       spanningTwoLines = try $
         anyLine >>= \f -> mappend (f <> " ") <$> onSingleLine

rawEnclosedBy :: Char
              -> OrgParser String
rawEnclosedBy c = enclosedRaw (atStart $ char c) (atEnd $ char c)

-- succeeds only if we're not right after a str (ie. in middle of word)
atStart :: OrgParser a -> OrgParser a
atStart p = do
  pos <- getPosition
  st <- getState
  guard $ orgLastStrPos st /= Just pos
  p

-- | succeeds only if we're at the end of a word
atEnd :: OrgParser a -> OrgParser a
atEnd p = try $ do
  p <* lookingAtEndOfWord
 where lookingAtEndOfWord = lookAhead . oneOf =<< postWordChars

postWordChars :: OrgParser [Char]
postWordChars = do
  st <- getState
  return $ "\t\n\r !\"'),-.:?}" ++ (safeSecond . orgInlineCharStack $ st)
 where safeSecond (_:x2:_) = [x2]
       safeSecond _        = []

-- FIXME: These functions are hacks and should be replaced
endsOnThisOrNextLine :: Char
                     -> OrgParser ()
endsOnThisOrNextLine c = do
  inp <- getInput
  let doOtherwise = \rest -> endsOnThisLine rest c (const mzero)
  endsOnThisLine inp c doOtherwise

endsOnThisLine :: [Char]
               -> Char
               -> ([Char] -> OrgParser ())
               -> OrgParser ()
endsOnThisLine input c doOnOtherLines = do
  postWordChars' <- postWordChars
  case break (`elem` c:"\n") input of
    (_,'\n':rest)    -> doOnOtherLines rest
    (_,_:rest@(n:_)) -> if n `elem` postWordChars'
                        then return ()
                        else endsOnThisLine rest c doOnOtherLines
    _                -> mzero
