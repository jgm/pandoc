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

Conversion of org-mode formatted plain text to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Org ( readOrg ) where

import qualified Text.Pandoc.Builder as B
import           Text.Pandoc.Builder (Inlines, Blocks, trimInlines, (<>), HasMeta(..))
import           Text.Pandoc.Definition
import           Text.Pandoc.Options
import qualified Text.Pandoc.Parsing as P
import           Text.Pandoc.Parsing hiding (newline, orderedListMarker, updateLastStrPos)
import           Text.Pandoc.Shared (compactify')

import           Control.Applicative (pure, (<$>), (<$), (<*>), (<*), (*>), (<**>))
import           Control.Arrow ((***))
import           Control.Monad (guard, when)
import           Data.Char (toLower)
import           Data.Default
import           Data.List (foldl', isPrefixOf, isSuffixOf)
import           Data.Maybe (listToMaybe, fromMaybe)
import           Data.Monoid (mconcat, mempty, mappend)

-- | Parse org-mode string and return a Pandoc document.
readOrg :: ReaderOptions -- ^ Reader options
        -> String        -- ^ String to parse (assuming @'\n'@ line endings)
        -> Pandoc
readOrg opts s = readWith parseOrg def{ orgStateOptions = opts } (s ++ "\n\n")

type OrgParser = Parser [Char] OrgParserState

parseOrg:: OrgParser Pandoc
parseOrg = do
  blocks' <- B.toList <$> parseBlocks
  st <- getState
  let meta = orgStateMeta st
  return $ Pandoc meta $ filter (/= Null) blocks'

--
-- Parser State for Org
--

-- | Org-mode parser state
data OrgParserState = OrgParserState
                      { orgStateOptions              :: ReaderOptions
                      , orgStateEmphasisCharStack    :: [Char]
                      , orgStateEmphasisNewlines     :: Maybe Int
                      , orgStateLastForbiddenCharPos :: Maybe SourcePos
                      , orgStateLastPreCharPos       :: Maybe SourcePos
                      , orgStateLastStrPos           :: Maybe SourcePos
                      , orgStateMeta                 :: Meta
                      } deriving (Show)

instance HasReaderOptions OrgParserState where
  extractReaderOptions = orgStateOptions

instance HasMeta OrgParserState where
  setMeta field val st =
    st{ orgStateMeta = setMeta field val $ orgStateMeta st }
  deleteMeta field st =
    st{ orgStateMeta = deleteMeta field $ orgStateMeta st }

instance Default OrgParserState where
  def = defaultOrgParserState

defaultOrgParserState :: OrgParserState
defaultOrgParserState = OrgParserState
                        { orgStateOptions = def
                        , orgStateEmphasisCharStack = []
                        , orgStateEmphasisNewlines = Nothing
                        , orgStateLastForbiddenCharPos = Nothing
                        , orgStateLastPreCharPos = Nothing
                        , orgStateLastStrPos = Nothing
                        , orgStateMeta = nullMeta
                        }

updateLastStrPos :: OrgParser ()
updateLastStrPos = getPosition >>= \p ->
  updateState $ \s -> s{ orgStateLastStrPos = Just p }

updateLastForbiddenCharPos :: OrgParser ()
updateLastForbiddenCharPos = getPosition >>= \p ->
  updateState $ \s -> s{ orgStateLastForbiddenCharPos = Just p}

updateLastPreCharPos :: OrgParser ()
updateLastPreCharPos = getPosition >>= \p ->
  updateState $ \s -> s{ orgStateLastPreCharPos = Just p}

pushToInlineCharStack :: Char -> OrgParser ()
pushToInlineCharStack c = updateState $ \st ->
  st { orgStateEmphasisCharStack = c:orgStateEmphasisCharStack st }

popInlineCharStack :: OrgParser ()
popInlineCharStack = updateState $ \st ->
  st { orgStateEmphasisCharStack = drop 1 . orgStateEmphasisCharStack $ st }

surroundingEmphasisChar :: OrgParser [Char]
surroundingEmphasisChar = take 1 . drop 1 . orgStateEmphasisCharStack <$> getState

startEmphasisNewlinesCounting :: Int -> OrgParser ()
startEmphasisNewlinesCounting maxNewlines = updateState $ \s ->
  s { orgStateEmphasisNewlines = Just maxNewlines }

decEmphasisNewlinesCount :: OrgParser ()
decEmphasisNewlinesCount = updateState $ \s ->
      s{ orgStateEmphasisNewlines = (\n -> n - 1) <$> orgStateEmphasisNewlines s }

newlinesCountWithinLimits :: OrgParser Bool
newlinesCountWithinLimits = do
  st <- getState
  return $ ((< 0) <$> orgStateEmphasisNewlines st) /= Just True

resetEmphasisNewlines :: OrgParser ()
resetEmphasisNewlines = updateState $ \s ->
  s{ orgStateEmphasisNewlines = Nothing }

newline :: OrgParser Char
newline =
  P.newline
       <* updateLastPreCharPos
       <* updateLastForbiddenCharPos

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
               , figure
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
                 <$> parseFromString parseBlocks blockStr

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
  if num < tabStop
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
drawerLine = try anyLine

drawerEnd :: OrgParser String
drawerEnd = try $
  skipSpaces *> stringAnyCase ":END:" <* skipSpaces <* newline


--
-- Figures
--

-- Figures (Image on a line by itself, preceded by name and/or caption)
figure :: OrgParser Blocks
figure = try $ do
  (tit, cap) <- (maybe mempty withFigPrefix *** fromMaybe mempty)
                <$> nameAndOrCaption
  src <- skipSpaces *> selfTarget <* skipSpaces <* newline
  guard (isImageFilename src)
  return . B.para $ B.image src tit cap
 where withFigPrefix cs = if "fig:" `isPrefixOf` cs
                          then cs
                          else "fig:" ++ cs

nameAndOrCaption :: OrgParser (Maybe String, Maybe Inlines)
nameAndOrCaption = try $ nameFirst <|> captionFirst
 where
   nameFirst = try $ do
                 n <- name
                 c <- optionMaybe caption
                 return (Just n, c)
   captionFirst = try $ do
                 c <- caption
                 n <- optionMaybe name
                 return (n, Just c)

caption :: OrgParser Inlines
caption = try $ annotation "CAPTION" *> inlinesTillNewline

name :: OrgParser String
name = try $ annotation "NAME" *> skipSpaces *> manyTill anyChar newline

annotation :: String -> OrgParser String
annotation ann = try $ metaLineStart *> stringAnyCase ann <* char ':'

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
  updateState $ \st -> st { orgStateMeta  = orgStateMeta st <> meta' }
  return mempty

metaValue :: OrgParser MetaValue
metaValue = MetaInlines . B.toList <$> inlinesTillNewline

metaKey :: OrgParser String
metaKey = map toLower <$> many1 (noneOf ": \n\r")
                      <*  char ':'
                      <*  skipSpaces

-- | Headers
header :: OrgParser Blocks
header = try $
  B.header <$> headerStart
           <*> inlinesTillNewline

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
  orgToPandocTable . normalizeTable . rowsToTable <$> tableRows

orgToPandocTable :: OrgTable
                 -> Blocks
orgToPandocTable (OrgTable _ aligns heads lns) =
  B.table "" (zip aligns $ repeat 0) heads lns

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
  choice [ try $ emptyCell *> return AlignDefault
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
                else fillColumns heads (B.plain mempty)
      lns'    = map (`fillColumns` B.plain mempty) lns
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
  parseInlines <**> option B.plain (try $ newline *> pure B.para)

inlinesTillNewline :: OrgParser Inlines
inlinesTillNewline = trimInlines . mconcat <$> manyTill inline newline


--
-- list blocks
--

list :: OrgParser Blocks
list = choice [ definitionList, bulletList, orderedList ] <?> "list"

definitionList :: OrgParser Blocks
definitionList = B.definitionList <$> many1 (definitionListItem bulletListStart)

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

definitionListItem :: OrgParser Int
                   -> OrgParser (Inlines, [Blocks])
definitionListItem parseMarkerGetLength = try $ do
  markerLength <- parseMarkerGetLength
  term <- manyTill (noneOf "\n\r") (try $ string "::")
  first <- anyLineNewline
  cont <- concat <$> many (listContinuation markerLength)
  term' <- parseFromString inline term
  contents' <- parseFromString parseBlocks $ first ++ cont
  return (term', [contents'])


-- parse raw text for one list item, excluding start marker and continuations
listItem :: OrgParser Int
         -> OrgParser Blocks
listItem start = try $ do
  markerLength <- try start
  firstLine <- anyLineNewline
  rest <- concat <$> many (listContinuation markerLength)
  parseFromString parseBlocks $ firstLine ++ rest

-- continuation of a list item - indented and separated by blankline or endline.
-- Note: nested lists are parsed as continuations.
listContinuation :: Int
                 -> OrgParser String
listContinuation markerLength = try $
  mappend <$> many blankline
          <*> (concat <$> many1 listLine)
 where listLine = try $ indentWith markerLength *> anyLineNewline

anyLineNewline :: OrgParser String
anyLineNewline = (++ "\n") <$> anyLine


--
-- inline
--

inline :: OrgParser Inlines
inline =
  choice [ whitespace
         , linebreak
         , link
         , str
         , endline
         , emph
         , strong
         , strikeout
         , underline
         , code
         , math
         , displayMath
         , verbatim
         , subscript
         , superscript
         , symbol
         ] <* (guard =<< newlinesCountWithinLimits)
  <?> "inline"

parseInlines :: OrgParser Inlines
parseInlines = trimInlines . mconcat <$> many1 inline

-- treat these as potentially non-text when parsing inline:
specialChars :: [Char]
specialChars = "\"$'()*+-./:<=>[\\]^_{|}~"


whitespace :: OrgParser Inlines
whitespace = B.space <$ skipMany1 spaceChar
                     <* updateLastPreCharPos
                     <* updateLastForbiddenCharPos
             <?> "whitespace"

linebreak :: OrgParser Inlines
linebreak = try $ B.linebreak <$ string "\\\\" <* skipSpaces <* newline

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
  decEmphasisNewlinesCount
  guard =<< newlinesCountWithinLimits
  updateLastPreCharPos
  return B.space

link :: OrgParser Inlines
link = explicitOrImageLink <|> selflinkOrImage <?> "link"

explicitOrImageLink :: OrgParser Inlines
explicitOrImageLink = try $ do
  char '['
  src    <- linkTarget
  title  <- enclosedRaw (char '[') (char ']')
  title' <- parseFromString (mconcat <$> many inline) title
  char ']'
  return . B.link src ""
         $ if isImageFilename src && isImageFilename title
           then B.image title "" ""
           else title'

selflinkOrImage :: OrgParser Inlines
selflinkOrImage = try $ do
  src <- char '[' *> linkTarget <* char ']'
  return $ if isImageFilename src
           then B.image src "" ""
           else B.link src "" (B.str src)

selfTarget :: OrgParser String
selfTarget = try $ char '[' *> linkTarget <* char ']'

linkTarget :: OrgParser String
linkTarget = enclosed (char '[') (char ']') (noneOf "\n\r]")

isImageFilename :: String -> Bool
isImageFilename filename =
  any (\x -> ('.':x)  `isSuffixOf` filename) imageExtensions &&
  any (\x -> (x++":") `isPrefixOf` filename) protocols ||
  ':' `notElem` filename
 where
   imageExtensions = [ "jpeg" , "jpg" , "png" , "gif" , "svg" ]
   protocols = [ "file", "http", "https" ]

emph      :: OrgParser Inlines
emph      = B.emph         <$> emphasisBetween '/'

strong    :: OrgParser Inlines
strong    = B.strong       <$> emphasisBetween '*'

strikeout :: OrgParser Inlines
strikeout = B.strikeout    <$> emphasisBetween '+'

-- There is no underline, so we use strong instead.
underline :: OrgParser Inlines
underline = B.strong       <$> emphasisBetween '_'

code      :: OrgParser Inlines
code      = B.code         <$> verbatimBetween '='

verbatim  :: OrgParser Inlines
verbatim  = B.rawInline "" <$> verbatimBetween '~'

math      :: OrgParser Inlines
math      = B.math         <$> choice [ math1CharBetween '$'
                                      , mathStringBetween '$'
                                      , rawMathBetween "\\(" "\\)"
                                      ]

displayMath :: OrgParser Inlines
displayMath = B.displayMath <$> choice [ rawMathBetween "\\[" "\\]"
                                       , rawMathBetween "$$"  "$$"
                                       ]

subscript   :: OrgParser Inlines
subscript   = B.subscript   <$> try (char '_' *> subOrSuperExpr)

superscript :: OrgParser Inlines
superscript = B.superscript <$> try (char '^' *> subOrSuperExpr)

symbol :: OrgParser Inlines
symbol = B.str . (: "") <$> (oneOf specialChars >>= updatePositions)
 where updatePositions c
           | c `elem` emphasisPreChars = c <$ updateLastPreCharPos
           | c `elem` emphasisForbiddenBorderChars = c <$ updateLastForbiddenCharPos
           | otherwise = return c

emphasisBetween :: Char
                -> OrgParser Inlines
emphasisBetween c = try $ do
  startEmphasisNewlinesCounting emphasisAllowedNewlines
  res <- enclosedInlines (emphasisStart c) (emphasisEnd c)
  isTopLevelEmphasis <- null . orgStateEmphasisCharStack <$> getState
  when isTopLevelEmphasis
       resetEmphasisNewlines
  return res

verbatimBetween :: Char
                -> OrgParser String
verbatimBetween c = try $
  emphasisStart c *>
  many1TillNOrLessNewlines 1 (noneOf "\n\r") (emphasisEnd c)

-- | Parses a raw string delimited by @c@ using Org's math rules
mathStringBetween :: Char
                  -> OrgParser String
mathStringBetween c = try $ do
  mathStart c
  body <- many1TillNOrLessNewlines mathAllowedNewlines
                                   (noneOf (c:"\n\r"))
                                   (lookAhead $ mathEnd c)
  final <- mathEnd c
  return $ body ++ [final]

-- | Parse a single character between @c@ using math rules
math1CharBetween :: Char
                -> OrgParser String
math1CharBetween c = try $ do
  char c
  res <- noneOf $ c:mathForbiddenBorderChars
  char c
  eof <|> () <$ lookAhead (oneOf mathPostChars)
  return [res]

rawMathBetween :: String
               -> String
               -> OrgParser String
rawMathBetween s e = try $ string s *> manyTill anyChar (try $ string e)

-- | Parses the start (opening character) of emphasis
emphasisStart :: Char -> OrgParser Char
emphasisStart c = try $ do
  guard =<< afterEmphasisPreChar
  guard =<< notAfterString
  char c
  lookAhead (noneOf emphasisForbiddenBorderChars)
  pushToInlineCharStack c
  return c

-- | Parses the closing character of emphasis
emphasisEnd :: Char -> OrgParser Char
emphasisEnd c = try $ do
  guard =<< notAfterForbiddenBorderChar
  char c
  eof <|> () <$ lookAhead acceptablePostChars
  updateLastStrPos
  popInlineCharStack
  return c
 where acceptablePostChars =
           surroundingEmphasisChar >>= \x -> oneOf (x ++ emphasisPostChars)

mathStart :: Char -> OrgParser Char
mathStart c = try $
  char c <* notFollowedBy' (oneOf (c:mathForbiddenBorderChars))

mathEnd :: Char -> OrgParser Char
mathEnd c = try $ do
  res <- noneOf (c:mathForbiddenBorderChars)
  char c
  eof <|> () <$ lookAhead (oneOf mathPostChars)
  return res


enclosedInlines :: OrgParser a
                -> OrgParser b
                -> OrgParser Inlines
enclosedInlines start end = try $
  trimInlines . mconcat <$> enclosed start end inline

enclosedRaw :: OrgParser a
            -> OrgParser b
            -> OrgParser String
enclosedRaw start end = try $
  start *> (onSingleLine <|> spanningTwoLines)
 where onSingleLine = try $ many1Till (noneOf "\n\r") end
       spanningTwoLines = try $
         anyLine >>= \f -> mappend (f <> " ") <$> onSingleLine

-- | Like many1Till, but parses at most @n+1@ lines.  @p@ must not consume
--   newlines.
many1TillNOrLessNewlines :: Int
                         -> OrgParser Char
                         -> OrgParser a
                         -> OrgParser String
many1TillNOrLessNewlines n p end = try $
  nMoreLines (Just n) mempty >>= oneOrMore
 where
   nMoreLines Nothing  cs = return cs
   nMoreLines (Just 0) cs = try $ (cs ++) <$> finalLine
   nMoreLines k        cs = try $ (final k cs <|> rest k cs)
                                  >>= uncurry nMoreLines
   final _ cs = (\x -> (Nothing,      cs ++ x)) <$> try finalLine
   rest  m cs = (\x -> (minus1 <$> m, cs ++ x ++ "\n")) <$> try (manyTill p P.newline)
   finalLine = try $ manyTill p end
   minus1 k = k - 1
   oneOrMore cs = guard (not $ null cs) *> return cs

-- Org allows customization of the way it reads emphasis.  We use the defaults
-- here (see, e.g., the Emacs Lisp variable `org-emphasis-regexp-components`
-- for details).

-- | Chars allowed to occur before emphasis (spaces and newlines are ok, too)
emphasisPreChars :: [Char]
emphasisPreChars = "\t \"'({"

-- | Chars allowed at after emphasis
emphasisPostChars :: [Char]
emphasisPostChars = "\t\n !\"'),-.:;?\\}"

-- | Chars not allowed at the (inner) border of emphasis
emphasisForbiddenBorderChars :: [Char]
emphasisForbiddenBorderChars = "\t\n\r \"',"

-- | The maximum number of newlines within
emphasisAllowedNewlines :: Int
emphasisAllowedNewlines = 1

-- LaTeX-style math: see `org-latex-regexps` for details

-- | Chars allowed after an inline ($...$) math statement
mathPostChars :: [Char]
mathPostChars = "\t\n \"'),-.:;?"

-- | Chars not allowed at the (inner) border of math
mathForbiddenBorderChars :: [Char]
mathForbiddenBorderChars = "\t\n\r ,;.$"

-- | Maximum number of newlines in an inline math statement
mathAllowedNewlines :: Int
mathAllowedNewlines = 2

-- | Whether we are right behind a char allowed before emphasis
afterEmphasisPreChar :: OrgParser Bool
afterEmphasisPreChar = do
  pos <- getPosition
  lastPrePos <- orgStateLastPreCharPos <$> getState
  return . fromMaybe True $ (== pos) <$> lastPrePos

-- | Whether we are right after the end of a string
notAfterString :: OrgParser Bool
notAfterString = do
  pos <- getPosition
  lastStrPos <- orgStateLastStrPos <$> getState
  return $ lastStrPos /= Just pos

-- | Whether the parser is right after a forbidden border char
notAfterForbiddenBorderChar :: OrgParser Bool
notAfterForbiddenBorderChar = do
  pos <- getPosition
  lastFBCPos <- orgStateLastForbiddenCharPos <$> getState
  return $ lastFBCPos /= Just pos

-- | Read a sub- or superscript expression
subOrSuperExpr :: OrgParser Inlines
subOrSuperExpr = try $ do
  choice [ balancedSexp '{' '}'
         , balancedSexp '(' ')' >>= return . enclosing ('(', ')')
         , simpleSubOrSuperString
         ] >>= parseFromString (mconcat <$> many inline)

-- | Read a balanced sexp
balancedSexp :: Char
             -> Char
             -> OrgParser String
balancedSexp l r = try $ do
  char l
  res <- concat <$> many (  many1 (noneOf ([l, r] ++ "\n\r"))
                        <|> try (string [l, r])
                        <|> enclosing (l, r) <$> balancedSexp l r
                         )
  char r
  return res

simpleSubOrSuperString :: OrgParser String
simpleSubOrSuperString = try $
  choice [ string "*"
         , mappend <$> option [] ((:[]) <$> oneOf "+-")
                   <*> many1 alphaNum
         ]

enclosing :: (a, a)
          -> [a]
          -> [a]
enclosing (left, right) s = left : s ++ [right]
