{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import           Text.Pandoc.Builder ( Inlines, Blocks, HasMeta(..), (<>)
                                     , trimInlines )
import           Text.Pandoc.Definition
import           Text.Pandoc.Options
import qualified Text.Pandoc.Parsing as P
import           Text.Pandoc.Parsing hiding ( F, unF, askF, asksF, runF
                                            , newline, orderedListMarker
                                            , parseFromString
                                            , updateLastStrPos )
import           Text.Pandoc.Shared (compactify')

import           Control.Applicative ( Applicative, pure
                                     , (<$>), (<$), (<*>), (<*), (*>), (<**>) )
import           Control.Monad (foldM, guard, liftM, liftM2, when)
import           Control.Monad.Reader (Reader, runReader, ask, asks)
import           Data.Char (toLower)
import           Data.Default
import           Data.List (intersperse, isPrefixOf, isSuffixOf)
import qualified Data.Map as M
import           Data.Maybe (listToMaybe, fromMaybe, isJust)
import           Data.Monoid (Monoid, mconcat, mempty, mappend)

-- | Parse org-mode string and return a Pandoc document.
readOrg :: ReaderOptions -- ^ Reader options
        -> String        -- ^ String to parse (assuming @'\n'@ line endings)
        -> Pandoc
readOrg opts s = readWith parseOrg def{ orgStateOptions = opts } (s ++ "\n\n")

type OrgParser = Parser [Char] OrgParserState

parseOrg :: OrgParser Pandoc
parseOrg = do
  blocks' <- parseBlocks
  st <- getState
  let meta = runF (orgStateMeta' st) st
  return $ Pandoc meta $ filter (/= Null) (B.toList $ runF blocks' st)

--
-- Parser State for Org
--

type OrgNoteRecord = (String, F Blocks)
type OrgNoteTable = [OrgNoteRecord]

type OrgBlockAttributes = M.Map String String

-- | Org-mode parser state
data OrgParserState = OrgParserState
                      { orgStateOptions              :: ReaderOptions
                      , orgStateBlockAttributes      :: OrgBlockAttributes
                      , orgStateEmphasisCharStack    :: [Char]
                      , orgStateEmphasisNewlines     :: Maybe Int
                      , orgStateLastForbiddenCharPos :: Maybe SourcePos
                      , orgStateLastPreCharPos       :: Maybe SourcePos
                      , orgStateLastStrPos           :: Maybe SourcePos
                      , orgStateMeta                 :: Meta
                      , orgStateMeta'                :: F Meta
                      , orgStateNotes'               :: OrgNoteTable
                      }

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
                        , orgStateBlockAttributes = M.empty
                        , orgStateEmphasisCharStack = []
                        , orgStateEmphasisNewlines = Nothing
                        , orgStateLastForbiddenCharPos = Nothing
                        , orgStateLastPreCharPos = Nothing
                        , orgStateLastStrPos = Nothing
                        , orgStateMeta = nullMeta
                        , orgStateMeta' = return nullMeta
                        , orgStateNotes' = []
                        }

addBlockAttribute :: String -> String -> OrgParser ()
addBlockAttribute key val = updateState $ \s ->
  let attrs = orgStateBlockAttributes s
  in s{ orgStateBlockAttributes = M.insert key val attrs }

lookupBlockAttribute :: String -> OrgParser (Maybe String)
lookupBlockAttribute key =
  M.lookup key . orgStateBlockAttributes <$> getState

resetBlockAttributes :: OrgParser ()
resetBlockAttributes = updateState $ \s ->
  s{ orgStateBlockAttributes = orgStateBlockAttributes def }

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
pushToInlineCharStack c = updateState $ \s ->
  s{ orgStateEmphasisCharStack = c:orgStateEmphasisCharStack s }

popInlineCharStack :: OrgParser ()
popInlineCharStack = updateState $ \s ->
  s{ orgStateEmphasisCharStack = drop 1 . orgStateEmphasisCharStack $ s }

surroundingEmphasisChar :: OrgParser [Char]
surroundingEmphasisChar = take 1 . drop 1 . orgStateEmphasisCharStack <$> getState

startEmphasisNewlinesCounting :: Int -> OrgParser ()
startEmphasisNewlinesCounting maxNewlines = updateState $ \s ->
  s{ orgStateEmphasisNewlines = Just maxNewlines }

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

addToNotesTable :: OrgNoteRecord -> OrgParser ()
addToNotesTable note = do
  oldnotes <- orgStateNotes' <$> getState
  updateState $ \s -> s{ orgStateNotes' = note:oldnotes }

-- The version Text.Pandoc.Parsing cannot be used, as we need additional parts
-- of the state saved and restored.
parseFromString :: OrgParser a -> String -> OrgParser a
parseFromString parser str' = do
  oldLastPreCharPos <- orgStateLastPreCharPos <$> getState
  updateState $ \s -> s{ orgStateLastPreCharPos = Nothing }
  result <- P.parseFromString parser str'
  updateState $ \s -> s{ orgStateLastPreCharPos = oldLastPreCharPos }
  return result


--
-- Adaptions and specializations of parsing utilities
--

newtype F a = F { unF :: Reader OrgParserState a
                } deriving (Monad, Applicative, Functor)

runF :: F a -> OrgParserState -> a
runF = runReader . unF

askF :: F OrgParserState
askF = F ask

asksF :: (OrgParserState -> a) -> F a
asksF f = F $ asks f

instance Monoid a => Monoid (F a) where
  mempty = return mempty
  mappend = liftM2 mappend
  mconcat = fmap mconcat . sequence

trimInlinesF :: F Inlines -> F Inlines
trimInlinesF = liftM trimInlines


-- | Like @Text.Parsec.Char.newline@, but causes additional state changes.
newline :: OrgParser Char
newline =
  P.newline
       <* updateLastPreCharPos
       <* updateLastForbiddenCharPos

--
-- parsing blocks
--

parseBlocks :: OrgParser (F Blocks)
parseBlocks = mconcat <$> manyTill block eof

block :: OrgParser (F Blocks)
block = choice [ mempty <$ blanklines
               , optionalAttributes $ choice
                 [ orgBlock
                 , figure
                 , table
                 ]
               , example
               , drawer
               , specialLine
               , header
               , return <$> hline
               , list
               , latexFragment
               , noteBlock
               , paraOrPlain
               ] <?> "block"

optionalAttributes :: OrgParser (F Blocks) -> OrgParser (F Blocks)
optionalAttributes parser = try $
  resetBlockAttributes *> parseBlockAttributes *> parser

parseBlockAttributes :: OrgParser ()
parseBlockAttributes = do
  attrs <- many attribute
  () <$ mapM (uncurry parseAndAddAttribute) attrs
 where
   attribute :: OrgParser (String, String)
   attribute = try $ do
         key <- metaLineStart *> many1Till (noneOf "\n\r") (char ':')
         val <- skipSpaces *> anyLine
         return (map toLower key, val)

parseAndAddAttribute :: String -> String -> OrgParser ()
parseAndAddAttribute key value = do
  let key' = map toLower key
  () <$ addBlockAttribute key' value

lookupInlinesAttr :: String -> OrgParser (Maybe (F Inlines))
lookupInlinesAttr attr = try $ do
  val <- lookupBlockAttribute attr
  maybe (return Nothing)
        (fmap Just . parseFromString parseInlines)
        val


--
-- Org Blocks (#+BEGIN_... / #+END_...)
--

orgBlock :: OrgParser (F Blocks)
orgBlock = try $ do
  (indent, blockType, args) <- blockHeader
  content <- rawBlockContent indent blockType
  contentBlocks <- parseFromString parseBlocks (content ++ "\n")
  let classArgs = [ translateLang . fromMaybe [] $ listToMaybe args ]
  case blockType of
    "comment" -> return mempty
    "html"    -> returnF $ B.rawBlock "html" content
    "latex"   -> returnF $ B.rawBlock "latex" content
    "ascii"   -> returnF $ B.rawBlock "ascii" content
    "example" -> returnF $ exampleCode content
    "quote"   -> return  $ B.blockQuote <$> contentBlocks
    "verse"   -> parseVerse content
    "src"     -> codeBlockWithAttr classArgs content
    _         -> return  $ B.divWith ("", [blockType], []) <$> contentBlocks
 where
   returnF :: a -> OrgParser (F a)
   returnF = return . return

   parseVerse :: String -> OrgParser (F Blocks)
   parseVerse cs =
       fmap B.para . mconcat . intersperse (pure B.linebreak)
       <$> mapM (parseFromString parseInlines) (lines cs)

blockHeader :: OrgParser (Int, String, [String])
blockHeader = (,,) <$> blockIndent
                   <*> blockType
                   <*> (skipSpaces *> blockArgs)
 where blockIndent = length <$> many spaceChar
       blockType = map toLower <$> (stringAnyCase "#+begin_" *> many letter)
       blockArgs = manyTill (many nonspaceChar <* skipSpaces) newline

codeBlockWithAttr :: [String] -> String -> OrgParser (F Blocks)
codeBlockWithAttr classArgs content = do
  identifier <- fromMaybe "" <$> lookupBlockAttribute "name"
  caption <- lookupInlinesAttr "caption"
  let codeBlck = B.codeBlockWith (identifier, classArgs, []) content
  return $ maybe (pure codeBlck) (labelDiv codeBlck) caption
 where
   labelDiv blk value =
       B.divWith nullAttr <$> (mappend <$> labelledBlock value
                                       <*> pure blk)
   labelledBlock = fmap (B.plain . B.spanWith ("", ["label"], []))

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

example :: OrgParser (F Blocks)
example = try $ do
  return . return . exampleCode =<< unlines <$> many1 exampleLine

exampleCode :: String -> Blocks
exampleCode = B.codeBlockWith ("", ["example"], [])

exampleLine :: OrgParser String
exampleLine = try $ string ": " *> anyLine

-- Drawers for properties or a logbook
drawer :: OrgParser (F Blocks)
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
figure :: OrgParser (F Blocks)
figure = try $ do
  (cap, nam) <- nameAndCaption
  src <- skipSpaces *> selfTarget <* skipSpaces <* newline
  guard (isImageFilename src)
  return $ do
    cap' <- cap
    return $ B.para $ B.image src nam cap'
 where
   nameAndCaption =
       do
         maybeCap <- lookupInlinesAttr "caption"
         maybeNam <- lookupBlockAttribute "name"
         guard $ isJust maybeCap || isJust maybeNam
         return ( fromMaybe mempty maybeCap
                , maybe mempty withFigPrefix maybeNam )
   withFigPrefix cs =
       if "fig:" `isPrefixOf` cs
       then cs
       else "fig:" ++ cs

--
-- Comments, Options and Metadata
specialLine :: OrgParser (F Blocks)
specialLine = fmap return . try $ metaLine <|> commentLine

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
  key <- metaKey
  inlinesF <- metaInlines
  updateState $ \st ->
    let meta' = B.setMeta <$> pure key <*> inlinesF <*> pure nullMeta
    in st { orgStateMeta' = orgStateMeta' st <> meta' }
  return mempty

metaInlines :: OrgParser (F MetaValue)
metaInlines = fmap (MetaInlines . B.toList) <$> inlinesTillNewline

metaKey :: OrgParser String
metaKey = map toLower <$> many1 (noneOf ": \n\r")
                      <*  char ':'
                      <*  skipSpaces

--
-- Headers
--

-- | Headers
header :: OrgParser (F Blocks)
header = try $ do
  level <- headerStart
  title <- inlinesTillNewline
  return $ B.header level <$> title

headerStart :: OrgParser Int
headerStart = try $
  (length <$> many1 (char '*')) <* many1 (char ' ')


-- Don't use (or need) the reader wrapper here, we want hline to be
-- @show@able.  Otherwise we can't use it with @notFollowedBy'@.

-- | Horizontal Line (five -- dashes or more)
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

data OrgTableRow = OrgContentRow (F [Blocks])
                 | OrgAlignRow [Alignment]
                 | OrgHlineRow

data OrgTable = OrgTable
  { orgTableColumns    :: Int
  , orgTableAlignments :: [Alignment]
  , orgTableHeader     :: [Blocks]
  , orgTableRows       :: [[Blocks]]
  }

table :: OrgParser (F Blocks)
table = try $ do
  lookAhead tableStart
  do
    rows <- tableRows
    cptn <- fromMaybe (pure "") <$> lookupInlinesAttr "caption"
    return $ (<$> cptn) . orgToPandocTable . normalizeTable =<< rowsToTable rows

orgToPandocTable :: OrgTable
                 -> Inlines
                 -> Blocks
orgToPandocTable (OrgTable _ aligns heads lns) caption =
  B.table caption (zip aligns $ repeat 0) heads lns

tableStart :: OrgParser Char
tableStart = try $ skipSpaces *> char '|'

tableRows :: OrgParser [OrgTableRow]
tableRows = try $ many (tableAlignRow <|> tableHline <|> tableContentRow)

tableContentRow :: OrgParser OrgTableRow
tableContentRow = try $
  OrgContentRow . sequence <$> (tableStart *> manyTill tableContentCell newline)

tableContentCell :: OrgParser (F Blocks)
tableContentCell = try $
  fmap B.plain . trimInlinesF . mconcat <$> many1Till inline endOfCell

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
            -> F OrgTable
rowsToTable = foldM (flip rowToContent) zeroTable
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
             -> F OrgTable
rowToContent OrgHlineRow        t = maybeBodyToHeader t
rowToContent (OrgAlignRow as)   t = setLongestRow as =<< setAligns as t
rowToContent (OrgContentRow rf) t = do
  rs <- rf
  setLongestRow rs =<< appendToBody rs t

setLongestRow :: [a]
              -> OrgTable
              -> F OrgTable
setLongestRow rs t =
  return t{ orgTableColumns = max (length rs) (orgTableColumns t) }

maybeBodyToHeader :: OrgTable
                  -> F OrgTable
maybeBodyToHeader t = case t of
  OrgTable{ orgTableHeader = [], orgTableRows = b:[] } ->
         return t{ orgTableHeader = b , orgTableRows = [] }
  _   -> return t

appendToBody :: [Blocks]
             -> OrgTable
             -> F OrgTable
appendToBody r t = return t{ orgTableRows = orgTableRows t ++ [r] }

setAligns :: [Alignment]
          -> OrgTable
          -> F OrgTable
setAligns aligns t = return $ t{ orgTableAlignments = aligns }


--
-- LaTeX fragments
--
latexFragment :: OrgParser (F Blocks)
latexFragment = try $ do
  envName <- latexEnvStart
  content <- mconcat <$> manyTill anyLineNewline (latexEnd envName)
  return . return $ B.rawBlock "latex" (content `inLatexEnv` envName)
 where
   c `inLatexEnv` e = mconcat [ "\\begin{", e, "}\n"
                              , c
                              , "\\end{", e, "}\n"
                              ]

latexEnvStart :: OrgParser String
latexEnvStart = try $ do
  skipSpaces *> string "\\begin{"
             *> latexEnvName
             <* string "}"
             <* blankline

latexEnd :: String -> OrgParser ()
latexEnd envName = try $
  () <$ skipSpaces
     <* string ("\\end{" ++ envName ++ "}")
     <* blankline

-- | Parses a LaTeX environment name.
latexEnvName :: OrgParser String
latexEnvName = try $ do
  mappend <$> many1 alphaNum
          <*> option "" (string "*")


--
-- Footnote defintions
--
noteBlock :: OrgParser (F Blocks)
noteBlock = try $ do
  ref <- noteMarker
  content <- skipSpaces *> paraOrPlain
  addToNotesTable (ref, content)
  return mempty

-- Paragraphs or Plain text
paraOrPlain :: OrgParser (F Blocks)
paraOrPlain = try $
  parseInlines <**> (fmap <$> option B.plain (try $ newline *> pure B.para))

inlinesTillNewline :: OrgParser (F Inlines)
inlinesTillNewline = trimInlinesF . mconcat <$> manyTill inline newline


--
-- list blocks
--

list :: OrgParser (F Blocks)
list = choice [ definitionList, bulletList, orderedList ] <?> "list"

definitionList :: OrgParser (F Blocks)
definitionList = fmap B.definitionList . sequence
                 <$> many1 (definitionListItem bulletListStart)

bulletList :: OrgParser (F Blocks)
bulletList = fmap B.bulletList . fmap compactify' . sequence
             <$> many1 (listItem bulletListStart)

orderedList :: OrgParser (F Blocks)
-- orderedList = B.orderedList . compactify' <$> many1 (listItem orderedListStart)
orderedList = fmap B.orderedList . fmap compactify' . sequence
              <$> many1 (listItem orderedListStart)

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
                   -> OrgParser (F (Inlines, [Blocks]))
definitionListItem parseMarkerGetLength = try $ do
  markerLength <- parseMarkerGetLength
  term <- manyTill (noneOf "\n\r") (try $ string "::")
  first <- anyLineNewline
  blank <- option "" ("\n" <$ blankline)
  cont <- concat <$> many (listContinuation markerLength)
  term' <- parseFromString inline term
  contents' <- parseFromString parseBlocks $ first ++ blank ++ cont
  return $ (,) <$> term' <*> fmap (:[]) contents'


-- parse raw text for one list item, excluding start marker and continuations
listItem :: OrgParser Int
         -> OrgParser (F Blocks)
listItem start = try $ do
  markerLength <- try start
  firstLine <- anyLineNewline
  blank <- option "" ("\n" <$ blankline)
  rest <- concat <$> many (listContinuation markerLength)
  parseFromString parseBlocks $ firstLine ++ blank ++ rest

-- continuation of a list item - indented and separated by blankline or endline.
-- Note: nested lists are parsed as continuations.
listContinuation :: Int
                 -> OrgParser String
listContinuation markerLength = try $
  notFollowedBy' blankline
  *> (mappend <$> (concat <$> many1 listLine)
              <*> many blankline)
 where listLine = try $ indentWith markerLength *> anyLineNewline

anyLineNewline :: OrgParser String
anyLineNewline = (++ "\n") <$> anyLine


--
-- inline
--

inline :: OrgParser (F Inlines)
inline =
  choice [ whitespace
         , linebreak
         , footnote
         , linkOrImage
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

parseInlines :: OrgParser (F Inlines)
parseInlines = trimInlinesF . mconcat <$> many1 inline

-- treat these as potentially non-text when parsing inline:
specialChars :: [Char]
specialChars = "\"$'()*+-./:<=>[\\]^_{|}~"


whitespace :: OrgParser (F Inlines)
whitespace = pure B.space <$ skipMany1 spaceChar
                          <* updateLastPreCharPos
                          <* updateLastForbiddenCharPos
             <?> "whitespace"

linebreak :: OrgParser (F Inlines)
linebreak = try $ pure B.linebreak <$ string "\\\\" <* skipSpaces <* newline

str :: OrgParser (F Inlines)
str = return . B.str <$> many1 (noneOf $ specialChars ++ "\n\r ")
      <* updateLastStrPos

-- | An endline character that can be treated as a space, not a structural
-- break.  This should reflect the values of the Emacs variable
-- @org-element-pagaraph-separate@.
endline :: OrgParser (F Inlines)
endline = try $ do
  newline
  notFollowedBy blankline
  notFollowedBy' exampleLine
  notFollowedBy' hline
  notFollowedBy' noteMarker
  notFollowedBy' tableStart
  notFollowedBy' drawerStart
  notFollowedBy' headerStart
  notFollowedBy' metaLineStart
  notFollowedBy' latexEnvStart
  notFollowedBy' commentLineStart
  notFollowedBy' bulletListStart
  notFollowedBy' orderedListStart
  decEmphasisNewlinesCount
  guard =<< newlinesCountWithinLimits
  updateLastPreCharPos
  return . return $ B.space

footnote :: OrgParser (F Inlines)
footnote = try $ inlineNote <|> referencedNote

inlineNote :: OrgParser (F Inlines)
inlineNote = try $ do
  string "[fn:"
  ref <- many alphaNum
  char ':'
  note <- fmap B.para . trimInlinesF . mconcat <$> many1Till inline (char ']')
  when (not $ null ref) $
       addToNotesTable ("fn:" ++ ref, note)
  return $ B.note <$> note

referencedNote :: OrgParser (F Inlines)
referencedNote = try $ do
  ref <- noteMarker
  return $ do
    notes <- asksF orgStateNotes'
    case lookup ref notes of
      Nothing   -> return $ B.str $ "[" ++ ref ++ "]"
      Just contents  -> do
        st <- askF
        let contents' = runF contents st{ orgStateNotes' = [] }
        return $ B.note contents'

noteMarker :: OrgParser String
noteMarker = try $ do
  char '['
  choice [ many1Till digit (char ']')
         , (++) <$> string "fn:"
                <*> many1Till (noneOf "\n\r\t ") (char ']')
         ]

linkOrImage :: OrgParser (F Inlines)
linkOrImage = explicitOrImageLink <|> selflinkOrImage <?> "link or image"

explicitOrImageLink :: OrgParser (F Inlines)
explicitOrImageLink = try $ do
  char '['
  src    <- linkTarget
  title  <- enclosedRaw (char '[') (char ']')
  title' <- parseFromString (mconcat <$> many inline) title
  char ']'
  return $ B.link src ""  <$>
         if isImageFilename src && isImageFilename title
            then return $ B.image title mempty mempty
            else title'

selflinkOrImage :: OrgParser (F Inlines)
selflinkOrImage = try $ do
  src <- char '[' *> linkTarget <* char ']'
  return . return $ if isImageFilename src
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

emph      :: OrgParser (F Inlines)
emph      = fmap B.emph         <$> emphasisBetween '/'

strong    :: OrgParser (F Inlines)
strong    = fmap B.strong       <$> emphasisBetween '*'

strikeout :: OrgParser (F Inlines)
strikeout = fmap B.strikeout    <$> emphasisBetween '+'

-- There is no underline, so we use strong instead.
underline :: OrgParser (F Inlines)
underline = fmap B.strong       <$> emphasisBetween '_'

code      :: OrgParser (F Inlines)
code      = return . B.code         <$> verbatimBetween '='

verbatim  :: OrgParser (F Inlines)
verbatim  = return . B.rawInline "" <$> verbatimBetween '~'

subscript   :: OrgParser (F Inlines)
subscript   = fmap B.subscript   <$> try (char '_' *> subOrSuperExpr)

superscript :: OrgParser (F Inlines)
superscript = fmap B.superscript <$> try (char '^' *> subOrSuperExpr)

math      :: OrgParser (F Inlines)
math      = return . B.math      <$> choice [ math1CharBetween '$'
                                            , mathStringBetween '$'
                                            , rawMathBetween "\\(" "\\)"
                                            ]

displayMath :: OrgParser (F Inlines)
displayMath = return . B.displayMath <$> choice [ rawMathBetween "\\[" "\\]"
                                                , rawMathBetween "$$"  "$$"
                                                ]
symbol :: OrgParser (F Inlines)
symbol = return . B.str . (: "") <$> (oneOf specialChars >>= updatePositions)
 where updatePositions c
           | c `elem` emphasisPreChars = c <$ updateLastPreCharPos
           | c `elem` emphasisForbiddenBorderChars = c <$ updateLastForbiddenCharPos
           | otherwise = return c

emphasisBetween :: Char
                -> OrgParser (F Inlines)
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
                -> OrgParser (F Inlines)
enclosedInlines start end = try $
  trimInlinesF . mconcat <$> enclosed start end inline

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
subOrSuperExpr :: OrgParser (F Inlines)
subOrSuperExpr = try $
  choice [ id                   <$> charsInBalanced '{' '}' (noneOf "\n\r")
         , enclosing ('(', ')') <$> charsInBalanced '(' ')' (noneOf "\n\r")
         , simpleSubOrSuperString
         ] >>= parseFromString (mconcat <$> many inline)
 where enclosing (left, right) s = left : s ++ [right]

simpleSubOrSuperString :: OrgParser String
simpleSubOrSuperString = try $
  choice [ string "*"
         , mappend <$> option [] ((:[]) <$> oneOf "+-")
                   <*> many1 alphaNum
         ]
