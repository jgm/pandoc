{-# LANGUAGE FlexibleContexts      #-}
{-
  Copyright (C) 2017-2018 Alexander Krotov <ilabdsf@gmail.com>

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
   Module      : Text.Pandoc.Readers.Muse
   Copyright   : Copyright (C) 2017-2018 Alexander Krotov
   License     : GNU GPL, version 2 or above

   Maintainer  : Alexander Krotov <ilabdsf@gmail.com>
   Stability   : alpha
   Portability : portable

Conversion of Muse text to 'Pandoc' document.
-}
{-
TODO:
- Page breaks (five "*")
- Org tables
- table.el tables
- Images with attributes (floating and width)
- Citations and <biblio>
- <play> environment
-}
module Text.Pandoc.Readers.Muse (readMuse) where

import Control.Monad
import Control.Monad.Except (throwError)
import Data.Char (isLetter)
import Data.List (stripPrefix, intercalate)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import System.FilePath (takeExtension)
import Text.HTML.TagSoup
import Text.Pandoc.Builder (Blocks, Inlines)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class (PandocMonad (..))
import Text.Pandoc.Definition
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import Text.Pandoc.Readers.HTML (htmlTag)
import Text.Pandoc.Shared (crFilter, underlineSpan)

-- | Read Muse from an input string and return a Pandoc document.
readMuse :: PandocMonad m
         => ReaderOptions
         -> Text
         -> m Pandoc
readMuse opts s = do
  res <- readWithM parseMuse def{ stateOptions = opts } (unpack (crFilter s))
  case res of
       Left e  -> throwError e
       Right d -> return d

type MuseParser = ParserT String ParserState

--
-- main parser
--

parseMuse :: PandocMonad m => MuseParser m Pandoc
parseMuse = do
  many directive
  blocks <- parseBlocks
  st <- getState
  let doc = runF (do Pandoc _ bs <- B.doc <$> blocks
                     meta <- stateMeta' st
                     return $ Pandoc meta bs) st
  reportLogMessages
  return doc

parseBlocks :: PandocMonad m => MuseParser m (F Blocks)
parseBlocks = do
  res <- mconcat <$> many block
  spaces
  eof
  return res

--
-- utility functions
--

eol :: Stream s m Char => ParserT s st m ()
eol = void newline <|> eof

htmlElement :: PandocMonad m => String -> MuseParser m (Attr, String)
htmlElement tag = try $ do
  (TagOpen _ attr, _) <- htmlTag (~== TagOpen tag [])
  content <- manyTill anyChar endtag
  return (htmlAttrToPandoc attr, content)
  where
    endtag = void $ htmlTag (~== TagClose tag)

htmlAttrToPandoc :: [Attribute String] -> Attr
htmlAttrToPandoc attrs = (ident, classes, keyvals)
  where
    ident   = fromMaybe "" $ lookup "id" attrs
    classes = maybe [] words $ lookup "class" attrs
    keyvals = [(k,v) | (k,v) <- attrs, k /= "id" && k /= "class"]

parseHtmlContentWithAttrs :: PandocMonad m
                          => String -> MuseParser m a -> MuseParser m (Attr, [a])
parseHtmlContentWithAttrs tag parser = do
  (attr, content) <- htmlElement tag
  parsedContent <- parseContent (content ++ "\n")
  return (attr, parsedContent)
  where
    parseContent = parseFromString $ manyTill parser endOfContent
    endOfContent = try $ skipMany blankline >> skipSpaces >> eof

parseHtmlContent :: PandocMonad m => String -> MuseParser m a -> MuseParser m [a]
parseHtmlContent tag p = fmap snd (parseHtmlContentWithAttrs tag p)

commonPrefix :: String -> String -> String
commonPrefix _ [] = []
commonPrefix [] _ = []
commonPrefix (x:xs) (y:ys)
  | x == y    = x : commonPrefix xs ys
  | otherwise = []

atStart :: PandocMonad m => MuseParser m a -> MuseParser m a
atStart p = do
  pos <- getPosition
  st <- getState
  guard $ stateLastStrPos st /= Just pos
  p

--
-- directive parsers
--

-- While not documented, Emacs Muse allows "-" in directive name
parseDirectiveKey :: PandocMonad m => MuseParser m String
parseDirectiveKey = do
  char '#'
  many (letter <|> char '-')

parseEmacsDirective :: PandocMonad m => MuseParser m (String, F Inlines)
parseEmacsDirective = do
  key <- parseDirectiveKey
  spaceChar
  value <- trimInlinesF . mconcat <$> manyTill (choice inlineList) eol
  return (key, value)

parseAmuseDirective :: PandocMonad m => MuseParser m (String, F Inlines)
parseAmuseDirective = do
  key <- parseDirectiveKey
  space
  spaces
  first <- manyTill anyChar eol
  rest <- manyTill anyLine endOfDirective
  many blankline
  value <- parseFromString (trimInlinesF . mconcat <$> many inline) $ unlines (first : rest)
  return (key, value)
  where
    endOfDirective = lookAhead $ endOfInput <|> try (void blankline) <|> try (void parseDirectiveKey)
    endOfInput     = try $ skipMany blankline >> skipSpaces >> eof

directive :: PandocMonad m => MuseParser m ()
directive = do
  ext <- getOption readerExtensions
  (key, value) <- if extensionEnabled Ext_amuse ext then parseAmuseDirective else parseEmacsDirective
  updateState $ \st -> st { stateMeta' = B.setMeta (translateKey key) <$> value <*> stateMeta' st }
  where translateKey "cover" = "cover-image"
        translateKey x = x

--
-- block parsers
--

parseBlock :: PandocMonad m => MuseParser m (F Blocks)
parseBlock = do
  res <- blockElements <|> para
  optionMaybe blankline
  trace (take 60 $ show $ B.toList $ runF res defaultParserState)
  return res

block :: PandocMonad m => MuseParser m (F Blocks)
block = parseBlock <* skipMany blankline

blockElements :: PandocMonad m => MuseParser m (F Blocks)
blockElements = choice [ mempty <$ blankline
                       , comment
                       , separator
                       , header
                       , example
                       , exampleTag
                       , literal
                       , centerTag
                       , rightTag
                       , quoteTag
                       , divTag
                       , verseTag
                       , lineBlock
                       , bulletList
                       , orderedList
                       , definitionList
                       , table
                       , commentTag
                       , amuseNoteBlock
                       , emacsNoteBlock
                       ]

comment :: PandocMonad m => MuseParser m (F Blocks)
comment = try $ do
  char ';'
  optionMaybe (spaceChar >> many (noneOf "\n"))
  eol
  return mempty

separator :: PandocMonad m => MuseParser m (F Blocks)
separator = try $ do
  string "----"
  many $ char '-'
  many spaceChar
  eol
  return $ return B.horizontalRule

header :: PandocMonad m => MuseParser m (F Blocks)
header = try $ do
  st <- stateParserContext <$> getState
  q <- stateQuoteContext <$> getState
  getPosition >>= \pos -> guard (st == NullState && q == NoQuote && sourceColumn pos == 1)
  level <- fmap length $ many1 $ char '*'
  guard $ level <= 5
  spaceChar
  content <- trimInlinesF . mconcat <$> manyTill inline eol
  anchorId <- option "" parseAnchor
  attr <- registerHeader (anchorId, [], []) (runF content defaultParserState)
  return $ B.headerWith attr level <$> content

example :: PandocMonad m => MuseParser m (F Blocks)
example = try $ do
  string "{{{"
  optionMaybe blankline
  contents <- manyTill anyChar $ try (optionMaybe blankline >> string "}}}")
  return $ return $ B.codeBlock contents

-- Trim up to one newline from the beginning and the end,
-- in case opening and/or closing tags are on separate lines.
chop :: String -> String
chop = lchop . rchop

lchop :: String -> String
lchop s = case s of
                    '\n':ss -> ss
                    _       -> s

rchop :: String -> String
rchop = reverse . lchop . reverse

dropSpacePrefix :: [String] -> [String]
dropSpacePrefix lns =
  map (drop maxIndent) lns
  where flns = filter (not . all (== ' ')) lns
        maxIndent = if null flns then maximum (map length lns) else length $ takeWhile (== ' ') $ foldl1 commonPrefix flns

exampleTag :: PandocMonad m => MuseParser m (F Blocks)
exampleTag = try $ do
  many spaceChar
  (attr, contents) <- htmlElement "example"
  return $ return $ B.codeBlockWith attr $ rchop $ intercalate "\n" $ dropSpacePrefix $ splitOn "\n" $ lchop contents

literal :: PandocMonad m => MuseParser m (F Blocks)
literal = do
  guardDisabled Ext_amuse -- Text::Amuse does not support <literal>
  (return . rawBlock) <$> htmlElement "literal"
  where
    -- FIXME: Emacs Muse inserts <literal> without style into all output formats, but we assume HTML
    format (_, _, kvs)        = fromMaybe "html" $ lookup "style" kvs
    rawBlock (attrs, content) = B.rawBlock (format attrs) $ chop content

blockTag :: PandocMonad m
          => (Blocks -> Blocks)
          -> String
          -> MuseParser m (F Blocks)
blockTag f s = do
  res <- parseHtmlContent s block
  return $ f <$> mconcat res

-- <center> tag is ignored
centerTag :: PandocMonad m => MuseParser m (F Blocks)
centerTag = blockTag id "center"

-- <right> tag is ignored
rightTag :: PandocMonad m => MuseParser m (F Blocks)
rightTag = blockTag id "right"

quoteTag :: PandocMonad m => MuseParser m (F Blocks)
quoteTag = withQuoteContext InDoubleQuote $ blockTag B.blockQuote "quote"

-- <div> tag is supported by Emacs Muse, but not Amusewiki 2.025
divTag :: PandocMonad m => MuseParser m (F Blocks)
divTag = do
  (attrs, content) <- parseHtmlContentWithAttrs "div" block
  return $ B.divWith attrs <$> mconcat content

verseLine :: PandocMonad m => MuseParser m String
verseLine = do
  line <- anyLine <|> many1Till anyChar eof
  let (white, rest) = span (== ' ') line
  return $ replicate (length white) '\160' ++ rest

verseLines :: PandocMonad m => MuseParser m (F Blocks)
verseLines = do
  lns <- many verseLine
  lns' <- mapM (parseFromString' (trimInlinesF . mconcat <$> many inline)) lns
  return $ B.lineBlock <$> sequence lns'

verseTag :: PandocMonad m => MuseParser m (F Blocks)
verseTag = do
  (_, content) <- htmlElement "verse"
  parseFromString verseLines (intercalate "\n" $ dropSpacePrefix $ splitOn "\n" $ lchop content)

commentTag :: PandocMonad m => MuseParser m (F Blocks)
commentTag = parseHtmlContent "comment" anyChar >> return mempty

-- Indented paragraph is either center, right or quote
para :: PandocMonad m => MuseParser m (F Blocks)
para = do
 indent <- length <$> many spaceChar
 st <- stateParserContext <$> getState
 let f = if st /= ListItemState && indent >= 2 && indent < 6 then B.blockQuote else id
 fmap (f . B.para) . trimInlinesF . mconcat <$> many1Till inline endOfParaElement
 where
   endOfParaElement = lookAhead $ endOfInput <|> endOfPara <|> newBlockElement
   endOfInput       = try $ skipMany blankline >> skipSpaces >> eof
   endOfPara        = try $ blankline >> skipMany1 blankline
   newBlockElement  = try $ blankline >> void blockElements

noteMarker :: PandocMonad m => MuseParser m String
noteMarker = try $ do
  char '['
  first <- oneOf "123456789"
  rest <- manyTill digit (char ']')
  return $ first:rest

-- Amusewiki version of note
-- Parsing is similar to list item, except that note marker is used instead of list marker
amuseNoteBlock :: PandocMonad m => MuseParser m (F Blocks)
amuseNoteBlock = try $ do
  guardEnabled Ext_amuse
  pos <- getPosition
  ref <- noteMarker <* spaceChar
  content <- listItemContents
  oldnotes <- stateNotes' <$> getState
  case M.lookup ref oldnotes of
    Just _  -> logMessage $ DuplicateNoteReference ref pos
    Nothing -> return ()
  updateState $ \s -> s{ stateNotes' = M.insert ref (pos, content) oldnotes }
  return mempty

-- Emacs version of note
-- Notes are allowed only at the end of text, no indentation is required.
emacsNoteBlock :: PandocMonad m => MuseParser m (F Blocks)
emacsNoteBlock = try $ do
  guardDisabled Ext_amuse
  pos <- getPosition
  ref <- noteMarker <* skipSpaces
  content <- mconcat <$> blocksTillNote
  oldnotes <- stateNotes' <$> getState
  case M.lookup ref oldnotes of
    Just _  -> logMessage $ DuplicateNoteReference ref pos
    Nothing -> return ()
  updateState $ \s -> s{ stateNotes' = M.insert ref (pos, content) oldnotes }
  return mempty
  where
    blocksTillNote =
      many1Till block (eof <|> () <$ lookAhead noteMarker)

--
-- Verse markup
--

lineVerseLine :: PandocMonad m => MuseParser m (F Inlines)
lineVerseLine = try $ do
  string "> "
  indent <- B.str <$> many (char ' ' >> pure '\160')
  rest <- manyTill (choice inlineList) eol
  return $ trimInlinesF $ mconcat (pure indent : rest)

blanklineVerseLine :: PandocMonad m => MuseParser m (F Inlines)
blanklineVerseLine = try $ do
  char '>'
  blankline
  pure mempty

lineBlock :: PandocMonad m => MuseParser m (F Blocks)
lineBlock = try $ do
  lns <- many1 (blanklineVerseLine <|> lineVerseLine)
  return $ B.lineBlock <$> sequence lns

--
-- lists
--

withListContext :: PandocMonad m => MuseParser m a -> MuseParser m a
withListContext p = do
  state <- getState
  let oldContext = stateParserContext state
  setState $ state { stateParserContext = ListItemState }
  parsed <- p
  updateState (\st -> st {stateParserContext = oldContext})
  return parsed

listItemContents' :: PandocMonad m => Int -> MuseParser m (F Blocks)
listItemContents' col = do
  first <- try $ withListContext parseBlock
  rest <- many $ try (skipMany blankline >> indentWith col >> withListContext parseBlock)
  return $ mconcat (first : rest)

listItemContents :: PandocMonad m => MuseParser m (F Blocks)
listItemContents = do
  pos <- getPosition
  let col = sourceColumn pos - 1
  listItemContents' col

listItem :: PandocMonad m => Int -> MuseParser m () -> MuseParser m (F Blocks)
listItem n p = try $ do
  optionMaybe blankline
  count n spaceChar
  p
  void spaceChar <|> lookAhead eol
  listItemContents

bulletList :: PandocMonad m => MuseParser m (F Blocks)
bulletList = try $ do
  many spaceChar
  pos <- getPosition
  let col = sourceColumn pos
  guard $ col /= 1
  char '-'
  void spaceChar <|> lookAhead eol
  first <- listItemContents
  rest <- many $ listItem (col - 1) (void (char '-'))
  return $ B.bulletList <$> sequence (first : rest)

orderedList :: PandocMonad m => MuseParser m (F Blocks)
orderedList = try $ do
  many spaceChar
  pos <- getPosition
  let col = sourceColumn pos
  guard $ col /= 1
  p@(_, style, delim) <- anyOrderedListMarker
  guard $ style `elem` [Decimal, LowerAlpha, UpperAlpha, LowerRoman, UpperRoman]
  guard $ delim == Period
  void spaceChar <|> lookAhead eol
  first <- listItemContents
  rest <- many $ listItem (col - 1) (void (orderedListMarker style delim))
  return $ B.orderedListWith p <$> sequence (first : rest)

definitionListItem :: PandocMonad m => Int -> MuseParser m (F (Inlines, [Blocks]))
definitionListItem n = try $ do
  count n spaceChar
  pos <- getPosition
  term <- trimInlinesF . mconcat <$> manyTill (choice inlineList) (string "::")
  void spaceChar <|> lookAhead eol
  contents <- listItemContents' $ sourceColumn pos
  pure $ do lineContent' <- contents
            term' <- term
            pure (term', [lineContent'])

definitionList :: PandocMonad m => MuseParser m (F Blocks)
definitionList = try $ do
  many spaceChar
  pos <- getPosition
  (guardDisabled Ext_amuse) <|> (guard (sourceColumn pos /= 1)) -- Initial space is required by Amusewiki, but not Emacs Muse
  first <- definitionListItem 0
  rest <- many $ try (optionMaybe blankline >> definitionListItem (sourceColumn pos - 1))
  return $ B.definitionList <$> sequence (first : rest)

--
-- tables
--

data MuseTable = MuseTable
  { museTableCaption :: Inlines
  , museTableHeaders :: [[Blocks]]
  , museTableRows    :: [[Blocks]]
  , museTableFooters :: [[Blocks]]
  }

data MuseTableElement = MuseHeaderRow (F [Blocks])
                      | MuseBodyRow   (F [Blocks])
                      | MuseFooterRow (F [Blocks])
                      | MuseCaption (F Inlines)

museToPandocTable :: MuseTable -> Blocks
museToPandocTable (MuseTable caption headers body footers) =
  B.table caption attrs headRow rows
  where ncol = maximum (0 : map length (headers ++ body ++ footers))
        attrs = replicate ncol (AlignDefault, 0.0)
        headRow = if null headers then [] else head headers
        rows = (if null headers then [] else tail headers) ++ body ++ footers

museAppendElement :: MuseTable
                  -> MuseTableElement
                  -> F MuseTable
museAppendElement tbl element =
  case element of
    MuseHeaderRow row -> do
      row' <- row
      return tbl{ museTableHeaders = museTableHeaders tbl ++ [row'] }
    MuseBodyRow row -> do
      row' <- row
      return tbl{ museTableRows = museTableRows tbl ++ [row'] }
    MuseFooterRow row-> do
      row' <- row
      return tbl{ museTableFooters = museTableFooters tbl ++ [row'] }
    MuseCaption inlines -> do
      inlines' <- inlines
      return tbl{ museTableCaption = inlines' }

tableCell :: PandocMonad m => MuseParser m (F Blocks)
tableCell = try $ fmap B.plain . trimInlinesF . mconcat <$> manyTill inline (lookAhead cellEnd)
  where cellEnd = try $ void (many1 spaceChar >> char '|') <|> eol

tableElements :: PandocMonad m => MuseParser m [MuseTableElement]
tableElements = tableParseElement `sepEndBy1` eol

elementsToTable :: [MuseTableElement] -> F MuseTable
elementsToTable = foldM museAppendElement emptyTable
  where emptyTable = MuseTable mempty mempty mempty mempty

table :: PandocMonad m => MuseParser m (F Blocks)
table = try $ do
  rows <- tableElements
  let tbl = elementsToTable rows
  let pandocTbl = museToPandocTable <$> tbl :: F Blocks
  return pandocTbl

tableParseElement :: PandocMonad m => MuseParser m MuseTableElement
tableParseElement = tableParseHeader
                <|> tableParseBody
                <|> tableParseFooter
                <|> tableParseCaption

tableParseRow :: PandocMonad m => Int -> MuseParser m (F [Blocks])
tableParseRow n = try $ do
  fields <- tableCell `sepBy2` fieldSep
  return $ sequence fields
    where p `sepBy2` sep = (:) <$> p <*> many1 (sep >> p)
          fieldSep = many1 spaceChar >> count n (char '|') >> (void (many1 spaceChar) <|> void (lookAhead newline))

tableParseHeader :: PandocMonad m => MuseParser m MuseTableElement
tableParseHeader = MuseHeaderRow <$> tableParseRow 2

tableParseBody :: PandocMonad m => MuseParser m MuseTableElement
tableParseBody = MuseBodyRow <$> tableParseRow 1

tableParseFooter :: PandocMonad m => MuseParser m MuseTableElement
tableParseFooter = MuseFooterRow <$> tableParseRow 3

tableParseCaption :: PandocMonad m => MuseParser m MuseTableElement
tableParseCaption = try $ do
  many spaceChar
  string "|+"
  MuseCaption <$> (trimInlinesF . mconcat <$> many1Till inline (string "+|"))

--
-- inline parsers
--

inlineList :: PandocMonad m => [MuseParser m (F Inlines)]
inlineList = [ whitespace
             , br
             , anchor
             , footnote
             , strong
             , strongTag
             , emph
             , emphTag
             , underlined
             , superscriptTag
             , subscriptTag
             , strikeoutTag
             , verbatimTag
             , nbsp
             , link
             , code
             , codeTag
             , inlineLiteralTag
             , str
             , symbol
             ]

inline :: PandocMonad m => MuseParser m (F Inlines)
inline = choice [endline, linebreak] <|> choice inlineList <?> "inline"

endline :: PandocMonad m => MuseParser m (F Inlines)
endline = try $ do
  newline
  notFollowedBy blankline
  returnF B.softbreak

parseAnchor :: PandocMonad m => MuseParser m String
parseAnchor = try $ do
  getPosition >>= \pos -> guard (sourceColumn pos == 1)
  char '#'
  first <- letter
  rest <- many (letter <|> digit)
  skipMany spaceChar <|> void newline
  return $ first:rest

anchor :: PandocMonad m => MuseParser m (F Inlines)
anchor = try $ do
  anchorId <- parseAnchor
  return $ return $ B.spanWith (anchorId, [], []) mempty

footnote :: PandocMonad m => MuseParser m (F Inlines)
footnote = try $ do
  ref <- noteMarker
  return $ do
    notes <- asksF stateNotes'
    case M.lookup ref notes of
      Nothing -> return $ B.str $ "[" ++ ref ++ "]"
      Just (_pos, contents) -> do
        st <- askF
        let contents' = runF contents st { stateNotes' = M.empty }
        return $ B.note contents'

linebreak :: PandocMonad m => MuseParser m (F Inlines)
linebreak = try $ do
  skipMany spaceChar
  newline
  notFollowedBy newline
  return $ return B.space

whitespace :: PandocMonad m => MuseParser m (F Inlines)
whitespace = try $ do
  skipMany1 spaceChar
  return $ return B.space

br :: PandocMonad m => MuseParser m (F Inlines)
br = try $ do
  string "<br>"
  return $ return B.linebreak

emphasisBetween :: (PandocMonad m, Show a) => MuseParser m a -> MuseParser m (F Inlines)
emphasisBetween c = try $ enclosedInlines c c

enclosedInlines :: (PandocMonad m, Show a, Show b)
                => MuseParser m a
                -> MuseParser m b
                -> MuseParser m (F Inlines)
enclosedInlines start end = try $
  trimInlinesF . mconcat <$> (enclosed (atStart start) end inline <* notFollowedBy (satisfy isLetter))

inlineTag :: PandocMonad m
          => (Inlines -> Inlines)
          -> String
          -> MuseParser m (F Inlines)
inlineTag f s = try $ do
  res <- parseHtmlContent s inline
  return $ f <$> mconcat res

strongTag :: PandocMonad m => MuseParser m (F Inlines)
strongTag = inlineTag B.strong "strong"

strong :: PandocMonad m => MuseParser m (F Inlines)
strong = fmap B.strong <$> emphasisBetween (string "**")

emph :: PandocMonad m => MuseParser m (F Inlines)
emph = fmap B.emph <$> emphasisBetween (char '*')

underlined :: PandocMonad m => MuseParser m (F Inlines)
underlined = do
  guardDisabled Ext_amuse -- Supported only by Emacs Muse
  fmap underlineSpan <$> emphasisBetween (char '_')

emphTag :: PandocMonad m => MuseParser m (F Inlines)
emphTag = inlineTag B.emph "em"

superscriptTag :: PandocMonad m => MuseParser m (F Inlines)
superscriptTag = inlineTag B.superscript "sup"

subscriptTag :: PandocMonad m => MuseParser m (F Inlines)
subscriptTag = inlineTag B.subscript "sub"

strikeoutTag :: PandocMonad m => MuseParser m (F Inlines)
strikeoutTag = inlineTag B.strikeout "del"

verbatimTag :: PandocMonad m => MuseParser m (F Inlines)
verbatimTag = do
  content <- parseHtmlContent "verbatim" anyChar
  return $ return $ B.text content

nbsp :: PandocMonad m => MuseParser m (F Inlines)
nbsp = try $ do
  string "~~"
  return $ return $ B.str "\160"

code :: PandocMonad m => MuseParser m (F Inlines)
code = try $ do
  atStart $ char '='
  contents <- many1Till (noneOf "\n\r" <|> (newline <* notFollowedBy newline)) $ char '='
  guard $ not $ null contents
  guard $ head contents `notElem` " \t\n"
  guard $ last contents `notElem` " \t\n"
  notFollowedBy $ satisfy isLetter
  return $ return $ B.code contents

codeTag :: PandocMonad m => MuseParser m (F Inlines)
codeTag = do
  (attrs, content) <- htmlElement "code"
  return $ return $ B.codeWith attrs content

inlineLiteralTag :: PandocMonad m => MuseParser m (F Inlines)
inlineLiteralTag = do
  guardDisabled Ext_amuse -- Text::Amuse does not support <literal>
  (attrs, content) <- parseHtmlContentWithAttrs "literal" anyChar
  return $ return $ rawInline (attrs, content)
  where
    -- FIXME: Emacs Muse inserts <literal> without style into all output formats, but we assume HTML
    format (_, _, kvs)        = fromMaybe "html" $ lookup "style" kvs
    rawInline (attrs, content) = B.rawInline (format attrs) content

str :: PandocMonad m => MuseParser m (F Inlines)
str = do
  result <- many1 alphaNum
  updateLastStrPos
  return $ return $ B.str result

symbol :: PandocMonad m => MuseParser m (F Inlines)
symbol = return . B.str <$> count 1 nonspaceChar

link :: PandocMonad m => MuseParser m (F Inlines)
link = try $ do
  st <- getState
  guard $ stateAllowLinks st
  setState $ st{ stateAllowLinks = False }
  (url, title, content) <- linkText
  setState $ st{ stateAllowLinks = True }
  return $ case stripPrefix "URL:" url of
             Nothing -> if isImageUrl url
                          then B.image url title <$> fromMaybe (return mempty) content
                          else B.link url title <$> fromMaybe (return $ B.str url) content
             Just url' -> B.link url' title <$> fromMaybe (return $ B.str url') content
    where -- Taken from muse-image-regexp defined in Emacs Muse file lisp/muse-regexps.el
          imageExtensions = [".eps", ".gif", ".jpg", ".jpeg", ".pbm", ".png", ".tiff", ".xbm", ".xpm"]
          isImageUrl = (`elem` imageExtensions) . takeExtension

linkContent :: PandocMonad m => MuseParser m (F Inlines)
linkContent = do
  char '['
  trimInlinesF . mconcat <$> many1Till inline (string "]")

linkText :: PandocMonad m => MuseParser m (String, String, Maybe (F Inlines))
linkText = do
  string "[["
  url <- many1Till anyChar $ char ']'
  content <- optionMaybe linkContent
  char ']'
  return (url, "", content)
