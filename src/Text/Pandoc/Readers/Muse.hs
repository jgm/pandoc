{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
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
- <cite> tag
-}
module Text.Pandoc.Readers.Muse (readMuse) where

import Prelude
import Control.Monad
import Control.Monad.Except (throwError)
import Data.Bifunctor
import Data.Char (isLetter)
import Data.Default
import Data.List (stripPrefix, intercalate)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text, unpack)
import System.FilePath (takeExtension)
import Text.HTML.TagSoup
import Text.Pandoc.Builder (Blocks, Inlines)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class (PandocMonad (..))
import Text.Pandoc.Definition
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (F)
import Text.Pandoc.Readers.HTML (htmlTag)
import Text.Pandoc.Shared (crFilter, underlineSpan)

-- | Read Muse from an input string and return a Pandoc document.
readMuse :: PandocMonad m
         => ReaderOptions
         -> Text
         -> m Pandoc
readMuse opts s = do
  res <- readWithM parseMuse def{ museOptions = opts } (unpack (crFilter s))
  case res of
       Left e  -> throwError e
       Right d -> return d

type F = Future MuseState

data MuseState = MuseState { museMeta :: F Meta -- ^ Document metadata
                           , museOptions :: ReaderOptions
                           , museHeaders :: M.Map Inlines String -- ^ List of headers and ids (used for implicit ref links)
                           , museIdentifierList :: Set.Set String
                           , museLastStrPos :: Maybe SourcePos -- ^ Position after last str parsed
                           , museLogMessages :: [LogMessage]
                           , museNotes :: M.Map String (SourcePos, F Blocks)
                           , museInLink :: Bool -- ^ True when parsing a link description to avoid nested links
                           , museInPara :: Bool -- ^ True when looking for a paragraph terminator
                           }

instance Default MuseState where
  def = MuseState { museMeta = return nullMeta
                  , museOptions = def
                  , museHeaders = M.empty
                  , museIdentifierList = Set.empty
                  , museLastStrPos = Nothing
                  , museLogMessages = []
                  , museNotes = M.empty
                  , museInLink = False
                  , museInPara = False
                  }

type MuseParser = ParserT String MuseState

instance HasReaderOptions MuseState where
  extractReaderOptions = museOptions

instance HasHeaderMap MuseState where
  extractHeaderMap     = museHeaders
  updateHeaderMap f st = st{ museHeaders = f $ museHeaders st }

instance HasIdentifierList MuseState where
  extractIdentifierList     = museIdentifierList
  updateIdentifierList f st = st{ museIdentifierList = f $ museIdentifierList st }

instance HasLastStrPosition MuseState where
  setLastStrPos pos st = st{ museLastStrPos = Just pos }
  getLastStrPos st     = museLastStrPos st

instance HasLogMessages MuseState where
  addLogMessage m s = s{ museLogMessages = m : museLogMessages s }
  getLogMessages = reverse . museLogMessages

-- | Parse Muse document
parseMuse :: PandocMonad m => MuseParser m Pandoc
parseMuse = do
  many directive
  blocks <- parseBlocks
  st <- getState
  let doc = runF (do Pandoc _ bs <- B.doc <$> blocks
                     meta <- museMeta st
                     return $ Pandoc meta bs) st
  reportLogMessages
  return doc

-- * Utility functions

commonPrefix :: String -> String -> String
commonPrefix _ [] = []
commonPrefix [] _ = []
commonPrefix (x:xs) (y:ys)
  | x == y    = x : commonPrefix xs ys
  | otherwise = []

-- | Trim up to one newline from the beginning of the string.
lchop :: String -> String
lchop s = case s of
                    '\n':ss -> ss
                    _       -> s

-- | Trim up to one newline from the end of the string.
rchop :: String -> String
rchop = reverse . lchop . reverse

dropSpacePrefix :: [String] -> [String]
dropSpacePrefix lns =
  map (drop maxIndent) lns
  where flns = filter (not . all (== ' ')) lns
        maxIndent = if null flns then maximum (map length lns) else length $ takeWhile (== ' ') $ foldl1 commonPrefix flns

atStart :: PandocMonad m => MuseParser m a -> MuseParser m a
atStart p = do
  pos <- getPosition
  st <- getState
  guard $ museLastStrPos st /= Just pos
  p

-- * Parsers

-- | Parse end-of-line, which can be either a newline or end-of-file.
eol :: Stream s m Char => ParserT s st m ()
eol = void newline <|> eof

someUntil :: (Stream s m t)
          => ParserT s u m a
          -> ParserT s u m b
          -> ParserT s u m ([a], b)
someUntil p end = first <$> ((:) <$> p) <*> manyUntil p end

-- ** HTML parsers

-- | Parse HTML tag, returning its attributes and literal contents.
htmlElement :: PandocMonad m
            => String -- ^ Tag name
            -> MuseParser m (Attr, String)
htmlElement tag = try $ do
  (TagOpen _ attr, _) <- htmlTag (~== TagOpen tag [])
  content <- manyTill anyChar endtag
  return (htmlAttrToPandoc attr, content)
  where
    endtag = void $ htmlTag (~== TagClose tag)

htmlBlock :: PandocMonad m
          => String -- ^ Tag name
          -> MuseParser m (Attr, String)
htmlBlock tag = try $ do
  many spaceChar
  res <- htmlElement tag
  manyTill spaceChar eol
  return res

-- | Convert HTML attributes to Pandoc 'Attr'
htmlAttrToPandoc :: [Attribute String] -> Attr
htmlAttrToPandoc attrs = (ident, classes, keyvals)
  where
    ident   = fromMaybe "" $ lookup "id" attrs
    classes = maybe [] words $ lookup "class" attrs
    keyvals = [(k,v) | (k,v) <- attrs, k /= "id" && k /= "class"]

parseHtmlContent :: PandocMonad m
                 => String -- ^ Tag name
                 -> MuseParser m (Attr, F Blocks)
parseHtmlContent tag = try $ do
  many spaceChar
  pos <- getPosition
  (TagOpen _ attr, _) <- htmlTag (~== TagOpen tag [])
  manyTill spaceChar eol
  content <- parseBlocksTill $ try $ count (sourceColumn pos - 1) spaceChar >> endtag
  manyTill spaceChar eol -- closing tag must be followed by optional whitespace and newline
  return (htmlAttrToPandoc attr, content)
  where
    endtag = void $ htmlTag (~== TagClose tag)

-- ** Directive parsers

-- While not documented, Emacs Muse allows "-" in directive name
parseDirectiveKey :: PandocMonad m => MuseParser m String
parseDirectiveKey = char '#' *> many (letter <|> char '-')

parseEmacsDirective :: PandocMonad m => MuseParser m (String, F Inlines)
parseEmacsDirective = do
  key <- parseDirectiveKey
  spaceChar
  value <- trimInlinesF . mconcat <$> manyTill (choice inlineList) eol
  return (key, value)

parseAmuseDirective :: PandocMonad m => MuseParser m (String, F Inlines)
parseAmuseDirective = do
  key <- parseDirectiveKey
  many1 spaceChar
  value <- trimInlinesF . mconcat <$> many1Till inline endOfDirective
  many blankline
  return (key, value)
  where
    endOfDirective = lookAhead $ eof <|> try (newline >> (void blankline <|> void parseDirectiveKey))

directive :: PandocMonad m => MuseParser m ()
directive = do
  ext <- getOption readerExtensions
  (key, value) <- if extensionEnabled Ext_amuse ext then parseAmuseDirective else parseEmacsDirective
  updateState $ \st -> st { museMeta = B.setMeta (translateKey key) <$> value <*> museMeta st }
  where translateKey "cover" = "cover-image"
        translateKey x = x

-- ** Block parsers

parseBlocks :: PandocMonad m
            => MuseParser m (F Blocks)
parseBlocks =
  try (parseEnd <|>
       blockStart <|>
       listStart <|>
       paraStart)
  where
    parseEnd = mempty <$ eof
    blockStart = (B.<>) <$> (header <|> blockElements <|> emacsNoteBlock)
                        <*> parseBlocks
    listStart = do
      updateState (\st -> st { museInPara = False })
      uncurry (B.<>) <$> (anyListUntil parseBlocks <|> amuseNoteBlockUntil parseBlocks)
    paraStart = do
      indent <- length <$> many spaceChar
      uncurry (B.<>) . first (p indent) <$> paraUntil parseBlocks
      where p indent = if indent >= 2 && indent < 6 then fmap B.blockQuote else id

parseBlocksTill :: PandocMonad m
                => MuseParser m a
                -> MuseParser m (F Blocks)
parseBlocksTill end =
  try (parseEnd <|>
       blockStart <|>
       listStart <|>
       paraStart)
  where
    parseEnd = mempty <$ end
    blockStart = (B.<>) <$> blockElements <*> continuation
    listStart = do
      updateState (\st -> st { museInPara = False })
      uncurry (B.<>) <$> anyListUntil (parseEnd <|> continuation)
    paraStart = uncurry (B.<>) <$> paraUntil (parseEnd <|> continuation)
    continuation = parseBlocksTill end

listItemContentsUntil :: PandocMonad m
                      => Int
                      -> MuseParser m a
                      -> MuseParser m a
                      -> MuseParser m (F Blocks, a)
listItemContentsUntil col pre end =
  try blockStart <|>
  try listStart <|>
  try paraStart
  where
    parsePre = (mempty,) <$> pre
    parseEnd = (mempty,) <$> end
    paraStart = do
      (f, (r, e)) <- paraUntil (parsePre <|> continuation <|> parseEnd)
      return (f B.<> r, e)
    blockStart = first <$> ((B.<>) <$> blockElements)
                       <*> (parsePre <|> continuation <|> parseEnd)
    listStart = do
      updateState (\st -> st { museInPara = False })
      (f, (r, e)) <- anyListUntil (parsePre <|> continuation <|> parseEnd)
      return (f B.<> r, e)
    continuation = try $ do blank <- optionMaybe blankline
                            skipMany blankline
                            indentWith col
                            updateState (\st -> st { museInPara = museInPara st && isNothing blank })
                            listItemContentsUntil col pre end

parseBlock :: PandocMonad m => MuseParser m (F Blocks)
parseBlock = do
  res <- blockElements <|> para
  trace (take 60 $ show $ B.toList $ runF res def)
  return res
  where para = fst <$> paraUntil (try (eof <|> void (lookAhead blockElements)))

blockElements :: PandocMonad m => MuseParser m (F Blocks)
blockElements = do
  updateState (\st -> st { museInPara = False })
  choice [ mempty <$ blankline
         , comment
         , separator
         , example
         , exampleTag
         , literalTag
         , centerTag
         , rightTag
         , quoteTag
         , divTag
         , biblioTag
         , playTag
         , verseTag
         , lineBlock
         , table
         , commentTag
         ]

-- | Parse a line comment, starting with @;@ in the first column.
comment :: PandocMonad m => MuseParser m (F Blocks)
comment = try $ do
  char ';'
  optional (spaceChar >> many (noneOf "\n"))
  eol
  return mempty

-- | Parse a horizontal rule, consisting of 4 or more @\'-\'@ characters.
separator :: PandocMonad m => MuseParser m (F Blocks)
separator = try $ do
  string "----"
  many $ char '-'
  many spaceChar
  eol
  return $ return B.horizontalRule

-- | Parse a heading.
header :: PandocMonad m => MuseParser m (F Blocks)
header = try $ do
  anchorId <- option "" $ try (parseAnchor <* manyTill spaceChar eol)
  getPosition >>= \pos -> guard (sourceColumn pos == 1)
  level <- fmap length $ many1 $ char '*'
  guard $ level <= 5
  spaceChar
  content <- trimInlinesF . mconcat <$> manyTill inline eol
  attr <- registerHeader (anchorId, [], []) (runF content def)
  return $ B.headerWith attr level <$> content

-- | Parse an example between @{{{@ and @}}}@.
-- It is an Amusewiki extension influenced by Creole wiki, as described in @Text::Amuse@ documentation.
example :: PandocMonad m => MuseParser m (F Blocks)
example = try $ do
  string "{{{"
  optional blankline
  contents <- manyTill anyChar $ try (optional blankline >> string "}}}")
  return $ return $ B.codeBlock contents

-- | Parse an @\<example>@ tag.
exampleTag :: PandocMonad m => MuseParser m (F Blocks)
exampleTag = try $ do
  (attr, contents) <- htmlBlock "example"
  return $ return $ B.codeBlockWith attr $ rchop $ intercalate "\n" $ dropSpacePrefix $ splitOn "\n" $ lchop contents

-- | Parse a @\<literal>@ tag as a raw block.
-- For 'RawInline' @\<literal>@ parser, see 'inlineLiteralTag'.
literalTag :: PandocMonad m => MuseParser m (F Blocks)
literalTag = try $ do
  many spaceChar
  (TagOpen _ attr, _) <- htmlTag (~== TagOpen "literal" [])
  manyTill spaceChar eol
  content <- manyTill anyChar endtag
  manyTill spaceChar eol
  return $ return $ rawBlock (htmlAttrToPandoc attr, content)
  where
    endtag = void $ htmlTag (~== TagClose "literal")
    -- FIXME: Emacs Muse inserts <literal> without style into all output formats, but we assume HTML
    format (_, _, kvs)        = fromMaybe "html" $ lookup "style" kvs
    rawBlock (attrs, content) = B.rawBlock (format attrs) $ rchop $ intercalate "\n" $ dropSpacePrefix $ splitOn "\n" $ lchop content

-- | Parse @\<center>@ tag.
-- Currently it is ignored as Pandoc cannot represent centered blocks.
centerTag :: PandocMonad m => MuseParser m (F Blocks)
centerTag = snd <$> parseHtmlContent "center"

-- | Parse @\<right>@ tag.
-- Currently it is ignored as Pandoc cannot represent centered blocks.
rightTag :: PandocMonad m => MuseParser m (F Blocks)
rightTag = snd <$> parseHtmlContent "right"

-- | Parse @\<quote>@ tag.
quoteTag :: PandocMonad m => MuseParser m (F Blocks)
quoteTag = fmap B.blockQuote . snd <$> parseHtmlContent "quote"

-- | Parse @\<div>@ tag.
-- @\<div>@ tag is supported by Emacs Muse, but not Amusewiki 2.025.
divTag :: PandocMonad m => MuseParser m (F Blocks)
divTag = do
  (attrs, content) <- parseHtmlContent "div"
  return $ B.divWith attrs <$> content

-- | Parse @\<biblio>@ tag, the result is the same as @\<div class="biblio">@.
-- @\<biblio>@ tag is supported only in Text::Amuse mode.
biblioTag :: PandocMonad m => MuseParser m (F Blocks)
biblioTag = do
  guardEnabled Ext_amuse
  fmap (B.divWith ("", ["biblio"], [])) . snd <$> parseHtmlContent "biblio"

-- | Parse @\<play>@ tag, the result is the same as @\<div class="play">@.
-- @\<play>@ tag is supported only in Text::Amuse mode.
playTag :: PandocMonad m => MuseParser m (F Blocks)
playTag = do
  guardEnabled Ext_amuse
  fmap (B.divWith ("", ["play"], [])) . snd <$> parseHtmlContent "play"

verseLine :: PandocMonad m => MuseParser m (F Inlines)
verseLine = do
  indent <- (B.str <$> many1 (char ' ' >> pure '\160')) <|> pure mempty
  rest <- manyTill (choice inlineList) newline
  return $ trimInlinesF $ mconcat (pure indent : rest)

verseLines :: PandocMonad m => MuseParser m (F Blocks)
verseLines = do
  lns <- many verseLine
  return $ B.lineBlock <$> sequence lns

-- | Parse @\<verse>@ tag.
verseTag :: PandocMonad m => MuseParser m (F Blocks)
verseTag = do
  (_, content) <- htmlBlock "verse"
  parseFromString verseLines (intercalate "\n" $ dropSpacePrefix $ splitOn "\n" $ lchop content)

-- | Parse @\<comment>@ tag.
commentTag :: PandocMonad m => MuseParser m (F Blocks)
commentTag = htmlBlock "comment" >> return mempty

-- | Parse a paragraph.
paraUntil :: PandocMonad m
          => MuseParser m a -- ^ Terminator parser
          -> MuseParser m (F Blocks, a)
paraUntil end = do
  state <- getState
  guard $ not $ museInPara state
  setState $ state{ museInPara = True }
  (l, e) <- someUntil inline $ try (manyTill spaceChar eol >> end)
  updateState (\st -> st { museInPara = False })
  return (fmap B.para $ trimInlinesF $ mconcat l, e)

noteMarker :: PandocMonad m => MuseParser m String
noteMarker = try $ do
  char '['
  (:) <$> oneOf "123456789" <*> manyTill digit (char ']')

-- Amusewiki version of note
-- Parsing is similar to list item, except that note marker is used instead of list marker
amuseNoteBlockUntil :: PandocMonad m
                    => MuseParser m a
                    -> MuseParser m (F Blocks, a)
amuseNoteBlockUntil end = try $ do
  guardEnabled Ext_amuse
  ref <- noteMarker <* spaceChar
  pos <- getPosition
  updateState (\st -> st { museInPara = False })
  (content, e) <- listItemContentsUntil (sourceColumn pos - 1) (fail "x") end
  oldnotes <- museNotes <$> getState
  when (isJust (M.lookup ref oldnotes))
    (logMessage $ DuplicateNoteReference ref pos)
  updateState $ \s -> s{ museNotes = M.insert ref (pos, content) oldnotes }
  return (mempty, e)

-- Emacs version of note
-- Notes are allowed only at the end of text, no indentation is required.
emacsNoteBlock :: PandocMonad m => MuseParser m (F Blocks)
emacsNoteBlock = try $ do
  guardDisabled Ext_amuse
  pos <- getPosition
  ref <- noteMarker <* skipSpaces
  content <- mconcat <$> blocksTillNote
  oldnotes <- museNotes <$> getState
  when (isJust (M.lookup ref oldnotes))
    (logMessage $ DuplicateNoteReference ref pos)
  updateState $ \s -> s{ museNotes = M.insert ref (pos, content) oldnotes }
  return mempty
  where
    blocksTillNote =
      many1Till parseBlock (eof <|> () <$ lookAhead noteMarker)

--
-- Verse markup
--

lineVerseLine :: PandocMonad m => MuseParser m (F Inlines)
lineVerseLine = try $ do
  string "> "
  indent <- many (char ' ' >> pure '\160')
  let indentEl = if null indent then mempty else B.str indent
  rest <- manyTill (choice inlineList) eol
  return $ trimInlinesF $ mconcat (pure indentEl : rest)

blanklineVerseLine :: PandocMonad m => MuseParser m (F Inlines)
blanklineVerseLine = try $ do
  char '>'
  blankline
  pure mempty

-- | Parse a line block indicated by @\'>\'@ characters.
lineBlock :: PandocMonad m => MuseParser m (F Blocks)
lineBlock = try $ do
  col <- sourceColumn <$> getPosition
  lns <- (blanklineVerseLine <|> lineVerseLine) `sepBy1'` try (indentWith (col - 1))
  return $ B.lineBlock <$> sequence lns

-- *** List parsers

bulletListItemsUntil :: PandocMonad m
                     => Int -- ^ Indentation
                     -> MuseParser m a -- ^ Terminator parser
                     -> MuseParser m ([F Blocks], a)
bulletListItemsUntil indent end = try $ do
  char '-'
  void spaceChar <|> lookAhead eol
  updateState (\st -> st { museInPara = False })
  (x, e) <- listItemContentsUntil (indent + 2) (Right <$> try (optional blankline >> indentWith indent >> bulletListItemsUntil indent end)) (Left <$> end)
  case e of
    Left ee -> return ([x], ee)
    Right (xs, ee) -> return (x:xs, ee)

-- | Parse a bullet list.
bulletListUntil :: PandocMonad m
                => MuseParser m a
                -> MuseParser m (F Blocks, a)
bulletListUntil end = try $ do
  many spaceChar
  pos <- getPosition
  let indent = sourceColumn pos - 1
  guard $ indent /= 0
  (items, e) <- bulletListItemsUntil indent end
  return (B.bulletList <$> sequence items, e)

-- | Parses an ordered list marker and returns list attributes.
anyMuseOrderedListMarker :: PandocMonad m => MuseParser m ListAttributes
anyMuseOrderedListMarker = do
  (style, start) <- decimal <|> lowerRoman <|> upperRoman <|> lowerAlpha <|> upperAlpha
  char '.'
  return (start, style, Period)

museOrderedListMarker :: PandocMonad m
                      => ListNumberStyle
                      -> MuseParser m Int
museOrderedListMarker style =
  snd <$> p <* char '.'
  where p = case style of
              Decimal    -> decimal
              UpperRoman -> upperRoman
              LowerRoman -> lowerRoman
              UpperAlpha -> upperAlpha
              LowerAlpha -> lowerAlpha
              _          -> fail "Unhandled case"

orderedListItemsUntil :: PandocMonad m
                      => Int
                      -> ListNumberStyle
                      -> MuseParser m a
                      -> MuseParser m ([F Blocks], a)
orderedListItemsUntil indent style end =
  continuation
  where
    continuation = try $ do
      pos <- getPosition
      void spaceChar <|> lookAhead eol
      updateState (\st -> st { museInPara = False })
      (x, e) <- listItemContentsUntil (sourceColumn pos) (Right <$> try (optional blankline >> indentWith indent >> museOrderedListMarker style >> continuation)) (Left <$> end)
      case e of
        Left ee -> return ([x], ee)
        Right (xs, ee) -> return (x:xs, ee)

-- | Parse an ordered list.
orderedListUntil :: PandocMonad m
                 => MuseParser m a
                 -> MuseParser m (F Blocks, a)
orderedListUntil end = try $ do
  many spaceChar
  pos <- getPosition
  let indent = sourceColumn pos - 1
  guard $ indent /= 0
  p@(_, style, _) <- anyMuseOrderedListMarker
  guard $ style `elem` [Decimal, LowerAlpha, UpperAlpha, LowerRoman, UpperRoman]
  (items, e) <- orderedListItemsUntil indent style end
  return (B.orderedListWith p <$> sequence items, e)

descriptionsUntil :: PandocMonad m
                  => Int
                  -> MuseParser m a
                  -> MuseParser m ([F Blocks], a)
descriptionsUntil indent end = do
  void spaceChar <|> lookAhead eol
  updateState (\st -> st { museInPara = False })
  (x, e) <- listItemContentsUntil indent (Right <$> try (optional blankline >> indentWith indent >> manyTill spaceChar (string "::") >> descriptionsUntil indent end)) (Left <$> end)
  case e of
    Right (xs, ee) -> return (x:xs, ee)
    Left ee -> return ([x], ee)

definitionListItemsUntil :: PandocMonad m
                         => Int
                         -> MuseParser m a
                         -> MuseParser m ([F (Inlines, [Blocks])], a)
definitionListItemsUntil indent end =
  continuation
  where
    continuation = try $ do
      pos <- getPosition
      term <- trimInlinesF . mconcat <$> manyTill (choice inlineList) (try $ string "::")
      (x, e) <- descriptionsUntil (sourceColumn pos) ((Right <$> try (optional blankline >> indentWith indent >> continuation)) <|> (Left <$> end))
      let xx = do
            term' <- term
            x' <- sequence x
            return (term', x')
      case e of
        Left ee -> return ([xx], ee)
        Right (xs, ee) -> return (xx:xs, ee)

-- | Parse a definition list.
definitionListUntil :: PandocMonad m
                    => MuseParser m a -- ^ Terminator parser
                    -> MuseParser m (F Blocks, a)
definitionListUntil end = try $ do
  many spaceChar
  pos <- getPosition
  let indent = sourceColumn pos - 1
  guardDisabled Ext_amuse <|> guard (indent /= 0) -- Initial space is required by Amusewiki, but not Emacs Muse
  first (fmap B.definitionList . sequence) <$> definitionListItemsUntil indent end

anyListUntil :: PandocMonad m
             => MuseParser m a -- ^ Terminator parser
             -> MuseParser m (F Blocks, a)
anyListUntil end =
  bulletListUntil end <|> orderedListUntil end <|> definitionListUntil end

-- *** Table parsers

-- | Internal Muse table representation.
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

-- | Parse a table.
table :: PandocMonad m => MuseParser m (F Blocks)
table = try $ fmap museToPandocTable <$> (elementsToTable <$> tableElements)

tableParseElement :: PandocMonad m => MuseParser m MuseTableElement
tableParseElement = tableParseHeader
                <|> tableParseBody
                <|> tableParseFooter
                <|> tableParseCaption

tableParseRow :: PandocMonad m
              => Int -- ^ Number of separator characters
              -> MuseParser m (F [Blocks])
tableParseRow n = try $ do
  fields <- tableCell `sepBy2` fieldSep
  return $ sequence fields
    where p `sepBy2` sep = (:) <$> p <*> many1 (sep >> p)
          fieldSep = many1 spaceChar >> count n (char '|') >> (void (many1 spaceChar) <|> void (lookAhead newline))

-- | Parse a table header row.
tableParseHeader :: PandocMonad m => MuseParser m MuseTableElement
tableParseHeader = MuseHeaderRow <$> tableParseRow 2

-- | Parse a table body row.
tableParseBody :: PandocMonad m => MuseParser m MuseTableElement
tableParseBody = MuseBodyRow <$> tableParseRow 1

-- | Parse a table footer row.
tableParseFooter :: PandocMonad m => MuseParser m MuseTableElement
tableParseFooter = MuseFooterRow <$> tableParseRow 3

-- | Parse table caption.
tableParseCaption :: PandocMonad m => MuseParser m MuseTableElement
tableParseCaption = try $ do
  many spaceChar
  string "|+"
  MuseCaption <$> (trimInlinesF . mconcat <$> many1Till inline (string "+|"))

-- ** Inline parsers

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
             , classTag
             , nbsp
             , link
             , code
             , codeTag
             , mathTag
             , inlineLiteralTag
             , str
             , symbol
             ]

inline :: PandocMonad m => MuseParser m (F Inlines)
inline = endline <|> choice inlineList <?> "inline"

-- | Parse a soft break.
endline :: PandocMonad m => MuseParser m (F Inlines)
endline = try $ do
  newline
  notFollowedBy blankline
  return $ return B.softbreak

parseAnchor :: PandocMonad m => MuseParser m String
parseAnchor = try $ do
  getPosition >>= \pos -> guard (sourceColumn pos == 1)
  char '#'
  (:) <$> letter <*> many (letter <|> digit)

anchor :: PandocMonad m => MuseParser m (F Inlines)
anchor = try $ do
  anchorId <- parseAnchor
  skipMany spaceChar <|> void newline
  return $ return $ B.spanWith (anchorId, [], []) mempty

-- | Parse a footnote reference.
footnote :: PandocMonad m => MuseParser m (F Inlines)
footnote = try $ do
  ref <- noteMarker
  return $ do
    notes <- asksF museNotes
    case M.lookup ref notes of
      Nothing -> return $ B.str $ "[" ++ ref ++ "]"
      Just (_pos, contents) -> do
        st <- askF
        let contents' = runF contents st { museNotes = M.empty }
        return $ B.note contents'

whitespace :: PandocMonad m => MuseParser m (F Inlines)
whitespace = try $ do
  skipMany1 spaceChar
  return $ return B.space

-- | Parse @\<br>@ tag.
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

-- | Parse an inline tag, such as @\<em>@ and @\<strong>@.
inlineTag :: PandocMonad m
          => String -- ^ Tag name
          -> MuseParser m (F Inlines)
inlineTag tag = try $ do
  htmlTag (~== TagOpen tag [])
  mconcat <$> manyTill inline (void $ htmlTag (~== TagClose tag))

-- | Parse strong inline markup, indicated by @**@.
strong :: PandocMonad m => MuseParser m (F Inlines)
strong = fmap B.strong <$> emphasisBetween (string "**")

-- | Parse emphasis inline markup, indicated by @*@.
emph :: PandocMonad m => MuseParser m (F Inlines)
emph = fmap B.emph <$> emphasisBetween (char '*')

-- | Parse underline inline markup, indicated by @_@.
-- Supported only in Emacs Muse mode, not Text::Amuse.
underlined :: PandocMonad m => MuseParser m (F Inlines)
underlined = do
  guardDisabled Ext_amuse -- Supported only by Emacs Muse
  fmap underlineSpan <$> emphasisBetween (char '_')

-- | Parse @\<strong>@ tag.
strongTag :: PandocMonad m => MuseParser m (F Inlines)
strongTag = fmap B.strong <$> inlineTag "strong"

-- | Parse @\<em>@ tag.
emphTag :: PandocMonad m => MuseParser m (F Inlines)
emphTag = fmap B.emph <$> inlineTag "em"

-- | Parse @\<sup>@ tag.
superscriptTag :: PandocMonad m => MuseParser m (F Inlines)
superscriptTag = fmap B.superscript <$> inlineTag "sup"

-- | Parse @\<sub>@ tag.
subscriptTag :: PandocMonad m => MuseParser m (F Inlines)
subscriptTag = fmap B.subscript <$> inlineTag "sub"

-- | Parse @\<del>@ tag.
strikeoutTag :: PandocMonad m => MuseParser m (F Inlines)
strikeoutTag = fmap B.strikeout <$> inlineTag "del"

-- | Parse @\<verbatim>@ tag.
verbatimTag :: PandocMonad m => MuseParser m (F Inlines)
verbatimTag = return . B.text . snd <$> htmlElement "verbatim"

-- | Parse @\<class>@ tag.
classTag :: PandocMonad m => MuseParser m (F Inlines)
classTag = do
  (TagOpen _ attrs, _) <- htmlTag (~== TagOpen "class" [])
  res <- manyTill inline (void $ htmlTag (~== TagClose "class"))
  let classes = maybe [] words $ lookup "name" attrs
  return $ B.spanWith ("", classes, []) <$> mconcat res

-- | Parse "~~" as nonbreaking space.
nbsp :: PandocMonad m => MuseParser m (F Inlines)
nbsp = try $ do
  string "~~"
  return $ return $ B.str "\160"

-- | Parse code markup, indicated by @\'=\'@ characters.
code :: PandocMonad m => MuseParser m (F Inlines)
code = try $ do
  atStart $ char '='
  contents <- many1Till (noneOf "\n\r" <|> (newline <* notFollowedBy newline)) $ char '='
  guard $ not $ null contents
  guard $ head contents `notElem` " \t\n"
  guard $ last contents `notElem` " \t\n"
  notFollowedBy $ satisfy isLetter
  return $ return $ B.code contents

-- | Parse @\<code>@ tag.
codeTag :: PandocMonad m => MuseParser m (F Inlines)
codeTag = return . uncurry B.codeWith <$> htmlElement "code"

-- | Parse @\<math>@ tag.
-- @\<math>@ tag is an Emacs Muse extension enabled by @(require 'muse-latex2png)@
mathTag :: PandocMonad m => MuseParser m (F Inlines)
mathTag = return . B.math . snd <$> htmlElement "math"

-- | Parse inline @\<literal>@ tag as a raw inline.
inlineLiteralTag :: PandocMonad m => MuseParser m (F Inlines)
inlineLiteralTag =
  (return . rawInline) <$> htmlElement "literal"
  where
    -- FIXME: Emacs Muse inserts <literal> without style into all output formats, but we assume HTML
    format (_, _, kvs)        = fromMaybe "html" $ lookup "style" kvs
    rawInline (attrs, content) = B.rawInline (format attrs) content

str :: PandocMonad m => MuseParser m (F Inlines)
str = return . B.str <$> many1 alphaNum <* updateLastStrPos

symbol :: PandocMonad m => MuseParser m (F Inlines)
symbol = return . B.str <$> count 1 nonspaceChar

-- | Parse a link or image.
link :: PandocMonad m => MuseParser m (F Inlines)
link = try $ do
  st <- getState
  guard $ not $ museInLink st
  setState $ st{ museInLink = True }
  (url, content) <- linkText
  updateState (\state -> state { museInLink = False })
  return $ case stripPrefix "URL:" url of
             Nothing -> if isImageUrl url
                          then B.image url "" <$> fromMaybe (return mempty) content
                          else B.link url "" <$> fromMaybe (return $ B.str url) content
             Just url' -> B.link url' "" <$> fromMaybe (return $ B.str url') content
    where -- Taken from muse-image-regexp defined in Emacs Muse file lisp/muse-regexps.el
          imageExtensions = [".eps", ".gif", ".jpg", ".jpeg", ".pbm", ".png", ".tiff", ".xbm", ".xpm"]
          isImageUrl = (`elem` imageExtensions) . takeExtension

linkContent :: PandocMonad m => MuseParser m (F Inlines)
linkContent = char '[' >> trimInlinesF . mconcat <$> manyTill inline (string "]")

linkText :: PandocMonad m => MuseParser m (String, Maybe (F Inlines))
linkText = do
  string "[["
  url <- many1Till anyChar $ char ']'
  content <- optionMaybe linkContent
  char ']'
  return (url, content)
