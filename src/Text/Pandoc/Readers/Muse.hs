{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Muse
   Copyright   : Copyright (C) 2017-2020 Alexander Krotov
   License     : GNU GPL, version 2 or above

   Maintainer  : Alexander Krotov <ilabdsf@gmail.com>
   Stability   : alpha
   Portability : portable

Conversion of Muse text to 'Pandoc' document.
-}
{-
TODO:
- <cite> tag
-}
module Text.Pandoc.Readers.Muse (readMuse) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except (throwError)
import Data.Bifunctor
import Data.Default
import Data.List (transpose, uncons)
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isNothing, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Builder (Blocks, Inlines, underline)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class.PandocMonad (PandocMonad (..))
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError (PandocParsecError))
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (F)
import Text.Pandoc.Shared (crFilter, trimr, tshow)

-- | Read Muse from an input string and return a Pandoc document.
readMuse :: PandocMonad m
         => ReaderOptions
         -> Text
         -> m Pandoc
readMuse opts s = do
  let input = crFilter s
  res <- flip runReaderT def $ runParserT parseMuse def{ museOptions = opts } "source" input
  case res of
       Left e  -> throwError $ PandocParsecError input e
       Right d -> return d

type F = Future MuseState

data MuseState = MuseState { museMeta :: F Meta -- ^ Document metadata
                           , museOptions :: ReaderOptions
                           , museIdentifierList :: Set.Set Text
                           , museLastSpacePos :: Maybe SourcePos -- ^ Position after last space or newline parsed
                           , museLastStrPos :: Maybe SourcePos -- ^ Position after last str parsed
                           , museLogMessages :: [LogMessage]
                           , museNotes :: M.Map Text (SourcePos, F Blocks)
                           }

instance Default MuseState where
  def = MuseState { museMeta = return nullMeta
                  , museOptions = def
                  , museIdentifierList = Set.empty
                  , museLastStrPos = Nothing
                  , museLastSpacePos = Nothing
                  , museLogMessages = []
                  , museNotes = M.empty
                  }

data MuseEnv =
  MuseEnv { museInLink :: Bool -- ^ True when parsing a link description to avoid nested links
          , museInPara :: Bool -- ^ True when parsing paragraph is not allowed
          }

instance Default MuseEnv where
  def = MuseEnv { museInLink = False
                , museInPara = False
                }

type MuseParser m = ParserT Text MuseState (ReaderT MuseEnv m)

instance HasReaderOptions MuseState where
  extractReaderOptions = museOptions

instance HasIdentifierList MuseState where
  extractIdentifierList     = museIdentifierList
  updateIdentifierList f st = st{ museIdentifierList = f $ museIdentifierList st }

instance HasLastStrPosition MuseState where
  setLastStrPos pos st = st{ museLastStrPos = pos }
  getLastStrPos st     = museLastStrPos st

instance HasLogMessages MuseState where
  addLogMessage m s = s{ museLogMessages = m : museLogMessages s }
  getLogMessages = reverse . museLogMessages

updateLastSpacePos :: Monad m => MuseParser m ()
updateLastSpacePos = getPosition >>= \pos ->
  updateState $ \s -> s { museLastSpacePos = Just pos }

-- | Parse Muse document
parseMuse :: PandocMonad m => MuseParser m Pandoc
parseMuse = do
  many directive
  blocks <- (:) <$> parseBlocks <*> many parseSection
  eof
  st <- getState
  runF (Pandoc <$> museMeta st <*> fmap B.toList (mconcat blocks)) st <$ reportLogMessages

-- * Utility functions

-- | Trim up to one newline from the beginning of the string.
lchop :: Text -> Text
lchop s = case T.uncons s of
  Just ('\n', xs) -> xs
  _               -> s

-- | Trim up to one newline from the end of the string.
rchop :: Text -> Text
rchop s = case T.unsnoc s of
  Just (xs, '\n') -> xs
  _               -> s

unindent :: Text -> Text
unindent = rchop . T.intercalate "\n" . dropSpacePrefix . T.splitOn "\n" . lchop

dropSpacePrefix :: [Text] -> [Text]
dropSpacePrefix lns = T.drop maxIndent <$> lns
  where isSpaceChar c = c == ' ' || c == '\t'
        maxIndent = length $ takeWhile (isSpaceChar . T.head) $ takeWhile same $ T.transpose lns
        same t = case T.uncons t of
          Just (c, cs) -> T.all (== c) cs
          Nothing      -> True

atStart :: PandocMonad m => MuseParser m ()
atStart = do
  pos <- getPosition
  st <- getState
  guard $ museLastStrPos st /= Just pos

noSpaceBefore :: PandocMonad m => MuseParser m ()
noSpaceBefore = do
  pos <- getPosition
  st <- getState
  guard $ museLastSpacePos st /= Just pos

firstColumn :: PandocMonad m => MuseParser m ()
firstColumn = getPosition >>= \pos -> guard (sourceColumn pos == 1)

-- * Parsers

-- | Parse end-of-line, which can be either a newline or end-of-file.
eol :: Stream s m Char => ParserT s st m ()
eol = void newline <|> eof

getIndent :: PandocMonad m
          => MuseParser m Int
getIndent = subtract 1 . sourceColumn <$ many spaceChar <*> getPosition

-- ** HTML parsers

openTag :: PandocMonad m => Text -> MuseParser m [(Text, Text)]
openTag tag = try $
  char '<' *> textStr tag *> manyTill attr (char '>')
  where
    attr = try $ (,)
      <$  many1 spaceChar
      <*> many1Char (noneOf "=\n")
      <*  string "=\""
      <*> manyTillChar (noneOf "\"") (char '"')

closeTag :: PandocMonad m => Text -> MuseParser m ()
closeTag tag = try $ string "</" *> textStr tag *> void (char '>')

-- | Convert HTML attributes to Pandoc 'Attr'
htmlAttrToPandoc :: [(Text, Text)] -> Attr
htmlAttrToPandoc attrs = (ident, classes, keyvals)
  where
    ident   = fromMaybe "" $ lookup "id" attrs
    classes = maybe [] T.words $ lookup "class" attrs
    keyvals = [(k,v) | (k,v) <- attrs, k /= "id", k /= "class"]

parseHtmlContent :: PandocMonad m
                 => Text -- ^ Tag name
                 -> MuseParser m (Attr, F Blocks)
parseHtmlContent tag = try $ getIndent >>= \indent -> (,)
  <$> fmap htmlAttrToPandoc (openTag tag)
  <*  manyTill spaceChar eol
  <*> allowPara (parseBlocksTill (try $ indentWith indent *> closeTag tag))
  <*  manyTill spaceChar eol -- closing tag must be followed by optional whitespace and newline

-- ** Directive parsers

-- While not documented, Emacs Muse allows "-" in directive name
parseDirectiveKey :: PandocMonad m => MuseParser m Text
parseDirectiveKey = char '#' *> manyChar (letter <|> char '-')

parseEmacsDirective :: PandocMonad m => MuseParser m (Text, F Inlines)
parseEmacsDirective = (,)
  <$> parseDirectiveKey
  <*  spaceChar
  <*> (trimInlinesF . mconcat <$> manyTill inline' eol)

parseAmuseDirective :: PandocMonad m => MuseParser m (Text, F Inlines)
parseAmuseDirective = (,)
  <$> parseDirectiveKey
  <*  many1 spaceChar
  <*> (trimInlinesF . mconcat <$> many1Till inline endOfDirective)
  <*  many blankline
  where
    endOfDirective = lookAhead $ eof <|> try (newline *> (void blankline <|> void parseDirectiveKey))

directive :: PandocMonad m => MuseParser m ()
directive = do
  ext <- getOption readerExtensions
  (key, value) <- if extensionEnabled Ext_amuse ext then parseAmuseDirective else parseEmacsDirective
  updateState $ \st -> st { museMeta = B.setMeta (translateKey key) <$> value <*> museMeta st }
  where translateKey "cover" = "cover-image"
        translateKey x = x

-- ** Block parsers

allowPara :: MonadReader MuseEnv m => m a -> m a
allowPara p = local (\s -> s { museInPara = False }) p

-- | Parse section contents until EOF or next header
parseBlocks :: PandocMonad m
            => MuseParser m (F Blocks)
parseBlocks =
  try (parseEnd <|>
       nextSection <|>
       listStart <|>
       blockStart <|>
       paraStart)
  where
    nextSection = mempty <$ lookAhead headingStart
    parseEnd = mempty <$ eof
    blockStart = (B.<>) <$> (blockElements <|> emacsNoteBlock)
                        <*> allowPara parseBlocks
    listStart =
      uncurry (B.<>) <$> allowPara (anyListUntil parseBlocks <|> amuseNoteBlockUntil parseBlocks)
    paraStart = do
      indent <- length <$> many spaceChar
      uncurry (B.<>) . first (p indent) <$> paraUntil parseBlocks
      where p indent = if indent >= 2 && indent < 6 then fmap B.blockQuote else id

-- | Parse section that starts with a header
parseSection :: PandocMonad m
             => MuseParser m (F Blocks)
parseSection =
  ((B.<>) <$> emacsHeading <*> parseBlocks) <|>
  (uncurry (B.<>) <$> amuseHeadingUntil parseBlocks)

parseBlocksTill :: PandocMonad m
                => MuseParser m a
                -> MuseParser m (F Blocks)
parseBlocksTill end = continuation
  where
    parseEnd = mempty <$ end
    blockStart = (B.<>) <$> blockElements <*> allowPara continuation
    listStart = uncurry (B.<>) <$> allowPara (anyListUntil (parseEnd <|> continuation))
    paraStart = uncurry (B.<>) <$> paraUntil (parseEnd <|> continuation)
    continuation = try $ parseEnd <|> listStart <|> blockStart <|> paraStart

listItemContentsUntil :: PandocMonad m
                      => Int
                      -> MuseParser m a
                      -> MuseParser m a
                      -> MuseParser m (F Blocks, a)
listItemContentsUntil col pre end = p
  where
    p = try listStart <|> try blockStart <|> try paraStart
    parsePre = (mempty,) <$> pre
    parseEnd = (mempty,) <$> end
    paraStart = do
      (f, (r, e)) <- paraUntil (parsePre <|> continuation <|> parseEnd)
      return (f B.<> r, e)
    blockStart = first <$> ((B.<>) <$> blockElements)
                       <*> allowPara (parsePre <|> continuation <|> parseEnd)
    listStart = do
      (f, (r, e)) <- allowPara $ anyListUntil (parsePre <|> continuation <|> parseEnd)
      return (f B.<> r, e)
    continuation = try $ do blank <- optionMaybe blankline
                            skipMany blankline
                            indentWith col
                            local (\s -> s { museInPara = museInPara s && isNothing blank }) p

parseBlock :: PandocMonad m => MuseParser m (F Blocks)
parseBlock = do
  res <- blockElements <|> para
  trace (T.take 60 $ tshow $ B.toList $ runF res def)
  return res
  where para = fst <$> paraUntil (try (eof <|> void (lookAhead blockElements)))

blockElements :: PandocMonad m => MuseParser m (F Blocks)
blockElements = (mempty <$ blankline)
            <|> comment
            <|> separator
            <|> pagebreak
            <|> example
            <|> exampleTag
            <|> literalTag
            <|> centerTag
            <|> rightTag
            <|> quoteTag
            <|> divTag
            <|> biblioTag
            <|> playTag
            <|> verseTag
            <|> lineBlock
            <|> museGridTable
            <|> table
            <|> commentTag

-- | Parse a line comment, starting with @;@ in the first column.
comment :: PandocMonad m => MuseParser m (F Blocks)
comment = try $ mempty
  <$ firstColumn
  <* char ';'
  <* optional (spaceChar *> many (noneOf "\n"))
  <* eol

-- | Parse a horizontal rule, consisting of 4 or more @\'-\'@ characters.
separator :: PandocMonad m => MuseParser m (F Blocks)
separator = try $ pure B.horizontalRule
  <$ string "----"
  <* many (char '-')
  <* many spaceChar
  <* eol

-- | Parse a page break
pagebreak :: PandocMonad m => MuseParser m (F Blocks)
pagebreak = try $ pure (B.divWith ("", [], [("style", "page-break-before: always;")]) mempty)
  <$ count 6 spaceChar
  <* many spaceChar
  <* string "* * * * *"
  <* manyTill spaceChar eol

headingStart :: PandocMonad m => MuseParser m (Text, Int)
headingStart = try $ (,)
  <$> option "" (try (parseAnchor <* manyTill spaceChar eol))
  <*  firstColumn
  <*> fmap length (many1 $ char '*')
  <*  spaceChar

-- | Parse a single-line heading.
emacsHeading :: PandocMonad m => MuseParser m (F Blocks)
emacsHeading = try $ do
  guardDisabled Ext_amuse
  (anchorId, level) <- headingStart
  content <- trimInlinesF . mconcat <$> manyTill inline eol
  attr <- registerHeader (anchorId, [], []) (runF content def)
  return $ B.headerWith attr level <$> content

-- | Parse a multi-line heading.
-- It is a Text::Amuse extension, Emacs Muse does not allow heading to span multiple lines.
amuseHeadingUntil :: PandocMonad m
                  => MuseParser m a -- ^ Terminator parser
                  -> MuseParser m (F Blocks, a)
amuseHeadingUntil end = try $ do
  guardEnabled Ext_amuse
  (anchorId, level) <- headingStart
  (content, e) <- paraContentsUntil end
  attr <- registerHeader (anchorId, [], []) (runF content def)
  return (B.headerWith attr level <$> content, e)

-- | Parse an example between @{{{@ and @}}}@.
-- It is an Amusewiki extension influenced by Creole wiki, as described in @Text::Amuse@ documentation.
example :: PandocMonad m => MuseParser m (F Blocks)
example = try $ pure . B.codeBlock
  <$  string "{{{"
  <*  many spaceChar
  <*> (unindent <$> manyTillChar anyChar (string "}}}"))

-- | Parse an @\<example>@ tag.
exampleTag :: PandocMonad m => MuseParser m (F Blocks)
exampleTag = try $ fmap pure $ B.codeBlockWith
  <$  many spaceChar
  <*> (htmlAttrToPandoc <$> openTag "example")
  <*> (unindent <$> manyTillChar anyChar (closeTag "example"))
  <*  manyTill spaceChar eol

-- | Parse a @\<literal>@ tag as a raw block.
-- For 'RawInline' @\<literal>@ parser, see 'inlineLiteralTag'.
literalTag :: PandocMonad m => MuseParser m (F Blocks)
literalTag = try $ fmap pure $ B.rawBlock
  <$  many spaceChar
  <*> (fromMaybe "html" . lookup "style" <$> openTag "literal") -- FIXME: Emacs Muse inserts <literal> without style into all output formats, but we assume HTML
  <*  manyTill spaceChar eol
  <*> (unindent <$> manyTillChar anyChar (closeTag "literal"))
  <*  manyTill spaceChar eol

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
biblioTag = fmap (B.divWith ("", ["biblio"], [])) . snd
  <$  guardEnabled Ext_amuse
  <*> parseHtmlContent "biblio"

-- | Parse @\<play>@ tag, the result is the same as @\<div class="play">@.
-- @\<play>@ tag is supported only in Text::Amuse mode.
playTag :: PandocMonad m => MuseParser m (F Blocks)
playTag = do
  guardEnabled Ext_amuse
  fmap (B.divWith ("", ["play"], [])) . snd <$> parseHtmlContent "play"

verseLine :: PandocMonad m => MuseParser m (F Inlines)
verseLine = (<>)
  <$> fmap pure (option mempty (B.str <$> many1Char ('\160' <$ char ' ')))
  <*> fmap (trimInlinesF . mconcat) (manyTill inline' eol)

-- | Parse @\<verse>@ tag.
verseTag :: PandocMonad m => MuseParser m (F Blocks)
verseTag = try $ getIndent >>= \indent -> fmap B.lineBlock . sequence
  <$  openTag "verse"
  <*  manyTill spaceChar eol
  <*> manyTill (indentWith indent *> verseLine) (try $ indentWith indent *> closeTag "verse")
  <*  manyTill spaceChar eol

-- | Parse @\<comment>@ tag.
commentTag :: PandocMonad m => MuseParser m (F Blocks)
commentTag = try $ mempty
  <$ many spaceChar
  <* openTag "comment"
  <* manyTill anyChar (closeTag "comment")
  <* manyTill spaceChar eol

-- | Parse paragraph contents.
paraContentsUntil :: PandocMonad m
                  => MuseParser m a -- ^ Terminator parser
                  -> MuseParser m (F Inlines, a)
paraContentsUntil end = first (trimInlinesF . mconcat)
  <$> manyUntil inline (try (manyTill spaceChar eol *> local (\s -> s { museInPara = True}) end))

-- | Parse a paragraph.
paraUntil :: PandocMonad m
          => MuseParser m a -- ^ Terminator parser
          -> MuseParser m (F Blocks, a)
paraUntil end = do
  inPara <- asks museInPara
  guard $ not inPara
  first (fmap B.para) <$> paraContentsUntil end

noteMarker' :: PandocMonad m
            => Char
            -> Char
            -> MuseParser m Text
noteMarker' l r = try $ (\x y -> T.pack $ l:x:y ++ [r])
  <$ char l
  <*> oneOf "123456789"
  <*> manyTill digit (char r)

noteMarker :: PandocMonad m => MuseParser m Text
noteMarker = noteMarker' '[' ']' <|> noteMarker' '{' '}'

addNote :: PandocMonad m
        => Text
        -> SourcePos
        -> F Blocks
        -> MuseParser m ()
addNote ref pos content = do
  oldnotes <- museNotes <$> getState
  when (M.member ref oldnotes)
    (logMessage $ DuplicateNoteReference ref pos)
  updateState $ \s -> s{ museNotes = M.insert ref (pos, content) oldnotes }

-- Amusewiki version of note
-- Parsing is similar to list item, except that note marker is used instead of list marker
amuseNoteBlockUntil :: PandocMonad m
                    => MuseParser m a
                    -> MuseParser m (F Blocks, a)
amuseNoteBlockUntil end = try $ do
  guardEnabled Ext_amuse
  ref <- noteMarker
  pos <- getPosition
  void spaceChar <|> lookAhead eol
  (content, e) <- allowPara $ listItemContentsUntil (sourceColumn pos) (Prelude.fail "x") end
  addNote ref pos content
  return (mempty, e)

-- Emacs version of note
-- Notes are allowed only at the end of text, no indentation is required.
emacsNoteBlock :: PandocMonad m => MuseParser m (F Blocks)
emacsNoteBlock = try $ do
  guardDisabled Ext_amuse
  ref <- noteMarker
  pos <- getPosition
  content <- fmap mconcat blocksTillNote
  addNote ref pos content
  return mempty
  where
    blocksTillNote =
      many1Till parseBlock (eof <|> () <$ lookAhead noteMarker)

--
-- Verse markup
--

-- | Parse a line block indicated by @\'>\'@ characters.
lineBlock :: PandocMonad m => MuseParser m (F Blocks)
lineBlock = try $ getIndent >>= \indent -> fmap B.lineBlock . sequence
  <$> (blankVerseLine <|> nonblankVerseLine) `sepBy1'` try (indentWith indent)
  where
    blankVerseLine = try $ mempty <$ char '>' <* blankline
    nonblankVerseLine = try (string "> ") *> verseLine

-- *** List parsers

bulletListItemsUntil :: PandocMonad m
                     => Int -- ^ Indentation
                     -> MuseParser m a -- ^ Terminator parser
                     -> MuseParser m ([F Blocks], a)
bulletListItemsUntil indent end = try $ do
  char '-'
  void spaceChar <|> lookAhead eol
  (x, (xs, e)) <- allowPara $ listItemContentsUntil (indent + 2) (try (optional blankline *> indentWith indent *> bulletListItemsUntil indent end)) (([],) <$> end)
  return (x:xs, e)

-- | Parse a bullet list.
bulletListUntil :: PandocMonad m
                => MuseParser m a
                -> MuseParser m (F Blocks, a)
bulletListUntil end = try $ do
  indent <- getIndent
  guard $ indent /= 0
  first (fmap B.bulletList . sequence) <$> bulletListItemsUntil indent end

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
              _          -> Prelude.fail "Unhandled case"

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
      (x, (xs, e)) <- allowPara $ listItemContentsUntil (sourceColumn pos) (try (optional blankline *> indentWith indent *> museOrderedListMarker style *> continuation)) (([],) <$> end)
      return (x:xs, e)

-- | Parse an ordered list.
orderedListUntil :: PandocMonad m
                 => MuseParser m a
                 -> MuseParser m (F Blocks, a)
orderedListUntil end = try $ do
  indent <- getIndent
  guard $ indent /= 0
  (style, start) <- decimal <|> lowerRoman <|> upperRoman <|> lowerAlpha <|> upperAlpha
  char '.'
  first (fmap (B.orderedListWith (start, style, Period)) . sequence)
    <$> orderedListItemsUntil indent style end

descriptionsUntil :: PandocMonad m
                  => Int
                  -> MuseParser m a
                  -> MuseParser m ([F Blocks], a)
descriptionsUntil indent end = do
  void spaceChar <|> lookAhead eol
  (x, (xs, e)) <- allowPara $ listItemContentsUntil indent (try (optional blankline *> indentWith indent *> manyTill spaceChar (string "::") *> descriptionsUntil indent end)) (([],) <$> end)
  return (x:xs, e)

definitionListItemsUntil :: PandocMonad m
                         => Int
                         -> MuseParser m a
                         -> MuseParser m ([F (Inlines, [Blocks])], a)
definitionListItemsUntil indent end =
  continuation
  where
    continuation = try $ do
      pos <- getPosition
      term <- trimInlinesF . mconcat <$> manyTill inline' (try $ string "::")
      (x, (xs, e)) <- descriptionsUntil (sourceColumn pos) (try (optional blankline *> indentWith indent *> continuation) <|> (([],) <$> end))
      let xx = (,) <$> term <*> sequence x
      return (xx:xs, e)

-- | Parse a definition list.
definitionListUntil :: PandocMonad m
                    => MuseParser m a -- ^ Terminator parser
                    -> MuseParser m (F Blocks, a)
definitionListUntil end = try $ do
  indent <- getIndent
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

data MuseTableElement = MuseHeaderRow [Blocks]
                      | MuseBodyRow [Blocks]
                      | MuseFooterRow [Blocks]
                      | MuseCaption Inlines

museToPandocTable :: MuseTable -> Blocks
museToPandocTable (MuseTable caption headers body footers) =
  B.table (B.simpleCaption $ B.plain caption)
          attrs
          (TableHead nullAttr $ toHeaderRow headRow)
          [TableBody nullAttr 0 [] $ map toRow $ rows ++ body ++ footers]
          (TableFoot nullAttr [])
  where attrs = (AlignDefault, ColWidthDefault) <$ transpose (headers ++ body ++ footers)
        (headRow, rows) = fromMaybe ([], []) $ uncons headers
        toRow = Row nullAttr . map B.simpleCell
        toHeaderRow l = if null l then [] else [toRow l]

museAppendElement :: MuseTableElement
                  -> MuseTable
                  -> MuseTable
museAppendElement element tbl =
  case element of
    MuseHeaderRow row -> tbl{ museTableHeaders = row : museTableHeaders tbl }
    MuseBodyRow row -> tbl{ museTableRows = row : museTableRows tbl }
    MuseFooterRow row -> tbl{ museTableFooters = row : museTableFooters tbl }
    MuseCaption inlines -> tbl{ museTableCaption = inlines }

tableElements :: PandocMonad m => MuseParser m (F [MuseTableElement])
tableElements = sequence <$> many1 tableParseElement

elementsToTable :: [MuseTableElement] -> MuseTable
elementsToTable = foldr museAppendElement emptyTable
  where emptyTable = MuseTable mempty mempty mempty mempty

museGridPart :: PandocMonad m => MuseParser m Int
museGridPart = try $ length <$> many1 (char '-') <* char '+'

museGridTableHeader :: PandocMonad m => MuseParser m [Int]
museGridTableHeader = try $ char '+' *> many1 museGridPart <* manyTill spaceChar eol

museGridTableRow :: PandocMonad m
                 => Int
                 -> [Int]
                 -> MuseParser m (F [Blocks])
museGridTableRow indent indices = try $ do
  lns <- many1 $ try (indentWith indent *> museGridTableRawLine indices)
  let cols = map (T.unlines . map trimr) $ transpose lns
  indentWith indent *> museGridTableHeader
  sequence <$> mapM (parseFromString' parseBlocks) cols

museGridTableRawLine :: PandocMonad m
                     => [Int]
                     -> MuseParser m [Text]
museGridTableRawLine indices =
  char '|' *> forM indices (\n -> countChar n anyChar <* char '|') <* manyTill spaceChar eol

museGridTable :: PandocMonad m => MuseParser m (F Blocks)
museGridTable = try $ do
  indent <- getIndent
  indices <- museGridTableHeader
  fmap rowsToTable . sequence <$> many1 (museGridTableRow indent indices)
  where rowsToTable rows = B.table B.emptyCaption
                                   attrs
                                   (TableHead nullAttr [])
                                   [TableBody nullAttr 0 [] $ map toRow rows]
                                   (TableFoot nullAttr [])
                           where attrs = (AlignDefault, ColWidthDefault) <$ transpose rows
                                 toRow = Row nullAttr . map B.simpleCell

-- | Parse a table.
table :: PandocMonad m => MuseParser m (F Blocks)
table = try $ fmap (museToPandocTable . elementsToTable) <$> tableElements

tableParseElement :: PandocMonad m => MuseParser m (F MuseTableElement)
tableParseElement = tableParseHeader
                <|> tableParseBody
                <|> tableParseFooter
                <|> tableParseCaption

tableParseRow :: PandocMonad m
              => Int -- ^ Number of separator characters
              -> MuseParser m (F [Blocks])
tableParseRow n = try $ sequence <$> tableCells
  where tableCells = (:) <$> tableCell sep <*> (tableCells <|> fmap pure (tableCell eol))
        tableCell p = try $ fmap B.plain . trimInlinesF . mconcat <$> manyTill inline' p
        sep = try $ many1 spaceChar *> count n (char '|') *> lookAhead (void (many1 spaceChar) <|> void eol)

-- | Parse a table header row.
tableParseHeader :: PandocMonad m => MuseParser m (F MuseTableElement)
tableParseHeader = fmap MuseHeaderRow <$> tableParseRow 2

-- | Parse a table body row.
tableParseBody :: PandocMonad m => MuseParser m (F MuseTableElement)
tableParseBody = fmap MuseBodyRow <$> tableParseRow 1

-- | Parse a table footer row.
tableParseFooter :: PandocMonad m => MuseParser m (F MuseTableElement)
tableParseFooter = fmap MuseFooterRow <$> tableParseRow 3

-- | Parse table caption.
tableParseCaption :: PandocMonad m => MuseParser m (F MuseTableElement)
tableParseCaption = try $ fmap MuseCaption . trimInlinesF . mconcat
  <$  many spaceChar
  <*  string "|+"
  <*> many1Till inline (try $ string "+|" *> eol)

-- ** Inline parsers

inline' :: PandocMonad m => MuseParser m (F Inlines)
inline' = whitespace
      <|> br
      <|> anchor
      <|> footnote
      <|> strongEmph
      <|> strong
      <|> strongTag
      <|> emph
      <|> emphTag
      <|> underlined
      <|> superscriptTag
      <|> subscriptTag
      <|> strikeoutTag
      <|> verbatimTag
      <|> classTag
      <|> inlineRtl
      <|> inlineLtr
      <|> nbsp
      <|> linkOrImage
      <|> code
      <|> codeTag
      <|> mathTag
      <|> inlineLiteralTag
      <|> str
      <|> asterisks
      <|> symbol
      <?> "inline"

inline :: PandocMonad m => MuseParser m (F Inlines)
inline = endline <|> inline'

-- | Parse a soft break.
endline :: PandocMonad m => MuseParser m (F Inlines)
endline = try $ pure B.softbreak <$ newline <* notFollowedBy blankline <* updateLastSpacePos

parseAnchor :: PandocMonad m => MuseParser m Text
parseAnchor = try $ T.cons
  <$  firstColumn
  <*  char '#'
  <*> letter
  <*> manyChar (letter <|> digit <|> char '-')

anchor :: PandocMonad m => MuseParser m (F Inlines)
anchor = try $ do
  anchorId <- parseAnchor
  skipMany spaceChar <|> void newline
  return $ return $ B.spanWith (anchorId, [], []) mempty

-- | Parse a footnote reference.
footnote :: PandocMonad m => MuseParser m (F Inlines)
footnote = try $ do
  inLink <- asks museInLink
  guard $ not inLink
  ref <- noteMarker
  return $ do
    notes <- asksF museNotes
    case M.lookup ref notes of
      Nothing -> return $ B.str ref
      Just (_pos, contents) -> do
        st <- askF
        let contents' = runF contents st { museNotes = M.delete ref (museNotes st) }
        return $ B.note contents'

whitespace :: PandocMonad m => MuseParser m (F Inlines)
whitespace = try $ pure B.space <$ skipMany1 spaceChar <* updateLastSpacePos

-- | Parse @\<br>@ tag.
br :: PandocMonad m => MuseParser m (F Inlines)
br = try $ pure B.linebreak <$ string "<br>"

emphasisBetween :: (PandocMonad m, Show a)
                => MuseParser m a
                -> MuseParser m (F Inlines)
emphasisBetween p = try $ trimInlinesF . mconcat
  <$  atStart
  <*  p
  <*  notFollowedBy space
  <*> many1Till inline (try $ noSpaceBefore *> p <* notFollowedBy alphaNum)

-- | Parse an inline tag, such as @\<em>@ and @\<strong>@.
inlineTag :: PandocMonad m
          => Text -- ^ Tag name
          -> MuseParser m (F Inlines)
inlineTag tag = try $ mconcat
  <$  openTag tag
  <*> manyTill inline (closeTag tag)

-- | Parse strong emphasis inline markup, indicated by @***@.
strongEmph :: PandocMonad m => MuseParser m (F Inlines)
strongEmph = fmap (B.strong . B.emph) <$> emphasisBetween (string "***" <* notFollowedBy (char '*'))

-- | Parse strong inline markup, indicated by @**@.
strong :: PandocMonad m => MuseParser m (F Inlines)
strong = fmap B.strong <$> emphasisBetween (string "**" <* notFollowedBy (char '*'))

-- | Parse emphasis inline markup, indicated by @*@.
emph :: PandocMonad m => MuseParser m (F Inlines)
emph = fmap B.emph <$> emphasisBetween (char '*' <* notFollowedBy (char '*'))

-- | Parse underline inline markup, indicated by @_@.
-- Supported only in Emacs Muse mode, not Text::Amuse.
underlined :: PandocMonad m => MuseParser m (F Inlines)
underlined = fmap underline
  <$  guardDisabled Ext_amuse -- Supported only by Emacs Muse
  <*> emphasisBetween (char '_')

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
verbatimTag = return . B.text
  <$  openTag "verbatim"
  <*> manyTillChar anyChar (closeTag "verbatim")

-- | Parse @\<class>@ tag.
classTag :: PandocMonad m => MuseParser m (F Inlines)
classTag = do
  classes <- maybe [] T.words . lookup "name" <$> openTag "class"
  fmap (B.spanWith ("", classes, [])) . mconcat <$> manyTill inline (closeTag "class")

-- | Parse @\<\<\<RTL>>>@ text.
inlineRtl :: PandocMonad m => MuseParser m (F Inlines)
inlineRtl = try $
  fmap (B.spanWith ("", [], [("dir", "rtl")])) . mconcat <$ string "<<<" <*> manyTill inline (string ">>>")

-- | Parse @\<\<\<LTR>>>@ text.
inlineLtr :: PandocMonad m => MuseParser m (F Inlines)
inlineLtr = try $
  fmap (B.spanWith ("", [], [("dir", "ltr")])) . mconcat <$ string ">>>" <*> manyTill inline (string "<<<")

-- | Parse "~~" as nonbreaking space.
nbsp :: PandocMonad m => MuseParser m (F Inlines)
nbsp = try $ pure (B.str "\160") <$ string "~~"

-- | Parse code markup, indicated by @\'=\'@ characters.
code :: PandocMonad m => MuseParser m (F Inlines)
code = try $ fmap pure $ B.code . uncurry (<>)
  <$  atStart
  <*  char '='
  <*  notFollowedBy (spaceChar <|> newline)
  <*> manyUntilChar (noneOf "\n\r" <|> (newline <* notFollowedBy newline)) (try $ fmap T.singleton $ noneOf " \t\n\r=" <* char '=')
  <*  notFollowedBy alphaNum

-- | Parse @\<code>@ tag.
codeTag :: PandocMonad m => MuseParser m (F Inlines)
codeTag = fmap pure $ B.codeWith
  <$> (htmlAttrToPandoc <$> openTag "code")
  <*> manyTillChar anyChar (closeTag "code")

-- | Parse @\<math>@ tag.
-- @\<math>@ tag is an Emacs Muse extension enabled by @(require 'muse-latex2png)@
mathTag :: PandocMonad m => MuseParser m (F Inlines)
mathTag = return . B.math
  <$  openTag "math"
  <*> manyTillChar anyChar (closeTag "math")

-- | Parse inline @\<literal>@ tag as a raw inline.
inlineLiteralTag :: PandocMonad m => MuseParser m (F Inlines)
inlineLiteralTag = try $ fmap pure $ B.rawInline
  <$> (fromMaybe "html" . lookup "style" <$> openTag "literal") -- FIXME: Emacs Muse inserts <literal> without style into all output formats, but we assume HTML
  <*> manyTillChar anyChar (closeTag "literal")

str :: PandocMonad m => MuseParser m (F Inlines)
str = return . B.str <$> many1Char alphaNum <* updateLastStrPos

-- | Consume asterisks that were not used as emphasis opening.
-- This prevents series of asterisks from being split into
-- literal asterisk and emphasis opening.
asterisks :: PandocMonad m => MuseParser m (F Inlines)
asterisks = pure . B.str <$> many1Char (char '*')

symbol :: PandocMonad m => MuseParser m (F Inlines)
symbol = pure . B.str . T.singleton <$> nonspaceChar

-- | Parse a link or image.
linkOrImage :: PandocMonad m => MuseParser m (F Inlines)
linkOrImage = try $ link "URL:" <|> image <|> link ""

linkContent :: PandocMonad m => MuseParser m (F Inlines)
linkContent = trimInlinesF . mconcat
  <$  char '['
  <*> manyTill inline (char ']')

-- | Parse a link starting with (possibly null) prefix
link :: PandocMonad m => Text -> MuseParser m (F Inlines)
link prefix = try $ do
  inLink <- asks museInLink
  guard $ not inLink
  textStr $ "[[" <> prefix
  url <- manyTillChar anyChar $ char ']'
  content <- option (pure $ B.str url) (local (\s -> s { museInLink = True }) linkContent)
  char ']'
  return $ B.link url "" <$> content

image :: PandocMonad m => MuseParser m (F Inlines)
image = try $ do
  string "[["
  (url, (ext, width, align)) <- manyUntilChar (noneOf "]") (imageExtensionAndOptions <* char ']')
  content <- option mempty linkContent
  char ']'
  let widthAttr = case align of
                    Just 'f' -> [("width", fromMaybe "100" width <> "%"), ("height", "75%")]
                    _ -> maybeToList (("width",) . (<> "%") <$> width)
  let alignClass = case align of
                     Just 'r' -> ["align-right"]
                     Just 'l' -> ["align-left"]
                     Just 'f' -> []
                     _        -> []
  return $ B.imageWith ("", alignClass, widthAttr) (url <> ext) mempty <$> content
  where -- Taken from muse-image-regexp defined in Emacs Muse file lisp/muse-regexps.el
        imageExtensions = [".eps", ".gif", ".jpg", ".jpeg", ".pbm", ".png", ".tiff", ".xbm", ".xpm"]
        imageExtension = choice (try . textStr <$> imageExtensions)
        imageExtensionAndOptions = do
          ext <- imageExtension
          (width, align) <- option (Nothing, Nothing) imageAttrs
          return (ext, width, align)
        imageAttrs = (,)
          <$  many1 spaceChar
          <*> optionMaybe (many1Char digit)
          <*  many spaceChar
          <*> optionMaybe (oneOf "rlf")
