{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{- |
   Module      : Text.Pandoc.Readers.RST
   Copyright   : Copyright (C) 2006-2024 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion from reStructuredText to 'Pandoc' document.
-}
module Text.Pandoc.Readers.RST ( readRST ) where
import Control.Arrow (second)
import Control.Monad (forM_, guard, liftM, mplus, mzero, when, unless)
import Control.Monad.Except (throwError)
import Control.Monad.Identity (Identity (..))
import Data.Char (isHexDigit, isSpace, toUpper, isAlphaNum, generalCategory,
                  GeneralCategory(OpenPunctuation, InitialQuote, FinalQuote,
                                  DashPunctuation, OtherSymbol))
import Data.List (deleteFirstsBy, elemIndex, nub, partition, sort, transpose)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, maybeToList, isJust)
import Data.Sequence (ViewR (..), viewr)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)
import Text.Pandoc.Builder (Blocks, Inlines, fromList, setMeta, trimInlines)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class (PandocMonad, readFileFromDirs, fetchItem, getTimestamp)
import Text.Pandoc.CSV (CSVOptions (..), defaultCSVOptions, parseCSV)
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.ImageSize (lengthToDim, scaleDimension)
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import Text.Pandoc.Shared
import Text.Pandoc.URI
import Text.Pandoc.Walk (walkM)
import qualified Text.Pandoc.UTF8 as UTF8
import Data.Time.Format
import System.FilePath (takeDirectory)

-- TODO:
-- [ ] .. parsed-literal

-- | Parse reStructuredText string and return Pandoc document.
readRST :: (PandocMonad m, ToSources a)
        => ReaderOptions -- ^ Reader options
        -> a
        -> m Pandoc
readRST opts s = do
  parsed <- readWithM parseRST def{ stateOptions = opts }
               (ensureFinalNewlines 2 (toSources s))
  case parsed of
    Right result -> return result
    Left e       -> throwError e

type RSTParser m = ParsecT Sources ParserState m

--
-- Constants and data structure definitions
---

bulletListMarkers :: [Char]
bulletListMarkers = "*+-•‣⁃"

underlineChars :: [Char]
underlineChars = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

-- treat these as potentially non-text when parsing inline:
specialChars :: [Char]
specialChars = "\\`|*_<>$:/[]{}()-.\"'\8216\8217\8220\8221"

--
-- parsing documents
--

isHeader :: Int -> Block -> Bool
isHeader n (Header x _ _) = x == n
isHeader _ _              = False

-- | Promote all headers in a list of blocks.  (Part of
-- title transformation for RST.)
promoteHeaders :: Int -> [Block] -> [Block]
promoteHeaders num (Header level attr text:rest) =
    Header (level - num) attr text:promoteHeaders num rest
promoteHeaders num (other:rest) = other:promoteHeaders num rest
promoteHeaders _   [] = []

-- | If list of blocks starts with a header (or a header and subheader)
-- of level that are not found elsewhere, return it as a title and
-- promote all the other headers.  Also process a definition list right
-- after the title block as metadata.
titleTransform :: ([Block], Meta)  -- ^ list of blocks, metadata
               -> ([Block], Meta)  -- ^ modified list of blocks, metadata
titleTransform (bs, meta) =
  let (bs', meta') =
       case bs of
          (Header 1 _ head1:Header 2 _ head2:rest)
           | not (any (isHeader 1) rest || any (isHeader 2) rest) -> -- tit/sub
            (promoteHeaders 2 rest, setMeta "title" (fromList head1) $
              setMeta "subtitle" (fromList head2) meta)
          (Header 1 _ head1:rest)
           | not (any (isHeader 1) rest) -> -- title only
            (promoteHeaders 1 rest,
                setMeta "title" (fromList head1) meta)
          _ -> (bs, meta)
  in   case bs' of
          (DefinitionList ds : rest) ->
            (rest, metaFromDefList ds meta')
          _ -> (bs', meta')

metaFromDefList :: [([Inline], [[Block]])] -> Meta -> Meta
metaFromDefList ds meta = adjustAuthors $ foldr f meta ds
 where f (k,v) =
         case v of
           [[Plain ils]] ->  setMeta (T.toLower (stringify k)) $ MetaInlines ils
           _ -> setMeta (T.toLower (stringify k)) $ mconcat $ map fromList v
       adjustAuthors (Meta metamap) = Meta $ M.adjust splitAuthors "author"
                                           $ M.mapKeys (\k ->
                                                 if k == "authors"
                                                    then "author"
                                                    else k) metamap
       splitAuthors (MetaInlines xs)  = MetaList $ map MetaInlines
                                                 $ splitAuthors' xs
       splitAuthors x                 = x
       splitAuthors'                  = map normalizeSpaces .
                                         splitOnSemi . concatMap factorSemi
       normalizeSpaces                = reverse . dropWhile isSp . reverse .
                                         dropWhile isSp
       isSp Space     = True
       isSp SoftBreak = True
       isSp LineBreak = True
       isSp _         = False
       splitOnSemi                    = splitBy (==Str ";")
       factorSemi (Str "")            = []
       factorSemi (Str s)             = case T.break (==';') s of
                                          (xs,"") -> [Str xs]
                                          (xs,T.uncons -> Just (';',ys)) -> Str xs : Str ";" :
                                            factorSemi (Str ys)
                                          (xs,ys) -> Str xs :
                                            factorSemi (Str ys)
       factorSemi x                   = [x]

parseRST :: PandocMonad m => RSTParser m Pandoc
parseRST = do
  standalone <- getOption readerStandalone
  optional blanklines -- skip blank lines at beginning of file
  blocks <- B.toList <$> parseBlocks
  citations <- sort . M.toList . stateCitations <$> getState
  citationItems <- mapM parseCitation citations
  let refBlock = [Div ("citations",[],[]) $
                 B.toList $ B.definitionList citationItems | not (null citationItems)]
  state <- getState
  let meta = stateMeta state
  let (blocks', meta') = if standalone
                            then titleTransform (blocks, meta)
                            else (blocks, meta)
  let reversedNotes = stateNotes state
  updateState $ \s -> s { stateNotes = reverse reversedNotes }
  doc <- walkM resolveReferences =<<
         walkM resolveBlockSubstitutions
         (Pandoc meta' (blocks' ++ refBlock))
  reportLogMessages
  return doc

resolveBlockSubstitutions :: PandocMonad m => Block -> RSTParser m Block
resolveBlockSubstitutions x@(Para [Link _attr _ (s,_)])
  | Just ref <- T.stripPrefix "##SUBST##" s = do
          substTable <- stateSubstitutions <$> getState
          let key@(Key key') = toKey $ stripFirstAndLast ref
          case M.lookup key substTable of
               Nothing     -> do
                 pos <- getPosition
                 logMessage $ ReferenceNotFound (tshow key') pos
                 return x
               Just target -> case
                 B.toList target of
                   [bl] -> return bl
                   bls -> return $ Div nullAttr bls
resolveBlockSubstitutions x = return x

resolveReferences :: PandocMonad m => Inline -> RSTParser m Inline
resolveReferences x@(Link _ ils (s,_))
  | Just ref <- T.stripPrefix "##REF##" s = do
      let isAnonKey (Key (T.uncons -> Just ('_',_))) = True
          isAnonKey _                                = False
      state <- getState
      let keyTable = stateKeys state
      let anonKeys = sort $ filter isAnonKey $ M.keys keyTable
      key <-  if ref == "_" -- anonymous key
                then
                  case anonKeys of
                    []    -> mzero -- TODO log?
                    (k:_) -> return k
                else return $ toKey ref
      ((src,tit), attr) <- lookupKey [] key
      -- if anonymous link, remove key so it won't be used again
      when (isAnonKey key) $ updateState $ \st ->
                              st{ stateKeys = M.delete key keyTable }
      return $ Link attr ils (src, tit)
  | Just ref <- T.stripPrefix "##NOTE##" s = do
      state <- getState
      let notes = stateNotes state
      case lookup ref notes of
        Nothing   -> do
          pos <- getPosition
          logMessage $ ReferenceNotFound ref pos
          return x
        Just raw  -> do
          -- We temporarily empty the note list while parsing the note,
          -- so that we don't get infinite loops with notes inside notes...
          -- Note references inside other notes are allowed in reST, but
          -- not yet in this implementation.
          updateState $ \st -> st{ stateNotes = [] }
          contents <- parseFromString' parseBlocks raw
          let newnotes = if ref == "*" || ref == "#" -- auto-numbered
                            -- delete the note so the next auto-numbered note
                            -- doesn't get the same contents:
                            then deleteFirstsBy (==) notes [(ref,raw)]
                            else notes
          updateState $ \st -> st{ stateNotes = newnotes }
          return $ Note (B.toList contents)
  | Just ref <- T.stripPrefix "##SUBST##" s = do
          substTable <- stateSubstitutions <$> getState
          let key@(Key key') = toKey $ stripFirstAndLast ref
          case M.lookup key substTable of
               Nothing     -> do
                 pos <- getPosition
                 logMessage $ ReferenceNotFound (tshow key') pos
                 return x
               Just target -> case
                 B.toList target of
                   [Para [t]] -> return t
                   [Para xs] -> return $ Span nullAttr xs
                   bls -> return $ Span nullAttr $ blocksToInlines bls
  | otherwise = return x
resolveReferences x = return x

parseCitation :: PandocMonad m
              => (Text, Text) -> RSTParser m (Inlines, [Blocks])
parseCitation (ref, raw) = do
  contents <- parseFromString' parseBlocks raw
  return (B.spanWith (ref, ["citation-label"], []) (B.str ref),
           [contents])


--
-- parsing blocks
--

parseBlocks :: PandocMonad m => RSTParser m Blocks
parseBlocks = mconcat <$> manyTill block eof

block :: PandocMonad m => RSTParser m Blocks
block = choice [ codeBlock
               , blockQuote
               , fieldList
               , optionList
               , referenceKey
               , noteBlock
               , citationBlock
               , directive
               , anchor
               , comment
               , header
               , hrule
               , lineBlock     -- must go before definitionList
               , table
               , list
               , lhsCodeBlock
               , para
               , mempty <$ blanklines
               ] <?> "block"

--
-- field list
--

rawFieldListItem :: Monad m => Int -> RSTParser m (Text, Text)
rawFieldListItem minIndent = try $ do
  indent <- length <$> many (char ' ')
  guard $ indent >= minIndent
  char ':'
  name <- many1TillChar (noneOf "\n") (char ':')
  (() <$ lookAhead newline) <|> skipMany1 spaceChar
  first <- anyLine
  rest <- option "" $ try $ do lookAhead (count indent (char ' ') >> spaceChar)
                               indentedBlock
  let raw = (if T.null first then "" else first <> "\n") <> rest <>
            (if T.null first && T.null rest then "" else "\n")
  return (name, raw)

fieldListItem :: PandocMonad m => Int -> RSTParser m (Inlines, [Blocks])
fieldListItem minIndent = try $ do
  (name, raw) <- rawFieldListItem minIndent
  term <- parseInlineFromText name
  contents <- parseFromString' parseBlocks raw
  optional blanklines
  let defn = case B.toList contents of
                [Para ils] -> [B.plain $ B.fromList ils] -- see #7766
                _ -> [contents]
  return (term, defn)

fieldList :: PandocMonad m => RSTParser m Blocks
fieldList = try $ do
  indent <- length <$> lookAhead (many spaceChar)
  items <- many1 $ fieldListItem indent
  case items of
     []     -> return mempty
     items' -> return $ B.definitionList items'

optionList :: PandocMonad m => RSTParser m Blocks
optionList = B.definitionList <$> many1 optionListItem

optionListItem :: PandocMonad m => RSTParser m (Inlines, [Blocks])
optionListItem = try $ do
  opts <- snd <$> withRaw (do
     let anyOpt = shortOpt <|> longOpt <|> dosOpt
     anyOpt
     many $ try (char ',' <* many spaceChar *> anyOpt))
  -- at least two spaces
  rawfirst <- try (char ' ' *> many1 (char ' ') *> anyLineNewline)
                    <|> try (mempty <$ skipMany spaceChar <* newline)
  bodyElements <- do
    raw <- option "" indentedBlock
    parseFromString' parseBlocks $ (rawfirst <> raw) <> "\n\n"
  optional blanklines
  pure (B.code opts, [bodyElements])

shortOpt :: PandocMonad m => RSTParser m ()
shortOpt = try $ do
  char '-'
  alphaNum
  optional $ try (optional (char ' ') *> optArg)

optArg :: PandocMonad m => RSTParser m ()
optArg = do
  c <- letter <|> char '<'
  if c == '<'
     then () <$ manyTill (noneOf "<>") (char '>')
     else skipMany (alphaNum <|> char '_' <|> char '-')

longOpt :: PandocMonad m => RSTParser m ()
longOpt = try $ do
  char '-'
  char '-'
  alphaNum
  skipMany1 (alphaNum <|> char '-' <|> char '_')
  optional $ try (oneOf " =" *> optArg)

dosOpt :: PandocMonad m => RSTParser m ()
dosOpt = try $ do
  char '/'
  alphaNum <|> char '?'
  optional $ try (char ' ' *> optArg)

--
-- line block
--

lineBlock :: PandocMonad m => RSTParser m Blocks
lineBlock = try $ do
  lines' <- lineBlockLines
  lines'' <- mapM parseInlineFromText lines'
  return $ B.lineBlock lines''

lineBlockDirective :: PandocMonad m => Text -> RSTParser m Blocks
lineBlockDirective body = do
  lines' <- mapM parseInlineFromText $ T.lines $ stripTrailingNewlines body
  return $ B.lineBlock lines'

--
-- paragraph block
--

-- note: paragraph can end in a :: starting a code block
para :: PandocMonad m => RSTParser m Blocks
para = try $ do
  result <- trimInlines . mconcat <$> many1 inline
  option (B.plain result) $ try $ do
    newline
    blanklines
    case viewr (B.unMany result) of
         ys :> Str xs | "::" `T.isSuffixOf` xs -> do
              raw <- option mempty codeBlockBody
              return $ B.para (B.Many ys <> B.str (T.take (T.length xs - 1) xs))
                         <> raw
         _ -> return (B.para result)

plain :: PandocMonad m => RSTParser m Blocks
plain = B.plain . trimInlines . mconcat <$> many1 inline

--
-- header blocks
--

header :: PandocMonad m => RSTParser m Blocks
header = doubleHeader <|> singleHeader <?> "header"

-- a header with lines on top and bottom
doubleHeader :: PandocMonad m => RSTParser m Blocks
doubleHeader = do
  (txt, c) <- doubleHeader'
  -- check to see if we've had this kind of header before.
  -- if so, get appropriate level.  if not, add to list.
  state <- getState
  let headerTable = stateHeaderTable state
  let (headerTable',level) = case elemIndex (DoubleHeader c) headerTable of
        Just ind -> (headerTable, ind + 1)
        Nothing  -> (headerTable ++ [DoubleHeader c], length headerTable + 1)
  setState (state { stateHeaderTable = headerTable' })
  attr@(ident,_,_) <- registerHeader nullAttr txt
  let key = toKey (stringify txt)
  updateState $ \s ->
    s { stateKeys = M.insert key (("#" <> ident,""), nullAttr) $ stateKeys s }
  return $ B.headerWith attr level txt

doubleHeader' :: PandocMonad m => RSTParser m (Inlines, Char)
doubleHeader' = try $ do
  c <- oneOf underlineChars
  rest <- many (char c)  -- the top line
  let lenTop = length (c:rest)
  skipSpaces
  newline
  txt <- trimInlines . mconcat <$> many1 (notFollowedBy blankline >> inline)
  pos <- getPosition
  let len = sourceColumn pos - 1
  when (len > lenTop) $ Prelude.fail "title longer than border"
  blankline              -- spaces and newline
  count lenTop (char c)  -- the bottom line
  blanklines
  return (txt, c)

-- a header with line on the bottom only
singleHeader :: PandocMonad m => RSTParser m Blocks
singleHeader = do
  (txt, c) <- singleHeader'
  state <- getState
  let headerTable = stateHeaderTable state
  let (headerTable',level) = case elemIndex (SingleHeader c) headerTable of
        Just ind -> (headerTable, ind + 1)
        Nothing  -> (headerTable ++ [SingleHeader c], length headerTable + 1)
  setState (state { stateHeaderTable = headerTable' })
  attr@(ident,_,_) <- registerHeader nullAttr txt
  let key = toKey (stringify txt)
  updateState $ \s ->
    s { stateKeys = M.insert key (("#" <> ident,""), nullAttr) $ stateKeys s }
  return $ B.headerWith attr level txt

singleHeader' :: PandocMonad m => RSTParser m (Inlines, Char)
singleHeader' = try $ do
  notFollowedBy' whitespace
  lookAhead $ anyLine >> oneOf underlineChars
  txt <- trimInlines . mconcat <$> many1 (notFollowedBy blankline >> inline)
  pos <- getPosition
  let len = sourceColumn pos - 1
  blankline
  c <- oneOf underlineChars
  count (len - 1) (char c)
  many (char c)
  blanklines
  return (txt, c)

--
-- hrule block
--

hrule :: Monad m => ParsecT Sources st m Blocks
hrule = try $ do
  chr <- oneOf underlineChars
  count 3 (char chr)
  skipMany (char chr)
  blankline
  blanklines
  return B.horizontalRule

--
-- code blocks
--

-- read a line indented by a given string
indentedLine :: (HasReaderOptions st, Monad m)
             => Int -> ParsecT Sources st m Text
indentedLine indents = try $ do
  lookAhead spaceChar
  gobbleAtMostSpaces indents
  anyLine

-- one or more indented lines, possibly separated by blank lines.
-- any amount of indentation will work.
indentedBlock :: (HasReaderOptions st, Monad m)
              => ParsecT Sources st m Text
indentedBlock = try $ do
  indents <- length <$> lookAhead (many1 spaceChar)
  lns <- many1 $ try $ do b <- option "" blanklines
                          l <- indentedLine indents
                          return (b <> l)
  optional blanklines
  return $ T.unlines lns

quotedBlock :: Monad m => ParsecT Sources st m Text
quotedBlock = try $ do
    quote <- lookAhead $ oneOf "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
    lns <- many1 $ lookAhead (char quote) >> anyLine
    optional blanklines
    return $ T.unlines lns

codeBlockStart :: Monad m => ParsecT Sources st m Char
codeBlockStart = string "::" >> blankline >> blankline

codeBlock :: Monad m => ParsecT Sources ParserState m Blocks
codeBlock = try $ codeBlockStart >> codeBlockBody

codeBlockBody :: Monad m => ParsecT Sources ParserState m Blocks
codeBlockBody = do
  lang <- stateRstHighlight <$> getState
  try $ B.codeBlockWith ("", maybeToList lang, []) . stripTrailingNewlines <$>
                (indentedBlock <|> quotedBlock)

lhsCodeBlock :: Monad m => RSTParser m Blocks
lhsCodeBlock = try $ do
  getPosition >>= guard . (==1) . sourceColumn
  guardEnabled Ext_literate_haskell
  optional codeBlockStart
  lns <- latexCodeBlock <|> birdCodeBlock
  blanklines
  return $ B.codeBlockWith ("", ["haskell","literate"], [])
         $ T.intercalate "\n" lns

latexCodeBlock :: Monad m => ParsecT Sources st m [Text]
latexCodeBlock = try $ do
  try (latexBlockLine "\\begin{code}")
  many1Till anyLine (try $ latexBlockLine "\\end{code}")
 where
  latexBlockLine s = skipMany spaceChar >> string s >> blankline

birdCodeBlock :: Monad m => ParsecT Sources st m [Text]
birdCodeBlock = filterSpace <$> many1 birdTrackLine
  where filterSpace lns =
            -- if (as is normal) there is always a space after >, drop it
            if all (\ln -> T.null ln || T.take 1 ln == " ") lns
               then map (T.drop 1) lns
               else lns

birdTrackLine :: Monad m => ParsecT Sources st m Text
birdTrackLine = char '>' >> anyLine

--
-- block quotes
--

blockQuote :: PandocMonad m => RSTParser m Blocks
blockQuote = do
  raw <- indentedBlock
  -- parse the extracted block, which may contain various block elements:
  contents <- parseFromString' parseBlocks $ raw <> "\n\n"
  return $ B.blockQuote contents

{-
Unsupported options for include:
tab-width
encoding
-}

includeDirective :: PandocMonad m
                 => Text
                 -> [(Text, Text)]
                 -> Text
                 -> RSTParser m Blocks
includeDirective top fields body = do
  let f = T.unpack $ trim top
  guard $ not $ null f
  guard $ T.null (trim body)
  let startLine = lookup "start-line" fields >>= safeRead
  let endLine = lookup "end-line" fields >>= safeRead
  let classes =  maybe [] T.words (lookup "class" fields)
  let ident = maybe "" trimr $ lookup "name" fields
  let parser =
       case lookup "code" fields `mplus` lookup "literal" fields of
         Just lang ->
           (codeblock ident classes fields (trimr lang) False
            . sourcesToText) <$> getInput
         Nothing   -> parseBlocks
  let isLiteral = isJust (lookup "code" fields `mplus` lookup "literal" fields)
  let selectLines =
        (case trim <$> lookup "end-before" fields of
                         Just patt -> takeWhile (not . (patt `T.isInfixOf`))
                         Nothing   -> id) .
        (case trim <$> lookup "start-after" fields of
                         Just patt -> drop 1 .
                                        dropWhile (not . (patt `T.isInfixOf`))
                         Nothing   -> id)

  let toStream t =
        Sources [(initialPos f,
                   (T.unlines . selectLines . T.lines $ t) <>
                    if isLiteral then mempty else "\n")]  -- see #7436
  currentDir <- takeDirectory . sourceName <$> getPosition
  insertIncludedFile parser toStream [currentDir] f startLine endLine

--
-- list blocks
--

list :: PandocMonad m => RSTParser m Blocks
list = choice [ bulletList, orderedList, definitionList ] <?> "list"

definitionListItem :: PandocMonad m => RSTParser m (Inlines, [Blocks])
definitionListItem = try $ do
  -- avoid capturing a directive or comment
  notFollowedBy (try $ char '.' >> char '.')
  term <- trimInlines . mconcat <$> many1Till inline endline
  raw <- indentedBlock
  -- parse the extracted block, which may contain various block elements:
  contents <- parseFromString' parseBlocks $ raw <> "\n"
  return (term, [contents])

definitionList :: PandocMonad m => RSTParser m Blocks
definitionList = B.definitionList <$> many1 definitionListItem

-- parses bullet list start and returns its length (inc. following whitespace)
bulletListStart :: Monad m => ParsecT Sources st m Int
bulletListStart = try $ do
  notFollowedBy' hrule  -- because hrules start out just like lists
  marker <- oneOf bulletListMarkers
  white <- many1 spaceChar <|> "" <$ lookAhead (char '\n')
  return $ length (marker:white)

-- parses ordered list start and returns its length (inc following whitespace)
orderedListStart :: Monad m => ListNumberStyle
                 -> ListNumberDelim
                 -> RSTParser m Int
orderedListStart style delim = try $ do
  (_, markerLen) <- withHorizDisplacement (orderedListMarker style delim)
  white <- many1 spaceChar <|> "" <$ lookAhead (char '\n')
  return $ markerLen + length white

-- parse a line of a list item
listLine :: Monad m => Int -> RSTParser m Text
listLine markerLength = try $ do
  notFollowedBy blankline
  indentWith markerLength
  anyLineNewline

-- parse raw text for one list item, excluding start marker and continuations
rawListItem :: Monad m => RSTParser m Int
            -> RSTParser m (Int, Text)
rawListItem start = try $ do
  markerLength <- start
  firstLine <- anyLineNewline
  restLines <- many (listLine markerLength)
  return (markerLength, firstLine <> T.concat restLines)

-- continuation of a list item - indented and separated by blankline or
-- (in compact lists) endline.
-- Note: nested lists are parsed as continuations.
listContinuation :: Monad m => Int -> RSTParser m Text
listContinuation markerLength = try $ do
  blanks <- many1Char blankline
  result <- many1 (listLine markerLength)
  return $ blanks <> T.concat result

listItem :: PandocMonad m
         => RSTParser m Int
         -> RSTParser m Blocks
listItem start = try $ do
  (markerLength, first) <- rawListItem start
  rest <- many (listContinuation markerLength)
  skipMany1 blankline <|> () <$ lookAhead start
  -- parsing with ListItemState forces markers at beginning of lines to
  -- count as list item markers, even if not separated by blank space.
  -- see definition of "endline"
  state <- getState
  let oldContext = stateParserContext state
  setState $ state {stateParserContext = ListItemState}
  -- parse the extracted block, which may itself contain block elements
  parsed <- parseFromString' parseBlocks $ T.concat (first:rest) <> "\n"
  updateState (\st -> st {stateParserContext = oldContext})
  return $ case B.toList parsed of
                [Para xs] ->
                   B.singleton $ Plain xs
                [Para xs, BulletList ys] ->
                   B.fromList [Plain xs, BulletList ys]
                [Para xs, OrderedList s ys] ->
                   B.fromList [Plain xs, OrderedList s ys]
                [Para xs, DefinitionList ys] ->
                   B.fromList [Plain xs, DefinitionList ys]
                _         -> parsed

orderedList :: PandocMonad m => RSTParser m Blocks
orderedList = try $ do
  (start, style, delim) <- lookAhead (anyOrderedListMarker <* spaceChar)
  items <- many1 (listItem (orderedListStart style delim))
  let items' = compactify items
  return $ B.orderedListWith (start, style, delim) items'

bulletList :: PandocMonad m => RSTParser m Blocks
bulletList = B.bulletList . compactify <$> many1 (listItem bulletListStart)

--
-- directive (e.g. comment, container, compound-paragraph)
--

comment :: Monad m => RSTParser m Blocks
comment = try $ do
  string ".."
  skipMany1 spaceChar <|> (() <$ lookAhead newline)
  -- notFollowedBy' directiveLabel -- comment comes after directive so unnec.
  _ <- anyLine
  optional indentedBlock
  optional blanklines
  return mempty

directiveLabel :: Monad m => RSTParser m Text
directiveLabel = T.toLower
  <$> many1TillChar (letter <|> char '-') (try $ string "::")

directive :: PandocMonad m => RSTParser m Blocks
directive = try $ do
  string ".."
  directive'

directive' :: PandocMonad m => RSTParser m Blocks
directive' = do
  skipMany1 spaceChar
  label <- directiveLabel
  skipMany spaceChar
  top <- manyChar $ satisfy (/='\n')
             <|> try (char '\n' <*
                      notFollowedBy' (rawFieldListItem 1) <*
                      many1 (char ' ') <*
                      notFollowedBy blankline)
  newline
  fields <- do
    fieldIndent <- length <$> lookAhead (many (char ' '))
    if fieldIndent == 0
       then return []
       else many $ rawFieldListItem fieldIndent
  let mbfile = trim <$> lookup "file" fields
  body <- case mbfile of
            Just f | label == "raw" -> do
               currentDir <- takeDirectory . sourceName <$> getPosition
               fromMaybe mempty <$> readFileFromDirs [currentDir] (T.unpack f)
            _ -> option "" $ try $ blanklines >> indentedBlock
  optional blanklines
  let body' = body <> "\n\n"
      name = trim $ fromMaybe "" (lookup "name" fields)
      classes = T.words $ maybe "" trim (lookup "class" fields)
      keyvals = [(k, trim v) | (k, v) <- fields, k /= "name", k /= "class"]
      imgAttr cl = (name, classes, alignClasses, widthAttr ++ heightAttr)
        where
          alignClasses = T.words $ maybe "" trim (lookup cl fields) <>
                          maybe "" (\x -> "align-" <> trim x)
                          (lookup "align" fields)
          scale = case trim <$> lookup "scale" fields of
                    Just v -> case T.unsnoc v of
                      Just (vv, '%') -> case safeRead vv of
                                          Just (percent :: Double)
                                            -> percent / 100.0
                                          Nothing -> 1.0
                      _ -> case safeRead v of
                             Just (s :: Double) -> s
                             Nothing            -> 1.0
                    Nothing -> 1.0
          widthAttr = maybe [] (\x -> [("width",
                                        tshow $ scaleDimension scale x)])
                        $ lookup "width" fields >>=
                          (lengthToDim . T.filter (not . isSpace))
          heightAttr = maybe [] (\x -> [("height",
                                         tshow $ scaleDimension scale x)])
                        $ lookup "height" fields >>=
                          (lengthToDim . T.filter (not . isSpace))
  case label of
        "include" -> includeDirective top fields body'
        "table" -> tableDirective top fields body'
        "list-table" -> listTableDirective top fields body'
        "csv-table" -> csvTableDirective top fields body'
        "line-block" -> lineBlockDirective body'
        "raw" -> return $ B.rawBlock (trim top) (stripTrailingNewlines body)
        "role" -> addNewRole top $ map (second trim) fields
        "container" -> B.divWith
                         (name, "container" : T.words top ++ classes, []) <$>
                          parseFromString' parseBlocks body'
        "replace" -> B.para <$>  -- consumed by substKey
                   parseInlineFromText (trim top)
        "date" -> B.para <$> do  -- consumed by substKey
                     t <- getTimestamp
                     let format = case T.unpack (T.strip top) of
                                    [] -> "%Y-%m-%d"
                                    x  -> x
                     return $ B.text $
                              T.pack $ formatTime defaultTimeLocale format t
        "unicode" -> B.para <$>  -- consumed by substKey
                   parseInlineFromText (trim $ unicodeTransform top)
        "compound" -> parseFromString' parseBlocks body'
        "pull-quote" -> B.blockQuote <$> parseFromString' parseBlocks body'
        "epigraph" -> B.blockQuote <$> parseFromString' parseBlocks body'
        "highlights" -> B.blockQuote <$> parseFromString' parseBlocks body'
        "rubric" -> B.para . B.strong <$> parseInlineFromText top
        _ | label `elem` ["attention","caution","danger","error","hint",
                          "important","note","tip","warning","admonition"] ->
           do bod <- parseFromString' parseBlocks $ top <> "\n\n" <> body'
              let lab = case label of
                          "admonition" -> mempty
                          (T.uncons -> Just (l, ls))
                            -> B.divWith ("",["title"],[])
                                          (B.para (B.str $ T.cons (toUpper l)  ls))
                          _ -> mempty
              return $ B.divWith (name,label:classes,keyvals) (lab <> bod)
        "sidebar" ->
           do let subtit = maybe "" trim $ lookup "subtitle" fields
              tit <- B.para . B.strong <$> parseInlineFromText
                          (trim top <> if T.null subtit
                                          then ""
                                          else ":  " <> subtit)
              bod <- parseFromString' parseBlocks body'
              return $ B.divWith (name,"sidebar":classes,keyvals) $ tit <> bod
        "topic" ->
           do tit <- B.para . B.strong <$> parseInlineFromText top
              bod <- parseFromString' parseBlocks body'
              return $ B.divWith (name,"topic":classes,keyvals) $ tit <> bod
        "default-role" -> mempty <$ updateState (\s ->
                              s { stateRstDefaultRole =
                                  case trim top of
                                     ""   -> stateRstDefaultRole def
                                     role -> role })
        "highlight" -> mempty <$ updateState (\s ->
                              s { stateRstHighlight =
                                  case trim top of
                                     ""   -> stateRstHighlight def
                                     lang -> Just lang })
        x | x == "code" || x == "code-block" || x == "sourcecode" ->
          return $ codeblock name classes (map (second trimr) fields)
             (trim top) True body
        "aafig" -> do
          let attribs = (name, ["aafig"], map (second trimr) fields)
          return $ B.codeBlockWith attribs $ stripTrailingNewlines body
        "math" -> return $ B.para
                  $ (case mkAttr name classes fields of
                       attr | attr == nullAttr -> id
                            | otherwise        -> B.spanWith attr)
                  $ mconcat $ map B.displayMath
                  $ toChunks $ top <> "\n\n" <> body
        "figure" -> do
           (caption, legend) <- parseFromString' extractCaption body'
           let src = escapeURI $ trim top
           let (imgident, imgcls, aligncls, imgkvs) = imgAttr "class"
           let (figclasskv, _) = partition ((== "figclass") . fst) keyvals
           let figcls = concatMap (T.words . snd) figclasskv
           let figattr = ("", figcls ++ aligncls, [])
           let capt = B.caption Nothing (B.plain caption <> legend)
           return $ B.figureWith figattr capt $
             B.plain (B.imageWith (imgident, imgcls, imgkvs) src "" (B.text src))
        "image" -> do
           let src = escapeURI $ trim top
           let alt = B.str $ maybe "image" trim $ lookup "alt" fields
           let attr = (ident, cls ++ align, dims) where
                 (ident, cls, align, dims) = imgAttr "class"
           return $ B.para
                  $ case lookup "target" fields of
                          Just t  -> B.link (escapeURI $ trim t) ""
                                     $ B.imageWith attr src "" alt
                          Nothing -> B.imageWith attr src "" alt
        "bibliography" -> pure $ B.divWith ("refs",[],[]) mempty
        "class" -> do
            let attrs = (name, T.words (trim top), map (second trimr) fields)
            --  directive content or the first immediately following element
            children <- case body of
                "" -> block
                _  -> parseFromString' parseBlocks  body'
            return $
              case B.toList children of
                [Header lev attrs' ils]
                  | T.null body -> -- # see #6699
                     B.headerWith (attrs' <> attrs) lev (B.fromList ils)
                _ -> B.divWith attrs children
        other     -> do
            pos <- getPosition
            logMessage $ SkippedContent (".. " <> other) pos
            bod <- parseFromString' parseBlocks $ top <> "\n\n" <> body'
            return $ B.divWith (name, other:classes, keyvals) bod

tableDirective :: PandocMonad m
               => Text -> [(Text, Text)] -> Text -> RSTParser m Blocks
tableDirective top fields body = do
  bs <- parseFromString' parseBlocks body
  case B.toList bs of
       [Table attr _ tspecs' thead@(TableHead _ thrs) tbody tfoot] -> do
         let (aligns', widths') = unzip tspecs'
         title <- parseFromString' (trimInlines . mconcat <$> many inline) top
         columns <- getOption readerColumns
         let numOfCols = case thrs of
               [] -> 0
               (r:_) -> rowLength r
         let normWidths ws =
                strictPos . (/ max 1.0 (fromIntegral (columns - numOfCols))) <$> ws
         let widths = case trim <$> lookup "widths" fields of
                           Just "auto" -> replicate numOfCols ColWidthDefault
                           Just "grid" -> widths'
                           Just specs -> normWidths
                               $ map (fromMaybe (0 :: Double) . safeRead)
                               $ splitTextBy (`elem` (" ," :: String)) specs
                           Nothing -> widths'
         -- align is not applicable since we can't represent whole table align
         let tspecs = zip aligns' widths
         return $ B.singleton $ Table attr (B.caption Nothing (B.plain title))
                                  tspecs thead tbody tfoot
       _ -> return mempty
  where
    -- only valid on the very first row of a table section
    rowLength (Row _ rb) = sum $ cellLength <$> rb
    cellLength (Cell _ _ _ (ColSpan w) _) = max 1 w
    strictPos w
      | w > 0     = ColWidth w
      | otherwise = ColWidthDefault

-- TODO: :stub-columns:.
-- Only the first row becomes the header even if header-rows: > 1,
-- since Pandoc doesn't support a table with multiple header rows.
-- We don't need to parse :align: as it represents the whole table align.
listTableDirective :: PandocMonad m
                   => Text -> [(Text, Text)] -> Text
                   -> RSTParser m Blocks
listTableDirective top fields body = do
  bs <- parseFromString' parseBlocks body
  title <- parseFromString' (trimInlines . mconcat <$> many inline) top
  let rows = takeRows $ B.toList bs
      headerRowsNum = fromMaybe (0 :: Int) $
         lookup "header-rows" fields >>= safeRead
      (headerRow,bodyRows,numOfCols) = case rows of
        x:xs -> if headerRowsNum > 0
                   then (x, xs, length x)
                   else ([], rows, length x)
        _ -> ([],[],0)
      widths = case trim <$> lookup "widths" fields of
        Just "auto" -> replicate numOfCols ColWidthDefault
        Just specs -> normWidths $ map (fromMaybe (0 :: Double) . safeRead) $
                           splitTextBy (`elem` (" ," :: String)) specs
        _ -> replicate numOfCols ColWidthDefault
      toRow = Row nullAttr . map B.simpleCell
      toHeaderRow l = [toRow l | not (null l)]
  return $ B.table (B.simpleCaption $ B.plain title)
             (zip (replicate numOfCols AlignDefault) widths)
             (TableHead nullAttr $ toHeaderRow headerRow)
             [TableBody nullAttr 0 [] $ map toRow bodyRows]
             (TableFoot nullAttr [])
    where takeRows [BulletList rows] = map takeCells rows
          takeRows _                 = []
          takeCells [BulletList cells] = map B.fromList cells
          takeCells _                  = []
          normWidths ws = strictPos . (/ max 1 (sum ws)) <$> ws
          strictPos w
            | w > 0     = ColWidth w
            | otherwise = ColWidthDefault

csvTableDirective :: PandocMonad m
                   => Text -> [(Text, Text)] -> Text
                   -> RSTParser m Blocks
csvTableDirective top fields rawcsv = do
  let explicitHeader = trim <$> lookup "header" fields
  let opts = defaultCSVOptions{
                csvDelim = case trim <$> lookup "delim" fields of
                                Just "tab"   -> '\t'
                                Just "space" -> ' '
                                Just (T.unpack -> [c])
                                             -> c
                                _            -> ','
              , csvQuote = case trim <$> lookup "quote" fields of
                                Just (T.unpack -> [c])
                                  -> Just c
                                _ -> Just '"'
              , csvEscape = case trim <$> lookup "escape" fields of
                                Just (T.unpack -> [c])
                                  -> Just c
                                _ -> Nothing
              , csvKeepSpace = case trim <$> lookup "keepspace" fields of
                                       Just "true" -> True
                                       _           -> False
              }
  let headerRowsNum = fromMaybe (case explicitHeader of
                                       Just _  -> 1 :: Int
                                       Nothing -> 0 :: Int) $
           lookup "header-rows" fields >>= safeRead
  rawcsv' <- case trim <$>
                    lookup "file" fields `mplus` lookup "url" fields of
                  Just u  -> do
                    (bs, _) <- fetchItem u
                    return $ UTF8.toText bs
                  Nothing -> return rawcsv
  let header' = case explicitHeader of
                  Just h  -> parseCSV defaultCSVOptions h
                  Nothing -> Right []
  let res = parseCSV opts rawcsv'
  case (<>) <$> header' <*> res of
       Left e  ->
         throwError $ fromParsecError (toSources rawcsv') e
       Right rawrows -> do
         let parseRow = mapM parseCell
         rows <- mapM parseRow rawrows
         let (headerRow,bodyRows,numOfCols) =
              case rows of
                   x:xs -> if headerRowsNum > 0
                          then (x, xs, length x)
                          else ([], rows, length x)
                   _ -> ([],[],0)
         title <- parseFromString' (trimInlines . mconcat <$> many inline) top
         let strictPos w
               | w > 0     = ColWidth w
               | otherwise = ColWidthDefault
         let normWidths ws = strictPos . (/ max 1 (sum ws)) <$> ws
         let widths =
               case trim <$> lookup "widths" fields of
                 Just "auto" -> replicate numOfCols ColWidthDefault
                 Just specs -> normWidths
                               $ map (fromMaybe (0 :: Double) . safeRead)
                               $ splitTextBy (`elem` (" ," :: String)) specs
                 _ -> replicate numOfCols ColWidthDefault
         let toRow = Row nullAttr . map B.simpleCell
             toHeaderRow l = [toRow l | not (null l)]
         return $ B.table (B.simpleCaption $ B.plain title)
                          (zip (replicate numOfCols AlignDefault) widths)
                          (TableHead nullAttr $ toHeaderRow headerRow)
                          [TableBody nullAttr 0 [] $ map toRow bodyRows]
                          (TableFoot nullAttr [])

singleParaToPlain :: Blocks -> Blocks
singleParaToPlain bs =
  case B.toList bs of
    [Para ils] -> B.fromList [Plain ils]
    _          -> bs

parseCell :: PandocMonad m => Text -> RSTParser m Blocks
parseCell t = singleParaToPlain
   <$> parseFromString' parseBlocks (t <> "\n\n")


-- TODO:
--  - Only supports :format: fields with a single format for :raw: roles,
--    change Text.Pandoc.Definition.Format to fix
addNewRole :: PandocMonad m
           => Text -> [(Text, Text)] -> RSTParser m Blocks
addNewRole roleText fields = do
    pos <- getPosition
    (role, parentRole) <- parseFromString' inheritedRole roleText
    customRoles <- stateRstCustomRoles <$> getState
    let getBaseRole (r, f, a) roles =
            case M.lookup r roles of
                 Just (r', f', a') -> getBaseRole (r', f', a') roles
                 Nothing           -> (r, f, a)
        (baseRole, baseFmt, baseAttr) =
               getBaseRole (parentRole, Nothing, nullAttr) customRoles
        fmt = if parentRole == "raw" then lookup "format" fields else baseFmt

        updateClasses :: [Text] -> [Text]
        updateClasses oldClasses = let

          codeLanguageClass = if baseRole == "code"
            then maybeToList (lookup "language" fields)
            else []

          -- if no ":class:" field is given, the default is the role name
          classFieldClasses = maybe [role] T.words (lookup "class" fields)

          -- nub in case role name & language class are the same
          in nub (classFieldClasses ++ codeLanguageClass ++ oldClasses)

        attr = let (ident, baseClasses, keyValues) = baseAttr
               in (ident, updateClasses baseClasses, keyValues)

    -- warn about syntax we ignore
    forM_ fields $ \(key, _) -> case key of
                 "language" -> when (baseRole /= "code") $ logMessage $
                     SkippedContent ":language: [because parent of role is not :code:]"
                        pos
                 "format" -> when (baseRole /= "raw") $ logMessage $
                     SkippedContent ":format: [because parent of role is not :raw:]" pos
                 _ -> logMessage $ SkippedContent (":" <> key <> ":") pos
    when (parentRole == "raw" && countKeys "format" > 1) $
        logMessage $ SkippedContent
                  ":format: [after first in definition of role]"
                  pos
    when (parentRole == "code" && countKeys "language" > 1) $
        logMessage $ SkippedContent
          ":language: [after first in definition of role]" pos

    updateState $ \s -> s {
        stateRstCustomRoles =
          M.insert role (baseRole, fmt, attr) customRoles
    }

    return mempty
  where
    countKeys k = length . filter (== k) . map fst $ fields
    inheritedRole =
        (,) <$> roleName <*> ((char '(' *> roleName <* char ')')
                            <|> pure "span")


-- Can contain character codes as decimal numbers or
-- hexadecimal numbers, prefixed by 0x, x, \x, U+, u, or \u
-- or as XML-style hexadecimal character entities, e.g. &#x1a2b;
-- or text, which is used as-is.  Comments start with ..
unicodeTransform :: Text -> Text
unicodeTransform t
  | Just xs <- T.stripPrefix ".." t  = unicodeTransform $ T.dropWhile (/= '\n') xs -- comment
  | Just xs <- T.stripPrefix "0x" t  = go "0x" xs
  | Just xs <- T.stripPrefix "x" t   = go "x" xs
  | Just xs <- T.stripPrefix "\\x" t = go "\\x" xs
  | Just xs <- T.stripPrefix "U+" t  = go "U+" xs
  | Just xs <- T.stripPrefix "u" t   = go "u" xs
  | Just xs <- T.stripPrefix "\\u" t = go "\\u" xs
  | Just xs <- T.stripPrefix "&#x" t = maybe ("&#x" <> unicodeTransform xs)
                                       -- drop semicolon
                                       (\(c,s) -> T.cons c $ unicodeTransform $ T.drop 1 s)
                                       $ extractUnicodeChar xs
  | Just (x, xs) <- T.uncons t       = T.cons x $ unicodeTransform xs
  | otherwise                        = ""
  where go pref zs = maybe (pref <> unicodeTransform zs)
                     (\(c,s) -> T.cons c $ unicodeTransform s)
                     $ extractUnicodeChar zs

extractUnicodeChar :: Text -> Maybe (Char, Text)
extractUnicodeChar s = fmap (\c -> (c,rest)) mbc
  where (ds,rest) = T.span isHexDigit s
        mbc = safeRead ("'\\x" <> ds <> "'")

extractCaption :: PandocMonad m => RSTParser m (Inlines, Blocks)
extractCaption = do
  capt <- trimInlines . mconcat <$> many inline
  legend <- optional blanklines >> (mconcat <$> many block)
  return (capt,legend)

-- divide string by blanklines, and surround with
-- \begin{aligned}...\end{aligned} if needed.
toChunks :: Text -> [Text]
toChunks = dropWhile T.null
           . map (addAligned . trim . T.unlines)
           . splitBy (T.all (`elem` (" \t" :: String))) . T.lines
  -- we put this in an aligned environment if it contains \\, see #4254
  where addAligned s = if "\\\\" `T.isInfixOf` s
                          then "\\begin{aligned}\n" <> s <> "\n\\end{aligned}"
                          else s

codeblock :: Text -> [Text] -> [(Text, Text)] -> Text -> Bool -> Text
          -> Blocks
codeblock ident classes fields lang rmTrailingNewlines body =
  B.codeBlockWith attribs $ stripTrailingNewlines' body
    where stripTrailingNewlines' = if rmTrailingNewlines
                                     then stripTrailingNewlines
                                     else id
          attribs = (ident, classes', kvs)
          classes' = lang
                    : ["numberLines" | isJust (lookup "number-lines" fields)]
                    ++ classes
          kvs = [(k,v) | (k,v) <- fields, k /= "number-lines", k /= "class",
                                          k /= "id", k /= "name"]
                ++ case lookup "number-lines" fields of
                     Just v | not (T.null v) -> [("startFrom", v)]
                     _ -> []

-- | Creates element attributes from a name, list of classes, and fields.
-- Removes fields named @name@, @id@, or @class@.
mkAttr :: Text -> [Text] -> [(Text, Text)] -> Attr
mkAttr ident classes fields = (ident, classes, fields')
  where fields' = [(k, v') | (k, v) <- fields
                           , let v' = trimr v
                           , k /= "name", k /= "id", k /= "class"]

---
--- note block
---

noteBlock :: Monad m => RSTParser m Blocks
noteBlock = try $ do
  (ref, raw) <- noteBlock' noteMarker
  updateState $ \s -> s { stateNotes = (ref, raw) : stateNotes s }
  return mempty

citationBlock :: Monad m => RSTParser m Blocks
citationBlock = try $ do
  (ref, raw) <- noteBlock' citationMarker
  updateState $ \s ->
     s { stateCitations = M.insert ref raw (stateCitations s),
         stateKeys = M.insert (toKey ref) (("#" <> ref,""), ("",["citation"],[]))
                               (stateKeys s) }
  return mempty

noteBlock' :: Monad m
           => RSTParser m Text -> RSTParser m (Text, Text)
noteBlock' marker = try $ do
  string ".."
  spaceChar >> skipMany spaceChar
  ref <- marker
  first <- (spaceChar >> skipMany spaceChar >> anyLine)
        <|> (newline >> return "")
  blanks <- option "" blanklines
  rest <- option "" indentedBlock
  let raw = first <> "\n" <> blanks <> rest <> "\n"
  return (ref, raw)

citationMarker :: Monad m => RSTParser m Text
citationMarker = do
  char '['
  res <- simpleReferenceName
  char ']'
  return res

noteMarker :: Monad m => RSTParser m Text
noteMarker = do
  char '['
  res <- many1Char digit
      <|>
                  try (char '#' >> liftM ("#" <>) simpleReferenceName)
      <|> countChar 1 (oneOf "#*")
  char ']'
  return res

--
-- reference key
--

quotedReferenceName :: PandocMonad m => RSTParser m Text
quotedReferenceName = try $ do
  char '`' >> notFollowedBy (char '`') -- `` means inline code!
  manyTillChar anyChar (char '`')

-- Simple reference names are single words consisting of alphanumerics
-- plus isolated (no two adjacent) internal hyphens, underscores,
-- periods, colons and plus signs; no whitespace or other characters
-- are allowed.
simpleReferenceName :: Monad m => ParsecT Sources st m Text
simpleReferenceName = do
  x <- alphaNum
  xs <- many $  alphaNum
            <|> try (oneOf "-_:+." <* lookAhead alphaNum)
  return $ T.pack (x:xs)

referenceName :: PandocMonad m => RSTParser m Text
referenceName = quotedReferenceName <|> simpleReferenceName

referenceKey :: PandocMonad m => RSTParser m Blocks
referenceKey = do
  choice [substKey, anonymousKey, regularKey]
  optional blanklines
  return mempty

targetURI :: Monad m => ParsecT Sources st m Text
targetURI = do
  skipSpaces
  optional $ try $ newline >> notFollowedBy blankline
  contents <- trim <$>
     many1Char (satisfy (/='\n')
     <|> try (newline >> many1 spaceChar >> noneOf " \t\n"))
  blanklines
  return $ stripBackticks contents
  where
    stripBackticks t
      | Just xs <- T.stripSuffix "`_" t = T.dropWhile (=='`') xs <> "_"
      | Just _  <- T.stripSuffix "_"  t = t
      | otherwise                       = escapeURI t

substKey :: PandocMonad m => RSTParser m ()
substKey = try $ do
  string ".."
  skipMany1 spaceChar
  (alt,ref) <- withRaw $ trimInlines . mconcat
                      <$> enclosed (char '|') (char '|') inline
  res <- B.toList <$> directive'
  bls <- case res of
             -- use alt unless :alt: attribute on image:
             [Para [Image attr [Str "image"] (src,tit)]] ->
                return $ B.para $ B.imageWith attr src tit alt
             [Para [Link _ [Image attr [Str "image"] (src,tit)] (src',tit')]] ->
                return $ B.para $ B.link src' tit' (B.imageWith attr src tit alt)
             _          -> return $ B.fromList res
  let key = toKey $ stripFirstAndLast ref
  updateState $ \s -> s{ stateSubstitutions =
                          M.insert key bls $ stateSubstitutions s }

anonymousKey :: PandocMonad m => RSTParser m ()
anonymousKey = try $ do
  oneOfStrings [".. __:", "__"]
  skipMany1 spaceChar
  src <- targetURI
  -- we need to ensure that the keys are ordered by occurrence in
  -- the document.
  numKeys <- M.size . stateKeys <$> getState
  let key = toKey $ "_" <> T.pack (printf "%04d" numKeys)
  updateState $ \s -> s { stateKeys = M.insert key ((src,""), nullAttr) $
                          stateKeys s }

referenceNames :: PandocMonad m => RSTParser m [Text]
referenceNames = do
  let rn = try $ do
             string ".. _"
             ref <- quotedReferenceName
                  <|> manyChar (  noneOf "\\:\n"
                              <|> try (char '\n' <*
                                       string "   " <*
                                       notFollowedBy blankline)
                              <|> try (char '\\' *> char ':')
                              <|> try (char ':' <* lookAhead alphaNum)
                               )
             char ':'
             return ref
  first <- rn
  rest  <- many (try (blanklines *> rn))
  return (first:rest)

regularKey :: PandocMonad m => RSTParser m ()
regularKey = try $ do
  -- we allow several references to the same URL, e.g.
  -- .. _hello:
  -- .. _goodbye: url.com
  refs <- referenceNames
  src <- targetURI
  guard $ not (T.null src)
  let keys = map toKey refs
  forM_ keys $ \key ->
    updateState $ \s -> s { stateKeys = M.insert key ((src,""), nullAttr) $
                            stateKeys s }

anchor :: PandocMonad m => RSTParser m Blocks
anchor = try $ do
  refs <- referenceNames
  blanklines
  forM_ refs $ \rawkey ->
    updateState $ \s -> s { stateKeys =
       M.insert (toKey rawkey) (("#" <> rawkey,""), nullAttr)
         (stateKeys s) }
  b <- block
  let addDiv ref = B.divWith (ref, [], [])
  let emptySpanWithId id' = Span (id',[],[]) []
  -- put identifier on next block:
  case B.toList b of
       [Header lev (_,classes,kvs) txt] ->
         case reverse refs of
              [] -> return b
              (r:rs) -> return $ B.singleton $
                           Header lev (r,classes,kvs)
                             (txt ++ map emptySpanWithId rs)
                -- we avoid generating divs for headers,
                -- because it hides them from promoteHeader, see #4240
       _ -> return $ foldr addDiv b refs

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

dashedLine :: Monad m => Char -> ParsecT Sources st m (Int, Int)
dashedLine ch = do
  dashes <- many1 (char ch)
  sp     <- many (char ' ')
  return (length dashes, length $ dashes ++ sp)

simpleDashedLines :: Monad m => Char -> ParsecT Sources st m [(Int,Int)]
simpleDashedLines ch = try $ many1 (dashedLine ch)

-- Parse a table row separator
simpleTableSep :: Monad m => Char -> RSTParser m Char
simpleTableSep ch = try $ simpleDashedLines ch >> newline

-- Parse a table footer
simpleTableFooter :: Monad m => RSTParser m Text
simpleTableFooter = try $ simpleTableSep '=' >> blanklines

-- Parse a raw line and split it into chunks by indices.
simpleTableRawLine :: Monad m => [Int] -> RSTParser m [Text]
simpleTableRawLine indices = simpleTableSplitLine indices <$> anyLine

simpleTableRawLineWithInitialEmptyCell :: Monad m => [Int] -> RSTParser m [Text]
simpleTableRawLineWithInitialEmptyCell indices = try $ do
  cs <- simpleTableRawLine indices
  let isEmptyCell = T.all (\c -> c == ' ' || c == '\t')
  case cs of
    c:_ | isEmptyCell c -> return cs
    _ -> mzero

-- Parse a table row and return a list of blocks (columns).
simpleTableRow :: PandocMonad m => [Int] -> RSTParser m [Blocks]
simpleTableRow indices = do
  notFollowedBy' simpleTableFooter
  firstLine <- simpleTableRawLine indices
  conLines  <- many $ simpleTableRawLineWithInitialEmptyCell indices
  let cols = map T.unlines . transpose $ firstLine : conLines ++
                                  [replicate (length indices) ""
                                    | not (null conLines)]
  mapM parseCell cols

simpleTableSplitLine :: [Int] -> Text -> [Text]
simpleTableSplitLine indices line =
  map trimr $ drop 1 $ splitTextByIndices (init indices) line

simpleTableHeader :: PandocMonad m
                  => Bool  -- ^ Headerless table
                  -> RSTParser m ([Blocks], [Alignment], [Int])
simpleTableHeader headless = try $ do
  optional blanklines
  rawContent  <- if headless
                    then return ""
                    else simpleTableSep '=' >> anyLine
  dashes      <- if headless
                    then simpleDashedLines '='
                    else simpleDashedLines '=' <|> simpleDashedLines '-'
  newline
  let lines'   = map snd dashes
  let indices  = scanl (+) 0 lines'
  let aligns   = replicate (length lines') AlignDefault
  let rawHeads = if headless
                    then []
                    else simpleTableSplitLine indices rawContent
  heads <- mapM ( parseFromString' (mconcat <$> many plain) . trim) rawHeads
  return (heads, aligns, indices)

-- Parse a simple table.
simpleTable :: PandocMonad m
            => Bool  -- ^ Headerless table
            -> RSTParser m Blocks
simpleTable headless = do
  let wrapIdFst (a, b, c) = (Identity a, b, c)
      wrapId = fmap Identity
  tbl <- runIdentity <$> tableWith
           (wrapIdFst <$> simpleTableHeader headless)
           (wrapId <$> simpleTableRow)
           sep simpleTableFooter
  -- Simple tables get 0s for relative column widths (i.e., use default)
  case B.toList tbl of
       [Table attr cap spec th tb tf] -> return $ B.singleton $
                                         Table attr cap (rewidth spec) th tb tf
       _ ->
         throwError $ PandocShouldNeverHappenError
            "tableWith returned something unexpected"
 where
  sep = return () -- optional (simpleTableSep '-')
  rewidth = fmap $ fmap $ const ColWidthDefault

gridTable :: PandocMonad m
          => RSTParser m Blocks
gridTable = runIdentity <$>
  gridTableWith (Identity <$> parseBlocks)

table :: PandocMonad m => RSTParser m Blocks
table = gridTable <|> simpleTable False <|> simpleTable True <?> "table"

--
-- inline
--

inline :: PandocMonad m => RSTParser m Inlines
inline =
  (note          -- can start with whitespace, so try before ws
    <|> do notAfterString >>= guard
           (link <|> inlineAnchor <|> strong <|> emph)
    <|> code
    <|> subst
    <|> interpretedRole
    <|> inlineContent) <?> "inline"

-- strings, spaces and other characters that can appear either by
-- themselves or within inline markup
inlineContent :: PandocMonad m => RSTParser m Inlines
inlineContent = choice [ whitespace
                       , str
                       , endline
                       , smart
                       , hyphens
                       , escapedChar
                       , symbol ] <?> "inline content"

parseInlineFromText :: PandocMonad m => Text -> RSTParser m Inlines
parseInlineFromText = parseFromString' (trimInlines . mconcat <$> many inline)

hyphens :: Monad m => RSTParser m Inlines
hyphens = do
  result <- many1Char (char '-')
  optional endline
  -- don't want to treat endline after hyphen or dash as a space
  return $ B.str result

escapedChar :: Monad m => RSTParser m Inlines
escapedChar = do c <- escaped anyChar
                 unless (canPrecedeOpener c) $ updateLastStrPos
                 return $ if c == ' ' || c == '\n' || c == '\r'
                             -- '\ ' is null in RST
                             then mempty
                             else B.str $ T.singleton c

canPrecedeOpener :: Char -> Bool
canPrecedeOpener c =
  generalCategory c `elem`
   [OpenPunctuation, InitialQuote, FinalQuote, DashPunctuation, OtherSymbol]

symbol :: Monad m => RSTParser m Inlines
symbol = do
  c <- oneOf specialChars
  unless (canPrecedeOpener c) $ updateLastStrPos
  return $ B.str $ T.singleton c

-- parses inline code, between codeStart and codeEnd
code :: Monad m => RSTParser m Inlines
code = try $ do
  string "``"
  result <- manyTillChar anyChar (try (string "``"))
  return $ B.code
         $ trim $ T.unwords $ T.lines result

-- succeeds only if we're not right after a str (ie. in middle of word)
atStart :: Monad m => RSTParser m a -> RSTParser m a
atStart p = do
  pos <- getPosition
  st <- getState
  -- single quote start can't be right after str
  guard $ stateLastStrPos st /= Just pos
  p

emph :: PandocMonad m => RSTParser m Inlines
emph = B.emph . trimInlines . mconcat <$>
         enclosed (atStart $ char '*') (char '*') inlineContent

strong :: PandocMonad m => RSTParser m Inlines
strong = B.strong . trimInlines . mconcat <$>
          enclosed (atStart $ string "**") (try $ string "**") inlineContent

-- Note, this doesn't precisely implement the complex rule in
-- http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#inline-markup-recognition-rules
-- but it should be good enough for most purposes
--
-- TODO:
--  - Classes are silently discarded in addNewRole
--  - Allows direct use of the :raw: role, rST only allows inherited use.
interpretedRole :: PandocMonad m => RSTParser m Inlines
interpretedRole = try $ do
  (role, contents) <- roleBefore <|> roleAfter
  renderRole contents Nothing role nullAttr

renderRole :: PandocMonad m
           => Text -> Maybe Text -> Text -> Attr -> RSTParser m Inlines
renderRole contents fmt role attr = case role of
    "sup"  -> return $ B.superscript $ treatAsText contents
    "superscript" -> return $ B.superscript $ treatAsText contents
    "sub"  -> return $ B.subscript $ treatAsText contents
    "subscript"  -> return $ B.subscript $ treatAsText contents
    "mark"  -> return $ B.spanWith ("",["mark"],[]) $ treatAsText contents
    "emphasis" -> return $ B.emph $ treatAsText contents
    "strong" -> return $ B.strong $ treatAsText contents
    "rfc-reference" -> return $ rfcLink contents
    "RFC" -> return $ rfcLink contents
    "pep-reference" -> return $ pepLink contents
    "PEP" -> return $ pepLink contents
    "literal" -> return $ B.codeWith attr contents
    "math" -> return $ B.math contents
    "title-reference" -> titleRef contents
    "title" -> titleRef contents
    "t" -> titleRef contents
    "code" -> return $ B.codeWith attr contents
    "span" -> return $ B.spanWith attr $ treatAsText contents
    "raw" -> return $ B.rawInline (fromMaybe "" fmt) contents
    custom
     | Just citeType <- T.stripPrefix "cite" custom
       -> cite citeType contents
     | otherwise -> do
        customRoles <- stateRstCustomRoles <$> getState
        case M.lookup custom customRoles of
            Just (newRole, newFmt, newAttr) ->
                renderRole contents newFmt newRole newAttr
            Nothing -> -- undefined role
                return $ B.codeWith ("",["interpreted-text"],[("role",role)])
                          contents
 where
   titleRef ref = return $ B.spanWith ("",["title-ref"],[]) $ treatAsText ref
   rfcLink rfcNo = B.link rfcUrl ("RFC " <> rfcNo) $ B.str ("RFC " <> rfcNo)
     where rfcUrl = "http://www.faqs.org/rfcs/rfc" <> rfcNo <> ".html"
   pepLink pepNo = B.link pepUrl ("PEP " <> pepNo) $ B.str ("PEP " <> pepNo)
     where padNo = T.replicate (4 - T.length pepNo) "0" <> pepNo
           pepUrl = "http://www.python.org/dev/peps/pep-" <> padNo <> "/"
   treatAsText = B.text . handleEscapes
   handleEscapes = T.concat . removeSpace . T.splitOn "\\"
     where headSpace t = fromMaybe t $ T.stripPrefix " " t
           removeSpace (x:xs) = x : map headSpace xs
           removeSpace []     = []

cite :: PandocMonad m => Text -> Text -> RSTParser m Inlines
cite citeType rawcite = do
  let citations =
        case map parseCite (T.splitOn "," rawcite) of
                (c:cs)
                  | citeType == ":t" || citeType == ":ct"
                     -> c{ citationMode = AuthorInText } : cs
                  | citeType == ":year" || citeType == ":yearpar"
                     -> c{ citationMode = SuppressAuthor } : cs
                cs -> cs
  pure $ B.cite citations (B.str rawcite)

parseCite :: Text -> Citation
parseCite t =
  let (_, pref, suff, ident) = T.foldl go (ParseStart, "", "", "") t
  in  Citation{citationId = ident
              ,citationPrefix = B.toList $ B.text pref
              ,citationSuffix = B.toList $ B.text suff
              ,citationMode = NormalCitation
              ,citationNoteNum = 0
              ,citationHash = 0}
 where
   go (ParseStart, p, s, i) '{' = (ParsePrefix, p, s, i)
   go (ParseStart, p, s, i) c = (ParseId, p, s, T.snoc i c)
   go (ParsePrefix, p, s, i) '}' = (ParseId, p, s, i)
   go (ParsePrefix, p, s, i) c = (ParsePrefix, T.snoc p c, s, i)
   go (ParseId, p, s, i) '{' = (ParseSuffix, p, s, i)
   go (ParseId, p, s, i) c = (ParseId, p, s, T.snoc i c)
   go (ParseSuffix, p, s, i) '}' = (ParseSuffix, p, s, i)
   go (ParseSuffix, p, s, i) c = (ParseSuffix, p, T.snoc s c, i)

data ParseCiteState = ParseStart | ParsePrefix | ParseSuffix | ParseId
  deriving (Show)

-- single words consisting of alphanumerics plus isolated (no two adjacent)
-- internal hyphens, underscores, periods, colons and plus signs;
-- no whitespace or other characters are allowed
roleName :: PandocMonad m => RSTParser m Text
roleName = many1Char (alphaNum <|> try (oneOf "-_.:+" <* lookAhead alphaNum))

roleMarker :: PandocMonad m => RSTParser m Text
roleMarker = char ':' *> roleName <* char ':'

roleBefore :: PandocMonad m => RSTParser m (Text,Text)
roleBefore = try $ do
  role <- roleMarker
  contents <- unmarkedInterpretedText
  return (role,contents)

roleAfter :: PandocMonad m => RSTParser m (Text,Text)
roleAfter = try $ do
  contents <- unmarkedInterpretedText
  role <- roleMarker <|> (stateRstDefaultRole <$> getState)
  return (role,contents)

unmarkedInterpretedText :: PandocMonad m => RSTParser m Text
unmarkedInterpretedText = try $ do
  atStart (char '`')
  contents <- mconcat <$> many1
       (  many1 (noneOf "`\\\n")
      <|> (char '\\' >> ((\c -> ['\\',c]) <$> noneOf "\n"))
      <|> (string "\n" <* notFollowedBy blankline)
      <|> try (string "`" <*
                notFollowedBy (() <$ roleMarker) <*
                lookAhead (satisfy isAlphaNum))
       )
  char '`'
  return $ T.pack contents

whitespace :: PandocMonad m => RSTParser m Inlines
whitespace = B.space <$ skipMany1 spaceChar <?> "whitespace"

str :: Monad m => RSTParser m Inlines
str = do
  let strChar = noneOf ("\t\n " ++ specialChars)
  result <- many1Char strChar
  updateLastStrPos
  return $ B.str result

-- an endline character that can be treated as a space, not a structural break
endline :: Monad m => RSTParser m Inlines
endline = try $ do
  newline
  notFollowedBy blankline
  -- parse potential list-starts at beginning of line differently in a list:
  st <- getState
  when (stateParserContext st == ListItemState) $ notFollowedBy (anyOrderedListMarker >> spaceChar) >>
          notFollowedBy' bulletListStart
  return B.softbreak

--
-- links
--

link :: PandocMonad m => RSTParser m Inlines
link = choice [explicitLink, referenceLink, autoLink]  <?> "link"

explicitLink :: PandocMonad m => RSTParser m Inlines
explicitLink = try $ do
  char '`'
  notFollowedBy (char '`') -- `` marks start of inline code
  label' <- trimInlines . mconcat <$>
              manyTill (notFollowedBy (char '`') >> inlineContent) (char '<')
  src <- trim . T.pack . filter (/= '\n') <$> -- see #10279
           manyTill (noneOf ">\n" <|> (char '\n' <* notFollowedBy blankline))
                    (char '>')
  skipSpaces
  string "`_"
  optional $ char '_' -- anonymous form
  let src' | isURI src = escapeURI src
           | otherwise =
              case T.unsnoc src of
                 Just (xs, '_') -> "##REF##" <> xs
                 _              -> src
  let label'' = if label' == mempty
                   then B.str src
                   else label'
  let key = toKey $ stringify label'
  unless (key == Key mempty) $ do
    updateState $ \s -> s{
      stateKeys = M.insert key ((src',""), nullAttr) $ stateKeys s }
  return $ B.linkWith nullAttr src' "" label''

citationName :: PandocMonad m => RSTParser m Text
citationName = do
  raw <- citationMarker
  return $ "[" <> raw <> "]"

-- We store the reference link label as the link target,
-- preceded by '##REF##'. This is replaced after the AST
-- has been built by the resolved reference.
referenceLink :: PandocMonad m => RSTParser m Inlines
referenceLink = try $ do
  ref <- (referenceName <|> citationName) <* char '_'
  isAnonymous <- (True <$ char '_') <|> pure False
  eof <|> notFollowedBy alphaNum
  let ref' = if isAnonymous
                then "_"
                else ref
  pure $ B.linkWith nullAttr ("##REF##" <> ref') "" (B.text ref)

-- We keep a list of oldkeys so we can detect lookup loops.
lookupKey :: PandocMonad m
          => [Key] -> Key -> RSTParser m ((Text, Text), Attr)
lookupKey oldkeys key = do
  pos <- getPosition
  state <- getState
  let keyTable = stateKeys state
  case M.lookup key keyTable of
       Nothing  -> do
         let Key key' = key
         logMessage $ ReferenceNotFound key' pos
         return (("",""),nullAttr)
       -- check for keys of the form link_, which need to be resolved:
       Just ((u, ""),_) | T.length u > 1, T.last u == '_', T.head u /= '#' -> do
         let rawkey = T.init u
         let newkey = toKey rawkey
         if newkey `elem` oldkeys
            then do
              -- TODO the pos is not going to be accurate
              -- because we're calling this after the AST is
              -- constructed. Probably good to remove that
              -- parameter form CircularReference at some point.
              logMessage $ CircularReference rawkey pos
              return (("",""),nullAttr)
            else lookupKey (key:oldkeys) newkey
       Just val -> return val

autoURI :: Monad m => RSTParser m Inlines
autoURI = do
  (orig, src) <- uri
  return $ B.link src "" $ B.str orig

autoEmail :: Monad m => RSTParser m Inlines
autoEmail = do
  (orig, src) <- emailAddress
  return $ B.link src "" $ B.str orig

autoLink :: PandocMonad m => RSTParser m Inlines
autoLink = autoURI <|> autoEmail

subst :: PandocMonad m => RSTParser m Inlines
subst = try $ do
  (_,ref) <- withRaw $ enclosed (char '|') (char '|') inline
  let substlink = B.linkWith nullAttr ("##SUBST##" <> ref) "" (B.text ref)
  reflink <- option False (True <$ char '_')
  if reflink
     then do
       let linkref = T.drop 1 $ T.dropEnd 1 ref
       return $ B.linkWith nullAttr ("##REF##" <> linkref) "" substlink
     else return substlink

note :: PandocMonad m => RSTParser m Inlines
note = try $ do
  optional whitespace
  ref <- noteMarker
  char '_'
  pure $ B.linkWith nullAttr ("##NOTE##" <> ref) "" (B.text ref)

smart :: PandocMonad m => RSTParser m Inlines
smart = smartPunctuation inline

inlineAnchor :: PandocMonad m => RSTParser m Inlines
inlineAnchor = try $ do
  char '_'
  name <- quotedReferenceName <|> simpleReferenceName
  let ident = textToIdentifier mempty name
  updateState $ \s ->
    s{ stateKeys = M.insert (toKey name) (("#" <> ident, ""), nullAttr)
                    (stateKeys s) }
  pure $ B.spanWith (ident,[],[]) (B.text name)
