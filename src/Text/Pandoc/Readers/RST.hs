{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-
Copyright (C) 2006-2015 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-2015 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion from reStructuredText to 'Pandoc' document.
-}
module Text.Pandoc.Readers.RST ( readRST ) where
import Text.Pandoc.Definition
import Text.Pandoc.Builder (setMeta, fromList)
import Text.Pandoc.Shared
import Text.Pandoc.Parsing
import Text.Pandoc.Options
import Text.Pandoc.Error
import qualified Text.Pandoc.UTF8 as UTF8
import Control.Monad ( when, liftM, guard, mzero )
import Data.List ( findIndex, intercalate, isInfixOf,
                   transpose, sort, deleteFirstsBy, isSuffixOf , nub, union)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Map as M
import Text.Printf ( printf )
import Text.Pandoc.Builder (Inlines, Blocks, trimInlines)
import qualified Text.Pandoc.Builder as B
import Data.Sequence (viewr, ViewR(..))
import Data.Char (toLower, isHexDigit, isSpace)
import Data.Monoid ((<>))
import Control.Monad.Except (throwError, catchError)
import Text.Pandoc.Class (PandocMonad, warning, readFileLazy, warningWithPos)

-- | Parse reStructuredText string and return Pandoc document.
readRST :: PandocMonad m
        => ReaderOptions -- ^ Reader options
        -> String        -- ^ String to parse (assuming @'\n'@ line endings)
        -> m Pandoc
readRST opts s = do
  parsed <- (readWithM parseRST) def{ stateOptions = opts } (s ++ "\n\n")
  case parsed of
    Right result -> return result
    Left e       -> throwError e

type RSTParser m = ParserT [Char] ParserState m

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
isHeader n (Header x _ _) = x == n
isHeader _ _              = False

-- | Promote all headers in a list of blocks.  (Part of
-- title transformation for RST.)
promoteHeaders :: Int -> [Block] -> [Block]
promoteHeaders num ((Header level attr text):rest) =
    (Header (level - num) attr text):(promoteHeaders num rest)
promoteHeaders num (other:rest) = other:(promoteHeaders num rest)
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
          ((Header 1 _ head1):(Header 2 _ head2):rest)
           | not (any (isHeader 1) rest || any (isHeader 2) rest) -> -- tit/sub
            (promoteHeaders 2 rest, setMeta "title" (fromList head1) $
              setMeta "subtitle" (fromList head2) meta)
          ((Header 1 _ head1):rest)
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
 where f (k,v) = setMeta (map toLower $ stringify k) (mconcat $ map fromList v)
       adjustAuthors (Meta metamap) = Meta $ M.adjust splitAuthors "author"
                                           $ M.adjust toPlain "date"
                                           $ M.adjust toPlain "title"
                                           $ M.mapKeys (\k -> if k == "authors" then "author" else k)
                                           $ metamap
       toPlain (MetaBlocks [Para xs]) = MetaInlines xs
       toPlain x                      = x
       splitAuthors (MetaBlocks [Para xs])
                                      = MetaList $ map MetaInlines
                                                 $ splitAuthors' xs
       splitAuthors x                 = x
       splitAuthors'                  = map normalizeSpaces .
                                         splitOnSemi . concatMap factorSemi
       splitOnSemi                    = splitBy (==Str ";")
       factorSemi (Str [])            = []
       factorSemi (Str s)             = case break (==';') s of
                                             (xs,[]) -> [Str xs]
                                             (xs,';':ys) -> Str xs : Str ";" :
                                                factorSemi (Str ys)
                                             (xs,ys) -> Str xs :
                                                factorSemi (Str ys)
       factorSemi x                   = [x]

parseRST :: PandocMonad m => RSTParser m Pandoc
parseRST = do
  optional blanklines -- skip blank lines at beginning of file
  startPos <- getPosition
  -- go through once just to get list of reference keys and notes
  -- docMinusKeys is the raw document with blanks where the keys were...
  docMinusKeys <- concat <$>
                  manyTill (referenceKey <|> noteBlock <|> lineClump) eof
  setInput docMinusKeys
  setPosition startPos
  st' <- getState
  let reversedNotes = stateNotes st'
  updateState $ \s -> s { stateNotes = reverse reversedNotes }
  -- now parse it for real...
  blocks <- B.toList <$> parseBlocks
  standalone <- getOption readerStandalone
  state <- getState
  let meta = stateMeta state
  let (blocks', meta') = if standalone
                            then titleTransform (blocks, meta)
                            else (blocks, meta)
  return $ Pandoc meta' blocks'

--
-- parsing blocks
--

parseBlocks :: PandocMonad m => RSTParser m Blocks
parseBlocks = mconcat <$> manyTill block eof

block :: PandocMonad m => RSTParser m Blocks
block = choice [ codeBlock
               , blockQuote
               , fieldList
               , include
               , directive
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

rawFieldListItem :: Monad m => Int -> RSTParser m (String, String)
rawFieldListItem minIndent = try $ do
  indent <- length <$> many (char ' ')
  guard $ indent >= minIndent
  char ':'
  name <- many1Till (noneOf "\n") (char ':')
  (() <$ lookAhead newline) <|> skipMany1 spaceChar
  first <- anyLine
  rest <- option "" $ try $ do lookAhead (count indent (char ' ') >> spaceChar)
                               indentedBlock
  let raw = (if null first then "" else (first ++ "\n")) ++ rest ++ "\n"
  return (name, raw)

fieldListItem :: PandocMonad m => Int -> RSTParser m (Inlines, [Blocks])
fieldListItem minIndent = try $ do
  (name, raw) <- rawFieldListItem minIndent
  term <- parseInlineFromString name
  contents <- parseFromString parseBlocks raw
  optional blanklines
  return (term, [contents])

fieldList :: PandocMonad m => RSTParser m Blocks
fieldList = try $ do
  indent <- length <$> lookAhead (many spaceChar)
  items <- many1 $ fieldListItem indent
  case items of
     []     -> return mempty
     items' -> return $ B.definitionList items'

--
-- line block
--

lineBlock :: PandocMonad m => RSTParser m Blocks
lineBlock = try $ do
  lines' <- lineBlockLines
  lines'' <- mapM parseInlineFromString lines'
  return $ B.lineBlock lines''

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
         ys :> (Str xs) | "::" `isSuffixOf` xs -> do
              raw <- option mempty codeBlockBody
              return $ B.para (B.Many ys <> B.str (take (length xs - 1) xs))
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
doubleHeader = try $ do
  c <- oneOf underlineChars
  rest <- many (char c)  -- the top line
  let lenTop = length (c:rest)
  skipSpaces
  newline
  txt <- trimInlines . mconcat <$> many1 (notFollowedBy blankline >> inline)
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
  attr <- registerHeader nullAttr txt
  return $ B.headerWith attr level txt

-- a header with line on the bottom only
singleHeader :: PandocMonad m => RSTParser m Blocks
singleHeader = try $ do
  notFollowedBy' whitespace
  txt <- trimInlines . mconcat <$> many1 (do {notFollowedBy blankline; inline})
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
  attr <- registerHeader nullAttr txt
  return $ B.headerWith attr level txt

--
-- hrule block
--

hrule :: Monad m => ParserT [Char] st m Blocks
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
indentedLine :: Monad m => String -> ParserT [Char] st m [Char]
indentedLine indents = try $ do
  string indents
  anyLine

-- one or more indented lines, possibly separated by blank lines.
-- any amount of indentation will work.
indentedBlock :: Monad m => ParserT [Char] st m [Char]
indentedBlock = try $ do
  indents <- lookAhead $ many1 spaceChar
  lns <- many1 $ try $ do b <- option "" blanklines
                          l <- indentedLine indents
                          return (b ++ l)
  optional blanklines
  return $ unlines lns

quotedBlock :: Monad m => ParserT [Char] st m [Char]
quotedBlock = try $ do
    quote <- lookAhead $ oneOf "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"
    lns <- many1 $ lookAhead (char quote) >> anyLine
    optional blanklines
    return $ unlines lns

codeBlockStart :: Monad m => ParserT [Char] st m Char
codeBlockStart = string "::" >> blankline >> blankline

codeBlock :: Monad m => ParserT [Char] st m Blocks
codeBlock = try $ codeBlockStart >> codeBlockBody

codeBlockBody :: Monad m => ParserT [Char] st m Blocks
codeBlockBody = try $ B.codeBlock . stripTrailingNewlines <$>
                (indentedBlock <|> quotedBlock)

lhsCodeBlock :: Monad m => RSTParser m Blocks
lhsCodeBlock = try $ do
  getPosition >>= guard . (==1) . sourceColumn
  guardEnabled Ext_literate_haskell
  optional codeBlockStart
  lns <- latexCodeBlock <|> birdCodeBlock
  blanklines
  return $ B.codeBlockWith ("", ["sourceCode", "literate", "haskell"], [])
         $ intercalate "\n" lns

latexCodeBlock :: Monad m => ParserT [Char] st m [[Char]]
latexCodeBlock = try $ do
  try (latexBlockLine "\\begin{code}")
  many1Till anyLine (try $ latexBlockLine "\\end{code}")
 where
  latexBlockLine s = skipMany spaceChar >> string s >> blankline

birdCodeBlock :: Monad m => ParserT [Char] st m [[Char]]
birdCodeBlock = filterSpace <$> many1 birdTrackLine
  where filterSpace lns =
            -- if (as is normal) there is always a space after >, drop it
            if all (\ln -> null ln || take 1 ln == " ") lns
               then map (drop 1) lns
               else lns

birdTrackLine :: Monad m => ParserT [Char] st m [Char]
birdTrackLine = char '>' >> anyLine

--
-- block quotes
--

blockQuote :: PandocMonad m => RSTParser m Blocks
blockQuote = do
  raw <- indentedBlock
  -- parse the extracted block, which may contain various block elements:
  contents <- parseFromString parseBlocks $ raw ++ "\n\n"
  return $ B.blockQuote contents

{-
Unsupported options for include:
tab-width
encoding
-}

include :: PandocMonad m => RSTParser m Blocks
include = try $ do
  string ".. include::"
  skipMany spaceChar
  f <- trim <$> anyLine
  fields <- many $ rawFieldListItem 3
  -- options
  let (startLine :: Maybe Int) = lookup "start-line" fields >>= safeRead
  let (endLine :: Maybe Int) = lookup "end-line" fields >>= safeRead
  guard $ not (null f)
  oldPos <- getPosition
  oldInput <- getInput
  containers <- stateContainers <$> getState
  when (f `elem` containers) $
    throwError $ PandocParseError $ "Include file loop at " ++ show oldPos
  updateState $ \s -> s{ stateContainers = f : stateContainers s }
  res <- readFileLazy' f
  contents <- case res of
                   Right x -> return x
                   Left _e -> do
                     warning $ "Could not read include file " ++ f ++ "."
                     return ""
  let contentLines = lines contents
  let numLines = length contentLines
  let startLine' = case startLine of
                        Nothing            -> 1
                        Just x | x >= 0    -> x
                               | otherwise -> numLines + x -- negative from end
  let endLine' = case endLine of
                        Nothing            -> numLines + 1
                        Just x | x >= 0    -> x
                               | otherwise -> numLines + x -- negative from end
  let contentLines' =   drop (startLine' - 1)
                      $ take (endLine' - 1)
                      $ contentLines
  let contentLines'' = (case trim <$> lookup "end-before" fields of
                             Just patt -> takeWhile (not . (patt `isInfixOf`))
                             Nothing   -> id) .
                       (case trim <$> lookup "start-after" fields of
                             Just patt -> drop 1 .
                                            dropWhile (not . (patt `isInfixOf`))
                             Nothing   -> id) $ contentLines'
  let contents' = unlines contentLines''
  case lookup "code" fields of
       Just lang -> do
         let numberLines = lookup "number-lines" fields
         let classes = trimr lang : ["numberLines" | isJust numberLines] ++
                          maybe [] words (lookup "class" fields)
         let kvs = maybe [] (\n -> [("startFrom", trimr n)]) numberLines
         let ident = maybe "" trimr $ lookup "name" fields
         let attribs = (ident, classes, kvs)
         return $ B.codeBlockWith attribs contents'
       Nothing   -> case lookup "literal" fields of
                         Just _  -> return $ B.rawBlock "rst" contents'
                         Nothing -> do
                           setPosition $ newPos f 1 1
                           setInput contents'
                           bs <- optional blanklines >>
                                  (mconcat <$> many block)
                           setInput oldInput
                           setPosition oldPos
                           updateState $ \s -> s{ stateContainers =
                                         tail $ stateContainers s }
                           return bs

readFileLazy' :: PandocMonad m => FilePath -> m (Either PandocError String)
readFileLazy' f = catchError ((Right . UTF8.toStringLazy) <$> readFileLazy f) $
  \(e :: PandocError) -> return (Left e)

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
  contents <- parseFromString parseBlocks $ raw ++ "\n"
  return (term, [contents])

definitionList :: PandocMonad m => RSTParser m Blocks
definitionList = B.definitionList <$> many1 definitionListItem

-- parses bullet list start and returns its length (inc. following whitespace)
bulletListStart :: Monad m => ParserT [Char] st m Int
bulletListStart = try $ do
  notFollowedBy' hrule  -- because hrules start out just like lists
  marker <- oneOf bulletListMarkers
  white <- many1 spaceChar
  return $ length (marker:white)

-- parses ordered list start and returns its length (inc following whitespace)
orderedListStart :: Monad m => ListNumberStyle
                 -> ListNumberDelim
                 -> RSTParser m Int
orderedListStart style delim = try $ do
  (_, markerLen) <- withHorizDisplacement (orderedListMarker style delim)
  white <- many1 spaceChar
  return $ markerLen + length white

-- parse a line of a list item
listLine :: Monad m => Int -> RSTParser m [Char]
listLine markerLength = try $ do
  notFollowedBy blankline
  indentWith markerLength
  line <- anyLine
  return $ line ++ "\n"

-- indent by specified number of spaces (or equiv. tabs)
indentWith :: Monad m => Int -> RSTParser m [Char]
indentWith num = do
  tabStop <- getOption readerTabStop
  if (num < tabStop)
     then count num  (char ' ')
     else choice [ try (count num (char ' ')),
                   (try (char '\t' >> count (num - tabStop) (char ' '))) ]

-- parse raw text for one list item, excluding start marker and continuations
rawListItem :: Monad m => RSTParser m Int
            -> RSTParser m (Int, [Char])
rawListItem start = try $ do
  markerLength <- start
  firstLine <- anyLine
  restLines <- many (listLine markerLength)
  return (markerLength, (firstLine ++ "\n" ++ (concat restLines)))

-- continuation of a list item - indented and separated by blankline or
-- (in compact lists) endline.
-- Note: nested lists are parsed as continuations.
listContinuation :: Monad m => Int -> RSTParser m [Char]
listContinuation markerLength = try $ do
  blanks <- many1 blankline
  result <- many1 (listLine markerLength)
  return $ blanks ++ concat result

listItem :: PandocMonad m
         => RSTParser m Int
         -> RSTParser m Blocks
listItem start = try $ do
  (markerLength, first) <- rawListItem start
  rest <- many (listContinuation markerLength)
  blanks <- choice [ try (many blankline <* lookAhead start),
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
  return $ case B.toList parsed of
                [Para xs] -> B.singleton $ Plain xs
                [Para xs, BulletList ys] -> B.fromList [Plain xs, BulletList ys]
                [Para xs, OrderedList s ys] -> B.fromList [Plain xs, OrderedList s ys]
                [Para xs, DefinitionList ys] -> B.fromList [Plain xs, DefinitionList ys]
                _         -> parsed

orderedList :: PandocMonad m => RSTParser m Blocks
orderedList = try $ do
  (start, style, delim) <- lookAhead (anyOrderedListMarker <* spaceChar)
  items <- many1 (listItem (orderedListStart style delim))
  let items' = compactify' items
  return $ B.orderedListWith (start, style, delim) items'

bulletList :: PandocMonad m => RSTParser m Blocks
bulletList = B.bulletList . compactify' <$> many1 (listItem bulletListStart)

--
-- directive (e.g. comment, container, compound-paragraph)
--

comment :: Monad m => RSTParser m Blocks
comment = try $ do
  string ".."
  skipMany1 spaceChar <|> (() <$ lookAhead newline)
  notFollowedBy' directiveLabel
  manyTill anyChar blanklines
  optional indentedBlock
  return mempty

directiveLabel :: Monad m => RSTParser m String
directiveLabel = map toLower
  <$> many1Till (letter <|> char '-') (try $ string "::")

directive :: PandocMonad m => RSTParser m Blocks
directive = try $ do
  string ".."
  directive'

-- TODO: line-block, parsed-literal, table, csv-table, list-table
-- date
-- include
-- title
directive' :: PandocMonad m => RSTParser m Blocks
directive' = do
  skipMany1 spaceChar
  label <- directiveLabel
  skipMany spaceChar
  top <- many $ satisfy (/='\n')
             <|> try (char '\n' <*
                      notFollowedBy' (rawFieldListItem 3) <*
                      count 3 (char ' ') <*
                      notFollowedBy blankline)
  newline
  fields <- many $ rawFieldListItem 3
  body <- option "" $ try $ blanklines >> indentedBlock
  optional blanklines
  let body' = body ++ "\n\n"
      imgAttr cl = ("", classes, getAtt "width" ++ getAtt "height")
        where
          classes = words $ maybe "" trim $ lookup cl fields
          getAtt k = case lookup k fields of
                       Just v  -> [(k, filter (not . isSpace) v)]
                       Nothing -> []
  case label of
        "raw" -> return $ B.rawBlock (trim top) (stripTrailingNewlines body)
        "role" -> addNewRole top $ map (\(k,v) -> (k, trim v)) fields
        "container" -> parseFromString parseBlocks body'
        "replace" -> B.para <$>  -- consumed by substKey
                   parseInlineFromString (trim top)
        "unicode" -> B.para <$>  -- consumed by substKey
                   parseInlineFromString (trim $ unicodeTransform top)
        "compound" -> parseFromString parseBlocks body'
        "pull-quote" -> B.blockQuote <$> parseFromString parseBlocks body'
        "epigraph" -> B.blockQuote <$> parseFromString parseBlocks body'
        "highlights" -> B.blockQuote <$> parseFromString parseBlocks body'
        "rubric" -> B.para . B.strong <$> parseInlineFromString top
        _ | label `elem` ["attention","caution","danger","error","hint",
                          "important","note","tip","warning"] ->
           do bod <- parseFromString parseBlocks $ top ++ "\n\n" ++ body'
              return $ B.divWith ("",["admonition", label],[]) bod
        "admonition" ->
           do bod <- parseFromString parseBlocks $ top ++ "\n\n" ++ body'
              return $ B.divWith ("",["admonition"],[]) bod
        "sidebar" ->
           do let subtit = maybe "" trim $ lookup "subtitle" fields
              tit <- B.para . B.strong <$> parseInlineFromString
                          (trim top ++ if null subtit
                                          then ""
                                          else (":  " ++ subtit))
              bod <- parseFromString parseBlocks body'
              return $ B.divWith ("",["sidebar"],[]) $ tit <> bod
        "topic" ->
           do tit <- B.para . B.strong <$> parseInlineFromString top
              bod <- parseFromString parseBlocks body'
              return $ B.divWith ("",["topic"],[]) $ tit <> bod
        "default-role" -> mempty <$ updateState (\s ->
                              s { stateRstDefaultRole =
                                  case trim top of
                                     ""   -> stateRstDefaultRole def
                                     role -> role })
        x | x == "code" || x == "code-block" ->
          codeblock (words $ fromMaybe [] $ lookup "class" fields)
                    (lookup "number-lines" fields) (trim top) body
        "aafig" -> do
          let attribs = ("", ["aafig"], map (\(k,v) -> (k, trimr v)) fields)
          return $ B.codeBlockWith attribs $ stripTrailingNewlines body
        "math" -> return $ B.para $ mconcat $ map B.displayMath
                         $ toChunks $ top ++ "\n\n" ++ body
        "figure" -> do
           (caption, legend) <- parseFromString extractCaption body'
           let src = escapeURI $ trim top
           return $ B.para (B.imageWith (imgAttr "figclass") src "fig:" caption) <> legend
        "image" -> do
           let src = escapeURI $ trim top
           let alt = B.str $ maybe "image" trim $ lookup "alt" fields
           let attr = imgAttr "class"
           return $ B.para
                  $ case lookup "target" fields of
                          Just t  -> B.link (escapeURI $ trim t) ""
                                     $ B.imageWith attr src "" alt
                          Nothing -> B.imageWith attr src "" alt
        "class" -> do
            let attrs = ("", (splitBy isSpace $ trim top), map (\(k,v) -> (k, trimr v)) fields)
            --  directive content or the first immediately following element
            children <- case body of
                "" -> block
                _ -> parseFromString parseBlocks  body'
            return $ B.divWith attrs children
        other     -> do
            pos <- getPosition
            warningWithPos pos $ "ignoring unknown directive: " ++ other
            return mempty

-- TODO:
--  - Only supports :format: fields with a single format for :raw: roles,
--    change Text.Pandoc.Definition.Format to fix
addNewRole :: PandocMonad m => String -> [(String, String)] -> RSTParser m Blocks
addNewRole roleString fields = do
    (role, parentRole) <- parseFromString inheritedRole roleString
    customRoles <- stateRstCustomRoles <$> getState
    let getBaseRole (r, f, a) roles =
            case M.lookup r roles of
                 Just (r', f', a') -> getBaseRole (r', f', a') roles
                 Nothing -> (r, f, a)
        (baseRole, baseFmt, baseAttr) =
               getBaseRole (parentRole, Nothing, nullAttr) customRoles
        fmt = if parentRole == "raw" then lookup "format" fields else baseFmt
        annotate :: [String] -> [String]
        annotate = maybe id (:) $
            if baseRole == "code"
               then lookup "language" fields
               else Nothing
        attr = let (ident, classes, keyValues) = baseAttr
        -- nub in case role name & language class are the same
               in (ident, nub . (role :) . annotate $ classes, keyValues)

    -- warn about syntax we ignore
    flip mapM_ fields $ \(key, _) -> case key of
        "language" -> when (baseRole /= "code") $ warning $
            "ignoring :language: field because the parent of role :" ++
            role ++ ": is :" ++ baseRole ++ ": not :code:"
        "format" -> when (baseRole /= "raw") $ warning $
            "ignoring :format: field because the parent of role :" ++
            role ++ ": is :" ++ baseRole ++ ": not :raw:"
        _ -> warning $ "ignoring unknown field :" ++ key ++
             ": in definition of role :" ++ role ++ ": in"
    when (parentRole == "raw" && countKeys "format" > 1) $
        warning $
        "ignoring :format: fields after the first in the definition of role :"
        ++ role ++": in"
    when (parentRole == "code" && countKeys "language" > 1) $
        warning $
        "ignoring :language: fields after the first in the definition of role :"
        ++ role ++": in"

    updateState $ \s -> s {
        stateRstCustomRoles =
          M.insert role (baseRole, fmt, attr) customRoles
    }

    return mempty
  where
    countKeys k = length . filter (== k) . map fst $ fields
    inheritedRole =
        (,) <$> roleName <*> ((char '(' *> roleName <* char ')') <|> pure "span")


-- Can contain character codes as decimal numbers or
-- hexadecimal numbers, prefixed by 0x, x, \x, U+, u, or \u
-- or as XML-style hexadecimal character entities, e.g. &#x1a2b;
-- or text, which is used as-is.  Comments start with ..
unicodeTransform :: String -> String
unicodeTransform t =
  case t of
       ('.':'.':xs)  -> unicodeTransform $ dropWhile (/='\n') xs -- comment
       ('0':'x':xs)  -> go "0x" xs
       ('x':xs)      -> go "x" xs
       ('\\':'x':xs) -> go "\\x" xs
       ('U':'+':xs)  -> go "U+" xs
       ('u':xs)      -> go "u" xs
       ('\\':'u':xs) -> go "\\u" xs
       ('&':'#':'x':xs) -> maybe ("&#x" ++ unicodeTransform xs)
                           -- drop semicolon
                           (\(c,s) -> c : unicodeTransform (drop 1 s))
                           $ extractUnicodeChar xs
       (x:xs)        -> x : unicodeTransform xs
       []            -> []
    where go pref zs = maybe (pref ++ unicodeTransform zs)
                         (\(c,s) -> c : unicodeTransform s)
                         $ extractUnicodeChar zs

extractUnicodeChar :: String -> Maybe (Char, String)
extractUnicodeChar s = maybe Nothing (\c -> Just (c,rest)) mbc
  where (ds,rest) = span isHexDigit s
        mbc = safeRead ('\'':'\\':'x':ds ++ "'")

extractCaption :: PandocMonad m => RSTParser m (Inlines, Blocks)
extractCaption = do
  capt <- trimInlines . mconcat <$> many inline
  legend <- optional blanklines >> (mconcat <$> many block)
  return (capt,legend)

-- divide string by blanklines
toChunks :: String -> [String]
toChunks = dropWhile null
           . map (trim . unlines)
           . splitBy (all (`elem` (" \t" :: String))) . lines

codeblock :: [String] -> Maybe String -> String -> String -> RSTParser m Blocks
codeblock classes numberLines lang body =
  return $ B.codeBlockWith attribs $ stripTrailingNewlines body
    where attribs = ("", classes', kvs)
          classes' = "sourceCode" : lang
                    : maybe [] (\_ -> ["numberLines"]) numberLines
                    ++ classes
          kvs     = case numberLines of
                          Just "" -> []
                          Nothing -> []
                          Just n  -> [("startFrom",trim n)]

---
--- note block
---

noteBlock :: Monad m => RSTParser m [Char]
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

noteMarker :: Monad m => RSTParser m [Char]
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

quotedReferenceName :: PandocMonad m => RSTParser m Inlines
quotedReferenceName = try $ do
  char '`' >> notFollowedBy (char '`') -- `` means inline code!
  label' <- trimInlines . mconcat <$> many1Till inline (char '`')
  return label'

unquotedReferenceName :: PandocMonad m => RSTParser m Inlines
unquotedReferenceName = try $ do
  label' <- trimInlines . mconcat <$> many1Till inline (lookAhead $ char ':')
  return label'

-- Simple reference names are single words consisting of alphanumerics
-- plus isolated (no two adjacent) internal hyphens, underscores,
-- periods, colons and plus signs; no whitespace or other characters
-- are allowed.
simpleReferenceName' :: Monad m => ParserT [Char] st m String
simpleReferenceName' = do
  x <- alphaNum
  xs <- many $  alphaNum
            <|> (try $ oneOf "-_:+." <* lookAhead alphaNum)
  return (x:xs)

simpleReferenceName :: Monad m => ParserT [Char] st m Inlines
simpleReferenceName = do
  raw <- simpleReferenceName'
  return $ B.str raw

referenceName :: PandocMonad m => RSTParser m Inlines
referenceName = quotedReferenceName <|>
                (try $ simpleReferenceName <* lookAhead (char ':')) <|>
                unquotedReferenceName

referenceKey :: PandocMonad m => RSTParser m [Char]
referenceKey = do
  startPos <- getPosition
  choice [substKey, anonymousKey, regularKey]
  optional blanklines
  endPos <- getPosition
  -- return enough blanks to replace key
  return $ replicate (sourceLine endPos - sourceLine startPos) '\n'

targetURI :: Monad m => ParserT [Char] st m [Char]
targetURI = do
  skipSpaces
  optional newline
  contents <- many1 (try (many spaceChar >> newline >>
                          many1 spaceChar >> noneOf " \t\n") <|> noneOf "\n")
  blanklines
  return $ escapeURI $ trim $ contents

substKey :: PandocMonad m => RSTParser m ()
substKey = try $ do
  string ".."
  skipMany1 spaceChar
  (alt,ref) <- withRaw $ trimInlines . mconcat
                      <$> enclosed (char '|') (char '|') inline
  res <- B.toList <$> directive'
  il <- case res of
             -- use alt unless :alt: attribute on image:
             [Para [Image attr [Str "image"] (src,tit)]] ->
                return $ B.imageWith attr src tit alt
             [Para [Link _ [Image attr [Str "image"] (src,tit)] (src',tit')]] ->
                return $ B.link src' tit' (B.imageWith attr src tit alt)
             [Para ils] -> return $ B.fromList ils
             _          -> mzero
  let key = toKey $ stripFirstAndLast ref
  updateState $ \s -> s{ stateSubstitutions = M.insert key il $ stateSubstitutions s }

anonymousKey :: Monad m => RSTParser m ()
anonymousKey = try $ do
  oneOfStrings [".. __:", "__"]
  src <- targetURI
  pos <- getPosition
  let key = toKey $ "_" ++ printf "%09d" (sourceLine pos)
  --TODO: parse width, height, class and name attributes
  updateState $ \s -> s { stateKeys = M.insert key ((src,""), nullAttr) $ stateKeys s }

stripTicks :: String -> String
stripTicks = reverse . stripTick . reverse . stripTick
  where stripTick ('`':xs) = xs
        stripTick xs = xs

regularKey :: PandocMonad m => RSTParser m ()
regularKey = try $ do
  string ".. _"
  (_,ref) <- withRaw referenceName
  char ':'
  src <- targetURI
  let key = toKey $ stripTicks ref
  --TODO: parse width, height, class and name attributes
  updateState $ \s -> s { stateKeys = M.insert key ((src,""), nullAttr) $ stateKeys s }

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

dashedLine :: Monad m => Char -> ParserT [Char] st m (Int, Int)
dashedLine ch = do
  dashes <- many1 (char ch)
  sp     <- many (char ' ')
  return (length dashes, length $ dashes ++ sp)

simpleDashedLines :: Monad m => Char -> ParserT [Char] st m [(Int,Int)]
simpleDashedLines ch = try $ many1 (dashedLine ch)

-- Parse a table row separator
simpleTableSep :: Monad m => Char -> RSTParser m Char
simpleTableSep ch = try $ simpleDashedLines ch >> newline

-- Parse a table footer
simpleTableFooter :: Monad m => RSTParser m [Char]
simpleTableFooter = try $ simpleTableSep '=' >> blanklines

-- Parse a raw line and split it into chunks by indices.
simpleTableRawLine :: Monad m => [Int] -> RSTParser m [String]
simpleTableRawLine indices = do
  line <- many1Till anyChar newline
  return (simpleTableSplitLine indices line)

-- Parse a table row and return a list of blocks (columns).
simpleTableRow :: PandocMonad m => [Int] -> RSTParser m [[Block]]
simpleTableRow indices = do
  notFollowedBy' simpleTableFooter
  firstLine <- simpleTableRawLine indices
  colLines  <- return [] -- TODO
  let cols = map unlines . transpose $ firstLine : colLines
  mapM (parseFromString (B.toList . mconcat <$> many plain)) cols

simpleTableSplitLine :: [Int] -> String -> [String]
simpleTableSplitLine indices line =
  map trim
  $ tail $ splitByIndices (init indices) line

simpleTableHeader :: PandocMonad m
                  => Bool  -- ^ Headerless table
                  -> RSTParser m ([[Block]], [Alignment], [Int])
simpleTableHeader headless = try $ do
  optional blanklines
  rawContent  <- if headless
                    then return ""
                    else simpleTableSep '=' >> anyLine
  dashes      <- simpleDashedLines '=' <|> simpleDashedLines '-'
  newline
  let lines'   = map snd dashes
  let indices  = scanl (+) 0 lines'
  let aligns   = replicate (length lines') AlignDefault
  let rawHeads = if headless
                    then replicate (length dashes) ""
                    else simpleTableSplitLine indices rawContent
  heads <- mapM (parseFromString (B.toList . mconcat <$> many plain)) $
             map trim rawHeads
  return (heads, aligns, indices)

-- Parse a simple table.
simpleTable :: PandocMonad m
            => Bool  -- ^ Headerless table
            -> RSTParser m Blocks
simpleTable headless = do
  Table c a _w h l <- tableWith (simpleTableHeader headless) simpleTableRow sep simpleTableFooter
  -- Simple tables get 0s for relative column widths (i.e., use default)
  return $ B.singleton $ Table c a (replicate (length a) 0) h l
 where
  sep = return () -- optional (simpleTableSep '-')

gridTable :: PandocMonad m
          => Bool -- ^ Headerless table
          -> RSTParser m Blocks
gridTable headerless = B.singleton
  <$> gridTableWith (B.toList <$> parseBlocks) headerless

table :: PandocMonad m => RSTParser m Blocks
table = gridTable False <|> simpleTable False <|>
        gridTable True  <|> simpleTable True <?> "table"

--
-- inline
--

inline :: PandocMonad m => RSTParser m Inlines
inline = choice [ note          -- can start with whitespace, so try before ws
                , whitespace
                , link
                , str
                , endline
                , strong
                , emph
                , code
                , subst
                , interpretedRole
                , smart
                , hyphens
                , escapedChar
                , symbol ] <?> "inline"

parseInlineFromString :: PandocMonad m => String -> RSTParser m Inlines
parseInlineFromString = parseFromString (trimInlines . mconcat <$> many inline)

hyphens :: Monad m => RSTParser m Inlines
hyphens = do
  result <- many1 (char '-')
  optional endline
  -- don't want to treat endline after hyphen or dash as a space
  return $ B.str result

escapedChar :: Monad m => ParserT [Char] st m Inlines
escapedChar = do c <- escaped anyChar
                 return $ if c == ' '  -- '\ ' is null in RST
                             then mempty
                             else B.str [c]

symbol :: Monad m => RSTParser m Inlines
symbol = do
  result <- oneOf specialChars
  return $ B.str [result]

-- parses inline code, between codeStart and codeEnd
code :: Monad m => RSTParser m Inlines
code = try $ do
  string "``"
  result <- manyTill anyChar (try (string "``"))
  return $ B.code
         $ trim $ unwords $ lines result

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
         enclosed (atStart $ char '*') (char '*') inline

strong :: PandocMonad m => RSTParser m Inlines
strong = B.strong . trimInlines . mconcat <$>
          enclosed (atStart $ string "**") (try $ string "**") inline

-- Note, this doesn't precisely implement the complex rule in
-- http://docutils.sourceforge.net/docs/ref/rst/restructuredtext.html#inline-markup-recognition-rules
-- but it should be good enough for most purposes
--
-- TODO:
--  - Classes are silently discarded in addNewRole
--  - Lacks sensible implementation for title-reference (which is the default)
--  - Allows direct use of the :raw: role, rST only allows inherited use.
interpretedRole :: PandocMonad m => RSTParser m Inlines
interpretedRole = try $ do
  (role, contents) <- roleBefore <|> roleAfter
  renderRole contents Nothing role nullAttr

renderRole :: PandocMonad m => String -> Maybe String -> String -> Attr -> RSTParser m Inlines
renderRole contents fmt role attr = case role of
    "sup"  -> return $ B.superscript $ B.str contents
    "superscript" -> return $ B.superscript $ B.str contents
    "sub"  -> return $ B.subscript $ B.str contents
    "subscript"  -> return $ B.subscript $ B.str contents
    "emphasis" -> return $ B.emph $ B.str contents
    "strong" -> return $ B.strong $ B.str contents
    "rfc-reference" -> return $ rfcLink contents
    "RFC" -> return $ rfcLink contents
    "pep-reference" -> return $ pepLink contents
    "PEP" -> return $ pepLink contents
    "literal" -> return $ B.codeWith attr contents
    "math" -> return $ B.math contents
    "title-reference" -> titleRef contents
    "title" -> titleRef contents
    "t" -> titleRef contents
    "code" -> return $ B.codeWith (addClass "sourceCode" attr) contents
    "span" -> return $ B.spanWith attr $ B.str contents
    "raw" -> return $ B.rawInline (fromMaybe "" fmt) contents
    custom -> do
        customRoles <- stateRstCustomRoles <$> getState
        case M.lookup custom customRoles of
            Just (newRole, newFmt, newAttr) ->
                renderRole contents newFmt newRole newAttr
            Nothing -> do
                pos <- getPosition
                warningWithPos pos $ "ignoring unknown role :" ++ custom ++ ": in"
                return $ B.str contents -- Undefined role
 where
   titleRef ref = return $ B.str ref -- FIXME: Not a sensible behaviour
   rfcLink rfcNo = B.link rfcUrl ("RFC " ++ rfcNo) $ B.str ("RFC " ++ rfcNo)
     where rfcUrl = "http://www.faqs.org/rfcs/rfc" ++ rfcNo ++ ".html"
   pepLink pepNo = B.link pepUrl ("PEP " ++ pepNo) $ B.str ("PEP " ++ pepNo)
     where padNo = replicate (4 - length pepNo) '0' ++ pepNo
           pepUrl = "http://www.python.org/dev/peps/pep-" ++ padNo ++ "/"

addClass :: String -> Attr -> Attr
addClass c (ident, classes, keyValues) = (ident, union classes [c], keyValues)

roleName :: PandocMonad m => RSTParser m String
roleName = many1 (letter <|> char '-')

roleMarker :: PandocMonad m => RSTParser m String
roleMarker = char ':' *> roleName <* char ':'

roleBefore :: PandocMonad m => RSTParser m (String,String)
roleBefore = try $ do
  role <- roleMarker
  contents <- unmarkedInterpretedText
  return (role,contents)

roleAfter :: PandocMonad m => RSTParser m (String,String)
roleAfter = try $ do
  contents <- unmarkedInterpretedText
  role <- roleMarker <|> (stateRstDefaultRole <$> getState)
  return (role,contents)

unmarkedInterpretedText :: PandocMonad m => RSTParser m [Char]
unmarkedInterpretedText = enclosed (atStart $ char '`') (char '`') anyChar

whitespace :: PandocMonad m => RSTParser m Inlines
whitespace = B.space <$ skipMany1 spaceChar <?> "whitespace"

str :: Monad m => RSTParser m Inlines
str = do
  let strChar = noneOf ("\t\n " ++ specialChars)
  result <- many1 strChar
  updateLastStrPos
  return $ B.str result

-- an endline character that can be treated as a space, not a structural break
endline :: Monad m => RSTParser m Inlines
endline = try $ do
  newline
  notFollowedBy blankline
  -- parse potential list-starts at beginning of line differently in a list:
  st <- getState
  if (stateParserContext st) == ListItemState
     then notFollowedBy (anyOrderedListMarker >> spaceChar) >>
          notFollowedBy' bulletListStart
     else return ()
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
             manyTill (notFollowedBy (char '`') >> inline) (char '<')
  src <- trim <$> manyTill (noneOf ">\n") (char '>')
  skipSpaces
  string "`_"
  optional $ char '_' -- anonymous form
  let label'' = if label' == mempty
                   then B.str src
                   else label'
  -- `link <google_>` is a reference link to _google!
  (src',tit,attr) <- case reverse src of
                          '_':xs -> do
                            keyTable <- stateKeys <$> getState
                            let key = toKey $ reverse xs
                            case M.lookup key keyTable of
                                 Nothing -> do
                                   pos <- getPosition
                                   warningWithPos pos $
                                     "Could not find reference for " ++
                                       show key
                                   return ("","",nullAttr)
                                 Just ((s,t),a) -> return (s,t,a)
                          _ -> return (src, "", nullAttr)
  return $ B.linkWith attr (escapeURI src') tit label''

referenceLink :: PandocMonad m => RSTParser m Inlines
referenceLink = try $ do
  (label',ref) <- withRaw (quotedReferenceName <|> simpleReferenceName) <*
                   char '_'
  state <- getState
  let keyTable = stateKeys state
  let isAnonKey (Key ('_':_)) = True
      isAnonKey _             = False
  key <- option (toKey $ stripTicks ref) $
                do char '_'
                   let anonKeys = sort $ filter isAnonKey $ M.keys keyTable
                   if null anonKeys
                      then mzero
                      else return (head anonKeys)
  ((src,tit), attr) <- case M.lookup key keyTable of
                         Nothing  -> do
                           pos <- getPosition
                           warningWithPos pos $
                             "Could not find reference for " ++
                               show key
                           return (("",""),nullAttr)
                         Just val -> return val
  -- if anonymous link, remove key so it won't be used again
  when (isAnonKey key) $ updateState $ \s -> s{ stateKeys = M.delete key keyTable }
  return $ B.linkWith attr src tit label'

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
  state <- getState
  let substTable = stateSubstitutions state
  let key = toKey $ stripFirstAndLast ref
  case M.lookup key substTable of
       Nothing     -> do
         pos <- getPosition
         warningWithPos pos $
           "Could not find reference for " ++ show key
         return mempty
       Just target -> return target

note :: PandocMonad m => RSTParser m Inlines
note = try $ do
  optional whitespace
  ref <- noteMarker
  char '_'
  state <- getState
  let notes = stateNotes state
  case lookup ref notes of
    Nothing   -> do
      pos <- getPosition
      warningWithPos pos $
        "Could not find note for " ++ show ref
      return mempty
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
      return $ B.note contents

smart :: PandocMonad m => RSTParser m Inlines
smart = do
  getOption readerSmart >>= guard
  doubleQuoted <|> singleQuoted <|>
    choice [apostrophe, dash, ellipses]

singleQuoted :: PandocMonad m => RSTParser m Inlines
singleQuoted = try $ do
  singleQuoteStart
  withQuoteContext InSingleQuote $
    B.singleQuoted . trimInlines . mconcat <$>
      many1Till inline singleQuoteEnd

doubleQuoted :: PandocMonad m => RSTParser m Inlines
doubleQuoted = try $ do
  doubleQuoteStart
  withQuoteContext InDoubleQuote $
    B.doubleQuoted . trimInlines . mconcat <$>
      many1Till inline doubleQuoteEnd
