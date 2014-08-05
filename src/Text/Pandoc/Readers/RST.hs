{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2006-2014 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-2014 John MacFarlane
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
import Text.Pandoc.Builder (setMeta, fromList)
import Text.Pandoc.Shared
import Text.Pandoc.Parsing
import Text.Pandoc.Options
import Control.Monad ( when, liftM, guard, mzero, mplus )
import Data.List ( findIndex, intersperse, intercalate,
                   transpose, sort, deleteFirstsBy, isSuffixOf )
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Text.Printf ( printf )
import Control.Applicative ((<$>), (<$), (<*), (*>), (<*>))
import Text.Pandoc.Builder (Inlines, Blocks, trimInlines, (<>))
import qualified Text.Pandoc.Builder as B
import Data.Monoid (mconcat, mempty)
import Data.Sequence (viewr, ViewR(..))
import Data.Char (toLower, isHexDigit)

-- | Parse reStructuredText string and return Pandoc document.
readRST :: ReaderOptions -- ^ Reader options
        -> String        -- ^ String to parse (assuming @'\n'@ line endings)
        -> Pandoc
readRST opts s = (readWith parseRST) def{ stateOptions = opts } (s ++ "\n\n")

type RSTParser = Parser [Char] ParserState

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

parseRST :: RSTParser Pandoc
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

parseBlocks :: RSTParser Blocks
parseBlocks = mconcat <$> manyTill block eof

block :: RSTParser Blocks
block = choice [ codeBlock
               , blockQuote
               , fieldList
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

rawFieldListItem :: Int -> RSTParser (String, String)
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

fieldListItem :: Int -> RSTParser (Inlines, [Blocks])
fieldListItem minIndent = try $ do
  (name, raw) <- rawFieldListItem minIndent
  let term = B.str name
  contents <- parseFromString parseBlocks raw
  optional blanklines
  return (term, [contents])

fieldList :: RSTParser Blocks
fieldList = try $ do
  indent <- length <$> lookAhead (many spaceChar)
  items <- many1 $ fieldListItem indent
  case items of
     []     -> return mempty
     items' -> return $ B.definitionList items'

--
-- line block
--

lineBlock :: RSTParser Blocks
lineBlock = try $ do
  lines' <- lineBlockLines
  lines'' <- mapM (parseFromString
                   (trimInlines . mconcat <$> many inline)) lines'
  return $ B.para (mconcat $ intersperse B.linebreak lines'')

--
-- paragraph block
--

-- note: paragraph can end in a :: starting a code block
para :: RSTParser Blocks
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

plain :: RSTParser Blocks
plain = B.plain . trimInlines . mconcat <$> many1 inline

--
-- header blocks
--

header :: RSTParser Blocks
header = doubleHeader <|> singleHeader <?> "header"

-- a header with lines on top and bottom
doubleHeader :: RSTParser Blocks
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
singleHeader :: RSTParser Blocks
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

hrule :: Parser [Char] st Blocks
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
indentedLine :: String -> Parser [Char] st [Char]
indentedLine indents = try $ do
  string indents
  anyLine

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

codeBlockStart :: Parser [Char] st Char
codeBlockStart = string "::" >> blankline >> blankline

codeBlock :: Parser [Char] st Blocks
codeBlock = try $ codeBlockStart >> codeBlockBody

codeBlockBody :: Parser [Char] st Blocks
codeBlockBody = try $ B.codeBlock . stripTrailingNewlines <$> indentedBlock

lhsCodeBlock :: RSTParser Blocks
lhsCodeBlock = try $ do
  getPosition >>= guard . (==1) . sourceColumn
  guardEnabled Ext_literate_haskell
  optional codeBlockStart
  lns <- latexCodeBlock <|> birdCodeBlock
  blanklines
  return $ B.codeBlockWith ("", ["sourceCode", "literate", "haskell"], [])
         $ intercalate "\n" lns

latexCodeBlock :: Parser [Char] st [[Char]]
latexCodeBlock = try $ do
  try (latexBlockLine "\\begin{code}")
  many1Till anyLine (try $ latexBlockLine "\\end{code}")
 where
  latexBlockLine s = skipMany spaceChar >> string s >> blankline

birdCodeBlock :: Parser [Char] st [[Char]]
birdCodeBlock = filterSpace <$> many1 birdTrackLine
  where filterSpace lns =
            -- if (as is normal) there is always a space after >, drop it
            if all (\ln -> null ln || take 1 ln == " ") lns
               then map (drop 1) lns
               else lns

birdTrackLine :: Parser [Char] st [Char]
birdTrackLine = char '>' >> anyLine

--
-- block quotes
--

blockQuote :: RSTParser Blocks
blockQuote = do
  raw <- indentedBlock
  -- parse the extracted block, which may contain various block elements:
  contents <- parseFromString parseBlocks $ raw ++ "\n\n"
  return $ B.blockQuote contents

--
-- list blocks
--

list :: RSTParser Blocks
list = choice [ bulletList, orderedList, definitionList ] <?> "list"

definitionListItem :: RSTParser (Inlines, [Blocks])
definitionListItem = try $ do
  -- avoid capturing a directive or comment
  notFollowedBy (try $ char '.' >> char '.')
  term <- trimInlines . mconcat <$> many1Till inline endline
  raw <- indentedBlock
  -- parse the extracted block, which may contain various block elements:
  contents <- parseFromString parseBlocks $ raw ++ "\n"
  return (term, [contents])

definitionList :: RSTParser Blocks
definitionList = B.definitionList <$> many1 definitionListItem

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
                 -> RSTParser Int
orderedListStart style delim = try $ do
  (_, markerLen) <- withHorizDisplacement (orderedListMarker style delim)
  white <- many1 spaceChar
  return $ markerLen + length white

-- parse a line of a list item
listLine :: Int -> RSTParser [Char]
listLine markerLength = try $ do
  notFollowedBy blankline
  indentWith markerLength
  line <- anyLine
  return $ line ++ "\n"

-- indent by specified number of spaces (or equiv. tabs)
indentWith :: Int -> RSTParser [Char]
indentWith num = do
  tabStop <- getOption readerTabStop
  if (num < tabStop)
     then count num  (char ' ')
     else choice [ try (count num (char ' ')),
                   (try (char '\t' >> count (num - tabStop) (char ' '))) ]

-- parse raw text for one list item, excluding start marker and continuations
rawListItem :: RSTParser Int
            -> RSTParser (Int, [Char])
rawListItem start = try $ do
  markerLength <- start
  firstLine <- anyLine
  restLines <- many (listLine markerLength)
  return (markerLength, (firstLine ++ "\n" ++ (concat restLines)))

-- continuation of a list item - indented and separated by blankline or
-- (in compact lists) endline.
-- Note: nested lists are parsed as continuations.
listContinuation :: Int -> RSTParser [Char]
listContinuation markerLength = try $ do
  blanks <- many1 blankline
  result <- many1 (listLine markerLength)
  return $ blanks ++ concat result

listItem :: RSTParser Int
         -> RSTParser Blocks
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

orderedList :: RSTParser Blocks
orderedList = try $ do
  (start, style, delim) <- lookAhead (anyOrderedListMarker <* spaceChar)
  items <- many1 (listItem (orderedListStart style delim))
  let items' = compactify' items
  return $ B.orderedListWith (start, style, delim) items'

bulletList :: RSTParser Blocks
bulletList = B.bulletList . compactify' <$> many1 (listItem bulletListStart)

--
-- directive (e.g. comment, container, compound-paragraph)
--

comment :: RSTParser Blocks
comment = try $ do
  string ".."
  skipMany1 spaceChar <|> (() <$ lookAhead newline)
  notFollowedBy' directiveLabel
  manyTill anyChar blanklines
  optional indentedBlock
  return mempty

directiveLabel :: RSTParser String
directiveLabel = map toLower
  <$> many1Till (letter <|> char '-') (try $ string "::")

directive :: RSTParser Blocks
directive = try $ do
  string ".."
  directive'

-- TODO: line-block, parsed-literal, table, csv-table, list-table
-- date
-- include
-- class
-- title
directive' :: RSTParser Blocks
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
  case label of
        "raw" -> return $ B.rawBlock (trim top) (stripTrailingNewlines body)
        "role" -> addNewRole top $ map (\(k,v) -> (k, trim v)) fields
        "container" -> parseFromString parseBlocks body'
        "replace" -> B.para <$>  -- consumed by substKey
                   parseFromString (trimInlines . mconcat <$> many inline)
                   (trim top)
        "unicode" -> B.para <$>  -- consumed by substKey
                   parseFromString (trimInlines . mconcat <$> many inline)
                   (trim $ unicodeTransform top)
        "compound" -> parseFromString parseBlocks body'
        "pull-quote" -> B.blockQuote <$> parseFromString parseBlocks body'
        "epigraph" -> B.blockQuote <$> parseFromString parseBlocks body'
        "highlights" -> B.blockQuote <$> parseFromString parseBlocks body'
        "rubric" -> B.para . B.strong <$> parseFromString
                          (trimInlines . mconcat <$> many inline) top
        _ | label `elem` ["attention","caution","danger","error","hint",
                          "important","note","tip","warning"] ->
           do let tit = B.para $ B.strong $ B.str label
              bod <- parseFromString parseBlocks $ top ++ "\n\n" ++ body'
              return $ B.blockQuote $ tit <> bod
        "admonition" ->
           do tit <- B.para . B.strong <$> parseFromString
                          (trimInlines . mconcat <$> many inline) top
              bod <- parseFromString parseBlocks body'
              return $ B.blockQuote $ tit <> bod
        "sidebar" ->
           do let subtit = maybe "" trim $ lookup "subtitle" fields
              tit <- B.para . B.strong <$> parseFromString
                          (trimInlines . mconcat <$> many inline)
                          (trim top ++ if null subtit
                                          then ""
                                          else (":  " ++ subtit))
              bod <- parseFromString parseBlocks body'
              return $ B.blockQuote $ tit <> bod
        "topic" ->
           do tit <- B.para . B.strong <$> parseFromString
                          (trimInlines . mconcat <$> many inline) top
              bod <- parseFromString parseBlocks body'
              return $ tit <> bod
        "default-role" -> mempty <$ updateState (\s ->
                              s { stateRstDefaultRole =
                                  case trim top of
                                     ""   -> stateRstDefaultRole def
                                     role -> role })
        "code" -> codeblock (lookup "number-lines" fields) (trim top) body
        "code-block" -> codeblock (lookup "number-lines" fields) (trim top) body
        "aafig" -> do
          let attribs = ("", ["aafig"], map (\(k,v) -> (k, trimr v)) fields)
          return $ B.codeBlockWith attribs $ stripTrailingNewlines body
        "math" -> return $ B.para $ mconcat $ map B.displayMath
                         $ toChunks $ top ++ "\n\n" ++ body
        "figure" -> do
           (caption, legend) <- parseFromString extractCaption body'
           let src = escapeURI $ trim top
           return $ B.para (B.image src "fig:" caption) <> legend
        "image" -> do
           let src = escapeURI $ trim top
           let alt = B.str $ maybe "image" trim $ lookup "alt" fields
           return $ B.para
                  $ case lookup "target" fields of
                          Just t  -> B.link (escapeURI $ trim t) ""
                                     $ B.image src "" alt
                          Nothing -> B.image src "" alt
        _     -> return mempty

-- TODO:
--  - Silently ignores illegal fields
--  - Silently drops classes
--  - Only supports :format: fields with a single format for :raw: roles,
--    change Text.Pandoc.Definition.Format to fix
addNewRole :: String -> [(String, String)] -> RSTParser Blocks
addNewRole roleString fields = do
    (role, parentRole) <- parseFromString inheritedRole roleString
    customRoles <- stateRstCustomRoles <$> getState
    baseRole <- case M.lookup parentRole customRoles of
        Just (base, _, _) -> return base
        Nothing -> return parentRole

    let fmt = if baseRole == "raw" then lookup "format" fields else Nothing
        annotate = maybe id addLanguage $
            if baseRole == "code"
               then lookup "language" fields
               else Nothing

    updateState $ \s -> s {
        stateRstCustomRoles =
          M.insert role (baseRole, fmt, (,) parentRole . annotate) customRoles
    }

    return $ B.singleton Null
  where
    addLanguage lang (ident, classes, keyValues) =
        (ident, "sourceCode" : lang : classes, keyValues)
    inheritedRole =
        (,) <$> roleNameEndingIn (char '(') <*> roleNameEndingIn (char ')')

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

extractCaption :: RSTParser (Inlines, Blocks)
extractCaption = do
  capt <- trimInlines . mconcat <$> many inline
  legend <- optional blanklines >> (mconcat <$> many block)
  return (capt,legend)

-- divide string by blanklines
toChunks :: String -> [String]
toChunks = dropWhile null
           . map (trim . unlines)
           . splitBy (all (`elem` " \t")) . lines

codeblock :: Maybe String -> String -> String -> RSTParser Blocks
codeblock numberLines lang body =
  return $ B.codeBlockWith attribs $ stripTrailingNewlines body
    where attribs = ("", classes, kvs)
          classes = "sourceCode" : lang
                    : maybe [] (\_ -> ["numberLines"]) numberLines
          kvs     = case numberLines of
                          Just "" -> []
                          Nothing -> []
                          Just n  -> [("startFrom",trim n)]

---
--- note block
---

noteBlock :: RSTParser [Char]
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

noteMarker :: RSTParser [Char]
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

quotedReferenceName :: RSTParser Inlines
quotedReferenceName = try $ do
  char '`' >> notFollowedBy (char '`') -- `` means inline code!
  label' <- trimInlines . mconcat <$> many1Till inline (char '`')
  return label'

unquotedReferenceName :: RSTParser Inlines
unquotedReferenceName = try $ do
  label' <- trimInlines . mconcat <$> many1Till inline (lookAhead $ char ':')
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

simpleReferenceName :: Parser [Char] st Inlines
simpleReferenceName = do
  raw <- simpleReferenceName'
  return $ B.str raw

referenceName :: RSTParser Inlines
referenceName = quotedReferenceName <|>
                (try $ simpleReferenceName <* lookAhead (char ':')) <|>
                unquotedReferenceName

referenceKey :: RSTParser [Char]
referenceKey = do
  startPos <- getPosition
  choice [substKey, anonymousKey, regularKey]
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
  return $ escapeURI $ trim $ contents

substKey :: RSTParser ()
substKey = try $ do
  string ".."
  skipMany1 spaceChar
  (alt,ref) <- withRaw $ trimInlines . mconcat
                      <$> enclosed (char '|') (char '|') inline
  res <- B.toList <$> directive'
  il <- case res of
             -- use alt unless :alt: attribute on image:
             [Para [Image [Str "image"] (src,tit)]] ->
                return $ B.image src tit alt
             [Para [Link [Image [Str "image"] (src,tit)] (src',tit')]] ->
                return $ B.link src' tit' (B.image src tit alt)
             [Para ils] -> return $ B.fromList ils
             _          -> mzero
  let key = toKey $ stripFirstAndLast ref
  updateState $ \s -> s{ stateSubstitutions = M.insert key il $ stateSubstitutions s }

anonymousKey :: RSTParser ()
anonymousKey = try $ do
  oneOfStrings [".. __:", "__"]
  src <- targetURI
  pos <- getPosition
  let key = toKey $ "_" ++ printf "%09d" (sourceLine pos)
  updateState $ \s -> s { stateKeys = M.insert key (src,"") $ stateKeys s }

stripTicks :: String -> String
stripTicks = reverse . stripTick . reverse . stripTick
  where stripTick ('`':xs) = xs
        stripTick xs = xs

regularKey :: RSTParser ()
regularKey = try $ do
  string ".. _"
  (_,ref) <- withRaw referenceName
  char ':'
  src <- targetURI
  let key = toKey $ stripTicks ref
  updateState $ \s -> s { stateKeys = M.insert key (src,"") $ stateKeys s }

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
simpleTableSep :: Char -> RSTParser Char
simpleTableSep ch = try $ simpleDashedLines ch >> newline

-- Parse a table footer
simpleTableFooter :: RSTParser [Char]
simpleTableFooter = try $ simpleTableSep '=' >> blanklines

-- Parse a raw line and split it into chunks by indices.
simpleTableRawLine :: [Int] -> RSTParser [String]
simpleTableRawLine indices = do
  line <- many1Till anyChar newline
  return (simpleTableSplitLine indices line)

-- Parse a table row and return a list of blocks (columns).
simpleTableRow :: [Int] -> RSTParser [[Block]]
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

simpleTableHeader :: Bool  -- ^ Headerless table
                  -> RSTParser ([[Block]], [Alignment], [Int])
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
simpleTable :: Bool  -- ^ Headerless table
            -> RSTParser Blocks
simpleTable headless = do
  Table c a _w h l <- tableWith (simpleTableHeader headless) simpleTableRow sep simpleTableFooter
  -- Simple tables get 0s for relative column widths (i.e., use default)
  return $ B.singleton $ Table c a (replicate (length a) 0) h l
 where
  sep = return () -- optional (simpleTableSep '-')

gridTable :: Bool -- ^ Headerless table
          -> RSTParser Blocks
gridTable headerless = B.singleton
  <$> gridTableWith (B.toList <$> parseBlocks) headerless

table :: RSTParser Blocks
table = gridTable False <|> simpleTable False <|>
        gridTable True  <|> simpleTable True <?> "table"

--
-- inline
--

inline :: RSTParser Inlines
inline = choice [ whitespace
                , link
                , str
                , endline
                , strong
                , emph
                , code
                , subst
                , interpretedRole
                , note
                , smart
                , hyphens
                , escapedChar
                , symbol ] <?> "inline"

hyphens :: RSTParser Inlines
hyphens = do
  result <- many1 (char '-')
  optional endline
  -- don't want to treat endline after hyphen or dash as a space
  return $ B.str result

escapedChar :: Parser [Char] st Inlines
escapedChar = do c <- escaped anyChar
                 return $ if c == ' '  -- '\ ' is null in RST
                             then mempty
                             else B.str [c]

symbol :: RSTParser Inlines
symbol = do
  result <- oneOf specialChars
  return $ B.str [result]

-- parses inline code, between codeStart and codeEnd
code :: RSTParser Inlines
code = try $ do
  string "``"
  result <- manyTill anyChar (try (string "``"))
  return $ B.code
         $ trim $ unwords $ lines result

-- succeeds only if we're not right after a str (ie. in middle of word)
atStart :: RSTParser a -> RSTParser a
atStart p = do
  pos <- getPosition
  st <- getState
  -- single quote start can't be right after str
  guard $ stateLastStrPos st /= Just pos
  p

emph :: RSTParser Inlines
emph = B.emph . trimInlines . mconcat <$>
         enclosed (atStart $ char '*') (char '*') inline

strong :: RSTParser Inlines
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
interpretedRole :: RSTParser Inlines
interpretedRole = try $ do
  (role, contents) <- roleBefore <|> roleAfter
  renderRole contents Nothing role nullAttr

renderRole :: String -> Maybe String -> String -> Attr -> RSTParser Inlines
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
    "literal" -> return $ B.str contents
    "math" -> return $ B.math contents
    "title-reference" -> titleRef contents
    "title" -> titleRef contents
    "t" -> titleRef contents
    "code" -> return $ B.codeWith attr contents
    "raw" -> return $ B.rawInline (fromMaybe "" fmt) contents
    custom -> do
        customRole <- stateRstCustomRoles <$> getState
        case M.lookup custom customRole of
            Just (_, newFmt, inherit) -> let
                fmtStr = fmt `mplus` newFmt
                (newRole, newAttr) = inherit attr
                in renderRole contents fmtStr newRole newAttr
            Nothing -> return $ B.str contents -- Undefined role
 where
   titleRef ref = return $ B.str ref -- FIXME: Not a sensible behaviour
   rfcLink rfcNo = B.link rfcUrl ("RFC " ++ rfcNo) $ B.str ("RFC " ++ rfcNo)
     where rfcUrl = "http://www.faqs.org/rfcs/rfc" ++ rfcNo ++ ".html"
   pepLink pepNo = B.link pepUrl ("PEP " ++ pepNo) $ B.str ("PEP " ++ pepNo)
     where padNo = replicate (4 - length pepNo) '0' ++ pepNo
           pepUrl = "http://www.python.org/dev/peps/pep-" ++ padNo ++ "/"

roleNameEndingIn :: RSTParser Char -> RSTParser String
roleNameEndingIn end = many1Till (letter <|> char '-') end

roleMarker :: RSTParser String
roleMarker = char ':' *> roleNameEndingIn (char ':')

roleBefore :: RSTParser (String,String)
roleBefore = try $ do
  role <- roleMarker
  contents <- unmarkedInterpretedText
  return (role,contents)

roleAfter :: RSTParser (String,String)
roleAfter = try $ do
  contents <- unmarkedInterpretedText
  role <- roleMarker <|> (stateRstDefaultRole <$> getState)
  return (role,contents)

unmarkedInterpretedText :: RSTParser [Char]
unmarkedInterpretedText = enclosed (atStart $ char '`') (char '`') anyChar

whitespace :: RSTParser Inlines
whitespace = B.space <$ skipMany1 spaceChar <?> "whitespace"

str :: RSTParser Inlines
str = do
  let strChar = noneOf ("\t\n " ++ specialChars)
  result <- many1 strChar
  updateLastStrPos
  return $ B.str result

-- an endline character that can be treated as a space, not a structural break
endline :: RSTParser Inlines
endline = try $ do
  newline
  notFollowedBy blankline
  -- parse potential list-starts at beginning of line differently in a list:
  st <- getState
  if (stateParserContext st) == ListItemState
     then notFollowedBy (anyOrderedListMarker >> spaceChar) >>
          notFollowedBy' bulletListStart
     else return ()
  return B.space

--
-- links
--

link :: RSTParser Inlines
link = choice [explicitLink, referenceLink, autoLink]  <?> "link"

explicitLink :: RSTParser Inlines
explicitLink = try $ do
  char '`'
  notFollowedBy (char '`') -- `` marks start of inline code
  label' <- trimInlines . mconcat <$>
             manyTill (notFollowedBy (char '`') >> inline) (char '<')
  src <- manyTill (noneOf ">\n") (char '>')
  skipSpaces
  string "`_"
  optional $ char '_' -- anonymous form
  return $ B.link (escapeURI $ trim src) "" label'

referenceLink :: RSTParser Inlines
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
  (src,tit) <- case M.lookup key keyTable of
                    Nothing     -> fail "no corresponding key"
                    Just target -> return target
  -- if anonymous link, remove key so it won't be used again
  when (isAnonKey key) $ updateState $ \s -> s{ stateKeys = M.delete key keyTable }
  return $ B.link src tit label'

autoURI :: RSTParser Inlines
autoURI = do
  (orig, src) <- uri
  return $ B.link src "" $ B.str orig

autoEmail :: RSTParser Inlines
autoEmail = do
  (orig, src) <- emailAddress
  return $ B.link src "" $ B.str orig

autoLink :: RSTParser Inlines
autoLink = autoURI <|> autoEmail

subst :: RSTParser Inlines
subst = try $ do
  (_,ref) <- withRaw $ enclosed (char '|') (char '|') inline
  state <- getState
  let substTable = stateSubstitutions state
  case M.lookup (toKey $ stripFirstAndLast ref) substTable of
       Nothing     -> fail "no corresponding key"
       Just target -> return target

note :: RSTParser Inlines
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
      return $ B.note contents

smart :: RSTParser Inlines
smart = do
  getOption readerSmart >>= guard
  doubleQuoted <|> singleQuoted <|>
    choice [apostrophe, dash, ellipses]

singleQuoted :: RSTParser Inlines
singleQuoted = try $ do
  singleQuoteStart
  withQuoteContext InSingleQuote $
    B.singleQuoted . trimInlines . mconcat <$>
      many1Till inline singleQuoteEnd

doubleQuoted :: RSTParser Inlines
doubleQuoted = try $ do
  doubleQuoteStart
  withQuoteContext InDoubleQuote $
    B.doubleQuoted . trimInlines . mconcat <$>
      many1Till inline doubleQuoteEnd
