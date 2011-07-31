{-# LANGUAGE RelaxedPolyRec #-} -- needed for inlinesBetween on GHC < 7
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
   Module      : Text.Pandoc.Readers.Markdown
   Copyright   : Copyright (C) 2006-2010 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of markdown-formatted plain text to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Markdown ( readMarkdown ) where

import Data.List ( transpose, sortBy, findIndex, intercalate )
import qualified Data.Map as M
import Data.Ord ( comparing )
import Data.Char ( isAlphaNum )
import Data.Maybe
import Text.Pandoc.Definition
import Text.Pandoc.Generic
import Text.Pandoc.Shared
import Text.Pandoc.Parsing
import Text.Pandoc.Readers.LaTeX ( rawLaTeXInline, rawLaTeXEnvironment' )
import Text.Pandoc.Readers.HTML ( htmlTag, htmlInBalanced, isInlineTag, isBlockTag,
                                  isTextTag, isCommentTag )
import Text.Pandoc.CharacterReferences ( decodeCharacterReferences )
import Text.ParserCombinators.Parsec
import Control.Monad (when, liftM, guard)
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match (tagOpen)

-- | Read markdown from an input string and return a Pandoc document.
readMarkdown :: ParserState -- ^ Parser state, including options for parser
             -> String      -- ^ String to parse (assuming @'\n'@ line endings)
             -> Pandoc
readMarkdown state s = (readWith parseMarkdown) state (s ++ "\n\n")

--
-- Constants and data structure definitions
--

isBulletListMarker :: Char -> Bool
isBulletListMarker '*' = True
isBulletListMarker '+' = True
isBulletListMarker '-' = True
isBulletListMarker _   = False

isHruleChar :: Char -> Bool
isHruleChar '*' = True
isHruleChar '-' = True
isHruleChar '_' = True
isHruleChar _   = False

setextHChars :: [Char]
setextHChars = "=-"

isBlank :: Char -> Bool
isBlank ' '  = True
isBlank '\t' = True
isBlank '\n' = True
isBlank _    = False

--
-- auxiliary functions
--

indentSpaces :: GenParser Char ParserState [Char]
indentSpaces = try $ do
  state <- getState
  let tabStop = stateTabStop state
  count tabStop (char ' ') <|>
    string "\t" <?> "indentation"

nonindentSpaces :: GenParser Char ParserState [Char]
nonindentSpaces = do
  state <- getState
  let tabStop = stateTabStop state
  sps <- many (char ' ')
  if length sps < tabStop 
     then return sps
     else unexpected "indented line"

skipNonindentSpaces :: GenParser Char ParserState ()
skipNonindentSpaces = do
  state <- getState
  atMostSpaces (stateTabStop state - 1)

atMostSpaces :: Int -> GenParser Char ParserState ()
atMostSpaces 0 = notFollowedBy (char ' ')
atMostSpaces n = (char ' ' >> atMostSpaces (n-1)) <|> return ()

-- | Fail unless we're at beginning of a line.
failUnlessBeginningOfLine :: GenParser tok st () 
failUnlessBeginningOfLine = do
  pos <- getPosition
  if sourceColumn pos == 1 then return () else fail "not beginning of line"

-- | Parse a sequence of inline elements between square brackets,
-- including inlines between balanced pairs of square brackets.
inlinesInBalancedBrackets :: GenParser Char ParserState Inline
                          -> GenParser Char ParserState [Inline]
inlinesInBalancedBrackets parser = try $ do
  char '['
  result <- manyTill ( (do lookAhead $ try $ do (Str res) <- parser
                                                guard (res == "[")
                           bal <- inlinesInBalancedBrackets parser
                           return $ [Str "["] ++ bal ++ [Str "]"])
                       <|> (count 1 parser))
                     (char ']')
  return $ concat result

--
-- document structure
--

titleLine :: GenParser Char ParserState [Inline]
titleLine = try $ do
  char '%'
  skipSpaces
  res <- many $ (notFollowedBy newline >> inline)
             <|> try (endline >> whitespace)
  newline
  return $ normalizeSpaces res

authorsLine :: GenParser Char ParserState [[Inline]]
authorsLine = try $ do 
  char '%'
  skipSpaces
  authors <- sepEndBy (many (notFollowedBy (satisfy $ \c ->
                                c == ';' || c == '\n') >> inline))
                       (char ';' <|>
                        try (newline >> notFollowedBy blankline >> spaceChar))
  newline
  return $ filter (not . null) $ map normalizeSpaces authors

dateLine :: GenParser Char ParserState [Inline]
dateLine = try $ do
  char '%'
  skipSpaces
  date <- manyTill inline newline
  return $ normalizeSpaces date

titleBlock :: GenParser Char ParserState ([Inline], [[Inline]], [Inline])
titleBlock = try $ do
  failIfStrict
  title <- option [] titleLine
  author <- option [] authorsLine
  date <- option [] dateLine
  optional blanklines
  return (title, author, date)

parseMarkdown :: GenParser Char ParserState Pandoc 
parseMarkdown = do
  -- markdown allows raw HTML
  updateState (\state -> state { stateParseRaw = True })
  startPos <- getPosition
  -- go through once just to get list of reference keys and notes
  -- docMinusKeys is the raw document with blanks where the keys/notes were...
  st <- getState
  let firstPassParser = referenceKey
                     <|> (if stateStrict st then pzero else noteBlock)
                     <|> lineClump
  docMinusKeys <- liftM concat $ manyTill firstPassParser eof
  setInput docMinusKeys
  setPosition startPos
  st' <- getState
  let reversedNotes = stateNotes st'
  updateState $ \s -> s { stateNotes = reverse reversedNotes }
  -- now parse it for real...
  (title, author, date) <- option ([],[],[]) titleBlock
  blocks <- parseBlocks
  let doc = Pandoc (Meta title author date) $ filter (/= Null) blocks
  -- if there are labeled examples, change references into numbers
  examples <- liftM stateExamples getState
  let handleExampleRef :: Inline -> Inline
      handleExampleRef z@(Str ('@':xs)) =
        case M.lookup xs examples of
              Just n     -> Str (show n)
              Nothing    -> z
      handleExampleRef z = z
  if M.null examples
     then return doc
     else return $ bottomUp handleExampleRef doc

-- 
-- initial pass for references and notes
--

referenceKey :: GenParser Char ParserState [Char]
referenceKey = try $ do
  startPos <- getPosition
  skipNonindentSpaces
  lab <- reference
  char ':'
  skipSpaces >> optional newline >> skipSpaces >> notFollowedBy (char '[')
  let nl = char '\n' >> notFollowedBy blankline >> return ' '
  let sourceURL = liftM unwords $ many $ try $ do
                    notFollowedBy' referenceTitle
                    skipMany spaceChar
                    optional nl
                    skipMany spaceChar
                    notFollowedBy' reference
                    many1 (satisfy $ not . isBlank)
  let betweenAngles = try $ char '<' >>
                            manyTill (noneOf ">\n" <|> nl) (char '>')
  src <- try betweenAngles <|> sourceURL
  tit <- option "" referenceTitle
  blanklines
  endPos <- getPosition
  let target = (escapeURI $ removeTrailingSpace src,  tit)
  st <- getState
  let oldkeys = stateKeys st
  updateState $ \s -> s { stateKeys = M.insert (toKey lab) target oldkeys }
  -- return blanks so line count isn't affected
  return $ replicate (sourceLine endPos - sourceLine startPos) '\n'

referenceTitle :: GenParser Char st String
referenceTitle = try $ do 
  skipSpaces >> optional newline >> skipSpaces
  tit <-    (charsInBalanced '(' ')' >>= return . unwords . words)
        <|> do delim <- char '\'' <|> char '"'
               manyTill anyChar (try (char delim >> skipSpaces >>
                                      notFollowedBy (noneOf ")\n")))
  return $ decodeCharacterReferences tit

noteMarker :: GenParser Char ParserState [Char]
noteMarker = string "[^" >> many1Till (satisfy $ not . isBlank) (char ']')

rawLine :: GenParser Char ParserState [Char]
rawLine = try $ do
  notFollowedBy blankline
  notFollowedBy' $ try $ skipNonindentSpaces >> noteMarker
  optional indentSpaces
  anyLine

rawLines :: GenParser Char ParserState [Char]
rawLines = do
  first <- anyLine
  rest <- many rawLine
  return $ unlines (first:rest)

noteBlock :: GenParser Char ParserState [Char]
noteBlock = try $ do
  startPos <- getPosition
  skipNonindentSpaces
  ref <- noteMarker
  char ':'
  optional blankline
  optional indentSpaces
  raw <- sepBy rawLines
             (try (blankline >> indentSpaces >>
                   notFollowedBy blankline))
  optional blanklines
  endPos <- getPosition
  let newnote = (ref, (intercalate "\n" raw) ++ "\n\n")
  st <- getState
  let oldnotes = stateNotes st
  updateState $ \s -> s { stateNotes = newnote : oldnotes }
  -- return blanks so line count isn't affected
  return $ replicate (sourceLine endPos - sourceLine startPos) '\n'

--
-- parsing blocks
--

parseBlocks :: GenParser Char ParserState [Block]
parseBlocks = manyTill block eof

block :: GenParser Char ParserState Block
block = do
  st <- getState
  choice (if stateStrict st
              then [ header
                   , codeBlockIndented
                   , blockQuote
                   , hrule
                   , bulletList
                   , orderedList
                   , htmlBlock
                   , para
                   , plain
                   , nullBlock ]
              else [ codeBlockDelimited
                   , macro
                   , header 
                   , table
                   , codeBlockIndented
                   , lhsCodeBlock
                   , blockQuote
                   , hrule
                   , bulletList
                   , orderedList
                   , definitionList
                   , rawTeXBlock
                   , para
                   , rawHtmlBlocks
                   , plain
                   , nullBlock ]) <?> "block"

--
-- header blocks
--

header :: GenParser Char ParserState Block
header = setextHeader <|> atxHeader <?> "header"

atxHeader :: GenParser Char ParserState Block
atxHeader = try $ do
  level <- many1 (char '#') >>= return . length
  notFollowedBy (char '.' <|> char ')') -- this would be a list
  skipSpaces
  text <- manyTill inline atxClosing >>= return . normalizeSpaces
  return $ Header level text

atxClosing :: GenParser Char st [Char]
atxClosing = try $ skipMany (char '#') >> blanklines

setextHeader :: GenParser Char ParserState Block
setextHeader = try $ do
  -- This lookahead prevents us from wasting time parsing Inlines
  -- unless necessary -- it gives a significant performance boost.
  lookAhead $ anyLine >> many1 (oneOf setextHChars) >> blankline
  text <- many1Till inline newline
  underlineChar <- oneOf setextHChars
  many (char underlineChar)
  blanklines
  let level = (fromMaybe 0 $ findIndex (== underlineChar) setextHChars) + 1
  return $ Header level (normalizeSpaces text)

--
-- hrule block
--

hrule :: GenParser Char st Block
hrule = try $ do
  skipSpaces
  start <- satisfy isHruleChar
  count 2 (skipSpaces >> char start)
  skipMany (spaceChar <|> char start)
  newline
  optional blanklines
  return HorizontalRule

--
-- code blocks
--

indentedLine :: GenParser Char ParserState [Char]
indentedLine = indentSpaces >> manyTill anyChar newline >>= return . (++ "\n")

codeBlockDelimiter :: Maybe Int
                   -> GenParser Char st (Int, ([Char], [[Char]], [([Char], [Char])]))
codeBlockDelimiter len = try $ do
  size <- case len of
              Just l  -> count l (char '~') >> many (char '~') >> return l
              Nothing -> count 3 (char '~') >> many (char '~') >>= 
                         return . (+ 3) . length 
  many spaceChar
  attr <- option ([],[],[]) attributes
  blankline
  return (size, attr) 

attributes :: GenParser Char st ([Char], [[Char]], [([Char], [Char])])
attributes = try $ do
  char '{'
  many spaceChar
  attrs <- many (attribute >>~ many spaceChar)
  char '}'
  let (ids, classes, keyvals) = unzip3 attrs
  let id' = if null ids then "" else head ids
  return (id', concat classes, concat keyvals)  

attribute :: GenParser Char st ([Char], [[Char]], [([Char], [Char])])
attribute = identifierAttr <|> classAttr <|> keyValAttr


identifier :: GenParser Char st [Char]
identifier = do
  first <- letter
  rest <- many $ alphaNum <|> oneOf "-_:."
  return (first:rest)

identifierAttr :: GenParser Char st ([Char], [a], [a1])
identifierAttr = try $ do
  char '#'
  result <- identifier
  return (result,[],[])

classAttr :: GenParser Char st ([Char], [[Char]], [a])
classAttr = try $ do
  char '.'
  result <- identifier
  return ("",[result],[])

keyValAttr :: GenParser Char st ([Char], [a], [([Char], [Char])])
keyValAttr = try $ do
  key <- identifier
  char '='
  char '"'
  val <- manyTill (satisfy (/='\n')) (char '"')
  return ("",[],[(key,val)])

codeBlockDelimited :: GenParser Char st Block
codeBlockDelimited = try $ do
  (size, attr) <- codeBlockDelimiter Nothing
  contents <- manyTill anyLine (codeBlockDelimiter (Just size))
  blanklines
  return $ CodeBlock attr $ intercalate "\n" contents

codeBlockIndented :: GenParser Char ParserState Block
codeBlockIndented = do
  contents <- many1 (indentedLine <|> 
                     try (do b <- blanklines
                             l <- indentedLine
                             return $ b ++ l))
  optional blanklines
  st <- getState
  return $ CodeBlock ("", stateIndentedCodeClasses st, []) $
           stripTrailingNewlines $ concat contents

lhsCodeBlock :: GenParser Char ParserState Block
lhsCodeBlock = do
  failUnlessLHS
  liftM (CodeBlock ("",["sourceCode","literate","haskell"],[]))
          (lhsCodeBlockBird <|> lhsCodeBlockLaTeX)
    <|> liftM (CodeBlock ("",["sourceCode","haskell"],[]))
          lhsCodeBlockInverseBird

lhsCodeBlockLaTeX :: GenParser Char ParserState String
lhsCodeBlockLaTeX = try $ do
  string "\\begin{code}"
  manyTill spaceChar newline
  contents <- many1Till anyChar (try $ string "\\end{code}")
  blanklines
  return $ stripTrailingNewlines contents

lhsCodeBlockBird :: GenParser Char ParserState String
lhsCodeBlockBird = lhsCodeBlockBirdWith '>'

lhsCodeBlockInverseBird :: GenParser Char ParserState String
lhsCodeBlockInverseBird = lhsCodeBlockBirdWith '<'

lhsCodeBlockBirdWith :: Char -> GenParser Char ParserState String
lhsCodeBlockBirdWith c = try $ do
  pos <- getPosition
  when (sourceColumn pos /= 1) $ fail "Not in first column"
  lns <- many1 $ birdTrackLine c
  -- if (as is normal) there is always a space after >, drop it
  let lns' = if all (\ln -> null ln || take 1 ln == " ") lns
                then map (drop 1) lns
                else lns
  blanklines
  return $ intercalate "\n" lns'

birdTrackLine :: Char -> GenParser Char st [Char]
birdTrackLine c = try $ do
  char c
  -- allow html tags on left margin:
  when (c == '<') $ notFollowedBy letter
  manyTill anyChar newline


--
-- block quotes
--

emailBlockQuoteStart :: GenParser Char ParserState Char
emailBlockQuoteStart = try $ skipNonindentSpaces >> char '>' >>~ optional (char ' ')

emailBlockQuote :: GenParser Char ParserState [[Char]]
emailBlockQuote = try $ do
  emailBlockQuoteStart
  raw <- sepBy (many (nonEndline <|> 
                      (try (endline >> notFollowedBy emailBlockQuoteStart >>
                       return '\n'))))
               (try (newline >> emailBlockQuoteStart))
  newline <|> (eof >> return '\n')
  optional blanklines
  return raw

blockQuote :: GenParser Char ParserState Block
blockQuote = do 
  raw <- emailBlockQuote
  -- parse the extracted block, which may contain various block elements:
  contents <- parseFromString parseBlocks $ (intercalate "\n" raw) ++ "\n\n"
  return $ BlockQuote contents
 
--
-- list blocks
--

bulletListStart :: GenParser Char ParserState ()
bulletListStart = try $ do
  optional newline -- if preceded by a Plain block in a list context
  skipNonindentSpaces
  notFollowedBy' hrule     -- because hrules start out just like lists
  satisfy isBulletListMarker
  spaceChar
  skipSpaces

anyOrderedListStart :: GenParser Char ParserState (Int, ListNumberStyle, ListNumberDelim) 
anyOrderedListStart = try $ do
  optional newline -- if preceded by a Plain block in a list context
  skipNonindentSpaces
  notFollowedBy $ string "p." >> spaceChar >> digit  -- page number
  state <- getState
  if stateStrict state
     then do many1 digit
             char '.'
             spaceChar
             return (1, DefaultStyle, DefaultDelim)
     else do (num, style, delim) <- anyOrderedListMarker
             -- if it could be an abbreviated first name, insist on more than one space
             if delim == Period && (style == UpperAlpha || (style == UpperRoman &&
                num `elem` [1, 5, 10, 50, 100, 500, 1000]))
                then char '\t' <|> (try $ char ' ' >> spaceChar)
                else spaceChar
             skipSpaces
             return (num, style, delim)

listStart :: GenParser Char ParserState ()
listStart = bulletListStart <|> (anyOrderedListStart >> return ())

-- parse a line of a list item (start = parser for beginning of list item)
listLine :: GenParser Char ParserState [Char]
listLine = try $ do
  notFollowedBy' listStart
  notFollowedBy blankline
  notFollowedBy' (do indentSpaces
                     many (spaceChar)
                     listStart)
  chunks <- manyTill (liftM snd (htmlTag isCommentTag) <|> count 1 anyChar) newline
  return $ concat chunks ++ "\n"

-- parse raw text for one list item, excluding start marker and continuations
rawListItem :: GenParser Char ParserState [Char]
rawListItem = try $ do
  listStart
  result <- many1 listLine
  blanks <- many blankline
  return $ concat result ++ blanks

-- continuation of a list item - indented and separated by blankline 
-- or (in compact lists) endline.
-- note: nested lists are parsed as continuations
listContinuation :: GenParser Char ParserState [Char]
listContinuation = try $ do
  lookAhead indentSpaces
  result <- many1 listContinuationLine
  blanks <- many blankline
  return $ concat result ++ blanks

listContinuationLine :: GenParser Char ParserState [Char]
listContinuationLine = try $ do
  notFollowedBy blankline
  notFollowedBy' listStart
  optional indentSpaces
  result <- manyTill anyChar newline
  return $ result ++ "\n"

listItem :: GenParser Char ParserState [Block]
listItem = try $ do 
  first <- rawListItem
  continuations <- many listContinuation
  -- parsing with ListItemState forces markers at beginning of lines to
  -- count as list item markers, even if not separated by blank space.
  -- see definition of "endline"
  state <- getState
  let oldContext = stateParserContext state
  setState $ state {stateParserContext = ListItemState}
  -- parse the extracted block, which may contain various block elements:
  let raw = concat (first:continuations)
  contents <- parseFromString parseBlocks raw
  updateState (\st -> st {stateParserContext = oldContext})
  return contents

orderedList :: GenParser Char ParserState Block
orderedList = try $ do
  (start, style, delim) <- lookAhead anyOrderedListStart
  items <- many1 listItem
  return $ OrderedList (start, style, delim) $ compactify items

bulletList :: GenParser Char ParserState Block
bulletList = try $ do
  lookAhead bulletListStart
  many1 listItem >>= return . BulletList . compactify

-- definition lists

defListMarker :: GenParser Char ParserState ()
defListMarker = do
  sps <- nonindentSpaces
  char ':' <|> char '~'
  st <- getState
  let tabStop = stateTabStop st
  let remaining = tabStop - (length sps + 1)
  if remaining > 0
     then count remaining (char ' ') <|> string "\t"
     else pzero
  return ()

definitionListItem :: GenParser Char ParserState ([Inline], [[Block]])
definitionListItem = try $ do
  -- first, see if this has any chance of being a definition list:
  lookAhead (anyLine >> optional blankline >> defListMarker)
  term <- manyTill inline newline
  optional blankline
  raw <- many1 defRawBlock
  state <- getState
  let oldContext = stateParserContext state
  -- parse the extracted block, which may contain various block elements:
  contents <- mapM (parseFromString parseBlocks) raw
  updateState (\st -> st {stateParserContext = oldContext})
  return ((normalizeSpaces term), contents)

defRawBlock :: GenParser Char ParserState [Char]
defRawBlock = try $ do
  defListMarker
  firstline <- anyLine
  rawlines <- many (notFollowedBy blankline >> indentSpaces >> anyLine)
  trailing <- option "" blanklines
  cont <- liftM concat $ many $ do
            lns <- many1 $ notFollowedBy blankline >> indentSpaces >> anyLine
            trl <- option "" blanklines
            return $ unlines lns ++ trl
  return $ firstline ++ "\n" ++ unlines rawlines ++ trailing ++ cont

definitionList :: GenParser Char ParserState Block
definitionList = do
  items <- many1 definitionListItem
  -- "compactify" the definition list:
  let defs = map snd items
  let defBlocks = reverse $ concat $ concat defs
  let isPara (Para _) = True
      isPara _        = False
  let items' = case take 1 defBlocks of
                [Para x]   -> if not $ any isPara (drop 1 defBlocks)
                                 then let (t,ds) = last items
                                          lastDef = last ds
                                          ds' = init ds ++
                                                [init lastDef ++ [Plain x]]
                                       in init items ++ [(t, ds')]
                                 else items
                _          -> items
  return $ DefinitionList items'

--
-- paragraph block
--

isHtmlOrBlank :: Inline -> Bool
isHtmlOrBlank (RawInline "html" _) = True
isHtmlOrBlank (Space)         = True
isHtmlOrBlank (LineBreak)     = True
isHtmlOrBlank _               = False

para :: GenParser Char ParserState Block
para = try $ do 
  result <- liftM normalizeSpaces $ many1 inline
  guard $ not . all isHtmlOrBlank $ result
  option (Plain result) $ try $ do
              newline
              blanklines <|>
                (getState >>= guard . stateStrict >>
                 lookAhead (blockQuote <|> header) >> return "")
              return $ Para result

plain :: GenParser Char ParserState Block
plain = many1 inline >>~ spaces >>= return . Plain . normalizeSpaces

-- 
-- raw html
--

htmlElement :: GenParser Char ParserState [Char]
htmlElement = strictHtmlBlock <|> liftM snd (htmlTag isBlockTag)

htmlBlock :: GenParser Char ParserState Block
htmlBlock = try $ do
    failUnlessBeginningOfLine
    first <- htmlElement
    finalSpace <- many spaceChar
    finalNewlines <- many newline
    return $ RawBlock "html" $ first ++ finalSpace ++ finalNewlines

strictHtmlBlock :: GenParser Char ParserState [Char]
strictHtmlBlock = do
  failUnlessBeginningOfLine
  htmlInBalanced (not . isInlineTag)

rawVerbatimBlock :: GenParser Char ParserState String
rawVerbatimBlock = try $ do
  (TagOpen tag _, open) <- htmlTag (tagOpen (\t ->
                                      t == "pre" || t == "style" || t == "script")
                                     (const True))
  contents <- manyTill anyChar (htmlTag (~== TagClose tag))
  return $ open ++ contents ++ renderTags [TagClose tag]

rawTeXBlock :: GenParser Char ParserState Block
rawTeXBlock = do
  failIfStrict
  result <- liftM (RawBlock "latex") rawLaTeXEnvironment'
          <|> liftM (RawBlock "context") rawConTeXtEnvironment'
  spaces
  return result

rawHtmlBlocks :: GenParser Char ParserState Block
rawHtmlBlocks = do
  htmlBlocks <- many1 $ do blk <- rawVerbatimBlock <|>
                                   liftM snd (htmlTag isBlockTag)
                           sps <- do sp1 <- many spaceChar
                                     sp2 <- option "" (blankline >> return "\n")
                                     sp3 <- many spaceChar
                                     sp4 <- option "" blanklines
                                     return $ sp1 ++ sp2 ++ sp3 ++ sp4
                           -- note: we want raw html to be able to
                           -- precede a code block, when separated
                           -- by a blank line
                           return $ blk ++ sps
  let combined = concat htmlBlocks
  let combined' = if last combined == '\n' then init combined else combined
  return $ RawBlock "html" combined'

--
-- Tables
-- 

-- Parse a dashed line with optional trailing spaces; return its length
-- and the length including trailing space.
dashedLine :: Char 
           -> GenParser Char st (Int, Int)
dashedLine ch = do
  dashes <- many1 (char ch)
  sp     <- many spaceChar
  return $ (length dashes, length $ dashes ++ sp)

-- Parse a table header with dashed lines of '-' preceded by 
-- one (or zero) line of text.
simpleTableHeader :: Bool  -- ^ Headerless table 
                  -> GenParser Char ParserState ([[Block]], [Alignment], [Int])
simpleTableHeader headless = try $ do
  rawContent  <- if headless
                    then return ""
                    else anyLine
  initSp      <- nonindentSpaces
  dashes      <- many1 (dashedLine '-')
  newline
  let (lengths, lines') = unzip dashes
  let indices  = scanl (+) (length initSp) lines'
  -- If no header, calculate alignment on basis of first row of text
  rawHeads <- liftM (tail . splitByIndices (init indices)) $
              if headless
                 then lookAhead anyLine 
                 else return rawContent
  let aligns   = zipWith alignType (map (\a -> [a]) rawHeads) lengths
  let rawHeads' = if headless
                     then replicate (length dashes) ""
                     else rawHeads 
  heads <- mapM (parseFromString (many plain)) $
             map removeLeadingTrailingSpace rawHeads'
  return (heads, aligns, indices)

-- Parse a table footer - dashed lines followed by blank line.
tableFooter :: GenParser Char ParserState [Char]
tableFooter = try $ skipNonindentSpaces >> many1 (dashedLine '-') >> blanklines

-- Parse a table separator - dashed line.
tableSep :: GenParser Char ParserState Char
tableSep = try $ skipNonindentSpaces >> many1 (dashedLine '-') >> char '\n'

-- Parse a raw line and split it into chunks by indices.
rawTableLine :: [Int]
             -> GenParser Char ParserState [String]
rawTableLine indices = do
  notFollowedBy' (blanklines <|> tableFooter)
  line <- many1Till anyChar newline
  return $ map removeLeadingTrailingSpace $ tail $ 
           splitByIndices (init indices) line

-- Parse a table line and return a list of lists of blocks (columns).
tableLine :: [Int]
          -> GenParser Char ParserState [[Block]]
tableLine indices = rawTableLine indices >>= mapM (parseFromString (many plain))

-- Parse a multiline table row and return a list of blocks (columns).
multilineRow :: [Int]
             -> GenParser Char ParserState [[Block]]
multilineRow indices = do
  colLines <- many1 (rawTableLine indices)
  let cols = map unlines $ transpose colLines
  mapM (parseFromString (many plain)) cols

-- Parses a table caption:  inlines beginning with 'Table:'
-- and followed by blank lines.
tableCaption :: GenParser Char ParserState [Inline]
tableCaption = try $ do
  skipNonindentSpaces
  string ":" <|> string "Table:"
  result <- many1 inline
  blanklines
  return $ normalizeSpaces result

-- Parse a simple table with '---' header and one line per row.
simpleTable :: Bool  -- ^ Headerless table
            -> GenParser Char ParserState Block
simpleTable headless = do
  Table c a _w h l <- tableWith (simpleTableHeader headless) tableLine
              (return ())
              (if headless then tableFooter else tableFooter <|> blanklines)
              tableCaption
  -- Simple tables get 0s for relative column widths (i.e., use default)
  return $ Table c a (replicate (length a) 0) h l

-- Parse a multiline table:  starts with row of '-' on top, then header
-- (which may be multiline), then the rows,
-- which may be multiline, separated by blank lines, and
-- ending with a footer (dashed line followed by blank line).
multilineTable :: Bool -- ^ Headerless table
               -> GenParser Char ParserState Block
multilineTable headless =
  tableWith (multilineTableHeader headless) multilineRow blanklines tableFooter tableCaption

multilineTableHeader :: Bool -- ^ Headerless table
                     -> GenParser Char ParserState ([[Block]], [Alignment], [Int])
multilineTableHeader headless = try $ do
  if headless
     then return '\n'
     else tableSep
  rawContent  <- if headless
                    then return $ repeat "" 
                    else many1
                         (notFollowedBy tableSep >> many1Till anyChar newline)
  initSp      <- nonindentSpaces
  dashes      <- many1 (dashedLine '-')
  newline
  let (lengths, lines') = unzip dashes
  let indices  = scanl (+) (length initSp) lines'
  rawHeadsList <- if headless
                     then liftM (map (:[]) . tail .
                              splitByIndices (init indices)) $ lookAhead anyLine
                     else return $ transpose $ map 
                           (\ln -> tail $ splitByIndices (init indices) ln)
                           rawContent
  let aligns   = zipWith alignType rawHeadsList lengths
  let rawHeads = if headless
                    then replicate (length dashes) ""
                    else map (intercalate " ") rawHeadsList
  heads <- mapM (parseFromString (many plain)) $
             map removeLeadingTrailingSpace rawHeads
  return (heads, aligns, indices)

-- Returns an alignment type for a table, based on a list of strings
-- (the rows of the column header) and a number (the length of the
-- dashed line under the rows.
alignType :: [String]
          -> Int
          -> Alignment
alignType [] _ = AlignDefault
alignType strLst len =
  let nonempties = filter (not . null) $ map removeTrailingSpace strLst
      (leftSpace, rightSpace) =
           case sortBy (comparing length) nonempties of
                 (x:_)  -> (head x `elem` " \t", length x < len)
                 []     -> (False, False)
  in  case (leftSpace, rightSpace) of
        (True,  False)   -> AlignRight
        (False, True)    -> AlignLeft
        (True,  True)    -> AlignCenter
        (False, False)   -> AlignDefault

gridTable :: Bool -- ^ Headerless table
          -> GenParser Char ParserState Block
gridTable = gridTableWith block tableCaption

table :: GenParser Char ParserState Block
table = multilineTable False <|> simpleTable True <|>
        simpleTable False <|> multilineTable True <|>
        gridTable False <|> gridTable True <?> "table"

-- 
-- inline
--

inline :: GenParser Char ParserState Inline
inline = choice inlineParsers <?> "inline"

inlineParsers :: [GenParser Char ParserState Inline]
inlineParsers = [ whitespace
                , str
                , endline
                , code
                , fours
                , strong
                , emph
                , note
                , link
                , cite
                , image
                , math
                , strikeout
                , superscript
                , subscript
                , inlineNote  -- after superscript because of ^[link](/foo)^
                , autoLink
                , rawHtmlInline
                , rawLaTeXInline'
                , escapedChar
                , exampleRef
                , smartPunctuation inline
                , charRef
                , symbol
                , ltSign ]

inlineNonLink :: GenParser Char ParserState Inline
inlineNonLink = (choice $
                 map (\parser -> try (parser >>= failIfLink)) inlineParsers)
                <?> "inline (non-link)"

failIfLink :: Inline -> GenParser tok st Inline
failIfLink (Link _ _) = pzero
failIfLink elt        = return elt

escapedChar :: GenParser Char ParserState Inline
escapedChar = try $ do
  char '\\'
  state <- getState
  result <- if stateStrict state 
               then oneOf "\\`*_{}[]()>#+-.!~"
               else satisfy (not . isAlphaNum)
  return $ case result of
                ' '   -> Str "\160" -- "\ " is a nonbreaking space
                '\n'  -> LineBreak  -- "\[newline]" is a linebreak
                _     -> Str [result]

ltSign :: GenParser Char ParserState Inline
ltSign = do
  st <- getState
  if stateStrict st
     then char '<'
     else notFollowedBy' rawHtmlBlocks >> char '<' -- unless it starts html
  return $ Str ['<']

exampleRef :: GenParser Char ParserState Inline
exampleRef = try $ do
  char '@'
  lab <- many1 (alphaNum <|> oneOf "-_")
  -- We just return a Str. These are replaced with numbers
  -- later. See the end of parseMarkdown.
  return $ Str $ '@' : lab

symbol :: GenParser Char ParserState Inline
symbol = do 
  result <- noneOf "<\\\n\t "
         <|> try (do lookAhead $ char '\\'
                     notFollowedBy' $ rawLaTeXEnvironment'
                                   <|> rawConTeXtEnvironment'
                     char '\\')
  return $ Str [result]

-- parses inline code, between n `s and n `s
code :: GenParser Char ParserState Inline
code = try $ do 
  starts <- many1 (char '`')
  skipSpaces
  result <- many1Till (many1 (noneOf "`\n") <|> many1 (char '`') <|>
                       (char '\n' >> notFollowedBy' blankline >> return " "))
                      (try (skipSpaces >> count (length starts) (char '`') >> 
                      notFollowedBy (char '`')))
  attr <- option ([],[],[]) (try $ optional whitespace >> attributes)
  return $ Code attr $ removeLeadingTrailingSpace $ concat result

mathWord :: GenParser Char st [Char]
mathWord = liftM concat $ many1 mathChunk

mathChunk :: GenParser Char st [Char]
mathChunk = do char '\\'
               c <- anyChar
               return ['\\',c]
        <|> many1 (satisfy $ \c -> not (isBlank c || c == '\\' || c == '$'))

math :: GenParser Char ParserState Inline
math =  (mathDisplay >>= applyMacros' >>= return . Math DisplayMath)
     <|> (mathInline >>= applyMacros' >>= return . Math InlineMath)

mathDisplay :: GenParser Char ParserState String 
mathDisplay = try $ do
  failIfStrict
  string "$$"
  many1Till (noneOf "\n" <|> (newline >>~ notFollowedBy' blankline)) (try $ string "$$")

mathInline :: GenParser Char ParserState String
mathInline = try $ do
  failIfStrict
  char '$'
  notFollowedBy space
  words' <- sepBy1 mathWord (many1 (spaceChar <|> (newline >>~ notFollowedBy' blankline)))
  char '$'
  notFollowedBy digit
  return $ intercalate " " words'

-- to avoid performance problems, treat 4 or more _ or * in a row as a literal
-- rather than attempting to parse for emph/strong
fours :: GenParser Char st Inline
fours = try $ do
  x <- char '*' <|> char '_'
  count 2 $ satisfy (==x)
  rest <- many1 (satisfy (==x))
  return $ Str (x:x:x:rest)

-- | Parses a list of inlines between start and end delimiters.
inlinesBetween :: (Show b)
               => GenParser Char ParserState a
               -> GenParser Char ParserState b
               -> GenParser Char ParserState [Inline]
inlinesBetween start end =
  normalizeSpaces `liftM` try (start >> many1Till inner end)
    where inner      = innerSpace <|> (notFollowedBy' whitespace >> inline)
          innerSpace = try $ whitespace >>~ notFollowedBy' end

emph :: GenParser Char ParserState Inline
emph = Emph `liftM`
  (inlinesBetween starStart starEnd <|> inlinesBetween ulStart ulEnd)
    where starStart = char '*' >> lookAhead nonspaceChar
          starEnd   = notFollowedBy' strong >> char '*'
          ulStart   = char '_' >> lookAhead nonspaceChar
          ulEnd     = notFollowedBy' strong >> char '_'

strong :: GenParser Char ParserState Inline
strong = Strong `liftM`
  (inlinesBetween starStart starEnd <|> inlinesBetween ulStart ulEnd)
    where starStart = string "**" >> lookAhead nonspaceChar
          starEnd   = try $ string "**"
          ulStart   = string "__" >> lookAhead nonspaceChar
          ulEnd     = try $ string "__"

strikeout :: GenParser Char ParserState Inline
strikeout = Strikeout `liftM`
 (failIfStrict >> inlinesBetween strikeStart strikeEnd)
    where strikeStart = string "~~" >> lookAhead nonspaceChar
                        >> notFollowedBy (char '~')
          strikeEnd   = try $ string "~~"

superscript :: GenParser Char ParserState Inline
superscript = failIfStrict >> enclosed (char '^') (char '^') 
              (notFollowedBy spaceChar >> inline) >>= -- may not contain Space
              return . Superscript

subscript :: GenParser Char ParserState Inline
subscript = failIfStrict >> enclosed (char '~') (char '~')
            (notFollowedBy spaceChar >> inline) >>=  -- may not contain Space
            return . Subscript 

whitespace :: GenParser Char ParserState Inline
whitespace = spaceChar >>
  (   (spaceChar >> skipMany spaceChar >> option Space (endline >> return LineBreak))
  <|> (skipMany spaceChar >> return Space) ) <?> "whitespace"

nonEndline :: GenParser Char st Char
nonEndline = satisfy (/='\n')

str :: GenParser Char ParserState Inline
str = do
  a <- alphaNum
  as <- many $ alphaNum <|> (try $ char '_' >>~ lookAhead alphaNum)
  let result = a:as
  state <- getState
  let spacesToNbr = map (\c -> if c == ' ' then '\160' else c)
  if stateSmart state
     then case likelyAbbrev result of
               []        -> return $ Str result
               xs        -> choice (map (\x ->
                               try (string x >> oneOf " \n" >>
                                    lookAhead alphaNum >>
                                    return (Str $ result ++ spacesToNbr x ++ "\160"))) xs)
                           <|> (return $ Str result)
     else return $ Str result

-- | if the string matches the beginning of an abbreviation (before
-- the first period, return strings that would finish the abbreviation.
likelyAbbrev :: String -> [String]
likelyAbbrev x =
  let abbrevs = [ "Mr.", "Mrs.", "Ms.", "Capt.", "Dr.", "Prof.",
                  "Gen.", "Gov.", "e.g.", "i.e.", "Sgt.", "St.",
                  "vol.", "vs.", "Sen.", "Rep.", "Pres.", "Hon.",
                  "Rev.", "Ph.D.", "M.D.", "M.A.", "p.", "pp.",
                  "ch.", "sec." ]
      abbrPairs = map (break (=='.')) abbrevs
  in  map snd $ filter (\(y,_) -> y == x) abbrPairs

-- an endline character that can be treated as a space, not a structural break
endline :: GenParser Char ParserState Inline
endline = try $ do
  newline
  notFollowedBy blankline
  st <- getState
  when (stateStrict st) $ do
    notFollowedBy emailBlockQuoteStart
    notFollowedBy (char '#')  -- atx header
  -- parse potential list-starts differently if in a list:
  when (stateParserContext st == ListItemState) $ do
     notFollowedBy' bulletListStart
     notFollowedBy' anyOrderedListStart
  return Space

--
-- links
--

-- a reference label for a link
reference :: GenParser Char ParserState [Inline]
reference = do notFollowedBy' (string "[^")   -- footnote reference
               result <- inlinesInBalancedBrackets inlineNonLink
               return $ normalizeSpaces result

-- source for a link, with optional title
source :: GenParser Char st (String, [Char])
source =
  (try $ charsInBalanced '(' ')' >>= parseFromString source') <|>
  -- the following is needed for cases like:  [ref](/url(a).
  (enclosed (char '(') (char ')') anyChar >>=
   parseFromString source')

-- auxiliary function for source
source' :: GenParser Char st (String, [Char])
source' = do
  skipSpaces
  let nl = char '\n' >>~ notFollowedBy blankline
  let sourceURL = liftM unwords $ many $ try $ do
                    notFollowedBy' linkTitle
                    skipMany spaceChar
                    optional nl
                    skipMany spaceChar
                    many1 (satisfy $ not . isBlank)
  let betweenAngles = try $ char '<' >>
                            manyTill (noneOf ">\n" <|> nl) (char '>')
  src <- try betweenAngles <|> sourceURL
  tit <- option "" linkTitle
  skipSpaces
  eof
  return (escapeURI $ removeTrailingSpace src, tit)

linkTitle :: GenParser Char st String
linkTitle = try $ do 
  (many1 spaceChar >> option '\n' newline) <|> newline
  skipSpaces
  delim <- oneOf "'\""
  tit <-   manyTill (optional (char '\\') >> anyChar)
                    (try (char delim >> skipSpaces >> eof))
  return $ decodeCharacterReferences tit

link :: GenParser Char ParserState Inline
link = try $ do
  lab <- reference
  (src, tit) <- source <|> referenceLink lab
  return $ Link lab (src, tit)

-- a link like [this][ref] or [this][] or [this]
referenceLink :: [Inline]
              -> GenParser Char ParserState (String, [Char])
referenceLink lab = do
  ref <- option [] (try (optional (char ' ') >> 
                         optional (newline >> skipSpaces) >> reference))
  let ref' = if null ref then lab else ref
  state <- getState
  case lookupKeySrc (stateKeys state) (toKey ref') of
     Nothing     -> fail "no corresponding key" 
     Just target -> return target 

autoLink :: GenParser Char ParserState Inline
autoLink = try $ do
  char '<'
  (orig, src) <- uri <|> emailAddress
  char '>'
  st <- getState
  return $ if stateStrict st
              then Link [Str orig] (src, "")
              else Link [Code ("",["url"],[]) orig] (src, "")

image :: GenParser Char ParserState Inline
image = try $ do
  char '!'
  (Link lab src) <- link
  return $ Image lab src

note :: GenParser Char ParserState Inline
note = try $ do
  failIfStrict
  ref <- noteMarker
  state <- getState
  let notes = stateNotes state
  case lookup ref notes of
    Nothing   -> fail "note not found"
    Just raw  -> do
       -- We temporarily empty the note list while parsing the note,
       -- so that we don't get infinite loops with notes inside notes...
       -- Note references inside other notes do not work.
       updateState $ \st -> st{ stateNotes = [] }
       contents <- parseFromString parseBlocks raw
       updateState $ \st -> st{ stateNotes = notes }
       return $ Note contents

inlineNote :: GenParser Char ParserState Inline
inlineNote = try $ do
  failIfStrict
  char '^'
  contents <- inlinesInBalancedBrackets inline
  return $ Note [Para contents]

rawLaTeXInline' :: GenParser Char ParserState Inline
rawLaTeXInline' = try $ do
  failIfStrict
  lookAhead $ char '\\'
  notFollowedBy' $ rawLaTeXEnvironment'
                <|> rawConTeXtEnvironment'
  RawInline _ s <- rawLaTeXInline
  return $ RawInline "tex" s  -- "tex" because it might be context or latex

rawConTeXtEnvironment' :: GenParser Char st String
rawConTeXtEnvironment' = try $ do
  string "\\start"
  completion <- inBrackets (letter <|> digit <|> spaceChar)
               <|> (many1 letter)
  contents <- manyTill (rawConTeXtEnvironment' <|> (count 1 anyChar))
                       (try $ string "\\stop" >> string completion)
  return $ "\\start" ++ completion ++ concat contents ++ "\\stop" ++ completion

inBrackets :: (GenParser Char st Char) -> GenParser Char st String
inBrackets parser = do
  char '['
  contents <- many parser
  char ']'
  return $ "[" ++ contents ++ "]"

rawHtmlInline :: GenParser Char ParserState Inline
rawHtmlInline = do
  st <- getState
  (_,result) <- if stateStrict st
                   then htmlTag (not . isTextTag)
                   else htmlTag isInlineTag
  return $ RawInline "html" result

-- Citations

cite :: GenParser Char ParserState Inline
cite = do
  failIfStrict
  citations <- textualCite <|> normalCite
  return $ Cite citations []

spnl :: GenParser Char st ()
spnl = try $ do
  skipSpaces
  optional newline
  skipSpaces
  notFollowedBy (char '\n')

textualCite :: GenParser Char ParserState [Citation]
textualCite = try $ do
  (_, key) <- citeKey
  let first = Citation{ citationId      = key
                      , citationPrefix  = []
                      , citationSuffix  = []
                      , citationMode    = AuthorInText
                      , citationNoteNum = 0
                      , citationHash    = 0
                      }
  rest <- option [] $ try $ spnl >> normalCite
  if null rest
     then option [first] $ bareloc first
     else return $ first : rest

bareloc :: Citation -> GenParser Char ParserState [Citation]
bareloc c = try $ do
  spnl
  char '['
  suff <- suffix
  rest <- option [] $ try $ char ';' >> citeList
  spnl
  char ']'
  return $ c{ citationSuffix = suff } : rest

normalCite :: GenParser Char ParserState [Citation]
normalCite = try $ do
  char '['
  spnl
  citations <- citeList
  spnl
  char ']'
  return citations

citeKey :: GenParser Char ParserState (Bool, String)
citeKey = try $ do
  suppress_author <- option False (char '-' >> return True)
  char '@'
  first <- letter
  rest <- many $ (noneOf ",;!?[]()@ \t\n")
  let key = first:rest
  st <- getState
  guard $ key `elem` stateCitations st
  return (suppress_author, key)

suffix :: GenParser Char ParserState [Inline]
suffix = try $ do
  spnl
  liftM normalizeSpaces $ many $ notFollowedBy (oneOf ";]") >> inline

prefix :: GenParser Char ParserState [Inline]
prefix = liftM normalizeSpaces $
  manyTill inline (char ']' <|> liftM (const ']') (lookAhead citeKey))

citeList :: GenParser Char ParserState [Citation]
citeList = sepBy1 citation (try $ char ';' >> spnl)

citation :: GenParser Char ParserState Citation
citation = try $ do
  pref <- prefix
  (suppress_author, key) <- citeKey
  suff <- suffix
  return $ Citation{ citationId        = key
                     , citationPrefix  = pref
                     , citationSuffix  = suff
                     , citationMode    = if suppress_author
                                            then SuppressAuthor
                                            else NormalCitation
                     , citationNoteNum = 0
                     , citationHash    = 0
                     }

