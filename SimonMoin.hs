{- 
Copyright (C) 2009 Simon Michael <simon@joyful.com>

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
   Module      : Text.Pandoc.Readers.MoinMoin
   Copyright   : Copyright (C) 2009-2011 Simon Michael, ranft, John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Partial conversion from MoinMoin-formatted text (plus some pandoc-isms
like smart punctuation) to Pandoc. Based on the Markdown reader. 

TODO:
[ ] subscript: ,,sub,,
[ ] strikeout: --(stroke)--
[ ] larger: ~+larger+~ [ignore]
[ ] smaller: +-smaller-+ [ignore]
[ ] table of contents: <<TableOfContents()>> or <<TableOfContents(2)>>
[ ] moin 1.6 double bracket links: [[FrontPage]], [[FrontPage|named link]],
  [[#anchorname]], [[#anchorname|description]], [[PageName#anchorname]],
  [[PageName#anchorname|description]], [[attachment:filename.txt]]
[ ] HelpOnEditing/SubPages should be a single link
[ ] /SubPage should be a link
[ ] Wiki''''''Name should result in plain string WikiName
[ ] Same with !WikiName
[ ] WikiName''''''s - the s should not be in the link
[ ] WikiName``s - the s should not be in the link
[ ] {{http://static.moinmo.in/logos/moinmoin.png}} should be an image
[ ] camel-case links assume ascii letters
[ ] indented blockquotes
[ ] definition lists
[ ] nested/multiply-indented lists, blocks, code blocks
[ ] tables
[ ] images
[ ] <<Anchor(anchorname)>> inserts a link anchor
[ ] <<BR>> a hard break
[ ] <<FootNote(Note)>> a note
[ ] <<Include(HelpOnMagros)>> - just ignore this
[ ] <<MailTo(user AT example DOT com)>> treat as email link; pandoc has obfuscation
  options
[ ] smileys and icons - use unicode char or just parse literal text
[ ] wiki parser with css classes:
  {{{#!wiki red/solid
  blah blah
  }}}
[ ] admonitions
  {{{#!wiki caution
  '''Don't overuse'''

  blah blah
  }}}
[ ] comments
  {{{#!wiki comment/dotted
  '''Don't overuse'''

  blah blah
  }}}
[ ] test suite - best approach would be src/Tests/Readers/MoinMoin.hs,
    see other reader tests for how to do this.
    or, old style:  tests/moinmoin-reader.native tests/moinmoin-reader.moinmoin


cf:
http://johnmacfarlane.net/pandoc/doc/pandoc/index.html
http://moinmo.in/HelpOnFormatting
http://moinmo.in/HelpOnMoinWikiSyntax

-}

module Text.Pandoc.Readers.MoinMoin ( readMoinMoin ) where
import Control.Monad ( when )
import Data.Char ( isUpper )
import Text.Pandoc.Definition
import Text.Pandoc.Parsing
import Text.Pandoc.Shared 
import Text.ParserCombinators.Parsec hiding ( label )

-- | Parse MoinMoin string and return Pandoc document.
readMoinMoin :: ParserState -> String -> Pandoc
readMoinMoin state s = readWith parseMoinMoin state (s ++ "\n\n")

--
-- Constants and data structure definitions
--

spaceChars :: [Char]
spaceChars = " \t"

bulletListMarkers :: [Char]
bulletListMarkers = ".*"

hruleChars :: [Char]
hruleChars = "-"

-- treat these as potentially non-text when parsing inline:
specialChars :: [Char]
specialChars = "\\[]*_~`<>$!^-.&'\"\8216\8217\8220\8221"

--
-- document structure
--

parseMoinMoin :: GenParser Char ParserState Pandoc
parseMoinMoin = do
  processingInstructions
  blocks <- parseBlocks 
  return $ Pandoc (Meta [] [] [] {-title author date-}) $ filter (/= Null) blocks

processingInstructions :: GenParser Char a ()
processingInstructions = many (char '#' >> manyTill anyChar newline) >> return ()

comment :: GenParser Char a ()
comment = try $ do
  pos <- getPosition
  when (sourceColumn pos /= 0) $ fail ""
  string "##"
  manyTill anyChar newline  
  return ()

--
-- parsing blocks
--

parseBlocks :: GenParser Char ParserState [Block]
parseBlocks = manyTill block eof

block :: GenParser Char ParserState Block
block = do
  choice ([ header
          , codeBlock
          , codeBlockIndented
          -- , blockQuote
          , hrule
          , bulletList
          , orderedList
          , para
          , plain
          , nullBlock
          ]) <?> "block"

--
-- header blocks
--

header :: GenParser Char ParserState Block
header = try $ do
  level <- many1 (char '=') >>= return . length
  skipSpaces
  text <- manyTill inline headerEnd >>= return . normalizeSpaces
  return (Header level text) <?> "header"

headerEnd :: GenParser Char st [Char]
headerEnd = try $ skipSpaces >> skipMany (char '=') >> blanklines

--
-- hrule block
--

hrule :: GenParser Char st Block
hrule = try $ do
  skipSpaces
  start <- oneOf hruleChars
  count 3 (char start)
  skipMany (char start)
  skipSpaces
  newline
  optional blanklines
  return HorizontalRule

--
-- code blocks
--

codeBlockStart :: GenParser Char st [String] 
codeBlockStart = try $ do
  string "{{{"
  classes <- option [] codeBlockClasses
  optional newline
  return classes 

codeBlockClasses :: GenParser Char st [String]
codeBlockClasses = try $ do
  string "#!"
  skipMany spaceChar
  sepEndBy (many1 alphaNum) (many1 spaceChar)

codeBlockEnd :: GenParser Char st ()
codeBlockEnd = try $ string "}}}" >> skipSpaces >> optional newline >> return ()

codeBlock :: GenParser Char st Block
codeBlock = try $ do
  classes <- codeBlockStart
  contents <- manyTill anyChar codeBlockEnd
  return $ CodeBlock ("",classes,[]) contents

codeInline :: GenParser Char st Inline
codeInline = try $ do
  codeBlockStart'
  contents <- manyTill anyChar codeBlockEnd'
  return $ Code nullAttr contents
      where
        codeBlockStart' = string "{{{" >> return ()
        codeBlockEnd' = try $ string "}}}" >> return ()


codeBlockIndented :: GenParser Char ParserState Block
codeBlockIndented = try $ do
  many1 whitespace >> codeBlockStart
  contents <- manyTill anyChar codeBlockEnd
  return $ BlockQuote [CodeBlock ([],[],[]) contents]

--
-- list blocks
--

-- these are just the markdown list parsers

bulletListStart :: GenParser Char ParserState ()
bulletListStart = try $ do
  optional newline -- if preceded by a Plain block in a list context
  nonindentSpaces
  notFollowedBy' hrule     -- because hrules start out just like lists
  oneOf bulletListMarkers
  spaceChar
  skipSpaces

anyOrderedListStart :: GenParser Char ParserState (Int, ListNumberStyle, ListNumberDelim) 
anyOrderedListStart = try $ do
  optional newline -- if preceded by a Plain block in a list context
  nonindentSpaces
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
                then char '\t' <|> (char ' ' >>~ notFollowedBy (satisfy isUpper))
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
  notFollowedBy' header
  line <- manyTill anyChar newline
  return $ line ++ "\n"

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

--
-- paragraph block
--

para :: GenParser Char ParserState Block
para = try $ do 
  result <- many1 inline
  newline
  blanklines <|> do lookAhead ((codeBlockStart >> return "") <|> {- blockQuote <|> -} (header >> return ""))
  return $ Para $ normalizeSpaces result

plain :: GenParser Char ParserState Block
plain = many1 inline >>~ spaces >>= return . Plain . normalizeSpaces

-- 
-- inline
--

inline :: GenParser Char ParserState Inline
inline = choice inlineParsers <?> "inline"

inlineParsers :: [GenParser Char ParserState Inline]
inlineParsers = [
                  link
                , codeInline
                , str
                , smartPunctuation inline
                , whitespace
                , endline
                , code
                , charRef
                , strong
                , emph
                , strikeout
                , superscript
                , subscript
--                 , escapedChar
                , symbol
                ]

symbol :: GenParser Char ParserState Inline
symbol = do 
  result <- oneOf specialChars
  return $ Str [result]

-- parses inline code, between n `s and n `s
code :: GenParser Char ParserState Inline
code = try $ do 
  starts <- many1 (char '`')
  skipSpaces
  result <- many1Till (many1 (noneOf "`\n") <|> many1 (char '`') <|>
                       (char '\n' >> return " ")) 
                      (try (skipSpaces >> count (length starts) (char '`') >> 
                      notFollowedBy (char '`')))
  return $ Code nullAttr $ removeLeadingTrailingSpace $ concat result

emph :: GenParser Char ParserState Inline
emph = (enclosed (string "''") (string "''") inline) >>= return . Emph . normalizeSpaces

strong :: GenParser Char ParserState Inline
strong = enclosed (string "'''") (string "'''") inline >>= return . Strong . normalizeSpaces
         

strikeout :: GenParser Char ParserState Inline
strikeout = failIfStrict >> enclosed (string "--(") (try $ string ")--") inline >>=
            return . Strikeout . normalizeSpaces

superscript :: GenParser Char ParserState Inline
superscript = failIfStrict >> enclosed (char '^') (char '^') 
              (notFollowedBy' whitespace >> inline) >>= -- may not contain Space
              return . Superscript

subscript :: GenParser Char ParserState Inline
subscript = failIfStrict >> enclosed (string ",,") (string ",,")
            (notFollowedBy' whitespace >> inline) >>=  -- may not contain Space
            return . Subscript 

whitespace :: GenParser Char ParserState Inline
whitespace = do
  sps <- many1 (oneOf spaceChars)
  if length sps >= 2
     then option Space (endline >> return LineBreak)
     else return Space <?> "whitespace"

strChar :: GenParser Char st Char
strChar = noneOf (specialChars ++ spaceChars ++ "\n")

str :: GenParser Char st Inline
str = notFollowedBy' comment >> many1 strChar >>= return . Str

-- an endline character that can be treated as a space, not a structural break
endline :: GenParser Char ParserState Inline
endline = try $ do
  newline
  notFollowedBy blankline
  notFollowedBy' codeBlockStart
  notFollowedBy' listStart
  notFollowedBy (char '=')
  -- st <- getState
  -- if stateStrict st 
  --   then do notFollowedBy (char '=')  -- header
  --   else return () 
  -- parse potential list-starts differently if in a list:
  -- if stateParserContext st == ListItemState
  --    then notFollowedBy' (bulletListStart <|> 
  --                         (anyOrderedListStart >> return ()))
  --    else return ()
  return Space

--
-- links
--

-- inlineNonLink :: GenParser Char ParserState Inline
-- inlineNonLink = (choice $
--                  map (\parser -> try (parser >>= failIfLink)) inlineParsers)
--                 <?> "inline (non-link)"

-- failIfLink :: Inline -> GenParser tok st Inline
-- failIfLink (Link _ _) = pzero
-- failIfLink elt        = return elt

-- -- a reference label for a link
-- reference :: GenParser Char ParserState [Inline]
-- reference = do notFollowedBy' (string "[^")   -- footnote reference
--                result <- inlinesInBalancedBrackets inlineNonLink
--                return $ normalizeSpaces result

-- -- source for a link, with optional title
-- source :: GenParser Char st (String, [Char])
-- source =
--   (try $ charsInBalanced '(' ')' >>= parseFromString source') <|>
--   -- the following is needed for cases like:  [ref](/url(a).
--   (enclosed (char '(') (char ')') anyChar >>=
--    parseFromString source')

-- -- auxiliary function for source
-- source' :: GenParser Char st (String, [Char])
-- source' = do
--   skipSpaces
--   let sourceURL excludes = many $
--         optional (char '\\') >> (noneOf (' ':excludes) <|> (notFollowedBy' linkTitle >> char ' '))
--   src <- try (char '<' >> sourceURL ">\t\n" >>~ char '>') <|> sourceURL "\t\n"
--   tit <- option "" linkTitle
--   skipSpaces
--   eof
--   return (intercalate "%20" $ words $ removeTrailingSpace src, tit)

-- linkTitle :: GenParser Char st String
-- linkTitle = try $ do 
--   (many1 spaceChar >> option '\n' newline) <|> newline
--   skipSpaces
--   delim <- oneOf "'\""
--   tit <-   manyTill (optional (char '\\') >> anyChar)
--                     (try (char delim >> skipSpaces >> eof))
--   return $ decodeCharacterReferences tit

link :: GenParser Char ParserState Inline
link = choice [uriLink
              ,emailAddressLink
              ,localPageCamelCaseLink
              ,moin15BracketLink
              ,moin16BracketLink
              ]

uriLink :: GenParser Char ParserState Inline
uriLink = try $ do
  (u, uri_escaped) <- uri
  return $ Link [Code nullAttr u] (uri_escaped, "")

emailAddressLink :: GenParser Char ParserState Inline
emailAddressLink = try $ do
  (e, escaped_mailto_uri) <- emailAddress
  return $ Link [Str e] (escaped_mailto_uri, "")

localPageCamelCaseLink :: GenParser Char ParserState Inline
localPageCamelCaseLink = try $ do
  (p,_) <- localPageCamelCase
  return $ Link [Str p] (p, "")

moin15BracketLink :: GenParser Char ParserState Inline
moin15BracketLink = try $ do
  (target,label) <- singleBracketed $ choice [
                                     uriSpaceLabel
                                    ,uriNoLabel
                                    ,localPageInQuotes
                                    ,localPageWithColonLabel
                                    ,localPageCamelCase
                                    ]
  return $ Link [Str label] (target, "")

moin16BracketLink :: GenParser Char ParserState Inline
moin16BracketLink = try $ do
  (target,label) <- doubleBracketed $ choice [
                                     uriPipeLabel
                                    ,uriNoLabel
                                    -- ,localPageInQuotes
                                    -- ,localPageWithColonLabel
                                    -- ,localPageCamelCase
                                    ]
  return $ Link [Str label] (target, "")

uriSpaceLabel :: GenParser Char ParserState (String,String)
uriSpaceLabel = try $ do
  (_, uri_escaped) <- uri
  many1 space
  label <- many1 $ noneOf "]"
  return (uri_escaped, label)

uriPipeLabel :: GenParser Char ParserState (String,String)
uriPipeLabel = try $ do
  (_, uri_escaped) <- uri
  char '|'
  label <- many1 $ noneOf "]"
  return (uri_escaped, label)

uriNoLabel :: GenParser Char ParserState (String,String)
uriNoLabel = try $ do
  skipSpaces
  s <- many1 $ noneOf "]"
  skipSpaces
  -- work around uri failing when there is a trailing ]
  state <- getState
  either (const $ fail "") (const $ return (s,s)) $ runParser uri state "" s

localPageInQuotes :: GenParser Char ParserState (String,String)
localPageInQuotes = try $ do
  char '"'
  p <- many1 $ noneOf "\""
  char '"'
  return (p,p)

-- I didn't find this in any moin docs, but it's in the darcs wiki
-- and moin was handling it
localPageWithColonLabel :: GenParser Char ParserState (String,String)
localPageWithColonLabel = try $ do
  char ':'
  p <- many1 $ noneOf ":"
  char ':'
  label <- many1 $ noneOf "]"
  return (p,label)

localPageCamelCase :: GenParser Char ParserState (String,String)
localPageCamelCase = try $ do
  w <- initialCapWord
  ws <- many1 initialCapWord
  let p = concat $ [w]++ws
  return (p,p)

initialCapWord :: GenParser Char ParserState String
initialCapWord = try $ do
  c <- upperChar
  cs <- many1 lowerChar
  return $ [c]++cs

upperChar, lowerChar :: GenParser Char ParserState Char
upperChar = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
lowerChar = oneOf "abcdefghijklmnopqrstuvwxyz"

-- image :: GenParser Char ParserState Inline
-- image = try $ do
--   char '!'
--   (Link lab src) <- link
--   return $ Image lab src

singleBracketed :: (GenParser Char st a) -> GenParser Char st a
singleBracketed parser = do
  string "["
  contents <- parser
  string "]"
  return contents

doubleBracketed :: (GenParser Char st a) -> GenParser Char st a
doubleBracketed parser = do
  string "[["
  contents <- parser
  string "]]"
  return contents

--
-- auxiliary functions
--

indentSpaces :: GenParser Char ParserState [Char]
indentSpaces = try $ do
  state <- getState
  let tabStop = stateTabStop state
  try (count tabStop (char ' ')) <|> 
    (many (char ' ') >> string "\t") <?> "indentation"

nonindentSpaces :: GenParser Char ParserState [Char]
nonindentSpaces = do
  state <- getState
  let tabStop = stateTabStop state
  sps <- many (char ' ')
  if length sps < tabStop 
     then return sps
     else unexpected "indented line"

