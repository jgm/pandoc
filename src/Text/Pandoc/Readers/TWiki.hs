{-# LANGUAGE RelaxedPolyRec, FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
-- RelaxedPolyRec needed for inlinesBetween on GHC < 7
{-
  Copyright (C) 2014 Alexander Sulfrian <alexander.sulfrian@fu-berlin.de>

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
   Module      : Text.Pandoc.Readers.TWiki
   Copyright   : Copyright (C) 2014 Alexander Sulfrian
   License     : GNU GPL, version 2 or above

   Maintainer  : Alexander Sulfrian <alexander.sulfrian@fu-berlin.de>
   Stability   : alpha
   Portability : portable

Conversion of twiki text to 'Pandoc' document.
-}
module Text.Pandoc.Readers.TWiki ( readTWiki
                                 , readTWikiWithWarnings
                                 ) where

import Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (enclosed, macro, nested)
import Text.Pandoc.Readers.HTML (htmlTag, isCommentTag)
import Control.Monad
import Text.Printf (printf)
import Debug.Trace (trace)
import Text.Pandoc.XML (fromEntities)
import Data.Maybe (fromMaybe)
import Text.HTML.TagSoup
import Data.Char (isAlphaNum)
import qualified Data.Foldable as F
import Text.Pandoc.Error

-- | Read twiki from an input string and return a Pandoc document.
readTWiki :: ReaderOptions -- ^ Reader options
          -> String        -- ^ String to parse (assuming @'\n'@ line endings)
          -> Either PandocError Pandoc
readTWiki opts s =
  (readWith parseTWiki) def{ stateOptions = opts } (s ++ "\n\n")

readTWikiWithWarnings :: ReaderOptions -- ^ Reader options
                      -> String        -- ^ String to parse (assuming @'\n'@ line endings)
                      -> Either PandocError (Pandoc, [String])
readTWikiWithWarnings opts s =
  (readWith parseTWikiWithWarnings) def{ stateOptions = opts } (s ++ "\n\n")
 where parseTWikiWithWarnings = do
         doc <- parseTWiki
         warnings <- stateWarnings <$> getState
         return (doc, warnings)

type TWParser = Parser [Char] ParserState

--
-- utility functions
--

tryMsg :: String -> TWParser a -> TWParser a
tryMsg msg p = try p <?> msg

skip :: TWParser a -> TWParser ()
skip parser = parser >> return ()

nested :: TWParser a -> TWParser a
nested p = do
  nestlevel <- stateMaxNestingLevel <$>  getState
  guard $ nestlevel > 0
  updateState $ \st -> st{ stateMaxNestingLevel = stateMaxNestingLevel st - 1 }
  res <- p
  updateState $ \st -> st{ stateMaxNestingLevel = nestlevel }
  return res

htmlElement :: String -> TWParser (Attr, String)
htmlElement tag = tryMsg tag $ do
  (TagOpen _ attr, _) <- htmlTag (~== TagOpen tag [])
  content <- manyTill anyChar (endtag <|> endofinput)
  return (htmlAttrToPandoc attr, trim content)
  where
    endtag     = skip $ htmlTag (~== TagClose tag)
    endofinput = lookAhead $ try $ skipMany blankline >> skipSpaces >> eof
    trim       = dropWhile (=='\n') . reverse . dropWhile (=='\n') . reverse

htmlAttrToPandoc :: [Attribute String] -> Attr
htmlAttrToPandoc attrs = (ident, classes, keyvals)
  where
    ident   = fromMaybe "" $ lookup "id" attrs
    classes = maybe [] words $ lookup "class" attrs
    keyvals = [(k,v) | (k,v) <- attrs, k /= "id" && k /= "class"]

parseHtmlContentWithAttrs :: String -> TWParser a -> TWParser (Attr, [a])
parseHtmlContentWithAttrs tag parser = do
  (attr, content) <- htmlElement tag
  parsedContent <- try $ parseContent content
  return (attr, parsedContent)
  where
    parseContent = parseFromString $ nested $ manyTill parser endOfContent
    endOfContent = try $ skipMany blankline >> skipSpaces >> eof

parseHtmlContent :: String -> TWParser a -> TWParser [a]
parseHtmlContent tag p = parseHtmlContentWithAttrs tag p >>= return . snd

--
-- main parser
--

parseTWiki :: TWParser Pandoc
parseTWiki = do
  bs <- mconcat <$> many block
  spaces
  eof
  return $ B.doc bs


--
-- block parsers
--

block :: TWParser B.Blocks
block = do
  tr <- getOption readerTrace
  pos <- getPosition
  res <- mempty <$ skipMany1 blankline
         <|> blockElements
         <|> para
  skipMany blankline
  when tr $
    trace (printf "line %d: %s" (sourceLine pos)
           (take 60 $ show $ B.toList res)) (return ())
  return res

blockElements :: TWParser B.Blocks
blockElements = choice [ separator
                       , header
                       , verbatim
                       , literal
                       , list ""
                       , table
                       , blockQuote
                       , noautolink
                       ]

separator :: TWParser B.Blocks
separator = tryMsg "separator" $ string "---" >> newline >> return B.horizontalRule

header :: TWParser B.Blocks
header = tryMsg "header" $ do
  string "---"
  level <- many1 (char '+') >>= return . length
  guard $ level <= 6
  classes <- option [] $ string "!!" >> return ["unnumbered"]
  skipSpaces
  content <- B.trimInlines . mconcat <$> manyTill inline newline
  attr <- registerHeader ("", classes, []) content
  return $ B.headerWith attr level $ content

verbatim :: TWParser B.Blocks
verbatim = (htmlElement "verbatim" <|> htmlElement "pre")
           >>= return . (uncurry B.codeBlockWith)

literal :: TWParser B.Blocks
literal = htmlElement "literal" >>= return . rawBlock
  where
    format (_, _, kvs)        = fromMaybe "html" $ lookup "format" kvs
    rawBlock (attrs, content) = B.rawBlock (format attrs) content

list :: String -> TWParser B.Blocks
list prefix = choice [ bulletList prefix
                     , orderedList prefix
                     , definitionList prefix]

definitionList :: String -> TWParser B.Blocks
definitionList prefix = tryMsg "definitionList" $ do
  indent <- lookAhead $ string prefix *> (many1 $ string "   ") <* string "$ "
  elements <- many $ parseDefinitionListItem (prefix ++ concat indent)
  return $ B.definitionList elements
  where
    parseDefinitionListItem :: String -> TWParser (B.Inlines, [B.Blocks])
    parseDefinitionListItem indent = do
      string (indent ++ "$ ") >> skipSpaces
      term <- many1Till inline $ string ": "
      line <- listItemLine indent $ string "$ "
      return $ (mconcat term, [line])

bulletList :: String -> TWParser B.Blocks
bulletList prefix = tryMsg "bulletList" $
                    parseList prefix (char '*') (char ' ')

orderedList :: String -> TWParser B.Blocks
orderedList prefix = tryMsg "orderedList" $
                     parseList prefix (oneOf "1iIaA") (string ". ")

parseList :: Show a => String -> TWParser Char -> TWParser a -> TWParser B.Blocks
parseList prefix marker delim = do
  (indent, style) <- lookAhead $ string prefix *> listStyle <* delim
  blocks <- many $ parseListItem (prefix ++ indent) (char style <* delim)
  return $ case style of
    '1' -> B.orderedListWith (1, DefaultStyle, DefaultDelim) blocks
    'i' -> B.orderedListWith (1, LowerRoman, DefaultDelim) blocks
    'I' -> B.orderedListWith (1, UpperRoman, DefaultDelim) blocks
    'a' -> B.orderedListWith (1, LowerAlpha, DefaultDelim) blocks
    'A' -> B.orderedListWith (1, UpperAlpha, DefaultDelim) blocks
    _   -> B.bulletList blocks
  where
    listStyle = do
      indent <- many1 $ string "   "
      style <- marker
      return (concat indent, style)

parseListItem :: Show a => String -> TWParser a -> TWParser B.Blocks
parseListItem prefix marker = string prefix >> marker >> listItemLine prefix marker

listItemLine :: Show a => String -> TWParser a -> TWParser B.Blocks
listItemLine prefix marker = lineContent >>= parseContent >>= return . mconcat
  where
    lineContent = do
      content <- anyLine
      continuation <- optionMaybe listContinuation
      return $ filterSpaces content ++ "\n" ++ (maybe "" ("   " ++) continuation)
    filterSpaces = reverse . dropWhile (== ' ') . reverse
    listContinuation = notFollowedBy (string prefix >> marker) >>
                       string "   " >> lineContent
    parseContent = parseFromString $ many1 $ nestedList <|> parseInline
    parseInline = many1Till inline (lastNewline <|> newlineBeforeNestedList) >>=
                  return . B.plain . mconcat
    nestedList = list prefix
    lastNewline = try $ char '\n' <* eof
    newlineBeforeNestedList = try $ char '\n' <* lookAhead nestedList

table :: TWParser B.Blocks
table = try $ do
  tableHead <- optionMaybe $ many1Till tableParseHeader newline >>= return . unzip
  rows <- many1 tableParseRow
  return $ buildTable mempty rows $ fromMaybe (align rows, columns rows) tableHead
  where
    buildTable caption rows (aligns, heads)
                    = B.table caption aligns heads rows
    align rows      = replicate (columCount rows) (AlignDefault, 0)
    columns rows    = replicate (columCount rows) mempty
    columCount rows = length $ head rows

tableParseHeader :: TWParser ((Alignment, Double), B.Blocks)
tableParseHeader = try $ do
  char '|'
  leftSpaces <- many spaceChar >>= return . length
  char '*'
  content <- tableColumnContent (char '*' >> skipSpaces >> char '|')
  char '*'
  rightSpaces <- many spaceChar >>= return . length
  optional tableEndOfRow
  return (tableAlign leftSpaces rightSpaces, content)
  where
    tableAlign left right
      | left >= 2 && left == right = (AlignCenter, 0)
      | left > right = (AlignRight, 0)
      | otherwise = (AlignLeft, 0)

tableParseRow :: TWParser [B.Blocks]
tableParseRow = many1Till tableParseColumn newline

tableParseColumn :: TWParser B.Blocks
tableParseColumn = char '|' *> skipSpaces *>
                   tableColumnContent (skipSpaces >> char '|')
                   <* skipSpaces <* optional tableEndOfRow

tableEndOfRow :: TWParser Char
tableEndOfRow = lookAhead (try $ char '|' >> char '\n') >> char '|'

tableColumnContent :: Show a => TWParser a -> TWParser B.Blocks
tableColumnContent end = manyTill content (lookAhead $ try end) >>= return . B.plain . mconcat
  where
    content = continuation <|> inline
    continuation = try $ char '\\' >> newline >> return mempty

blockQuote :: TWParser B.Blocks
blockQuote = parseHtmlContent "blockquote" block >>= return . B.blockQuote . mconcat

noautolink :: TWParser B.Blocks
noautolink = do
  (_, content) <- htmlElement "noautolink"
  st <- getState
  setState $ st{ stateAllowLinks = False }
  blocks <- try $ parseContent content
  setState $ st{ stateAllowLinks = True }
  return $ mconcat blocks
  where
    parseContent      = parseFromString $ many $ block

para :: TWParser B.Blocks
para = many1Till inline endOfParaElement >>= return . result . mconcat
 where
   endOfParaElement = lookAhead $ endOfInput <|> endOfPara <|> newBlockElement
   endOfInput       = try $ skipMany blankline >> skipSpaces >> eof
   endOfPara        = try $ blankline >> skipMany1 blankline
   newBlockElement  = try $ blankline >> skip blockElements
   result content   = if F.all (==Space) content
                      then mempty
                      else B.para $ B.trimInlines content


--
-- inline parsers
--

inline :: TWParser B.Inlines
inline = choice [ whitespace
                , br
                , macro
                , strong
                , strongHtml
                , strongAndEmph
                , emph
                , emphHtml
                , boldCode
                , smart
                , link
                , htmlComment
                , code
                , codeHtml
                , nop
                , autoLink
                , str
                , symbol
                ] <?> "inline"

whitespace :: TWParser B.Inlines
whitespace = (lb <|> regsp) >>= return
  where lb = try $ skipMany spaceChar >> linebreak >> return B.space
        regsp = try $ skipMany1 spaceChar >> return B.space

br :: TWParser B.Inlines
br = try $ string "%BR%" >> return B.linebreak

linebreak :: TWParser B.Inlines
linebreak = newline >> notFollowedBy newline >> (lastNewline <|> innerNewline)
  where lastNewline  = eof >> return mempty
        innerNewline = return B.space

between :: (Show b, Monoid c) => TWParser a -> TWParser b -> (TWParser b -> TWParser c) -> TWParser c
between start end p =
  mconcat <$> try (start >> notFollowedBy whitespace >> many1Till (p end) end)

enclosed :: (Show a, Monoid b) => TWParser a -> (TWParser a -> TWParser b) -> TWParser b
enclosed sep p = between sep (try $ sep <* endMarker) p
  where
    endMarker   = lookAhead $ skip endSpace <|> skip (oneOf ".,!?:)|") <|> eof
    endSpace    = (spaceChar <|> newline) >> return B.space

macro :: TWParser B.Inlines
macro = macroWithParameters <|> withoutParameters
  where
    withoutParameters = enclosed (char '%') (\_ -> macroName) >>= return . emptySpan
    emptySpan name = buildSpan name [] mempty

macroWithParameters :: TWParser B.Inlines
macroWithParameters = try $ do
  char '%'
  name <- macroName
  (content, kvs) <- attributes
  char '%'
  return $ buildSpan name kvs $ B.str content

buildSpan :: String -> [(String, String)] -> B.Inlines -> B.Inlines
buildSpan className kvs = B.spanWith attrs
  where
    attrs             = ("", ["twiki-macro", className] ++ additionalClasses, kvsWithoutClasses)
    additionalClasses = maybe [] words $ lookup "class" kvs
    kvsWithoutClasses = [(k,v) | (k,v) <- kvs, k /= "class"]

macroName :: TWParser String
macroName = do
  first <- letter
  rest <- many $ alphaNum <|> char '_'
  return (first:rest)

attributes :: TWParser (String, [(String, String)])
attributes = char '{' *> spnl *> many (attribute <* spnl) <* char '}' >>=
             return . foldr (either mkContent mkKvs) ([], [])
  where
    spnl                      = skipMany (spaceChar <|> newline)
    mkContent c  ([], kvs)    = (c, kvs)
    mkContent c  (rest, kvs)  = (c ++ " " ++ rest, kvs)
    mkKvs     kv (cont, rest) = (cont, (kv : rest))

attribute :: TWParser (Either String (String, String))
attribute = withKey <|> withoutKey
  where
    withKey = try $ do
      key <- macroName
      char '='
      parseValue False >>= return . (curry Right key)
    withoutKey = try $ parseValue True >>= return . Left
    parseValue allowSpaces = (withQuotes <|> withoutQuotes allowSpaces) >>= return . fromEntities
    withQuotes             = between (char '"') (char '"') (\_ -> count 1 $ noneOf ['"'])
    withoutQuotes allowSpaces
      | allowSpaces == True = many1 $ noneOf "}"
      | otherwise           = many1 $ noneOf " }"

nestedInlines :: Show a => TWParser a -> TWParser B.Inlines
nestedInlines end = innerSpace <|> nestedInline
  where
    innerSpace   = try $ whitespace <* (notFollowedBy end)
    nestedInline = notFollowedBy whitespace >> nested inline

strong :: TWParser B.Inlines
strong = try $ enclosed (char '*') nestedInlines >>= return . B.strong

strongHtml :: TWParser B.Inlines
strongHtml = (parseHtmlContent "strong" inline <|> parseHtmlContent "b" inline)
             >>= return . B.strong . mconcat

strongAndEmph :: TWParser B.Inlines
strongAndEmph = try $ enclosed (string "__") nestedInlines >>= return . B.emph . B.strong

emph :: TWParser B.Inlines
emph = try $ enclosed (char '_') nestedInlines >>= return . B.emph

emphHtml :: TWParser B.Inlines
emphHtml = (parseHtmlContent "em" inline <|> parseHtmlContent "i" inline)
           >>= return . B.emph . mconcat

nestedString :: Show a => TWParser a -> TWParser String
nestedString end = innerSpace <|> (count 1 nonspaceChar)
  where
    innerSpace = try $ many1 spaceChar <* notFollowedBy end

boldCode :: TWParser B.Inlines
boldCode = try $ enclosed (string "==") nestedString >>= return . B.strong . B.code . fromEntities

htmlComment :: TWParser B.Inlines
htmlComment = htmlTag isCommentTag >> return mempty

code :: TWParser B.Inlines
code = try $ enclosed (char '=') nestedString >>= return . B.code . fromEntities

codeHtml :: TWParser B.Inlines
codeHtml = do
  (attrs, content) <- parseHtmlContentWithAttrs "code" anyChar
  return $ B.codeWith attrs $ fromEntities content

autoLink :: TWParser B.Inlines
autoLink = try $ do
  state <- getState
  guard $ stateAllowLinks state
  (text, url) <- parseLink
  guard $ checkLink (head $ reverse url)
  return $ makeLink (text, url)
  where
    parseLink            = notFollowedBy nop >> (uri <|> emailAddress)
    makeLink (text, url) = B.link url "" $ B.str text
    checkLink c
      | c == '/' = True
      | otherwise = isAlphaNum c

str :: TWParser B.Inlines
str = (many1 alphaNum <|> count 1 characterReference) >>= return . B.str

nop :: TWParser B.Inlines
nop = try $ (skip exclamation <|> skip nopTag) >> followContent
  where
    exclamation   = char '!'
    nopTag        = stringAnyCase "<nop>"
    followContent = many1 nonspaceChar >>= return . B.str . fromEntities

symbol :: TWParser B.Inlines
symbol = count 1 nonspaceChar >>= return . B.str

smart :: TWParser B.Inlines
smart = do
  getOption readerSmart >>= guard
  doubleQuoted <|> singleQuoted <|>
    choice [ apostrophe
           , dash
           , ellipses
           ]

singleQuoted :: TWParser B.Inlines
singleQuoted = try $ do
  singleQuoteStart
  withQuoteContext InSingleQuote $
    many1Till inline singleQuoteEnd >>=
    (return . B.singleQuoted . B.trimInlines . mconcat)

doubleQuoted :: TWParser B.Inlines
doubleQuoted = try $ do
  doubleQuoteStart
  contents <- mconcat <$> many (try $ notFollowedBy doubleQuoteEnd >> inline)
  (withQuoteContext InDoubleQuote $ doubleQuoteEnd >>
   return (B.doubleQuoted $ B.trimInlines contents))
   <|> (return $ (B.str "\8220") B.<> contents)

link :: TWParser B.Inlines
link = try $ do
  st <- getState
  guard $ stateAllowLinks st
  setState $ st{ stateAllowLinks = False }
  (url, title, content) <- linkText
  setState $ st{ stateAllowLinks = True }
  return $ B.link url title content

linkText :: TWParser (String, String, B.Inlines)
linkText = do
  string "[["
  url <- many1Till anyChar (char ']')
  content <- option [B.str url] linkContent
  char ']'
  return (url, "", mconcat content)
  where
    linkContent      = (char '[') >> many1Till anyChar (char ']') >>= parseLinkContent
    parseLinkContent = parseFromString $ many1 inline
