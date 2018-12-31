{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-
  Copyright (C) 2018 Alexander Krotov <ilabdsf@gmail.com>

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
   Module      : Text.Pandoc.Readers.DokuWiki
   Copyright   : Copyright (C) 2018 Alexander Krotov
   License     : GNU GPL, version 2 or above

   Maintainer  : Alexander Krotov <ilabdsf@gmail.com>
   Stability   : alpha
   Portability : portable

Conversion of DokuWiki text to 'Pandoc' document.
-}
module Text.Pandoc.Readers.DokuWiki (readDokuWiki) where

import Prelude
import Control.Monad
import Control.Monad.Except (throwError)
import Data.Char (isAlphaNum)
import qualified Data.Foldable as F
import Data.List (transpose)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class (PandocMonad (..))
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError (PandocParsecError))
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (enclosed, nested)
import Text.Pandoc.Shared (crFilter, underlineSpan)

-- | Read DokuWiki from an input string and return a Pandoc document.
readDokuWiki :: PandocMonad m
             => ReaderOptions
             -> Text
             -> m Pandoc
readDokuWiki opts s = do
  let input = crFilter s
  res <- runParserT parseDokuWiki def {stateOptions = opts } "source" input
  case res of
       Left e  -> throwError $ PandocParsecError (T.unpack input) e
       Right d -> return d

type DWParser = ParserT Text ParserState

-- * Utility functions

-- | Parse end-of-line, which can be either a newline or end-of-file.
eol :: Stream s m Char => ParserT s st m ()
eol = void newline <|> eof

nested :: PandocMonad m => DWParser m a -> DWParser m a
nested p = do
  nestlevel <- stateMaxNestingLevel <$>  getState
  guard $ nestlevel > 0
  updateState $ \st -> st{ stateMaxNestingLevel = stateMaxNestingLevel st - 1 }
  res <- p
  updateState $ \st -> st{ stateMaxNestingLevel = nestlevel }
  return res

guardColumnOne :: PandocMonad m => DWParser m ()
guardColumnOne = getPosition >>= \pos -> guard (sourceColumn pos == 1)

-- | Parse DokuWiki document.
parseDokuWiki :: PandocMonad m => DWParser m Pandoc
parseDokuWiki =
  B.doc . mconcat <$> many block <* spaces <* eof

-- | Parse <code> and <file> attributes
codeLanguage :: PandocMonad m => DWParser m (String, [String], [(String, String)])
codeLanguage = try $ do
  rawLang <- option "-" (spaceChar *> manyTill anyChar (lookAhead (spaceChar <|> char '>')))
  let attr = case rawLang of
               "-" -> []
               l -> [l]
  return ("", attr, [])

-- | Generic parser for <code> and <file> tags
codeTag :: PandocMonad m
        => ((String, [String], [(String, String)]) -> String -> a)
        -> String
        -> DWParser m a
codeTag f tag = try $ f
  <$  char '<'
  <*  string tag
  <*> codeLanguage
  <*  manyTill anyChar (char '>')
  <*  optional (manyTill spaceChar eol)
  <*> manyTill anyChar (try $ string "</" <* string tag <* char '>')

-- * Inline parsers

-- | Parse any inline element but softbreak.
inline' :: PandocMonad m => DWParser m B.Inlines
inline' = whitespace
      <|> br
      <|> bold
      <|> italic
      <|> underlined
      <|> nowiki
      <|> percent
      <|> link
      <|> image
      <|> monospaced
      <|> subscript
      <|> superscript
      <|> deleted
      <|> footnote
      <|> inlineCode
      <|> inlineFile
      <|> inlineHtml
      <|> inlinePhp
      <|> autoLink
      <|> autoEmail
      <|> notoc
      <|> nocache
      <|> str
      <|> symbol
      <?> "inline"

-- | Parse any inline element, including soft break.
inline :: PandocMonad m => DWParser m B.Inlines
inline = endline <|> inline'

endline :: PandocMonad m => DWParser m B.Inlines
endline = try $ B.softbreak <$ skipMany spaceChar <* linebreak

whitespace :: PandocMonad m => DWParser m B.Inlines
whitespace = try $ B.space <$ skipMany1 spaceChar

br :: PandocMonad m => DWParser m B.Inlines
br = try $ B.linebreak <$ string "\\\\" <* space

linebreak :: PandocMonad m => DWParser m B.Inlines
linebreak = newline >> notFollowedBy newline >> (lastNewline <|> innerNewline)
  where lastNewline  = mempty <$ eof
        innerNewline = pure B.space

between :: (Monoid c, PandocMonad m, Show b)
        => DWParser m a -> DWParser m b -> (DWParser m b -> DWParser m c)
        -> DWParser m c
between start end p =
  mconcat <$> try (start >> notFollowedBy whitespace >> many1Till (p end) end)

enclosed :: (Monoid b, PandocMonad m, Show a)
         => DWParser m a -> (DWParser m a -> DWParser m b) -> DWParser m b
enclosed sep p = between sep (try sep) p

nestedInlines :: (Show a, PandocMonad m)
              => DWParser m a -> DWParser m B.Inlines
nestedInlines end = innerSpace <|> nestedInline
  where
    innerSpace   = try $ whitespace <* notFollowedBy end
    nestedInline = notFollowedBy whitespace >> nested inline

bold :: PandocMonad m => DWParser m B.Inlines
bold = try $ B.strong <$> enclosed (string "**") nestedInlines

italic :: PandocMonad m => DWParser m B.Inlines
italic = try $ B.emph <$> enclosed (string "//") nestedInlines

underlined :: PandocMonad m => DWParser m B.Inlines
underlined = try $ underlineSpan <$> enclosed (string "__") nestedInlines

nowiki :: PandocMonad m => DWParser m B.Inlines
nowiki = try $ B.text <$ string "<nowiki>" <*> manyTill anyChar (try $ string "</nowiki>")

percent :: PandocMonad m => DWParser m B.Inlines
percent = try $ B.text <$> enclosed (string "%%") nestedString

nestedString :: (Show a, PandocMonad m)
             => DWParser m a -> DWParser m String
nestedString end = innerSpace <|> count 1 nonspaceChar
  where
    innerSpace = try $ many1 spaceChar <* notFollowedBy end

monospaced :: PandocMonad m => DWParser m B.Inlines
monospaced = try $ B.code <$> enclosed (string "''") nestedString

subscript :: PandocMonad m => DWParser m B.Inlines
subscript = try $ B.subscript <$> between (string "<sub>") (try $ string "</sub>") nestedInlines

superscript :: PandocMonad m => DWParser m B.Inlines
superscript = try $ B.superscript <$> between (string "<sup>") (try $ string "</sup>") nestedInlines

deleted :: PandocMonad m => DWParser m B.Inlines
deleted = try $ B.strikeout <$> between (string "<del>") (try $ string "</del>") nestedInlines

-- | Parse a footnote.
footnote :: PandocMonad m => DWParser m B.Inlines
footnote = try $ B.note . B.para <$> between (string "((") (try $ string "))") nestedInlines

inlineCode :: PandocMonad m => DWParser m B.Inlines
inlineCode = codeTag B.codeWith "code"

inlineFile :: PandocMonad m => DWParser m B.Inlines
inlineFile = codeTag B.codeWith "file"

inlineHtml :: PandocMonad m => DWParser m B.Inlines
inlineHtml = try $ B.rawInline "html" <$ string "<html>" <*> manyTill anyChar (try $ string "</html>")

inlinePhp :: PandocMonad m => DWParser m B.Inlines
inlinePhp = try $ B.codeWith ("", ["php"], []) <$ string "<php>" <*> manyTill anyChar (try $ string "</php>")

makeLink :: (String, String) -> B.Inlines
makeLink (text, url) = B.link url "" $ B.str text

autoEmail :: PandocMonad m => DWParser m B.Inlines
autoEmail = try $ do
  state <- getState
  guard $ stateAllowLinks state
  makeLink <$ char '<' <*> emailAddress <* char '>'

autoLink :: PandocMonad m => DWParser m B.Inlines
autoLink = try $ do
  state <- getState
  guard $ stateAllowLinks state
  (text, url) <- uri
  guard $ checkLink (last url)
  return $ makeLink (text, url)
  where
    checkLink c
      | c == '/' = True
      | otherwise = isAlphaNum c

notoc :: PandocMonad m => DWParser m B.Inlines
notoc = try $ mempty <$ string "~~NOTOC~~"

nocache :: PandocMonad m => DWParser m B.Inlines
nocache = try $ mempty <$ string "~~NOCACHE~~"

str :: PandocMonad m => DWParser m B.Inlines
str = B.str <$> (many1 alphaNum <|> count 1 characterReference)

symbol :: PandocMonad m => DWParser m B.Inlines
symbol = B.str <$> count 1 nonspaceChar

link :: PandocMonad m => DWParser m B.Inlines
link = try $ do
  st <- getState
  guard $ stateAllowLinks st
  setState $ st{ stateAllowLinks = False }
  (url, title, content) <- linkText
  setState $ st{ stateAllowLinks = True }
  return $ B.link url title content

linkText :: PandocMonad m => DWParser m (String, String, B.Inlines)
linkText = do
  string "[["
  url <- many1Till anyChar (lookAhead (void (char '|') <|> try (void $ string "]]")))
  description <- option (B.str url) (mconcat <$> (char '|' *> manyTill inline (try $ lookAhead $ string "]]")))
  string "]]"
  return (url, "", description)

image :: PandocMonad m => DWParser m B.Inlines
image = try $ do
  string "{{"
  filename <- many1Till anyChar (lookAhead (void (char '|') <|> try (void $ string "}}")))
  description <- mconcat <$> option mempty (char '|' *> manyTill inline (try $ lookAhead $ string "}}"))
  string "}}"
  pure $ B.image filename "" description

-- * Block parsers

block :: PandocMonad m => DWParser m B.Blocks
block = do
  res <- mempty <$ skipMany1 blankline
         <|> blockElements
         <|> para
  skipMany blankline
  trace (take 60 $ show $ B.toList res)
  return res

blockElements :: PandocMonad m => DWParser m B.Blocks
blockElements = horizontalLine
            <|> header
            <|> list "  "
            <|> indentedCode
            <|> quote
            <|> blockCode
            <|> blockFile
            <|> blockHtml
            <|> blockPhp
            <|> table

horizontalLine :: PandocMonad m => DWParser m B.Blocks
horizontalLine = try $ B.horizontalRule <$ string "---" <* many1 (char '-') <* eol

header :: PandocMonad m => DWParser m B.Blocks
header = try $ do
  guardColumnOne
  eqs <- many1 (char '=')
  let lev = length eqs
  guard $ lev < 7
  contents <- B.trimInlines . mconcat <$> manyTill inline (try $ char '=' *> many1 (char '='))
  attr <- registerHeader nullAttr contents
  return $ B.headerWith attr (7 - lev) contents

list :: PandocMonad m => String -> DWParser m B.Blocks
list prefix = bulletList prefix <|> orderedList prefix

bulletList :: PandocMonad m => String -> DWParser m B.Blocks
bulletList prefix = try $ B.bulletList <$> parseList prefix '*'

orderedList :: PandocMonad m => String -> DWParser m B.Blocks
orderedList prefix = try $ B.orderedList <$> parseList prefix '-'

parseList :: PandocMonad m
          => String
          -> Char
          -> DWParser m [B.Blocks]
parseList prefix marker =
  many1 ((<>) <$> item <*> fmap mconcat (many continuation))
  where
    continuation = try $ list ("  " ++ prefix)
    item = try $ string prefix *> char marker *> char ' ' *> itemContents
    itemContents = B.plain . mconcat <$> many1Till inline' eol

indentedCode :: PandocMonad m => DWParser m B.Blocks
indentedCode = try $ B.codeBlock . unlines <$> many1 indentedLine
 where
   indentedLine = try $ string "  " *> manyTill anyChar eol

quote :: PandocMonad m => DWParser m B.Blocks
quote = try $ nestedQuote 0
  where
    prefix level = count level (char '>')
    contents level = nestedQuote level <|> quoteLine
    quoteLine = try $ B.plain . B.trimInlines . mconcat <$> many1Till inline' eol
    quoteContents level = (<>) <$> contents level <*> quoteContinuation level
    quoteContinuation level = mconcat <$> many (try $ prefix level *> contents level)
    nestedQuote level = B.blockQuote <$ char '>' <*> quoteContents (level + 1 :: Int)

blockHtml :: PandocMonad m => DWParser m B.Blocks
blockHtml = try $ B.rawBlock "html"
  <$  string "<HTML>"
  <*  optional (manyTill spaceChar eol)
  <*> manyTill anyChar (try $ string "</HTML>")

blockPhp :: PandocMonad m => DWParser m B.Blocks
blockPhp = try $ B.codeBlockWith ("", ["php"], [])
  <$  string "<PHP>"
  <*  optional (manyTill spaceChar eol)
  <*> manyTill anyChar (try $ string "</PHP>")

table :: PandocMonad m => DWParser m B.Blocks
table = do
  firstSeparator <- lookAhead tableCellSeparator
  rows <- tableRows
  let (headerRow, body) = if firstSeparator == '^'
                            then (head rows, tail rows)
                            else ([], rows)
  let attrs = const (AlignDefault, 0.0) <$> transpose rows
  pure $ B.table mempty attrs headerRow body

tableRows :: PandocMonad m => DWParser m [[B.Blocks]]
tableRows = many1 tableRow

tableRow :: PandocMonad m => DWParser m [B.Blocks]
tableRow = many1Till tableCell tableRowEnd

tableRowEnd :: PandocMonad m => DWParser m Char
tableRowEnd = try $ tableCellSeparator <* manyTill spaceChar eol

tableCellSeparator :: PandocMonad m => DWParser m Char
tableCellSeparator = char '|' <|> char '^'

tableCell :: PandocMonad m => DWParser m B.Blocks
tableCell = try $ B.plain . B.trimInlines . mconcat <$> (normalCell <|> headerCell)
  where
    normalCell = char '|' *> manyTill inline' (lookAhead tableCellSeparator)
    headerCell = char '^' *> manyTill inline' (lookAhead tableCellSeparator)

blockCode :: PandocMonad m => DWParser m B.Blocks
blockCode = codeTag B.codeBlockWith "code"

blockFile :: PandocMonad m => DWParser m B.Blocks
blockFile = codeTag B.codeBlockWith "file"

para :: PandocMonad m => DWParser m B.Blocks
para = result . mconcat <$> many1Till inline endOfParaElement
 where
   endOfParaElement = lookAhead $ endOfInput <|> endOfPara <|> newBlockElement
   endOfInput       = try $ skipMany blankline >> skipSpaces >> eof
   endOfPara        = try $ blankline >> skipMany1 blankline
   newBlockElement  = try $ blankline >> void blockElements
   result content   = if F.all (==Space) content
                      then mempty
                      else B.para $ B.trimInlines content
