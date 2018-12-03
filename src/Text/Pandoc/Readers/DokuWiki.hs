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
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class (PandocMonad (..))
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError (PandocParsecError))
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (enclosed, nested)
import Text.Pandoc.Shared (crFilter, underlineSpan)

{-
 - TODO: images
 - TODO: quotes
 - TODO: tables
 -}

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
      <|> monospaced
      <|> subscript
      <|> superscript
      <|> deleted
      <|> footnote
      <|> code
      <|> inlineHtml
      <|> autoLink
      <|> autoEmail
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
enclosed sep p = between sep (try $ sep) p

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

code :: PandocMonad m => DWParser m B.Inlines
code = try $ B.code <$ string "<code>" <*> manyTill anyChar (try $ string "</code>")

inlineHtml :: PandocMonad m => DWParser m B.Inlines
inlineHtml = try $ B.rawInline "html" <$ string "<html>" <*> manyTill anyChar (try $ string "</html>")

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
  url <- many1Till anyChar (char '|')
  content <- option (B.str url) (mconcat <$> linkContent)
  char ']'
  return (url, "", content)
  where
    linkContent      = many1Till anyChar (char ']') >>= parseLinkContent
    parseLinkContent = parseFromString' $ many1 inline

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
            <|> blockHtml

horizontalLine :: PandocMonad m => DWParser m B.Blocks
horizontalLine = try $ B.horizontalRule <$ string "---" <* many1 (char '-') <* eol

header :: PandocMonad m => DWParser m B.Blocks
header = try $ do
  guardColumnOne
  eqs <- many1 (char '=')
  let lev = length eqs
  guard $ lev < 7
  contents <- B.trimInlines . mconcat <$> manyTill inline (count lev $ char '=')
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
  many1 $ ((<>) <$> item <*> fmap mconcat (many continuation))
  where
    continuation = try $ list ("  " ++ prefix)
    item = try $ string prefix *> char marker *> char ' ' *> itemContents
    itemContents = B.plain . mconcat <$> many1Till inline' eol

blockHtml :: PandocMonad m => DWParser m B.Blocks
blockHtml = try $ B.rawBlock "html" <$ string "<HTML>" <*> manyTill anyChar (try $ string "</HTML>")

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
