{-# LANGUAGE FlexibleContexts #-}
{-
Copyright (C) 2014-2016 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

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
   Module      : Text.Pandoc.Readers.Org.Meta
   Copyright   : Copyright (C) 2014-2016 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Parsers for Org-mode meta declarations.
-}
module Text.Pandoc.Readers.Org.Meta
  ( metaLine
  ) where

import           Text.Pandoc.Readers.Org.BlockStarts
import           Text.Pandoc.Readers.Org.ExportSettings ( exportSettings )
import           Text.Pandoc.Readers.Org.Inlines
import           Text.Pandoc.Readers.Org.ParserState
import           Text.Pandoc.Readers.Org.Parsing

import qualified Text.Pandoc.Builder as B
import           Text.Pandoc.Builder ( Blocks, Inlines )
import           Text.Pandoc.Definition
import           Text.Pandoc.Compat.Monoid ((<>))

import           Control.Monad ( mzero )
import           Data.Char ( toLower )
import           Data.List ( intersperse )
import qualified Data.Map as M
import           Network.HTTP ( urlEncode )

-- | Parse and handle a single line containing meta information
-- The order, in which blocks are tried, makes sure that we're not looking at
-- the beginning of a block, so we don't need to check for it
metaLine :: OrgParser Blocks
metaLine = mempty <$ metaLineStart <* (optionLine <|> declarationLine)

declarationLine :: OrgParser ()
declarationLine = try $ do
  key   <- map toLower <$> metaKey
  value <- metaValue key
  updateState $ \st ->
    let meta' = B.setMeta key <$> value <*> pure nullMeta
    in st { orgStateMeta = orgStateMeta st <> meta' }

metaKey :: OrgParser String
metaKey = map toLower <$> many1 (noneOf ": \n\r")
                      <*  char ':'
                      <*  skipSpaces

metaValue :: String -> OrgParser (F MetaValue)
metaValue key = do
  case key of
    "author" -> metaInlinesCommaSeparated
    "title"  -> metaInlines
    "date"   -> metaInlines
    _        -> metaString

metaInlines :: OrgParser (F MetaValue)
metaInlines = fmap (MetaInlines . B.toList) <$> inlinesTillNewline

metaInlinesCommaSeparated :: OrgParser (F MetaValue)
metaInlinesCommaSeparated = do
  authStrs <- (many1 (noneOf ",\n")) `sepBy1` (char ',')
  newline
  authors <- mapM (parseFromString inlinesTillNewline . (++ "\n")) authStrs
  let toMetaInlines = MetaInlines . B.toList
  return $ MetaList . map toMetaInlines <$> sequence authors

metaString :: OrgParser (F MetaValue)
metaString =  return . MetaString <$> anyLine


--
-- export options
--
optionLine :: OrgParser ()
optionLine = try $ do
  key <- metaKey
  case key of
    "link"    -> parseLinkFormat >>= uncurry addLinkFormat
    "options" -> exportSettings
    _         -> mzero

addLinkFormat :: String
              -> (String -> String)
              -> OrgParser ()
addLinkFormat key formatter = updateState $ \s ->
  let fs = orgStateLinkFormatters s
  in s{ orgStateLinkFormatters = M.insert key formatter fs }

parseLinkFormat :: OrgParser ((String, String -> String))
parseLinkFormat = try $ do
  linkType <- (:) <$> letter <*> many (alphaNum <|> oneOf "-_") <* skipSpaces
  linkSubst <- parseFormat
  return (linkType, linkSubst)

-- | An ad-hoc, single-argument-only implementation of a printf-style format
-- parser.
parseFormat :: OrgParser (String -> String)
parseFormat = try $ do
  replacePlain <|> replaceUrl <|> justAppend
 where
   -- inefficient, but who cares
   replacePlain = try $ (\x -> concat . flip intersperse x)
                     <$> sequence [tillSpecifier 's', rest]
   replaceUrl   = try $ (\x -> concat . flip intersperse x . urlEncode)
                     <$> sequence [tillSpecifier 'h', rest]
   justAppend   = try $ (++) <$> rest

   rest            = manyTill anyChar         (eof <|> () <$ oneOf "\n\r")
   tillSpecifier c = manyTill (noneOf "\n\r") (try $ string ('%':c:""))

inlinesTillNewline :: OrgParser (F Inlines)
inlinesTillNewline = trimInlinesF . mconcat <$> manyTill inline newline
