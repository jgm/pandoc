{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Docx.Fields
   Copyright   : Copyright (C) 2014-2020 Jesse Rosenthal
   License     : GNU GPL, version 2 or above

   Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
   Stability   : alpha
   Portability : portable

For parsing Field definitions in instText tags, as described in
ECMA-376-1:2016, §17.16.5 -}

module Text.Pandoc.Readers.Docx.Fields ( FieldInfo(..)
                                       , parseFieldInfo
                                       ) where

import Data.Functor (($>), void)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text (Parser)

type URL = T.Text
type Anchor = T.Text

data FieldInfo = HyperlinkField URL
                -- The boolean indicates whether the field is a hyperlink.
               | PagerefField Anchor Bool
               | CslCitation T.Text
               | CslBibliography
               | EndNoteCite T.Text
               | EndNoteRefList
               | UnknownField
               deriving (Show)

parseFieldInfo :: T.Text -> Either ParseError FieldInfo
parseFieldInfo = parse fieldInfo ""

fieldInfo :: Parser FieldInfo
fieldInfo =
  try (HyperlinkField <$> hyperlink)
  <|>
  try ((uncurry PagerefField) <$> pageref)
  <|>
  try addIn
  <|>
  return UnknownField

addIn :: Parser FieldInfo
addIn = do
  spaces
  string "ADDIN"
  spaces
  try cslCitation <|> cslBibliography <|> endnoteCite <|> endnoteRefList

cslCitation :: Parser FieldInfo
cslCitation = do
  optional (string "ZOTERO_ITEM")
  spaces
  string "CSL_CITATION"
  spaces
  CslCitation <$> getInput

cslBibliography :: Parser FieldInfo
cslBibliography = do
  string "ZOTERO_BIBL" <|> string "Mendeley Bibliography CSL_BIBLIOGRAPHY"
  return CslBibliography

endnoteCite :: Parser FieldInfo
endnoteCite = try $ do
  string "EN.CITE"
  spaces
  EndNoteCite <$> getInput

endnoteRefList :: Parser FieldInfo
endnoteRefList = try $ do
  string "EN.REFLIST"
  return EndNoteRefList


escapedQuote :: Parser T.Text
escapedQuote = string "\\\"" $> "\\\""

inQuotes :: Parser T.Text
inQuotes =
  try escapedQuote <|> (T.singleton <$> anyChar)

quotedString :: Parser T.Text
quotedString = do
  char '"'
  T.concat <$> manyTill inQuotes (try (char '"'))

unquotedString :: Parser T.Text
unquotedString = T.pack <$> manyTill anyChar (try $ void (lookAhead space) <|> eof)

fieldArgument :: Parser T.Text
fieldArgument = quotedString <|> unquotedString

-- there are other switches, but this is the only one I've seen in the wild so far, so it's the first one I'll implement. See §17.16.5.25
hyperlinkSwitch :: Parser (T.Text, T.Text)
hyperlinkSwitch = do
  sw <- string "\\l"
  spaces
  farg <- fieldArgument
  return (T.pack sw, farg)

hyperlink :: Parser URL
hyperlink = do
  many space
  string "HYPERLINK"
  spaces
  farg <- fieldArgument
  switches <- spaces *> many hyperlinkSwitch
  let url = case switches of
              ("\\l", s) : _ -> farg <> "#" <> s
              _              -> farg
  return url

-- See §17.16.5.45
pagerefSwitch :: Parser (T.Text, T.Text)
pagerefSwitch = do
  sw <- string "\\h"
  spaces
  farg <- fieldArgument
  return (T.pack sw, farg)

pageref :: Parser (Anchor, Bool)
pageref = do
  many space
  string "PAGEREF"
  spaces
  farg <- fieldArgument
  switches <- spaces *> many pagerefSwitch
  let isLink = case switches of
              ("\\h", _) : _ -> True 
              _              -> False
  return (farg, isLink)
