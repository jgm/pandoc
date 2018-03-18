{-# LANGUAGE NoImplicitPrelude #-}
{-
Copyright (C) 2014-2018 Jesse Rosenthal <jrosenthal@jhu.edu>

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
   Module      : Text.Pandoc.Readers.Docx.Fields
   Copyright   : Copyright (C) 2014-2018 Jesse Rosenthal
   License     : GNU GPL, version 2 or above

   Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
   Stability   : alpha
   Portability : portable

For parsing Field definitions in instText tags, as described in
ECMA-376-1:2016, §17.16.5 -}

module Text.Pandoc.Readers.Docx.Fields ( FieldInfo(..)
                                       , parseFieldInfo
                                       ) where

import Prelude
import Text.Parsec
import Text.Parsec.String (Parser)

type URL = String

data FieldInfo = HyperlinkField URL
               | UnknownField
               deriving (Show)

parseFieldInfo :: String -> Either ParseError FieldInfo
parseFieldInfo = parse fieldInfo ""

fieldInfo :: Parser FieldInfo
fieldInfo =
  try (HyperlinkField <$> hyperlink)
  <|>
  return UnknownField

escapedQuote :: Parser String
escapedQuote = string "\\\""

inQuotes :: Parser String
inQuotes =
  (try escapedQuote) <|> (anyChar >>= (\c -> return [c]))

quotedString :: Parser String
quotedString = do
  char '"'
  concat <$> manyTill inQuotes (try (char '"'))

unquotedString :: Parser String
unquotedString = manyTill anyChar (try $ lookAhead space *> return () <|> eof)

fieldArgument :: Parser String
fieldArgument = quotedString <|> unquotedString

-- there are other switches, but this is the only one I've seen in the wild so far, so it's the first one I'll implement. See §17.16.5.25
hyperlinkSwitch :: Parser (String, String)
hyperlinkSwitch = do
  sw <- string "\\l"
  spaces
  farg <- fieldArgument
  return (sw, farg)

hyperlink :: Parser URL
hyperlink = do
  many space
  string "HYPERLINK"
  spaces
  farg <- fieldArgument
  switches <- spaces *> many hyperlinkSwitch
  let url = case switches of
              ("\\l", s) : _ -> farg ++ ('#': s)
              _              -> farg
  return url
