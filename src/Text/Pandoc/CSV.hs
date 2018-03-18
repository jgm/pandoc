{-# LANGUAGE NoImplicitPrelude #-}
{-
Copyright (C) 2017–2018 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.CSV
   Copyright   : Copyright (C) 2017–2018 John MacFarlane <jgm@berkeley.edu>
   License     : GNU GPL, version 2 or above
   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Simple CSV parser.
-}

module Text.Pandoc.CSV (
  CSVOptions(..),
  defaultCSVOptions,
  parseCSV,
  ParseError
) where

import Prelude
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text (Parser)

data CSVOptions = CSVOptions{
    csvDelim     :: Char
  , csvQuote     :: Char
  , csvKeepSpace :: Bool -- treat whitespace following delim as significant
  , csvEscape    :: Maybe Char -- default is to double up quote
} deriving (Read, Show)

defaultCSVOptions :: CSVOptions
defaultCSVOptions = CSVOptions{
    csvDelim = ','
  , csvQuote = '"'
  , csvKeepSpace = False
  , csvEscape = Nothing }

parseCSV :: CSVOptions -> Text -> Either ParseError [[Text]]
parseCSV opts t = parse (pCSV opts) "csv" t

pCSV :: CSVOptions -> Parser [[Text]]
pCSV opts =
  (pCSVRow opts `sepEndBy` endline) <* (spaces *> eof)

pCSVRow :: CSVOptions -> Parser [Text]
pCSVRow opts = notFollowedBy blank >> pCSVCell opts `sepBy` pCSVDelim opts

blank :: Parser ()
blank = try $ spaces >> (() <$ endline <|> eof)

pCSVCell :: CSVOptions -> Parser Text
pCSVCell opts = pCSVQuotedCell opts <|> pCSVUnquotedCell opts

pCSVQuotedCell :: CSVOptions -> Parser Text
pCSVQuotedCell opts = do
  char (csvQuote opts)
  res <- many (satisfy (\c -> c /= csvQuote opts &&
                              Just c /= csvEscape opts) <|> escaped opts)
  char (csvQuote opts)
  return $ T.pack res

escaped :: CSVOptions -> Parser Char
escaped opts =
  case csvEscape opts of
       Nothing -> try $ char (csvQuote opts) >> char (csvQuote opts)
       Just c  -> try $ char c >> noneOf "\r\n"

pCSVUnquotedCell :: CSVOptions -> Parser Text
pCSVUnquotedCell opts = T.pack <$>
  many (satisfy (\c -> c /= csvDelim opts && c /= '\r' && c /= '\n'
                  && c /= csvQuote opts))

pCSVDelim :: CSVOptions -> Parser ()
pCSVDelim opts = do
  char (csvDelim opts)
  if csvKeepSpace opts
     then return ()
     else skipMany (oneOf " \t")

endline :: Parser ()
endline = do
  optional (void $ char '\r')
  void $ char '\n'
