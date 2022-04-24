{- |
   Module      : Text.Pandoc.CSV
   Copyright   : Copyright (C) 2017-2022 John MacFarlane <jgm@berkeley.edu>
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

import Control.Monad (unless, void, mzero)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text (Parser)

data CSVOptions = CSVOptions{
    csvDelim     :: Char
  , csvQuote     :: Maybe Char
  , csvKeepSpace :: Bool -- treat whitespace following delim as significant
  , csvEscape    :: Maybe Char -- default is to double up quote
} deriving (Read, Show)

defaultCSVOptions :: CSVOptions
defaultCSVOptions = CSVOptions{
    csvDelim = ','
  , csvQuote = Just '"'
  , csvKeepSpace = False
  , csvEscape = Nothing }

parseCSV :: CSVOptions -> Text -> Either ParseError [[Text]]
parseCSV opts t = parse (pCSV opts) "csv" t

pCSV :: CSVOptions -> Parser [[Text]]
pCSV opts =
  (pCSVRow opts `sepEndBy` endline) <* (spaces *> eof)

pCSVRow :: CSVOptions -> Parser [Text]
pCSVRow opts = do
  x <- pCSVCell opts
  xs <- (if T.null x then many1 else many) $ pCSVDelim opts *> pCSVCell opts
  return (x:xs)

pCSVCell :: CSVOptions -> Parser Text
pCSVCell opts = pCSVQuotedCell opts <|> pCSVUnquotedCell opts

pCSVQuotedCell :: CSVOptions -> Parser Text
pCSVQuotedCell opts =
  case csvQuote opts of
    Nothing -> mzero
    Just quotechar -> do
      char quotechar
      res <- many (satisfy (\c -> c /= quotechar &&
                                  Just c /= csvEscape opts) <|> escaped opts)
      char quotechar
      return $ T.pack res

escaped :: CSVOptions -> Parser Char
escaped opts =
  case csvEscape opts of
    Nothing ->
      case csvQuote opts of
        Nothing -> mzero
        Just q -> try $ char q >> char q
    Just c  -> try $ char c >> noneOf "\r\n"

pCSVUnquotedCell :: CSVOptions -> Parser Text
pCSVUnquotedCell opts = T.pack <$>
  many (satisfy (\c -> c /= csvDelim opts && c /= '\r' && c /= '\n'))

pCSVDelim :: CSVOptions -> Parser ()
pCSVDelim opts = do
  char (csvDelim opts)
  unless (csvKeepSpace opts) $ skipMany (oneOf " \t")

endline :: Parser ()
endline = do
  optional (void $ char '\r')
  void $ char '\n'
