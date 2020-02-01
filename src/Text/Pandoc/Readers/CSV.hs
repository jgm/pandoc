{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{- |
   Module      : Text.Pandoc.Readers.RST
   Copyright   : Copyright (C) 2006-2019 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion from CSV to a 'Pandoc' table.
-}
module Text.Pandoc.Readers.CSV ( readCSV ) where
import Prelude
import Control.Monad (guard)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as B
import Text.Parsec
import Text.Parsec.Text (Parser)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Shared (crFilter)
import Text.Pandoc.Error
import Text.Pandoc.Options (ReaderOptions)
import Control.Monad.Except (throwError)

readCSV :: PandocMonad m
        => ReaderOptions -- ^ Reader options
        -> Text          -- ^ Text to parse (assuming @'\n'@ line endings)
        -> m Pandoc
readCSV _opts s = do
  case parse pCSV "input" (crFilter s) of
    Right (r:rs) -> return $ B.doc $ B.table capt (zip aligns widths) hdrs rows
       where capt = mempty
             numcols = length r
             hdrs = map (B.plain . B.text) r
             rows = map (map (B.plain . B.text)) rs
             aligns = replicate numcols AlignDefault
             widths = replicate numcols 0
    Right []     -> return $ B.doc mempty
    Left e       -> throwError $ PandocParsecError s e

{- from RFC 4180

   The ABNF grammar [2] appears as follows:

   file = [header CRLF] record *(CRLF record) [CRLF]

   header = name *(COMMA name)

   record = field *(COMMA field)

   name = field

   field = (escaped / non-escaped)

   escaped = DQUOTE *(TEXTDATA / COMMA / CR / LF / 2DQUOTE) DQUOTE

   non-escaped = *TEXTDATA

   COMMA = %x2C

   CR = %x0D ;as per section 6.1 of RFC 2234 [2]

   DQUOTE =  %x22 ;as per section 6.1 of RFC 2234 [2]

   LF = %x0A ;as per section 6.1 of RFC 2234 [2]

   CRLF = CR LF ;as per section 6.1 of RFC 2234 [2]

   TEXTDATA =  %x20-21 / %x23-2B / %x2D-7E
-}

pCSV :: Parser [[Text]]
pCSV = many pRecord

pRecord :: Parser [Text]
pRecord = do
  x <- pField
  xs <- many $ pComma >> pField
  () <$ newline <|> (guard (not (T.null x) || not (null xs)) >> eof)
  return (x:xs)

pField :: Parser Text
pField = pEscaped <|> pUnescaped

pComma :: Parser Char
pComma = char ','

pUnescaped :: Parser Text
pUnescaped = T.strip . T.pack <$> many (noneOf "\n\r\",")

pEscaped :: Parser Text
pEscaped = do
  char '"'
  t <- T.pack <$> many (pDoubledQuote <|> noneOf "\"")
  char '"'
  return t

pDoubledQuote :: Parser Char
pDoubledQuote = try $ char '"' >> char '"'
