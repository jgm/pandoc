{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{- |
Module      : Text.Pandoc.Parsing.Citations
Copyright   : © 2006-2022 John MacFarlane
License     : GPL-2.0-or-later
Maintainer  : John MacFarlane <jgm@berkeley.edu>

Citation parsing.
-}

module Text.Pandoc.Parsing.Citations
  ( citeKey
  )
where

import Control.Monad (guard, MonadPlus(mzero))
import Data.Char (isAlphaNum , isSpace)
import Data.Text (Text)
import Text.Pandoc.Sources
import Text.Parsec
  ( (<|>)
  , Stream(..)
  , lookAhead
  , many
  , option
  , try
  )
import Text.Pandoc.Parsing.Capabilities (HasLastStrPosition, notAfterString)
import Text.Pandoc.Parsing.General
import Text.Pandoc.Parsing.Types (ParserT)

import qualified Data.Text as T

citeKey :: (Stream s m Char, UpdateSourcePos s Char, HasLastStrPosition st)
        => Bool -- ^ If True, allow expanded @{..} syntax.
        -> ParserT s st m (Bool, Text)
citeKey allowBraced = try $ do
  guard =<< notAfterString
  suppress_author <- option False (True <$ char '-')
  char '@'
  key <- simpleCiteIdentifier
        <|> if allowBraced
               then charsInBalanced '{' '}' (satisfy (not . isSpace))
               else mzero
  return (suppress_author, key)

simpleCiteIdentifier :: (Stream s m Char, UpdateSourcePos s Char)
                      => ParserT s st m Text
simpleCiteIdentifier = do
  firstChar <- alphaNum <|> char '_' <|> char '*' -- @* for wildcard in nocite
  let regchar = satisfy (\c -> isAlphaNum c || c == '_')
  let internal p = try $ p <* lookAhead regchar
  rest <- many $ regchar <|> internal (oneOf ":.#$%&-+?<>~/") <|>
                 try (oneOf ":/" <* lookAhead (char '/'))
  return $ T.pack $ firstChar:rest
