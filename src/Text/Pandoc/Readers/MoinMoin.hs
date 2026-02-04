{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.MoinMoin
   Copyright   : Copyright (C) 2026 Jonathan Dowland
   License     : GNU GPL, version 2 or above

   Maintainer  : Jonathan Dowland <jmtd@debian.org>
   Stability   : alpha
   Portability : portable

Conversion of MoinMoin text to 'Pandoc' document.
-}

module Text.Pandoc.Readers.MoinMoin( readMoinMoin ) where

import Control.Monad (guard)
import Control.Monad.Except (throwError)
import Text.Pandoc.Definition
import Text.Pandoc.Class.PandocMonad (PandocMonad (..))
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Parsing
import qualified Text.Pandoc.Builder as B

-- | Read MoinMoin from an input string and return a Pandoc document.
readMoinMoin :: (PandocMonad m, ToSources a) => ReaderOptions -> a -> m Pandoc
readMoinMoin opts s = do
  let sources = toSources s
  parsed <- readWithM parseMoinMoin MoinState sources
  case parsed of
    Left  err -> throwError err
    Right res -> return res

data MoinState = MoinState -- context that needs to be passed around the parser
type MoinParser m = ParsecT Sources MoinState m

parseMoinMoin :: PandocMonad m => MoinParser m Pandoc
parseMoinMoin = do
  many processingInstruction
  blocks <- mconcat <$> many block
  spaces -- optional space?
  eof

  let meta = nullMeta

  -- reportLogMessages -- could not deduce 'HasLogMessages MoinState'
  return $ Pandoc meta (B.toList blocks)

{-
 - we may wish to handle:
 -  #format creole|plain|python|rst|<any installed parser name>
 -  #REDIRECT | #refresh Xs <target>
 -  #pragma
 -    section-numbers (headings)
 -    keywords => meta keywords
 -    description => meta description
 -  #DEPRECATED
 -  #language (iso-639-1 code)
 -
 - -}
processingInstruction :: PandocMonad m => MoinParser m ()
processingInstruction = do
  char '#'
  manyUntil anyChar newline
  return ()

block :: PandocMonad m => MoinParser m B.Blocks
block = do
  res <- mempty <$ skipMany1 blankline
     <|> header
     <|> para
  return res

-- from Readers.Mediawiki
header :: PandocMonad m => MoinParser m B.Blocks
header = try $ do
  guardColumnOne
  lev <- length <$> many1 (char '=')
  guard $ lev <= 5
  contents <- B.trimInlines . mconcat <$> manyTill inline (count lev $ char '=')
  return $ B.header lev contents

-- from Readers.Mediawiki
guardColumnOne :: PandocMonad m => MoinParser m ()
guardColumnOne = getPosition >>= \pos -> guard (sourceColumn pos == 1)

-- from Readers.Mediawiki
para :: PandocMonad m => MoinParser m B.Blocks
para = do
  contents <- B.trimInlines . mconcat <$> many1 inline
  return $ B.para contents

inline :: PandocMonad m => MoinParser m B.Inlines
inline =  whitespace
      <|> str

-- from Readers.Mediawiki
whitespace :: PandocMonad m => MoinParser m B.Inlines
whitespace = B.space <$ skipMany1 spaceChar

-- from Readers.Mediawiki
str :: PandocMonad m => MoinParser m B.Inlines
str = B.str <$> many1Char (noneOf $ specialChars ++ spaceChars)

-- from Readers.Mediawiki
specialChars :: [Char]
specialChars = "'[]<=&*{}|\":\\_"

-- from Readers.Mediawiki
spaceChars :: [Char]
spaceChars = " \n\t"
