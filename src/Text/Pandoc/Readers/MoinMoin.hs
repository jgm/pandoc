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
import Data.Char -- isUpper, isAlphaNum
import Text.Pandoc.Definition
import Text.Pandoc.Class.PandocMonad (PandocMonad (..))
import Text.Pandoc.Class (runPure) -- debug
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Parsing
import qualified Text.Pandoc.Builder as B
import qualified Data.Text as T

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
  spaces
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
     <|> bulletList
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

bulletList :: PandocMonad m => MoinParser m B.Blocks
bulletList = many1 bulletListItem >>= return . B.bulletList

bulletListItem :: PandocMonad m => MoinParser m B.Blocks
bulletListItem = try $ do
  lev <- length <$> many1 space
  char '*'
  spaces
  B.plain . B.trimInlines . mconcat <$> manyTill inline newline

inline :: PandocMonad m => MoinParser m B.Inlines
inline =  whitespace
      <|> camelCaseLink
      <|> str
      <|> italic
      <|> bold
      <|> underline
      <|> superscript
      <|> subscript
--    <|> stroke
      <|> externalLink
      <|> special

-- from Readers.Mediawiki
whitespace :: PandocMonad m => MoinParser m B.Inlines
whitespace = B.space <$ skipMany1 spaceChar

many2 :: PandocMonad m => MoinParser m a -> MoinParser m [a]
many2 p = do
  first <- p
  rest  <- many1 p
  return (first:rest)

camelWord :: PandocMonad m => MoinParser m String
camelWord = do
  f    <- satisfy isUpper
  rest <- many1 (satisfy (\c -> isAlphaNum c && not (isUpper c)))
  return (f:rest)

camelCaseLink :: PandocMonad m => MoinParser m B.Inlines
camelCaseLink = try $ do
  src <- mconcat <$> many2 camelWord
  let tsrc  = T.pack src
  let title = ""
  let label = B.str tsrc
  return $ B.link tsrc title label

-- from Readers.Mediawiki
str :: PandocMonad m => MoinParser m B.Inlines
str = B.str <$> many1Char (noneOf $ specialChars ++ spaceChars)

-- utility fn for most of the inline text formatters
formatter :: PandocMonad m
          => String
          -> (B.Inlines -> B.Inlines)
          -> MoinParser m B.Inlines
formatter delim inliner =
  enclosed delim' delim' inline >>= return . inliner . mconcat
  where delim' = string delim

italic      :: PandocMonad m => MoinParser m B.Inlines
italic      = formatter ("''") B.emph
bold        :: PandocMonad m => MoinParser m B.Inlines
bold        = formatter ("'''") B.strong
-- monospace: B.code (Code Attr Text) needs different handling
-- code: as monospace
-- however, B.code :: Text -> Inlines
underline   :: PandocMonad m => MoinParser m B.Inlines
underline   = formatter ("__") B.underline
superscript :: PandocMonad m => MoinParser m B.Inlines
superscript = formatter "^" B.superscript
subscript   :: PandocMonad m => MoinParser m B.Inlines
subscript   = formatter ",," B.subscript
-- smaller/larger: needs some thought
stroke :: PandocMonad m => MoinParser m B.Inlines
stroke = enclosed (string "--(") (string ")--") inline >>=
    return . B.strikeout . mconcat

special :: PandocMonad m => MoinParser m B.Inlines
special = B.str . T.singleton <$> oneOf specialChars

externalLink :: PandocMonad m => MoinParser m B.Inlines
externalLink = do
  string "[["
  (src,label) <-  try labelledLink <|> unlabelledLink
  --string "]]"
  return $ B.link src "" $ B.str label
  where
    labelledLink = do
      src <- manyTillChar (noneOf "|") (try (char '|'))
      lbl <- manyTillChar (noneOf "]") (string "]]")
      return (src,lbl)

    unlabelledLink = do
      src <- manyTillChar (noneOf "|") (string "]]")
      return (src,src)

-- from Readers.Mediawiki
specialChars :: [Char]
specialChars = "'[]<=&*{}|\":\\_^,"

-- from Readers.Mediawiki
spaceChars :: [Char]
spaceChars = " \n\t"

-- debug function to run the inline parser in GHCi
debugParse :: PandocMonad m => T.Text -> m (Either ParseError B.Inlines)
debugParse t =
  runParserT (mconcat <$> many inline) MoinState "srcname" (toSources t)
