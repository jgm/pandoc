{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{- |
   Module      : Text.Pandoc.Readers.Mdoc.Lex
   Copyright   : Copyright (C) 2018-2020 Yan Pashkovsky and John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : Yan Pashkovsky <yanp.bugz@gmail.com>
   Stability   : WIP
   Portability : portable

Tokenizer for mdoc format
-}
module Text.Pandoc.Readers.Mdoc.Lex
  ( MdocToken(..)
  , MdocTokens(..)
  , DelimSide(..)
  , lexMdoc
  , toString
  )
where

import Control.Monad (void, guard, when)
import Control.Monad.Except (throwError)
import Text.Pandoc.Class.PandocMonad (PandocMonad(..))
import Data.Char (isAlphaNum)
import Data.Default (Default)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import Text.Pandoc.Readers.Roff.Escape
import Text.Pandoc.Readers.Mdoc.Macros
import qualified Data.Sequence as Seq

-- import Debug.Trace (traceShowId)

--
-- Data Types
--
type TableOption = (T.Text, T.Text)


data CellFormat =
  CellFormat
  { columnType     :: Char
  , pipePrefix     :: Bool
  , pipeSuffix     :: Bool
  , columnSuffixes :: [T.Text]
  } deriving (Show, Eq, Ord)

type TableRow = ([CellFormat], [MdocTokens])

data DelimSide = Open | Middle | Close deriving (Show, Eq)

data MdocToken = Str T.Text SourcePos
               | Macro T.Text SourcePos
               | Lit T.Text SourcePos
               | Blank SourcePos
               | Delim DelimSide T.Text SourcePos
               | Tbl [TableOption] [TableRow] SourcePos
               | Eol
               deriving Show

toString :: MdocToken -> T.Text
toString (Str x _) = x
toString (Macro x _) = x
toString (Lit x _) = x
toString (Delim _ x _) = x
toString Blank{} = mempty
toString Tbl{} = mempty
toString Eol = mempty

newtype MdocTokens = MdocTokens { unRoffTokens :: Seq.Seq MdocToken }
        deriving (Show, Semigroup, Monoid)

singleTok :: MdocToken -> MdocTokens
singleTok t = MdocTokens (Seq.singleton t)

data RoffState = RoffState  deriving Show

instance Default RoffState where
  def = RoffState

type Lexer m = ParsecT Sources RoffState m

instance RoffMonad MdocTokens where
  type Token MdocTokens = T.Text
  type State MdocTokens = RoffState
  expandString = return ()
  escString = return mempty
  emit = id
  backslash = (mempty <* char '\\') <|> (mempty <* string "\\E")

--
-- Lexer: T.Text -> RoffToken
--

eofline :: (Stream s m Char, UpdateSourcePos s Char) => ParsecT s u m MdocToken
eofline = do
  void newline <|> eof
  return Eol


-- separate function from lexMacro since real man files sometimes do not
-- follow the rules
lexComment :: PandocMonad m => Lexer m MdocTokens
lexComment = do
  try $ string ".\\\""
  skipMany $ noneOf "\n"
  eofline
  return mempty


argText :: PandocMonad m => Lexer m T.Text
argText = mconcat <$> many1 (escape <|> regularText)

spaceTabChar :: PandocMonad m => Lexer m T.Text
spaceTabChar = T.singleton <$> spaceChar

quotedArg :: PandocMonad m => Lexer m T.Text
quotedArg = do
  quoteChar
  t <- mconcat <$> many (try innerQuote <|> escape <|> regularText <|> spaceTabChar)
  quoteChar
  notFollowedBy quoteChar
  return t
  where
    innerQuote = do
      string "\"\""
      return "\""

anyText :: PandocMonad m => Lexer m T.Text
anyText = escape <|> regularText <|> quoteChar <|> spaceTabChar

regularText :: PandocMonad m => Lexer m T.Text
regularText = many1Char $ noneOf "\n\r\t \\\""

quoteChar :: PandocMonad m => Lexer m T.Text
quoteChar = T.singleton <$> char '"'

mdocToken :: PandocMonad m => Lexer m MdocTokens
mdocToken = lexComment <|> lexControlLine <|> lexTextLine

lexMacroName :: PandocMonad m => Lexer m T.Text
lexMacroName = many1Char (satisfy isMacroChar)
  where
    isMacroChar '%' = True
    isMacroChar x = isAlphaNum x

lexMacro :: PandocMonad m => Lexer m MdocToken
lexMacro = do
  pos <- getPosition
  name <- lexMacroName
  eof <|> void (lookAhead (spaceChar <|> newline))
  skipSpaces
  return $ Macro name pos

lexCallableMacro :: PandocMonad m => Lexer m MdocToken
lexCallableMacro = do
  pos <- getPosition
  q <- optionMaybe quoteChar
  name <- lexMacroName
  when (isJust q) (void quoteChar)
  eof <|> void (lookAhead (spaceChar <|> newline))
  skipSpaces
  guard $ isCallableMacro name
  return $ Macro name pos

lexDelim :: (PandocMonad m) => Lexer m MdocToken
lexDelim = do
  pos <- getPosition
  q <- optionMaybe quoteChar
  t <-
    Delim Open <$> oneOfStrings ["(", "["]
      <|> Delim Close <$> oneOfStrings [".", ",", ":", ";", ")", "]", "?", "!"]
      <|> Delim Middle <$> textStr "|"
  when (isJust q) (void quoteChar)
  eof <|> void (lookAhead (spaceChar <|> newline))
  skipSpaces
  return $ t pos

lexLit :: PandocMonad m => Lexer m MdocToken
lexLit = do
  pos <- getPosition
  t <- argText <|> quotedArg
  guard $ not $ T.null t
  skipSpaces
  return $ Lit t pos

lexTextLine :: PandocMonad m => Lexer m MdocTokens
lexTextLine = do
  pos <- getPosition
  guard $ sourceColumn pos == 1
  t <- mconcat <$> many anyText
  eofline
  if T.null $ T.strip t
     then return $ singleTok $ Blank pos
     else return $ singleTok $ Str t pos

lexControlLine :: PandocMonad m => Lexer m MdocTokens
lexControlLine = do
  pos <- getPosition
  guard $ sourceColumn pos == 1
  char '.'
  m@(Macro name _) <- lexMacro
  let parsed = isParsedMacro name
  (wds, e) <- manyUntil (l parsed) eofline
  return $ MdocTokens $ Seq.fromList $ (m:wds) <> [e]
    where
      l True = try lexDelim <|> try lexCallableMacro <|> lexLit
      l False = try lexDelim <|> lexLit

-- | Tokenize a string as a sequence of roff tokens.
lexMdoc :: PandocMonad m => SourcePos -> T.Text -> m MdocTokens
lexMdoc pos txt = do
  eithertokens <- readWithM (do setPosition pos
                                mconcat <$> manyTill mdocToken eof) def txt
  case eithertokens of
    Left e       -> throwError e
    Right tokenz -> return tokenz
