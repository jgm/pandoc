{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{- |
   Module      : Text.Pandoc.Readers.Mdoc.Lex
   Copyright   : Copyright (C) 2024 Evan Silberman
   License     : GNU GPL, version 2 or above

   Maintainer  : Evan Silberman <evan@jklol.net>
   Stability   : WIP
   Portability : portable

Tokenizer for mdoc
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
import Data.Maybe (isJust)
import qualified Data.Text as T
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import Text.Pandoc.Readers.Roff.Escape
import Text.Pandoc.Readers.Mdoc.Macros
import qualified Data.Sequence as Seq

-- As a higher level language with a wealth of semantic macros, mdoc
-- discourages authors from falling back to low-level roff features like font
-- selection, custom macros, defined strings, etc. Pandoc's mdoc reader is
-- accordingly implemented as a high-level interpreter of mdoc's semantic macros
-- and almost no raw roff requests are supported.
--
-- tbl(7) and eqn(7) macros are rare but not completely unseen in mdoc manuals.
-- they are not yet implemented. most use of tbl macros in mdoc could probably
-- be replaced with .Bl -column

data DelimSide = Open | Middle | Close deriving (Show, Eq)

-- | Tokens for Mdoc documents
data MdocToken = Str T.Text SourcePos -- ^ The contents of a text line
               | Macro T.Text SourcePos  -- ^ A macro to be processed
               | Lit T.Text SourcePos  -- ^ Literal text on a control line
               | Blank SourcePos  -- ^ A blank line
               | Delim DelimSide T.Text SourcePos  -- ^ A delimiter character
               | Eol  -- ^ The end of a control line
               deriving Show

toString :: MdocToken -> T.Text
toString (Str x _) = x
toString (Macro x _) = x
toString (Lit x _) = x
toString (Delim _ x _) = x
toString Blank{} = mempty
toString Eol = mempty

newtype MdocTokens = MdocTokens { unMdocTokens :: Seq.Seq MdocToken }
        deriving (Show, Semigroup, Monoid)

singleTok :: MdocToken -> MdocTokens
singleTok t = MdocTokens (Seq.singleton t)

type Lexer m = ParsecT Sources () m

instance RoffLikeLexer MdocTokens where
  -- This is a bit confusing. We're lexing to MdocTokens, but for escaping
  -- purposes we just want Texts.
  type Token MdocTokens = T.Text
  -- We don't need a state
  type State MdocTokens = ()
  -- We don't support predefined string expansion
  expandString = return ()
  escString = return mempty
  -- what token type the unescaped text gets wrapped in is decided by other
  -- parts of the lexer.
  emit = id
  -- All escapes are resolved in the lexer and we never need to emit anything,
  -- vs. the roff lexer which has to push the backlashes to the output while
  -- in copy mode.
  backslash = (mempty <* char '\\') <|> (mempty <* string "\\E")
  -- We don't support macro definition and we don't output anything for \A
  checkDefined = const mempty
  -- We don't support copy mode and \E is treated as backslash
  escE = return mempty
  -- We don't support low-level font selection
  escFont = escIgnore 'f' [escapeArg, countChar 1 (satisfy (/='\n'))]

eofline :: (Stream s m Char, UpdateSourcePos s Char) => ParsecT s u m MdocToken
eofline = do
  void newline <|> eof
  return Eol

lexComment :: PandocMonad m => Lexer m MdocTokens
lexComment = do
  try $ string ".\\\""
  skipMany $ noneOf "\n"
  eofline
  return mempty

argText :: PandocMonad m => Lexer m T.Text
argText = do
  beg <- escape <|> regularText
  end <- mconcat <$> many (escape <|> regularText <|> quoteChar)
  return $ beg <> end

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
  eofline *> mempty <|> do
    m@(Macro name _) <- lexMacro
    -- .Ns macros at the start of a line are ignored. We'd have to look behind
    -- to keep track of the "start of the line" in the parser, so we'll drop
    -- those macros in lexing.
    let start | name == "Ns" = []
              | otherwise = [m]
    let parsed = isParsedMacro name
    (wds, e) <- manyUntil (l parsed) eofline
    return $ MdocTokens $ Seq.fromList $ start <> wds <> [e]
      where
        l True = try lexDelim <|> try lexCallableMacro <|> lexLit
        l False = try lexDelim <|> lexLit

-- | Tokenize a string as a sequence of mdoc tokens.
lexMdoc :: PandocMonad m => SourcePos -> T.Text -> m MdocTokens
lexMdoc pos txt = do
  eithertokens <- readWithM (do setPosition pos
                                mconcat <$> manyTill mdocToken eof) def txt
  case eithertokens of
    Left e       -> throwError e
    Right tokenz -> return tokenz
