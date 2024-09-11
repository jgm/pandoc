{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{- |
   Module      : Text.Pandoc.Readers.Mdoc.Lex
   Copyright   : Copyright (C) 2018-2020 Yan Pashkovsky and John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : Yan Pashkovsky <yanp.bugz@gmail.com>
   Stability   : WIP
   Portability : portable

Tokenizer for roff formats (man, ms).
-}
module Text.Pandoc.Readers.Mdoc.Lex
  ( MdocToken(..)
  , MdocTokens(..)
  , DelimSide(..)
  , lexMdoc
  , toString
  )
where

import Safe (lastDef)
import Control.Monad (void, mzero, mplus, guard)
import Control.Monad.Except (throwError)
import Text.Pandoc.Class.PandocMonad
       (getResourcePath, readFileFromDirs, PandocMonad(..), report)
import Data.Char (isLower, toLower, toUpper, chr, isAscii, isAlphaNum)
import Data.Default (Default)
import qualified Data.Map as M
import Data.List (intercalate)
import qualified Data.Text as T
import Text.Pandoc.Logging (LogMessage(..))
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import Text.Pandoc.Shared (safeRead)
import Text.Pandoc.RoffChar (characterCodes, combiningAccents)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable
import qualified Data.Text.Normalize as Normalize

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
               | Delim DelimSide T.Text SourcePos
               | Tbl [TableOption] [TableRow] SourcePos
               | Eol
               deriving Show

toString :: MdocToken -> T.Text
toString (Str x _) = x
toString (Macro x _) = x
toString (Lit x _) = x
toString (Delim _ x _) = x
toString Tbl{} = mempty
toString Eol = mempty

newtype MdocTokens = MdocTokens { unRoffTokens :: Seq.Seq MdocToken }
        deriving (Show, Semigroup, Monoid)

singleTok :: MdocToken -> MdocTokens
singleTok t = MdocTokens (Seq.singleton t)

data RoffState = RoffState { tableTabChar :: Char
                           } deriving Show

instance Default RoffState where
  def = RoffState { tableTabChar = '\t'
                  }

type RoffLexer m = ParsecT Sources RoffState m

--
-- Lexer: T.Text -> RoffToken
--

eofline :: (Stream s m Char, UpdateSourcePos s Char) => ParsecT s u m MdocToken
eofline = do
  void newline <|> eof
  return Eol

characterCodeMap :: M.Map T.Text Char
characterCodeMap =
  M.fromList $ map (\(x,y) -> (y,x)) characterCodes

combiningAccentsMap :: M.Map T.Text Char
combiningAccentsMap =
  M.fromList $ map (\(x,y) -> (y,x)) combiningAccents

escape :: PandocMonad m => RoffLexer m T.Text
escape = try $ do
  backslash
  escapeGlyph <|> escapeNormal

escapeGlyph :: PandocMonad m => RoffLexer m T.Text
escapeGlyph = do
  c <- lookAhead (oneOf ['[','('])
  escapeArg >>= resolveGlyph c

resolveGlyph :: PandocMonad m => Char -> T.Text -> RoffLexer m T.Text
resolveGlyph delimChar glyph = do
  let cs = T.replace "_u" " u" glyph -- unicode glyphs separated by _
  (case T.words cs of
      []  -> mzero
      [s] -> case M.lookup s characterCodeMap `mplus` readUnicodeChar s of
               Nothing -> mzero
               Just c  -> return $ T.singleton c
      (s:ss) -> do
        basechar <- case M.lookup s characterCodeMap `mplus`
                         readUnicodeChar s of
                      Nothing ->
                        case T.unpack s of
                          [ch] | isAscii ch && isAlphaNum ch ->
                                 return ch
                          _ -> mzero
                      Just c  -> return c
        let addAccents [] xs = return $ Normalize.normalize Normalize.NFC $
                                        T.reverse xs
            addAccents (a:as) xs =
              case M.lookup a combiningAccentsMap `mplus` readUnicodeChar a of
                Just x  -> addAccents as $ T.cons x xs
                Nothing -> mzero
        addAccents ss (T.singleton basechar) >>= \xs -> return xs)
      <|> case delimChar of
            '['  -> escUnknown ("\\[" <> glyph <> "]")
            '('  -> escUnknown ("\\(" <> glyph)
            '\'' -> escUnknown ("\\C'" <> glyph <> "'")
            _    -> Prelude.fail "resolveGlyph: unknown glyph delimiter"

readUnicodeChar :: T.Text -> Maybe Char
readUnicodeChar t = case T.uncons t of
  Just ('u', cs) | T.length cs > 3 -> chr <$> safeRead ("0x" <> cs)
  _ -> Nothing

escapeNormal :: PandocMonad m => RoffLexer m T.Text
escapeNormal = do
  c <- noneOf "{}"
  let groffSkip = [escapeArg, countChar 1 (satisfy (/='\n'))]
  case c of
    ' ' -> return " " -- mandoc_char(7) says this should be a nonbreaking space
    '"' -> mempty <$ skipMany (satisfy (/='\n')) -- line comment
    '#' -> mempty <$ manyTill anyChar newline
    '%' -> return mempty  -- optional hyphenation
    '&' -> return mempty  -- nonprintable zero-width
    ')' -> return mempty  -- nonprintable zero-width
    '*' -> escIgnore '*' groffSkip
    ',' -> return mempty  -- to fix spacing after roman
    '-' -> return "-"
    '.' -> return "."
    '/' -> return mempty  -- to fix spacing before roman
    '0' -> return "\x2007" -- digit-width space
    ':' -> return mempty  -- zero-width break
    'A' -> escIgnore 'A' [quoteArg]
    'B' -> escIgnore 'B' [quoteArg]
    'C' -> quoteArg >>= resolveGlyph '\''
    'D' -> escIgnore 'D' [quoteArg]
    'F' -> escIgnore 'F' groffSkip
    'H' -> escIgnore 'H' [quoteArg]
    'L' -> escIgnore 'L' [quoteArg]
    'M' -> escIgnore 'M' groffSkip
    'N' -> escIgnore 'N' [quoteArg]
    'O' -> escIgnore 'O' groffSkip
    'R' -> escIgnore 'R' [quoteArg]
    'S' -> escIgnore 'S' [quoteArg]
    'V' -> escIgnore 'V' groffSkip
    'X' -> escIgnore 'X' [quoteArg]
    'Y' -> escIgnore 'Y' groffSkip
    'Z' -> escIgnore 'Z' [quoteArg]
    '\'' -> return "'"
    '\n' -> return mempty  -- line continuation
    '^' -> return "\x200A" -- 1/12 em space
    '_' -> return "_"
    '`' -> return "`"
    'a' -> return mempty  -- "non-interpreted leader character"
    'b' -> escIgnore 'b' [quoteArg]
    'c' -> return mempty  -- interrupt text processing
    'd' -> escIgnore 'd' [] -- forward down 1/2em
    'e' -> return "\\"
    'f' -> escIgnore 'f' groffSkip
    'g' -> escIgnore 'g' groffSkip
    'h' -> escIgnore 'h' [quoteArg]
    'k' -> escIgnore 'k' groffSkip
    'l' -> escIgnore 'l' [quoteArg]
    'm' -> escIgnore 'm' groffSkip
    'n' -> escIgnore 'm' groffSkip
    'o' -> escIgnore 'o' [quoteArg]
    'p' -> escIgnore 'p' []
    'r' -> escIgnore 'r' []
    's' -> escIgnore 's' [escapeArg, signedNumber]
    't' -> return "\t"
    'u' -> escIgnore 'u' []
    'v' -> escIgnore 'v' [quoteArg]
    'w' -> escIgnore 'w' [quoteArg]
    'x' -> escIgnore 'x' [quoteArg]
    'z' -> escIgnore 'z' [countChar 1 anyChar]
    '|' -> return "\x2006" --1/6 em space
    '~' -> return "\160" -- nonbreaking space
    '\\' -> return "\\"
    _   -> return $ T.singleton c
    -- man 7 groff: "If  a  backslash  is followed by a character that
    -- does not constitute a defined escape sequence, the backslash
    -- is  silently  ignored  and  the character maps to itself."

escIgnore :: PandocMonad m
          => Char
          -> [RoffLexer m T.Text]
          -> RoffLexer m T.Text
escIgnore c argparsers = do
  pos <- getPosition
  arg <- snd <$> withRaw (choice argparsers) <|> return ""
  report $ SkippedContent ("\\" <> T.cons c arg) pos
  return mempty

escUnknown :: PandocMonad m => T.Text -> RoffLexer m T.Text
escUnknown s = do
  pos <- getPosition
  report $ SkippedContent s pos
  return "\xFFFD"

signedNumber :: PandocMonad m => RoffLexer m T.Text
signedNumber = try $ do
  sign <- option "" ("-" <$ char '-' <|> "" <$ char '+')
  ds <- many1Char digit
  return (sign <> ds)

-- Parses: [..] or (..
escapeArg :: PandocMonad m => RoffLexer m T.Text
escapeArg = choice
    [ char '[' *> manyTillChar (noneOf ['\n',']']) (char ']')
    , char '(' *> countChar 2 (satisfy (/='\n'))
    ]

-- Parses: '..'
quoteArg :: PandocMonad m => RoffLexer m T.Text
quoteArg = char '\'' *> manyTillChar (noneOf ['\n','\'']) (char '\'')

-- separate function from lexMacro since real man files sometimes do not
-- follow the rules
lexComment :: PandocMonad m => RoffLexer m MdocTokens
lexComment = do
  try $ string ".\\\""
  skipMany $ noneOf "\n"
  eofline
  return mempty


argText :: PandocMonad m => RoffLexer m T.Text
argText = mconcat <$> many1 (escape <|> regularText)

spaceTabChar :: PandocMonad m => RoffLexer m T.Text
spaceTabChar = T.singleton <$> spaceChar

quotedArg :: PandocMonad m => RoffLexer m T.Text
quotedArg = do
  quoteChar
  t <- mconcat <$> many (escape <|> regularText <|> innerQuote <|> spaceTabChar)
  quoteChar
  notFollowedBy quoteChar
  return t
  where
    innerQuote = do
      string "\"\""
      return "\""

anyText :: PandocMonad m => RoffLexer m T.Text
anyText = escape <|> regularText <|> quoteChar <|> spaceTabChar

backslash :: PandocMonad m => RoffLexer m ()
backslash = 
  (mempty <* char '\\') <|> (mempty <* string "\\E")

regularText :: PandocMonad m => RoffLexer m T.Text
regularText = many1Char $ noneOf "\n\r\t \\\""

quoteChar :: PandocMonad m => RoffLexer m T.Text
quoteChar = T.singleton <$> char '"'

mdocToken :: PandocMonad m => RoffLexer m MdocTokens
mdocToken = lexComment <|> lexControlLine <|> lexTextLine

lexMacro :: PandocMonad m => RoffLexer m MdocToken
lexMacro = do
  pos <- getPosition
  name <- many1Char (satisfy isMacroChar)
  skipSpaces
  return $ Macro name pos
  where
    isMacroChar '%' = True
    isMacroChar x = isAlphaNum x

lexDelim :: PandocMonad m => RoffLexer m MdocToken
lexDelim = do
  pos <- getPosition
  t <- Delim Open <$> oneOfStrings ["(", "["] <|> Delim Close <$> oneOfStrings [".", ",", ":", ";", ")", "]", "?", "!"]
  return $ t pos

lexLit :: PandocMonad m => RoffLexer m MdocToken
lexLit = do
  pos <- getPosition
  t <- argText <|> quotedArg
  guard $ not $ T.null t
  return $ Lit t pos

lexTextLine :: PandocMonad m => RoffLexer m MdocTokens
lexTextLine = do
  pos <- getPosition
  guard $ sourceColumn pos == 1
  t <- mconcat <$> many anyText
  eofline
  return $ singleTok $ Str t pos

lexControlLine :: PandocMonad m => RoffLexer m MdocTokens
lexControlLine = do
  pos <- getPosition
  guard $ sourceColumn pos == 1
  char '.'
  m <- lexMacro
  wds <- sepBy (lexDelim <|> lexLit) spacetab
  skipSpaces
  e <- eofline
  return $ MdocTokens $ Seq.fromList $ (m:wds) <> [e]

-- | Tokenize a string as a sequence of roff tokens.
lexMdoc :: PandocMonad m => SourcePos -> T.Text -> m MdocTokens
lexMdoc pos txt = do
  eithertokens <- readWithM (do setPosition pos
                                mconcat <$> manyTill mdocToken eof) def txt
  case eithertokens of
    Left e       -> throwError e
    Right tokenz -> return tokenz
