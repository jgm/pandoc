{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module Text.Pandoc.Readers.Roff.Escape
  ( escape,
    escapeArg,
    escIgnore,
    RoffLikeLexer(..),
  )
where
import Text.Pandoc.Class.PandocMonad
    ( PandocMonad(..), report, PandocMonad(..), report )
import Control.Monad
    ( mzero, mplus, mzero, mplus )
import Data.Char (chr, isAscii, isAlphaNum)
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Pandoc.Logging (LogMessage(..))
import Text.Pandoc.Parsing
import Text.Pandoc.Shared (safeRead)
import qualified Data.Text.Normalize as Normalize
import Text.Pandoc.RoffChar (characterCodes, combiningAccents)

-- | Functions and typeclass for escaping special characters in languages
-- that inherit the Roff syntax.
--
-- For various reasons, the mdoc reader doesn't directly reuse the previously
-- existing roff lexer. The main one is to make it possible to simultaneously
-- process roff escapes and to tokenize mdoc macros correctly based on the
-- control line contents. The extracted interface here allows the same `escape`
-- function to work with lexers that target different token types and support
-- different subsets of the original roff language.

type Lexer m x = ParsecT Sources (State x) m

-- | Lexers for Roff macro
class (Monoid (Token x)) => RoffLikeLexer x where
  -- | Type family for the lexer state
  type State x = a | a -> x
  -- | Type family for the token type being lexed
  type Token x = a | a -> x
  -- | Turn a `T.Text` into a token of the output
  emit :: T.Text -> Token x
  -- | Attempt to parse a roff predefined string sequence and push its expansion
  -- onto the input stream.
  expandString :: PandocMonad m => Lexer m x ()
  -- | Parse the name of a defined string and return its expansion as a `Token`
  escString :: PandocMonad m => Lexer m x (Token x)
  -- | Parse the escape character
  backslash :: PandocMonad m => Lexer m x ()
  -- | If the given custom macro is defined in this document, emit a
  -- tokenized "1", otherwise emit a tokenized "0", implementing the roff
  -- escape @\A@.
  checkDefined :: PandocMonad m => T.Text -> Lexer m x (Token x)
  -- | Output an appropriate token for the @\E@ escape sequence. In roff, @\E@
  -- is an "escape character intended to not be interpreted in copy mode".
  -- If the lexer being defined doesn't implement copy mode, @\E@ can just be
  -- lexed by 'backslash'
  escE :: PandocMonad m => Lexer m x (Token x)
  -- | Lex the low-level roff font selection escape @\f@.
  escFont :: PandocMonad m => Lexer m x (Token x)


characterCodeMap :: M.Map T.Text Char
characterCodeMap =
  M.fromList $ map (\(x,y) -> (y,x)) characterCodes

combiningAccentsMap :: M.Map T.Text Char
combiningAccentsMap =
  M.fromList $ map (\(x,y) -> (y,x)) combiningAccents

escape ::  (PandocMonad m, RoffLikeLexer x) => Lexer m x (Token x)
escape = try $ do
  backslash
  escapeGlyph <|> escapeNormal

escapeGlyph :: (PandocMonad m, RoffLikeLexer x) => Lexer m x (Token x)
escapeGlyph = do
  c <- lookAhead (oneOf ['[','('])
  escapeArg >>= resolveGlyph c

resolveGlyph :: (PandocMonad m, RoffLikeLexer x) => Char -> T.Text -> Lexer m x (Token x)
resolveGlyph delimChar glyph = do
  let cs = T.replace "_u" " u" glyph -- unicode glyphs separated by _
  (case T.words cs of
      []  -> mzero
      [s] -> case M.lookup s characterCodeMap `mplus` readUnicodeChar s of
               Nothing -> mzero
               Just c  -> return $ emit $ T.singleton c
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
        addAccents ss (T.singleton basechar) >>= \xs -> return $ emit xs)
      <|> case delimChar of
            '['  -> escUnknown ("\\[" <> glyph <> "]")
            '('  -> escUnknown ("\\(" <> glyph)
            '\'' -> escUnknown ("\\C'" <> glyph <> "'")
            _    -> Prelude.fail "resolveGlyph: unknown glyph delimiter"

readUnicodeChar :: T.Text -> Maybe Char
readUnicodeChar t = case T.uncons t of
  Just ('u', cs) | T.length cs > 3 -> chr <$> safeRead ("0x" <> cs)
  _ -> Nothing

escapeNormal :: (PandocMonad m, RoffLikeLexer x) => Lexer m x (Token x)
escapeNormal = do
  c <- noneOf "{}"
  optional expandString
  let groffSkip = [escapeArg, countChar 1 (satisfy (/='\n'))]
  case c of
    ' ' -> return $ emit " " -- mandoc_char(7) says this should be a nonbreaking space
    '"' -> mempty <$ skipMany (satisfy (/='\n')) -- line comment
    '#' -> mempty <$ manyTill anyChar newline
    '%' -> return mempty  -- optional hyphenation
    '&' -> return mempty  -- nonprintable zero-width
    ')' -> return mempty  -- nonprintable zero-width
    '*' -> escString
    ',' -> return mempty  -- to fix spacing after roman
    '-' -> return $ emit "-"
    '.' -> return $ emit "."
    '/' -> return mempty  -- to fix spacing before roman
    '0' -> return $ emit "\x2007" -- digit-width space
    ':' -> return mempty  -- zero-width break
    'A' -> quoteArg >>= checkDefined
    'B' -> escIgnore 'B' [quoteArg]
    'C' -> quoteArg >>= resolveGlyph '\''
    'D' -> escIgnore 'D' [quoteArg]
    'E' -> escE
    'F' -> escIgnore 'F' groffSkip
    'H' -> escIgnore 'H' [quoteArg]
    'L' -> escIgnore 'L' [quoteArg]
    'M' -> escIgnore 'M' groffSkip
    'N' -> escIgnore 'N' [quoteArg]
    'O' -> escIgnore 'O' [countChar 1 (oneOf ['0','1'])]
    'R' -> escIgnore 'R' [quoteArg]
    'S' -> escIgnore 'S' [quoteArg]
    'V' -> escIgnore 'V' groffSkip
    'X' -> escIgnore 'X' [quoteArg]
    'Y' -> escIgnore 'Y' groffSkip
    'Z' -> escIgnore 'Z' [quoteArg]
    '\'' -> return $ emit "'"
    '\n' -> return mempty  -- line continuation
    '^' -> return $ emit "\x200A" -- 1/12 em space
    '_' -> return $ emit "_"
    '`' -> return $ emit "`"
    'a' -> return mempty  -- "non-interpreted leader character"
    'b' -> escIgnore 'b' [quoteArg]
    'c' -> return mempty  -- interrupt text processing
    'd' -> escIgnore 'd' [] -- forward down 1/2em
    'e' -> return $ emit "\\"
    'f' -> escFont
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
    't' -> return $ emit "\t"
    'u' -> escIgnore 'u' []
    'v' -> escIgnore 'v' [quoteArg]
    'w' -> escIgnore 'w' [quoteArg]
    'x' -> escIgnore 'x' [quoteArg]
    'z' -> escIgnore 'z' [countChar 1 anyChar]
    '|' -> return $ emit "\x2006" --1/6 em space
    '~' -> return $ emit "\160" -- nonbreaking space
    '\\' -> return $ emit "\\"
    _   -> return $ emit $ T.singleton c
    -- man 7 groff: "If  a  backslash  is followed by a character that
    -- does not constitute a defined escape sequence, the backslash
    -- is  silently  ignored  and  the character maps to itself."

escIgnore :: (PandocMonad m, RoffLikeLexer x)
          => Char
          -> [Lexer m x T.Text]
          -> Lexer m x (Token x)
escIgnore c argparsers = do
  pos <- getPosition
  arg <- snd <$> withRaw (choice argparsers) <|> return ""
  report $ SkippedContent ("\\" <> T.cons c arg) pos
  return mempty

escUnknown :: (PandocMonad m, RoffLikeLexer x) => T.Text -> Lexer m x (Token x)
escUnknown s = do
  pos <- getPosition
  report $ SkippedContent s pos
  return $ emit "\xFFFD"

signedNumber :: (PandocMonad m, RoffLikeLexer x) => Lexer m x T.Text
signedNumber = try $ do
  sign <- option "" ("-" <$ char '-' <|> "" <$ char '+')
  ds <- many1Char digit
  return (sign <> ds)

-- Parses: [..] or (..
escapeArg :: (PandocMonad m, RoffLikeLexer x) => Lexer m x T.Text
escapeArg = choice
    [ char '[' *> optional expandString *>
                  manyTillChar (noneOf ['\n',']']) (char ']')
    , char '(' *> optional expandString *>
                  countChar 2 (satisfy (/='\n'))
    ]

-- Parses: '..'
quoteArg :: (PandocMonad m, RoffLikeLexer x) => Lexer m x T.Text
quoteArg = char '\'' *> manyTillChar (noneOf ['\n','\'']) (char '\'')
