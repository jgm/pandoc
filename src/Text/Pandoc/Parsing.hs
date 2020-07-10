{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE OverloadedStrings          #-}
{- |
   Module      : Text.Pandoc.Parsing
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

A utility library with parsers used in pandoc readers.
-}

module Text.Pandoc.Parsing ( take1WhileP,
                             takeP,
                             countChar,
                             textStr,
                             anyLine,
                             anyLineNewline,
                             indentWith,
                             manyChar,
                             many1Char,
                             manyTillChar,
                             many1TillChar,
                             many1Till,
                             manyUntil,
                             manyUntilChar,
                             sepBy1',
                             notFollowedBy',
                             oneOfStrings,
                             oneOfStringsCI,
                             spaceChar,
                             nonspaceChar,
                             skipSpaces,
                             blankline,
                             blanklines,
                             gobbleSpaces,
                             gobbleAtMostSpaces,
                             enclosed,
                             stringAnyCase,
                             parseFromString,
                             parseFromString',
                             lineClump,
                             charsInBalanced,
                             romanNumeral,
                             emailAddress,
                             uri,
                             mathInline,
                             mathDisplay,
                             withHorizDisplacement,
                             withRaw,
                             escaped,
                             characterReference,
                             upperRoman,
                             lowerRoman,
                             decimal,
                             lowerAlpha,
                             upperAlpha,
                             anyOrderedListMarker,
                             orderedListMarker,
                             charRef,
                             lineBlockLines,
                             tableWith,
                             widthsFromIndices,
                             gridTableWith,
                             gridTableWith',
                             readWith,
                             readWithM,
                             testStringWith,
                             guardEnabled,
                             guardDisabled,
                             updateLastStrPos,
                             notAfterString,
                             logMessage,
                             reportLogMessages,
                             ParserState (..),
                             HasReaderOptions (..),
                             HasIdentifierList (..),
                             HasMacros (..),
                             HasLogMessages (..),
                             HasLastStrPosition (..),
                             HasIncludeFiles (..),
                             defaultParserState,
                             HeaderType (..),
                             ParserContext (..),
                             QuoteContext (..),
                             HasQuoteContext (..),
                             NoteTable,
                             NoteTable',
                             KeyTable,
                             SubstTable,
                             Key (..),
                             toKey,
                             registerHeader,
                             smartPunctuation,
                             singleQuoteStart,
                             singleQuoteEnd,
                             doubleQuoteStart,
                             doubleQuoteEnd,
                             ellipses,
                             apostrophe,
                             dash,
                             nested,
                             citeKey,
                             Parser,
                             ParserT,
                             F,
                             Future(..),
                             runF,
                             askF,
                             asksF,
                             returnF,
                             trimInlinesF,
                             token,
                             (<+?>),
                             extractIdClass,
                             insertIncludedFile,
                             insertIncludedFileF,
                             -- * Re-exports from Text.Parsec
                             Stream,
                             runParser,
                             runParserT,
                             parse,
                             tokenPrim,
                             anyToken,
                             getInput,
                             setInput,
                             unexpected,
                             char,
                             letter,
                             digit,
                             alphaNum,
                             skipMany,
                             skipMany1,
                             spaces,
                             space,
                             anyChar,
                             satisfy,
                             newline,
                             string,
                             count,
                             eof,
                             noneOf,
                             oneOf,
                             lookAhead,
                             notFollowedBy,
                             many,
                             many1,
                             manyTill,
                             (<|>),
                             (<?>),
                             choice,
                             try,
                             sepBy,
                             sepBy1,
                             sepEndBy,
                             sepEndBy1,
                             endBy,
                             endBy1,
                             option,
                             optional,
                             optionMaybe,
                             getState,
                             setState,
                             updateState,
                             SourcePos,
                             getPosition,
                             setPosition,
                             sourceColumn,
                             sourceLine,
                             setSourceColumn,
                             setSourceLine,
                             incSourceColumn,
                             newPos,
                             Line,
                             Column
                             )
where

import Control.Monad.Identity
import Control.Monad.Reader
import Data.Char (chr, isAlphaNum, isAscii, isAsciiUpper,
                  isPunctuation, isSpace, ord, toLower, toUpper)
import Data.Default
import Data.Functor (($>))
import Data.List (intercalate, transpose)
import qualified Data.Map as M
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Set as Set
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Text.HTML.TagSoup.Entity (lookupEntity)
import Text.Pandoc.Asciify (toAsciiChar)
import Text.Pandoc.Builder (Blocks, HasMeta (..), Inlines, trimInlines)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class.PandocMonad (PandocMonad, readFileFromDirs, report)
import Text.Pandoc.Definition
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Readers.LaTeX.Types (Macro)
import Text.Pandoc.Shared
import qualified Text.Pandoc.UTF8 as UTF8 (putStrLn)
import Text.Pandoc.XML (fromEntities)
import Text.Parsec hiding (token)
import Text.Parsec.Pos (initialPos, newPos, updatePosString)

import Control.Monad.Except
import Text.Pandoc.Error

type Parser t s = Parsec t s

type ParserT = ParsecT

-- | Reader monad wrapping the parser state. This is used to possibly delay
-- evaluation until all relevant information has been parsed and made available
-- in the parser state.
newtype Future s a = Future { runDelayed :: Reader s a }
  deriving (Monad, Applicative, Functor)

type F = Future ParserState

runF :: Future s a -> s -> a
runF = runReader . runDelayed

askF :: Future s s
askF = Future ask

asksF :: (s -> a) -> Future s a
asksF f = Future $ asks f

returnF :: Monad m => a -> m (Future s a)
returnF = return . return

trimInlinesF :: Future s Inlines -> Future s Inlines
trimInlinesF = liftM trimInlines

instance Semigroup a => Semigroup (Future s a) where
  (<>) = liftM2 (<>)
instance (Semigroup a, Monoid a) => Monoid (Future s a) where
  mempty = return mempty
  mappend = (<>)

-- | Like @count@, but packs its result
countChar :: (Stream s m Char, Monad m)
          => Int
          -> ParsecT s st m Char
          -> ParsecT s st m Text
countChar n = fmap T.pack . count n

-- | Like @string@, but uses @Text@.
textStr :: Stream s m Char => Text -> ParsecT s u m Text
textStr t = string (T.unpack t) $> t

-- | Parse characters while a predicate is true.
take1WhileP :: Monad m
           => (Char -> Bool)
           -> ParserT Text st m Text
take1WhileP f = do
  -- needed to persuade parsec that this won't match an empty string:
  c <- satisfy f
  inp <- getInput
  pos <- getPosition
  let (t, rest) = T.span f inp
  setInput rest
  setPosition $
    if f '\t' || f '\n'
       then updatePosString pos $ T.unpack t
       else incSourceColumn pos (T.length t)
  return $ T.singleton c <> t

-- Parse n characters of input (or the rest of the input if
-- there aren't n characters).
takeP :: Monad m => Int -> ParserT Text st m Text
takeP n = do
  guard (n > 0)
  -- faster than 'count n anyChar'
  inp <- getInput
  pos <- getPosition
  let (xs, rest) = T.splitAt n inp
  -- needed to persuade parsec that this won't match an empty string:
  anyChar
  setInput rest
  setPosition $ updatePosString pos $ T.unpack xs
  return xs

-- | Parse any line of text
anyLine :: Monad m => ParserT Text st m Text
anyLine = do
  -- This is much faster than:
  -- manyTill anyChar newline
  inp <- getInput
  pos <- getPosition
  case T.break (=='\n') inp of
       (this, T.uncons -> Just ('\n', rest)) -> do
         -- needed to persuade parsec that this won't match an empty string:
         anyChar
         setInput rest
         setPosition $ incSourceLine (setSourceColumn pos 1) 1
         return this
       _ -> mzero

-- | Parse any line, include the final newline in the output
anyLineNewline :: Monad m => ParserT Text st m Text
anyLineNewline = (<> "\n") <$> anyLine

-- | Parse indent by specified number of spaces (or equiv. tabs)
indentWith :: Stream s m Char
           => HasReaderOptions st
           => Int -> ParserT s st m Text
indentWith num = do
  tabStop <- getOption readerTabStop
  if num < tabStop
     then countChar num (char ' ')
     else choice [ try (countChar num (char ' '))
                 , try (char '\t' >> indentWith (num - tabStop)) ]

-- | Like @many@, but packs its result.
manyChar :: Stream s m t
         => ParserT s st m Char
         -> ParserT s st m Text
manyChar = fmap T.pack . many

-- | Like @many1@, but packs its result.
many1Char :: Stream s m t
          => ParserT s st m Char
          -> ParserT s st m Text
many1Char = fmap T.pack . many1

-- | Like @manyTill@, but packs its result.
manyTillChar :: Stream s m t
             => ParserT s st m Char
             -> ParserT s st m a
             -> ParserT s st m Text
manyTillChar p = fmap T.pack . manyTill p

-- | Like @manyTill@, but reads at least one item.
many1Till :: (Show end, Stream s m t)
          => ParserT s st m a
          -> ParserT s st m end
          -> ParserT s st m [a]
many1Till p end = do
         notFollowedBy' end
         first <- p
         rest <- manyTill p end
         return (first:rest)

-- | Like @many1Till@, but packs its result
many1TillChar :: (Show end, Stream s m t)
              => ParserT s st m Char
              -> ParserT s st m end
              -> ParserT s st m Text
many1TillChar p = fmap T.pack . many1Till p

-- | Like @manyTill@, but also returns the result of end parser.
manyUntil :: ParserT s u m a
          -> ParserT s u m b
          -> ParserT s u m ([a], b)
manyUntil p end = scan
  where scan =
          (do e <- end
              return ([], e)
          ) <|>
          (do x <- p
              (xs, e) <- scan
              return (x:xs, e))

-- | Like @manyUntil@, but also packs its result.
manyUntilChar :: ParserT s u m Char
              -> ParserT s u m b
              -> ParserT s u m (Text, b)
manyUntilChar p = fmap go . manyUntil p
  where
    go (x, y) = (T.pack x, y)

-- | Like @sepBy1@ from Parsec,
-- but does not fail if it @sep@ succeeds and @p@ fails.
sepBy1' :: ParsecT s u m a
        -> ParsecT s u m sep
        -> ParsecT s u m [a]
sepBy1' p sep = (:) <$> p <*> many (try $ sep >> p)

-- | A more general form of @notFollowedBy@.  This one allows any
-- type of parser to be specified, and succeeds only if that parser fails.
-- It does not consume any input.
notFollowedBy' :: (Show b, Stream s m a) => ParserT s st m b -> ParserT s st m ()
notFollowedBy' p  = try $ join $  do  a <- try p
                                      return (unexpected (show a))
                                  <|>
                                  return (return ())
-- (This version due to Andrew Pimlott on the Haskell mailing list.)

oneOfStrings' :: Stream s m Char => (Char -> Char -> Bool) -> [Text] -> ParserT s st m Text
oneOfStrings' f = fmap T.pack . oneOfStrings'' f . fmap T.unpack

-- TODO: This should be re-implemented in a Text-aware way
oneOfStrings'' :: Stream s m Char => (Char -> Char -> Bool) -> [String] -> ParserT s st m String
oneOfStrings'' _ []   = Prelude.fail "no strings"
oneOfStrings'' matches strs = try $ do
  c <- anyChar
  let strs' = [xs | (x:xs) <- strs, x `matches` c]
  case strs' of
       []  -> Prelude.fail "not found"
       _   -> (c:) <$> oneOfStrings'' matches strs'
               <|> if "" `elem` strs'
                      then return [c]
                      else Prelude.fail "not found"

-- | Parses one of a list of strings.  If the list contains
-- two strings one of which is a prefix of the other, the longer
-- string will be matched if possible.
oneOfStrings :: Stream s m Char => [Text] -> ParserT s st m Text
oneOfStrings = oneOfStrings' (==)

-- | Parses one of a list of strings (tried in order), case insensitive.

-- TODO: This will not be accurate with general Unicode (neither
-- Text.toLower nor Text.toCaseFold can be implemented with a map)
oneOfStringsCI :: Stream s m Char => [Text] -> ParserT s st m Text
oneOfStringsCI = oneOfStrings' ciMatch
  where ciMatch x y = toLower' x == toLower' y
        -- this optimizes toLower by checking common ASCII case
        -- first, before calling the expensive unicode-aware
        -- function:
        toLower' c | isAsciiUpper c = chr (ord c + 32)
                   | isAscii c = c
                   | otherwise = toLower c

-- | Parses a space or tab.
spaceChar :: Stream s m Char => ParserT s st m Char
spaceChar = satisfy $ \c -> c == ' ' || c == '\t'

-- | Parses a nonspace, nonnewline character.
nonspaceChar :: Stream s m Char => ParserT s st m Char
nonspaceChar = noneOf ['\t', '\n', ' ', '\r']

-- | Skips zero or more spaces or tabs.
skipSpaces :: Stream s m Char => ParserT s st m ()
skipSpaces = skipMany spaceChar

-- | Skips zero or more spaces or tabs, then reads a newline.
blankline :: Stream s m Char => ParserT s st m Char
blankline = try $ skipSpaces >> newline

-- | Parses one or more blank lines and returns a string of newlines.
blanklines :: Stream s m Char => ParserT s st m Text
blanklines = T.pack <$> many1 blankline

-- | Gobble n spaces; if tabs are encountered, expand them
-- and gobble some or all of their spaces, leaving the rest.
gobbleSpaces :: (HasReaderOptions st, Monad m)
             => Int -> ParserT Text st m ()
gobbleSpaces 0 = return ()
gobbleSpaces n
  | n < 0     = error "gobbleSpaces called with negative number"
  | otherwise = try $ do
      char ' ' <|> eatOneSpaceOfTab
      gobbleSpaces (n - 1)

eatOneSpaceOfTab :: (HasReaderOptions st, Monad m) => ParserT Text st m Char
eatOneSpaceOfTab = do
  char '\t'
  tabstop <- getOption readerTabStop
  inp <- getInput
  setInput $ T.replicate (tabstop - 1) " " <> inp
  return ' '

-- | Gobble up to n spaces; if tabs are encountered, expand them
-- and gobble some or all of their spaces, leaving the rest.
gobbleAtMostSpaces :: (HasReaderOptions st, Monad m)
                   => Int -> ParserT Text st m Int
gobbleAtMostSpaces 0 = return 0
gobbleAtMostSpaces n
  | n < 0     = error "gobbleAtMostSpaces called with negative number"
  | otherwise = option 0 $ do
      char ' ' <|> eatOneSpaceOfTab
      (+ 1) <$> gobbleAtMostSpaces (n - 1)

-- | Parses material enclosed between start and end parsers.
enclosed :: (Show end, Stream s  m Char) => ParserT s st m t   -- ^ start parser
         -> ParserT s st m end  -- ^ end parser
         -> ParserT s st m a    -- ^ content parser (to be used repeatedly)
         -> ParserT s st m [a]
enclosed start end parser = try $
  start >> notFollowedBy space >> many1Till parser end

-- | Parse string, case insensitive.
stringAnyCase :: Stream s m Char => Text -> ParserT s st m Text
stringAnyCase = fmap T.pack . stringAnyCase' . T.unpack

stringAnyCase' :: Stream s m Char => String -> ParserT s st m String
stringAnyCase' [] = string ""
stringAnyCase' (x:xs) = do
  firstChar <- char (toUpper x) <|> char (toLower x)
  rest <- stringAnyCase' xs
  return (firstChar:rest)

-- | Parse contents of 'str' using 'parser' and return result.
parseFromString :: (Stream s m Char, IsString s)
                => ParserT s st m r
                -> Text
                -> ParserT s st m r
parseFromString parser str = do
  oldPos <- getPosition
  setPosition $ initialPos "chunk"
  oldInput <- getInput
  setInput $ fromString $ T.unpack str
  result <- parser
  spaces
  eof
  setInput oldInput
  setPosition oldPos
  return result

-- | Like 'parseFromString' but specialized for 'ParserState'.
-- This resets 'stateLastStrPos', which is almost always what we want.
parseFromString' :: (Stream s m Char, IsString s, HasLastStrPosition u)
                 => ParserT s u m a
                 -> Text
                 -> ParserT s u m a
parseFromString' parser str = do
  oldLastStrPos <- getLastStrPos <$> getState
  updateState $ setLastStrPos Nothing
  res <- parseFromString parser str
  updateState $ setLastStrPos oldLastStrPos
  return res

-- | Parse raw line block up to and including blank lines.
lineClump :: Monad m => ParserT Text st m Text
lineClump = blanklines
          <|> (T.unlines <$> many1 (notFollowedBy blankline >> anyLine))

-- | Parse a string of characters between an open character
-- and a close character, including text between balanced
-- pairs of open and close, which must be different. For example,
-- @charsInBalanced '(' ')' anyChar@ will parse "(hello (there))"
-- and return "hello (there)".
charsInBalanced :: Stream s m Char => Char -> Char -> ParserT s st m Char
                -> ParserT s st m Text
charsInBalanced open close parser = try $ do
  char open
  let isDelim c = c == open || c == close
  raw <- many $  T.pack <$> many1 (notFollowedBy (satisfy isDelim) >> parser)
             <|> (do res <- charsInBalanced open close parser
                     return $ T.singleton open <> res <> T.singleton close)
  char close
  return $ T.concat raw

-- old charsInBalanced would be:
-- charsInBalanced open close (noneOf "\n" <|> char '\n' >> notFollowedBy blankline)
-- old charsInBalanced' would be:
-- charsInBalanced open close anyChar

-- Auxiliary functions for romanNumeral:

-- | Parses a roman numeral (uppercase or lowercase), returns number.
romanNumeral :: Stream s m Char => Bool                  -- ^ Uppercase if true
             -> ParserT s st m Int
romanNumeral upperCase = do
    let rchar uc = char $ if upperCase then uc else toLower uc
    let one         = rchar 'I'
    let five        = rchar 'V'
    let ten         = rchar 'X'
    let fifty       = rchar 'L'
    let hundred     = rchar 'C'
    let fivehundred = rchar 'D'
    let thousand    = rchar 'M'
    lookAhead $ choice [one, five, ten, fifty, hundred, fivehundred, thousand]
    thousands <- ((1000 *) . length) <$> many thousand
    ninehundreds <- option 0 $ try $ hundred >> thousand >> return 900
    fivehundreds <- option 0 $ 500 <$ fivehundred
    fourhundreds <- option 0 $ try $ hundred >> fivehundred >> return 400
    hundreds <- ((100 *) . length) <$> many hundred
    nineties <- option 0 $ try $ ten >> hundred >> return 90
    fifties <- option 0 (50 <$ fifty)
    forties <- option 0 $ try $ ten >> fifty >> return 40
    tens <- ((10 *) . length) <$> many ten
    nines <- option 0 $ try $ one >> ten >> return 9
    fives <- option 0 (5 <$ five)
    fours <- option 0 $ try $ one >> five >> return 4
    ones <- length <$> many one
    let total = thousands + ninehundreds + fivehundreds + fourhundreds +
                hundreds + nineties + fifties + forties + tens + nines +
                fives + fours + ones
    if total == 0
       then Prelude.fail "not a roman numeral"
       else return total

-- Parsers for email addresses and URIs

-- | Parses an email address; returns original and corresponding
-- escaped mailto: URI.
emailAddress :: Stream s m Char => ParserT s st m (Text, Text)
emailAddress = try $ toResult <$> mailbox <*> (char '@' *> domain)
 where toResult mbox dom = let full = fromEntities $ T.pack $ mbox ++ '@':dom
                           in  (full, escapeURI $ "mailto:" <> full)
       mailbox           = intercalate "." <$> (emailWord `sepBy1'` dot)
       domain            = intercalate "." <$> (subdomain `sepBy1'` dot)
       dot               = char '.'
       subdomain         = many1 $ alphaNum <|> innerPunct
       -- this excludes some valid email addresses, since an
       -- email could contain e.g. '__', but gives better results
       -- for our purposes, when combined with markdown parsing:
       innerPunct        = try (satisfy (\c -> isEmailPunct c || c == '@')
                                 <* notFollowedBy space
                                 <* notFollowedBy (satisfy isPunctuation))
       -- technically an email address could begin with a symbol,
       -- but allowing this creates too many problems.
       -- See e.g. https://github.com/jgm/pandoc/issues/2940
       emailWord         = do x <- satisfy isAlphaNum
                              xs <- many (satisfy isEmailChar)
                              return (x:xs)
       isEmailChar c     = isAlphaNum c || isEmailPunct c
       isEmailPunct c    = T.any (== c) "!\"#$%&'*+-/=?^_{|}~;"


uriScheme :: Stream s m Char => ParserT s st m Text
uriScheme = oneOfStringsCI (Set.toList schemes)

-- | Parses a URI. Returns pair of original and URI-escaped version.
uri :: Stream s m Char => ParserT s st m (Text, Text)
uri = try $ do
  scheme <- uriScheme
  char ':'
  -- Avoid parsing e.g. "**Notes:**" as a raw URI:
  notFollowedBy (oneOf "*_]")
  -- We allow sentence punctuation except at the end, since
  -- we don't want the trailing '.' in 'http://google.com.' We want to allow
  -- http://en.wikipedia.org/wiki/State_of_emergency_(disambiguation)
  -- as a URL, while NOT picking up the closing paren in
  -- (http://wikipedia.org). So we include balanced parens in the URL.
  str <- T.concat <$> many1 (uriChunkBetween '(' ')'
                        <|> uriChunkBetween '{' '}'
                        <|> uriChunkBetween '[' ']'
                        <|> T.pack <$> uriChunk)
  str' <- option str $ char '/' >> return (str <> "/")
  let uri' = scheme <> ":" <> fromEntities str'
  return (uri', escapeURI uri')
  where
    wordChar = alphaNum <|> oneOf "#$%+/@\\_-&="
    percentEscaped = try $ (:) <$> char '%' <*> many1 hexDigit
    entity = try $ pure <$> characterReference
    punct = try $ many1 (char ',') <|> fmap pure (satisfy (\c -> not (isSpace c) && c /= '<' && c /= '>'))
    uriChunk = many1 wordChar
           <|> percentEscaped
           <|> entity
           <|> try (punct <* lookAhead (void wordChar <|> void percentEscaped))
    uriChunkBetween l r = try $ do chunk <- between (char l) (char r) uriChunk
                                   return (T.pack $ [l] ++ chunk ++ [r])

mathInlineWith :: Stream s m Char  => Text -> Text -> ParserT s st m Text
mathInlineWith op cl = try $ do
  textStr op
  when (op == "$") $ notFollowedBy space
  words' <- many1Till (countChar 1 (noneOf " \t\n\\")
                   <|> (char '\\' >>
                           -- This next clause is needed because \text{..} can
                           -- contain $, \(\), etc.
                           (try (string "text" >>
                                 (("\\text" <>) <$> inBalancedBraces 0 ""))
                            <|>  (\c -> T.pack ['\\',c]) <$> anyChar))
                   <|> do (blankline <* notFollowedBy' blankline) <|>
                             (oneOf " \t" <* skipMany (oneOf " \t"))
                          notFollowedBy (char '$')
                          return " "
                    ) (try $ textStr cl)
  notFollowedBy digit  -- to prevent capture of $5
  return $ trimMath $ T.concat words'
 where
  inBalancedBraces :: Stream s m Char => Int -> Text -> ParserT s st m Text
  inBalancedBraces n = fmap T.pack . inBalancedBraces' n . T.unpack

  inBalancedBraces' :: Stream s m Char => Int -> String -> ParserT s st m String
  inBalancedBraces' 0 "" = do
    c <- anyChar
    if c == '{'
       then inBalancedBraces' 1 "{"
       else mzero
  inBalancedBraces' 0 s = return $ reverse s
  inBalancedBraces' numOpen ('\\':xs) = do
    c <- anyChar
    inBalancedBraces' numOpen (c:'\\':xs)
  inBalancedBraces' numOpen xs = do
    c <- anyChar
    case c of
         '}' -> inBalancedBraces' (numOpen - 1) (c:xs)
         '{' -> inBalancedBraces' (numOpen + 1) (c:xs)
         _   -> inBalancedBraces' numOpen (c:xs)

mathDisplayWith :: Stream s m Char => Text -> Text -> ParserT s st m Text
mathDisplayWith op cl = try $ fmap T.pack $ do
  textStr op
  many1Till (noneOf "\n" <|> (newline <* notFollowedBy' blankline)) (try $ textStr cl)

mathDisplay :: (HasReaderOptions st, Stream s m Char)
            => ParserT s st m Text
mathDisplay =
      (guardEnabled Ext_tex_math_dollars >> mathDisplayWith "$$" "$$")
  <|> (guardEnabled Ext_tex_math_single_backslash >>
       mathDisplayWith "\\[" "\\]")
  <|> (guardEnabled Ext_tex_math_double_backslash >>
       mathDisplayWith "\\\\[" "\\\\]")

mathInline :: (HasReaderOptions st , Stream s m Char)
           => ParserT s st m Text
mathInline =
      (guardEnabled Ext_tex_math_dollars >> mathInlineWith "$" "$")
  <|> (guardEnabled Ext_tex_math_single_backslash >>
       mathInlineWith "\\(" "\\)")
  <|> (guardEnabled Ext_tex_math_double_backslash >>
       mathInlineWith "\\\\(" "\\\\)")

-- | Applies a parser, returns tuple of its results and its horizontal
-- displacement (the difference between the source column at the end
-- and the source column at the beginning). Vertical displacement
-- (source row) is ignored.
withHorizDisplacement :: Stream s m Char
                      => ParserT s st m a  -- ^ Parser to apply
                      -> ParserT s st m (a, Int) -- ^ (result, displacement)
withHorizDisplacement parser = do
  pos1 <- getPosition
  result <- parser
  pos2 <- getPosition
  return (result, sourceColumn pos2 - sourceColumn pos1)

-- | Applies a parser and returns the raw string that was parsed,
-- along with the value produced by the parser.
withRaw :: Monad m
        => ParsecT Text st m a
        -> ParsecT Text st m (a, Text)
withRaw parser = do
  pos1 <- getPosition
  inp <- getInput
  result <- parser
  pos2 <- getPosition
  let (l1,c1) = (sourceLine pos1, sourceColumn pos1)
  let (l2,c2) = (sourceLine pos2, sourceColumn pos2)
  let inplines = take ((l2 - l1) + 1) $ T.lines inp
  let raw = case inplines of
                []  -> ""
                [l] -> T.take (c2 - c1) l
                ls  -> T.unlines (init ls) <> T.take (c2 - 1) (last ls)
  return (result, raw)

-- | Parses backslash, then applies character parser.
escaped :: Stream s m Char
        => ParserT s st m Char  -- ^ Parser for character to escape
        -> ParserT s st m Char
escaped parser = try $ char '\\' >> parser

-- | Parse character entity.
characterReference :: Stream s m Char => ParserT s st m Char
characterReference = try $ do
  char '&'
  ent <- many1Till nonspaceChar (char ';')
  let ent' = case ent of
                  '#':'X':xs -> '#':'x':xs  -- workaround tagsoup bug
                  '#':_      -> ent
                  _          -> ent ++ ";"
  case lookupEntity ent' of
       Just (c : _) -> return c
       _            -> Prelude.fail "entity not found"

-- | Parses an uppercase roman numeral and returns (UpperRoman, number).
upperRoman :: Stream s m Char => ParserT s st m (ListNumberStyle, Int)
upperRoman = do
  num <- romanNumeral True
  return (UpperRoman, num)

-- | Parses a lowercase roman numeral and returns (LowerRoman, number).
lowerRoman :: Stream s m Char => ParserT s st m (ListNumberStyle, Int)
lowerRoman = do
  num <- romanNumeral False
  return (LowerRoman, num)

-- | Parses a decimal numeral and returns (Decimal, number).
decimal :: Stream s m Char => ParserT s st m (ListNumberStyle, Int)
decimal = do
  num <- many1 digit
  return (Decimal, fromMaybe 1 $ safeRead $ T.pack num)

-- | Parses a '@' and optional label and
-- returns (DefaultStyle, [next example number]).  The next
-- example number is incremented in parser state, and the label
-- (if present) is added to the label table.
exampleNum :: Stream s m Char
           => ParserT s ParserState m (ListNumberStyle, Int)
exampleNum = do
  char '@'
  lab <- T.pack <$> many (alphaNum <|> satisfy (\c -> c == '_' || c == '-'))
  st <- getState
  let num = stateNextExample st
  let newlabels = if T.null lab
                     then stateExamples st
                     else M.insert lab num $ stateExamples st
  updateState $ \s -> s{ stateNextExample = num + 1
                       , stateExamples    = newlabels }
  return (Example, num)

-- | Parses a '#' returns (DefaultStyle, 1).
defaultNum :: Stream s m Char => ParserT s st m (ListNumberStyle, Int)
defaultNum = do
  char '#'
  return (DefaultStyle, 1)

-- | Parses a lowercase letter and returns (LowerAlpha, number).
lowerAlpha :: Stream s m Char => ParserT s st m (ListNumberStyle, Int)
lowerAlpha = do
  ch <- oneOf ['a'..'z']
  return (LowerAlpha, ord ch - ord 'a' + 1)

-- | Parses an uppercase letter and returns (UpperAlpha, number).
upperAlpha :: Stream s m Char => ParserT s st m (ListNumberStyle, Int)
upperAlpha = do
  ch <- oneOf ['A'..'Z']
  return (UpperAlpha, ord ch - ord 'A' + 1)

-- | Parses a roman numeral i or I
romanOne :: Stream s m Char => ParserT s st m (ListNumberStyle, Int)
romanOne = (char 'i' >> return (LowerRoman, 1)) <|>
           (char 'I' >> return (UpperRoman, 1))

-- | Parses an ordered list marker and returns list attributes.
anyOrderedListMarker :: Stream s m Char => ParserT s ParserState m ListAttributes
anyOrderedListMarker = choice
  [delimParser numParser | delimParser <- [inPeriod, inOneParen, inTwoParens],
                           numParser <- [decimal, exampleNum, defaultNum, romanOne,
                           lowerAlpha, lowerRoman, upperAlpha, upperRoman]]

-- | Parses a list number (num) followed by a period, returns list attributes.
inPeriod :: Stream s m Char
         => ParserT s st m (ListNumberStyle, Int)
         -> ParserT s st m ListAttributes
inPeriod num = try $ do
  (style, start) <- num
  char '.'
  let delim = if style == DefaultStyle
                 then DefaultDelim
                 else Period
  return (start, style, delim)

-- | Parses a list number (num) followed by a paren, returns list attributes.
inOneParen :: Stream s m Char
           => ParserT s st m (ListNumberStyle, Int)
           -> ParserT s st m ListAttributes
inOneParen num = try $ do
  (style, start) <- num
  char ')'
  return (start, style, OneParen)

-- | Parses a list number (num) enclosed in parens, returns list attributes.
inTwoParens :: Stream s m Char
            => ParserT s st m (ListNumberStyle, Int)
            -> ParserT s st m ListAttributes
inTwoParens num = try $ do
  char '('
  (style, start) <- num
  char ')'
  return (start, style, TwoParens)

-- | Parses an ordered list marker with a given style and delimiter,
-- returns number.
orderedListMarker :: Stream s m Char
                  => ListNumberStyle
                  -> ListNumberDelim
                  -> ParserT s ParserState m Int
orderedListMarker style delim = do
  let num = defaultNum <|>  -- # can continue any kind of list
            case style of
               DefaultStyle -> decimal
               Example      -> exampleNum
               Decimal      -> decimal
               UpperRoman   -> upperRoman
               LowerRoman   -> lowerRoman
               UpperAlpha   -> upperAlpha
               LowerAlpha   -> lowerAlpha
  let context = case delim of
               DefaultDelim -> inPeriod
               Period       -> inPeriod
               OneParen     -> inOneParen
               TwoParens    -> inTwoParens
  (start, _, _) <- context num
  return start

-- | Parses a character reference and returns a Str element.
charRef :: Stream s m Char => ParserT s st m Inline
charRef = Str . T.singleton <$> characterReference

lineBlockLine :: Monad m => ParserT Text st m Text
lineBlockLine = try $ do
  char '|'
  char ' '
  white <- T.pack <$> many (spaceChar >> return '\160')
  notFollowedBy newline
  line <- anyLine
  continuations <- many (try $ char ' ' >> anyLine)
  return $ white <> T.unwords (line : continuations)

blankLineBlockLine :: Stream s m Char => ParserT s st m Char
blankLineBlockLine = try (char '|' >> blankline)

-- | Parses an RST-style line block and returns a list of strings.
lineBlockLines :: Monad m => ParserT Text st m [Text]
lineBlockLines = try $ do
  lines' <- many1 (lineBlockLine <|> (T.singleton <$> blankLineBlockLine))
  skipMany blankline
  return lines'

-- | Parse a table using 'headerParser', 'rowParser',
-- 'lineParser', and 'footerParser'.
tableWith :: (Stream s m Char, HasReaderOptions st, Monad mf)
          => ParserT s st m (mf [Blocks], [Alignment], [Int])
          -> ([Int] -> ParserT s st m (mf [Blocks]))
          -> ParserT s st m sep
          -> ParserT s st m end
          -> ParserT s st m (mf Blocks)
tableWith headerParser rowParser lineParser footerParser = try $ do
  (aligns, widths, heads, rows) <- tableWith' headerParser rowParser
                                                lineParser footerParser
  let th = TableHead nullAttr <$> heads
      tb = (:[]) . TableBody nullAttr 0 [] <$> rows
      tf = pure $ TableFoot nullAttr []
  return $ B.table B.emptyCaption (zip aligns (map fromWidth widths)) <$> th <*> tb <*> tf
  where
    fromWidth n
      | n > 0     = ColWidth n
      | otherwise = ColWidthDefault

type TableComponents mf = ([Alignment], [Double], mf [Row], mf [Row])

tableWith' :: (Stream s m Char, HasReaderOptions st, Monad mf)
           => ParserT s st m (mf [Blocks], [Alignment], [Int])
           -> ([Int] -> ParserT s st m (mf [Blocks]))
           -> ParserT s st m sep
           -> ParserT s st m end
           -> ParserT s st m (TableComponents mf)
tableWith' headerParser rowParser lineParser footerParser = try $ do
    (heads, aligns, indices) <- headerParser
    lines' <- sequence <$> rowParser indices `sepEndBy1` lineParser
    footerParser
    numColumns <- getOption readerColumns
    let widths = if null indices
                    then replicate (length aligns) 0.0
                    else widthsFromIndices numColumns indices
    let toRow =  Row nullAttr . map B.simpleCell
        toHeaderRow l = if null l then [] else [toRow l]
    return (aligns, widths, toHeaderRow <$> heads, map toRow <$> lines')

-- Calculate relative widths of table columns, based on indices
widthsFromIndices :: Int      -- Number of columns on terminal
                  -> [Int]    -- Indices
                  -> [Double] -- Fractional relative sizes of columns
widthsFromIndices _ [] = []
widthsFromIndices numColumns' indices =
  let numColumns = max numColumns' (if null indices then 0 else last indices)
      lengths' = zipWith (-) indices (0:indices)
      lengths  = reverse $
                 case reverse lengths' of
                      []       -> []
                      [x]      -> [x]
                      -- compensate for the fact that intercolumn
                      -- spaces are counted in widths of all columns
                      -- but the last...
                      (x:y:zs) -> if x < y && y - x <= 2
                                     then y:y:zs
                                     else x:y:zs
      totLength = sum lengths
      quotient = if totLength > numColumns
                   then fromIntegral totLength
                   else fromIntegral numColumns
      fracs = map (\l -> fromIntegral l / quotient) lengths in
  tail fracs

---

-- Parse a grid table:  starts with row of '-' on top, then header
-- (which may be grid), then the rows,
-- which may be grid, separated by blank lines, and
-- ending with a footer (dashed line followed by blank line).
gridTableWith :: (Stream s m Char, HasReaderOptions st, HasLastStrPosition st,
                  Monad mf, IsString s)
              => ParserT s st m (mf Blocks)  -- ^ Block list parser
              -> Bool                        -- ^ Headerless table
              -> ParserT s st m (mf Blocks)
gridTableWith blocks headless =
  tableWith (gridTableHeader headless blocks) (gridTableRow blocks)
            (gridTableSep '-') gridTableFooter

gridTableWith' :: (Stream s m Char, HasReaderOptions st, HasLastStrPosition st,
                   Monad mf, IsString s)
               => ParserT s st m (mf Blocks)  -- ^ Block list parser
               -> Bool                        -- ^ Headerless table
               -> ParserT s st m (TableComponents mf)
gridTableWith' blocks headless =
  tableWith' (gridTableHeader headless blocks) (gridTableRow blocks)
             (gridTableSep '-') gridTableFooter

gridTableSplitLine :: [Int] -> Text -> [Text]
gridTableSplitLine indices line = map removeFinalBar $ tail $
  splitTextByIndices (init indices) $ trimr line

gridPart :: Stream s m Char => Char -> ParserT s st m ((Int, Int), Alignment)
gridPart ch = do
  leftColon <- option False (True <$ char ':')
  dashes <- many1 (char ch)
  rightColon <- option False (True <$ char ':')
  char '+'
  let lengthDashes = length dashes + (if leftColon then 1 else 0) +
                       (if rightColon then 1 else 0)
  let alignment = case (leftColon, rightColon) of
                       (True, True)   -> AlignCenter
                       (True, False)  -> AlignLeft
                       (False, True)  -> AlignRight
                       (False, False) -> AlignDefault
  return ((lengthDashes, lengthDashes + 1), alignment)

gridDashedLines :: Stream s m Char => Char -> ParserT s st m [((Int, Int), Alignment)]
gridDashedLines ch = try $ char '+' >> many1 (gridPart ch) <* blankline

removeFinalBar :: Text -> Text
removeFinalBar = T.dropWhileEnd go . T.dropWhileEnd (=='|')
  where
    go c = T.any (== c) " \t"

-- | Separator between rows of grid table.
gridTableSep :: Stream s m Char => Char -> ParserT s st m Char
gridTableSep ch = try $ gridDashedLines ch >> return '\n'

-- | Parse header for a grid table.
gridTableHeader :: (Stream s m Char, Monad mf, IsString s, HasLastStrPosition st)
                => Bool -- ^ Headerless table
                -> ParserT s st m (mf Blocks)
                -> ParserT s st m (mf [Blocks], [Alignment], [Int])
gridTableHeader headless blocks = try $ do
  optional blanklines
  dashes <- gridDashedLines '-'
  rawContent  <- if headless
                    then return $ repeat ""
                    else many1
                         (notFollowedBy (gridTableSep '=') >> char '|' >>
                           T.pack <$> many1Till anyChar newline)
  underDashes <- if headless
                    then return dashes
                    else gridDashedLines '='
  guard $ length dashes == length underDashes
  let lines'   = map (snd . fst) underDashes
  let indices  = scanl (+) 0 lines'
  let aligns   = map snd underDashes
  let rawHeads = if headless
                    then replicate (length underDashes) ""
                    else map (T.unlines . map trim) $ transpose
                       $ map (gridTableSplitLine indices) rawContent
  heads <- sequence <$> mapM (parseFromString' blocks . trim) rawHeads
  return (heads, aligns, indices)

gridTableRawLine :: Stream s m Char => [Int] -> ParserT s st m [Text]
gridTableRawLine indices = do
  char '|'
  line <- many1Till anyChar newline
  return (gridTableSplitLine indices $ T.pack line)

-- | Parse row of grid table.
gridTableRow :: (Stream s m Char, Monad mf, IsString s, HasLastStrPosition st)
             => ParserT s st m (mf Blocks)
             -> [Int]
             -> ParserT s st m (mf [Blocks])
gridTableRow blocks indices = do
  colLines <- many1 (gridTableRawLine indices)
  let cols = map ((<> "\n") . T.unlines . removeOneLeadingSpace) $
               transpose colLines
      compactifyCell bs = case compactify [bs] of
                            []  -> mempty
                            x:_ -> x
  cells <- sequence <$> mapM (parseFromString' blocks) cols
  return $ fmap (map compactifyCell) cells

removeOneLeadingSpace :: [Text] -> [Text]
removeOneLeadingSpace xs =
  if all startsWithSpace xs
     then map (T.drop 1) xs
     else xs
   where startsWithSpace t = case T.uncons t of
           Nothing     -> True
           Just (c, _) -> c == ' '

-- | Parse footer for a grid table.
gridTableFooter :: Stream s m Char => ParserT s st m ()
gridTableFooter = optional blanklines

---

-- | Removes the ParsecT layer from the monad transformer stack
readWithM :: (Stream s m Char, ToText s)
          => ParserT s st m a    -- ^ parser
          -> st                  -- ^ initial state
          -> s                   -- ^ input
          -> m (Either PandocError a)
readWithM parser state input =
    mapLeft (PandocParsecError $ toText input) `liftM` runParserT parser state "source" input

-- | Parse a string with a given parser and state
readWith :: Parser Text st a
         -> st
         -> Text
         -> Either PandocError a
readWith p t inp = runIdentity $ readWithM p t inp

-- | Parse a string with @parser@ (for testing).
testStringWith :: Show a
               => ParserT Text ParserState Identity a
               -> Text
               -> IO ()
testStringWith parser str = UTF8.putStrLn $ show $
                            readWith parser defaultParserState str

-- | Parsing options.
data ParserState = ParserState
    { stateOptions           :: ReaderOptions, -- ^ User options
      stateParserContext     :: ParserContext, -- ^ Inside list?
      stateQuoteContext      :: QuoteContext,  -- ^ Inside quoted environment?
      stateAllowLinks        :: Bool,          -- ^ Allow parsing of links
      stateAllowLineBreaks   :: Bool,          -- ^ Allow parsing of line breaks
      stateMaxNestingLevel   :: Int,           -- ^ Max # of nested Strong/Emph
      stateLastStrPos        :: Maybe SourcePos, -- ^ Position after last str parsed
      stateKeys              :: KeyTable,      -- ^ List of reference keys
      stateHeaderKeys        :: KeyTable,      -- ^ List of implicit header ref keys
      stateSubstitutions     :: SubstTable,    -- ^ List of substitution references
      stateNotes             :: NoteTable,     -- ^ List of notes (raw bodies)
      stateNotes'            :: NoteTable',    -- ^ List of notes (parsed bodies)
      stateNoteRefs          :: Set.Set Text, -- ^ List of note references used
      stateMeta              :: Meta,          -- ^ Document metadata
      stateMeta'             :: F Meta,        -- ^ Document metadata
      stateCitations         :: M.Map Text Text, -- ^ RST-style citations
      stateHeaderTable       :: [HeaderType],  -- ^ Ordered list of header types used
      stateIdentifiers       :: Set.Set Text, -- ^ Header identifiers used
      stateNextExample       :: Int,           -- ^ Number of next example
      stateExamples          :: M.Map Text Int, -- ^ Map from example labels to numbers
      stateMacros            :: M.Map Text Macro, -- ^ Table of macros defined so far
      stateRstDefaultRole    :: Text,        -- ^ Current rST default interpreted text role
      stateRstHighlight      :: Maybe Text,  -- ^ Current rST literal block language
      stateRstCustomRoles    :: M.Map Text (Text, Maybe Text, Attr), -- ^ Current rST custom text roles
      -- Triple represents: 1) Base role, 2) Optional format (only for :raw:
      -- roles), 3) Additional classes (rest of Attr is unused)).
      stateCaption           :: Maybe Inlines, -- ^ Caption in current environment
      stateInHtmlBlock       :: Maybe Text,  -- ^ Tag type of HTML block being parsed
      stateFencedDivLevel    :: Int,           -- ^ Depth of fenced div
      stateContainers        :: [Text],      -- ^ parent include files
      stateLogMessages       :: [LogMessage],  -- ^ log messages
      stateMarkdownAttribute :: Bool         -- ^ True if in markdown=1 context
    }

instance Default ParserState where
  def = defaultParserState

instance HasMeta ParserState where
  setMeta field val st =
    st{ stateMeta = setMeta field val $ stateMeta st }
  deleteMeta field st =
    st{ stateMeta = deleteMeta field $ stateMeta st }

class HasReaderOptions st where
  extractReaderOptions :: st -> ReaderOptions
  getOption            :: (Stream s m t) => (ReaderOptions -> b) -> ParserT s st m b
  -- default
  getOption  f         = (f . extractReaderOptions) <$> getState

instance HasReaderOptions ParserState where
  extractReaderOptions = stateOptions

class HasQuoteContext st m where
  getQuoteContext :: (Stream s m t) => ParsecT s st m QuoteContext
  withQuoteContext :: QuoteContext -> ParsecT s st m a -> ParsecT s st m a

instance Monad m => HasQuoteContext ParserState m where
  getQuoteContext = stateQuoteContext <$> getState
  withQuoteContext context parser = do
    oldState <- getState
    let oldQuoteContext = stateQuoteContext oldState
    setState oldState { stateQuoteContext = context }
    result <- parser
    newState <- getState
    setState newState { stateQuoteContext = oldQuoteContext }
    return result

class HasIdentifierList st where
  extractIdentifierList  :: st -> Set.Set Text
  updateIdentifierList   :: (Set.Set Text -> Set.Set Text) -> st -> st

instance HasIdentifierList ParserState where
  extractIdentifierList     = stateIdentifiers
  updateIdentifierList f st = st{ stateIdentifiers = f $ stateIdentifiers st }

class HasMacros st where
  extractMacros         :: st -> M.Map Text Macro
  updateMacros          :: (M.Map Text Macro -> M.Map Text Macro) -> st -> st

instance HasMacros ParserState where
  extractMacros        = stateMacros
  updateMacros f st    = st{ stateMacros = f $ stateMacros st }

class HasLastStrPosition st where
  setLastStrPos  :: Maybe SourcePos -> st -> st
  getLastStrPos  :: st -> Maybe SourcePos

instance HasLastStrPosition ParserState where
  setLastStrPos pos st = st{ stateLastStrPos = pos }
  getLastStrPos st     = stateLastStrPos st

class HasLogMessages st where
  addLogMessage :: LogMessage -> st -> st
  getLogMessages :: st -> [LogMessage]

instance HasLogMessages ParserState where
  addLogMessage msg st = st{ stateLogMessages = msg : stateLogMessages st }
  getLogMessages st = reverse $ stateLogMessages st

class HasIncludeFiles st where
  getIncludeFiles :: st -> [Text]
  addIncludeFile :: Text -> st -> st
  dropLatestIncludeFile :: st -> st

instance HasIncludeFiles ParserState where
  getIncludeFiles = stateContainers
  addIncludeFile f s = s{ stateContainers = f : stateContainers s }
  dropLatestIncludeFile s = s { stateContainers = drop 1 $ stateContainers s }

defaultParserState :: ParserState
defaultParserState =
    ParserState { stateOptions         = def,
                  stateParserContext   = NullState,
                  stateQuoteContext    = NoQuote,
                  stateAllowLinks      = True,
                  stateAllowLineBreaks = True,
                  stateMaxNestingLevel = 6,
                  stateLastStrPos      = Nothing,
                  stateKeys            = M.empty,
                  stateHeaderKeys      = M.empty,
                  stateSubstitutions   = M.empty,
                  stateNotes           = [],
                  stateNotes'          = M.empty,
                  stateNoteRefs        = Set.empty,
                  stateMeta            = nullMeta,
                  stateMeta'           = return nullMeta,
                  stateCitations       = M.empty,
                  stateHeaderTable     = [],
                  stateIdentifiers     = Set.empty,
                  stateNextExample     = 1,
                  stateExamples        = M.empty,
                  stateMacros          = M.empty,
                  stateRstDefaultRole  = "title-reference",
                  stateRstHighlight    = Nothing,
                  stateRstCustomRoles  = M.empty,
                  stateCaption         = Nothing,
                  stateInHtmlBlock     = Nothing,
                  stateFencedDivLevel  = 0,
                  stateContainers      = [],
                  stateLogMessages     = [],
                  stateMarkdownAttribute = False
                  }

-- | Add a log message.
logMessage :: (Stream s m a, HasLogMessages st)
           => LogMessage -> ParserT s st m ()
logMessage msg = updateState (addLogMessage msg)

-- | Report all the accumulated log messages, according to verbosity level.
reportLogMessages :: (PandocMonad m, HasLogMessages st) => ParserT s st m ()
reportLogMessages = do
  msgs <- getLogMessages <$> getState
  mapM_ report msgs

-- | Succeed only if the extension is enabled.
guardEnabled :: (Stream s m a,  HasReaderOptions st) => Extension -> ParserT s st m ()
guardEnabled ext = getOption readerExtensions >>= guard . extensionEnabled ext

-- | Succeed only if the extension is disabled.
guardDisabled :: (Stream s m a, HasReaderOptions st) => Extension -> ParserT s st m ()
guardDisabled ext = getOption readerExtensions >>= guard . not . extensionEnabled ext

-- | Update the position on which the last string ended.
updateLastStrPos :: (Stream s m a, HasLastStrPosition st) => ParserT s st m ()
updateLastStrPos = getPosition >>= updateState . setLastStrPos . Just

-- | Whether we are right after the end of a string.
notAfterString :: (Stream s m a, HasLastStrPosition st) => ParserT s st m Bool
notAfterString = do
  pos <- getPosition
  st  <- getState
  return $ getLastStrPos st /= Just pos

data HeaderType
    = SingleHeader Char  -- ^ Single line of characters underneath
    | DoubleHeader Char  -- ^ Lines of characters above and below
    deriving (Eq, Show)

data ParserContext
    = ListItemState   -- ^ Used when running parser on list item contents
    | NullState       -- ^ Default state
    deriving (Eq, Show)

data QuoteContext
    = InSingleQuote   -- ^ Used when parsing inside single quotes
    | InDoubleQuote   -- ^ Used when parsing inside double quotes
    | NoQuote         -- ^ Used when not parsing inside quotes
    deriving (Eq, Show)

type NoteTable = [(Text, Text)]

type NoteTable' = M.Map Text (SourcePos, F Blocks)
-- used in markdown reader

newtype Key = Key Text deriving (Show, Read, Eq, Ord)

toKey :: Text -> Key
toKey = Key . T.toLower . T.unwords . T.words . unbracket
  where unbracket t
          | Just ('[', t') <- T.uncons t
          , Just (t'', ']') <- T.unsnoc t'
          = t''
          | otherwise
          = t

type KeyTable = M.Map Key (Target, Attr)

type SubstTable = M.Map Key Inlines

--  | Add header to the list of headers in state, together
--  with its associated identifier.  If the identifier is null
--  and the auto_identifiers extension is set, generate a new
--  unique identifier, and update the list of identifiers
--  in state.  Issue a warning if an explicit identifier
--  is encountered that duplicates an earlier identifier
--  (explicit or automatically generated).
registerHeader :: (Stream s m a, HasReaderOptions st,
                   HasLogMessages st, HasIdentifierList st)
               => Attr -> Inlines -> ParserT s st m Attr
registerHeader (ident,classes,kvs) header' = do
  ids <- extractIdentifierList <$> getState
  exts <- getOption readerExtensions
  if T.null ident && Ext_auto_identifiers `extensionEnabled` exts
     then do
       let id' = uniqueIdent exts (B.toList header') ids
       let id'' = if Ext_ascii_identifiers `extensionEnabled` exts
                     then T.pack $ mapMaybe toAsciiChar $ T.unpack id'
                     else id'
       updateState $ updateIdentifierList $ Set.insert id'
       updateState $ updateIdentifierList $ Set.insert id''
       return (id'',classes,kvs)
     else do
        unless (T.null ident) $ do
          when (ident `Set.member` ids) $ do
            pos <- getPosition
            logMessage $ DuplicateIdentifier ident pos
          updateState $ updateIdentifierList $ Set.insert ident
        return (ident,classes,kvs)

smartPunctuation :: (HasReaderOptions st, HasLastStrPosition st, HasQuoteContext st m, Stream s m Char)
                 => ParserT s st m Inlines
                 -> ParserT s st m Inlines
smartPunctuation inlineParser = do
  guardEnabled Ext_smart
  choice [ quoted inlineParser, apostrophe, dash, ellipses ]

apostrophe :: Stream s m Char => ParserT s st m Inlines
apostrophe = (char '\'' <|> char '\8217') >> return (B.str "\x2019")

quoted :: (HasLastStrPosition st, HasQuoteContext st m, Stream s m Char)
       => ParserT s st m Inlines
       -> ParserT s st m Inlines
quoted inlineParser = doubleQuoted inlineParser <|> singleQuoted inlineParser

singleQuoted :: (HasLastStrPosition st, HasQuoteContext st m, Stream s m Char)
             => ParserT s st m Inlines
             -> ParserT s st m Inlines
singleQuoted inlineParser = try $ B.singleQuoted . mconcat
  <$  singleQuoteStart
  <*> withQuoteContext InSingleQuote (many1Till inlineParser singleQuoteEnd)

doubleQuoted :: (HasQuoteContext st m, Stream s m Char)
             => ParserT s st m Inlines
             -> ParserT s st m Inlines
doubleQuoted inlineParser = try $ B.doubleQuoted . mconcat
  <$  doubleQuoteStart
  <*> withQuoteContext InDoubleQuote (manyTill inlineParser doubleQuoteEnd)

failIfInQuoteContext :: (HasQuoteContext st m, Stream s m t)
                     => QuoteContext
                     -> ParserT s st m ()
failIfInQuoteContext context = do
  context' <- getQuoteContext
  when (context' == context) $ Prelude.fail "already inside quotes"

charOrRef :: Stream s m Char => [Char] -> ParserT s st m Char
charOrRef cs =
  oneOf cs <|> try (do c <- characterReference
                       guard (c `elem` cs)
                       return c)

singleQuoteStart :: (HasLastStrPosition st, HasQuoteContext st m, Stream s m Char)
                 => ParserT s st m ()
singleQuoteStart = do
  failIfInQuoteContext InSingleQuote
  -- single quote start can't be right after str
  guard =<< notAfterString
  try $ do
    charOrRef "'\8216\145"
    notFollowedBy (oneOf [' ', '\t', '\n'])

singleQuoteEnd :: Stream s m Char
               => ParserT s st m ()
singleQuoteEnd = try $ do
  charOrRef "'\8217\146"
  notFollowedBy alphaNum

doubleQuoteStart :: (HasQuoteContext st m, Stream s m Char)
                 => ParserT s st m ()
doubleQuoteStart = do
  failIfInQuoteContext InDoubleQuote
  try $ do charOrRef "\"\8220\147"
           notFollowedBy (oneOf [' ', '\t', '\n'])

doubleQuoteEnd :: Stream s m Char
               => ParserT s st m ()
doubleQuoteEnd = void (charOrRef "\"\8221\148")

ellipses :: Stream s m Char
         => ParserT s st m Inlines
ellipses = try (string "..." >> return (B.str "\8230"))

dash :: (HasReaderOptions st, Stream s m Char)
     => ParserT s st m Inlines
dash = try $ do
  oldDashes <- extensionEnabled Ext_old_dashes <$> getOption readerExtensions
  if oldDashes
     then do
       char '-'
       (char '-' >> return (B.str "\8212"))
         <|> (lookAhead digit >> return (B.str "\8211"))
     else do
       string "--"
       (char '-' >> return (B.str "\8212"))
         <|> return (B.str "\8211")

-- This is used to prevent exponential blowups for things like:
-- a**a*a**a*a**a*a**a*a**a*a**a*a**
nested :: Stream s m a
       => ParserT s ParserState m a
       -> ParserT s ParserState m a
nested p = do
  nestlevel <- stateMaxNestingLevel <$>  getState
  guard $ nestlevel > 0
  updateState $ \st -> st{ stateMaxNestingLevel = stateMaxNestingLevel st - 1 }
  res <- p
  updateState $ \st -> st{ stateMaxNestingLevel = nestlevel }
  return res

citeKey :: (Stream s m Char, HasLastStrPosition st)
        => ParserT s st m (Bool, Text)
citeKey = try $ do
  guard =<< notAfterString
  suppress_author <- option False (True <$ char '-')
  char '@'
  firstChar <- alphaNum <|> char '_' <|> char '*' -- @* for wildcard in nocite
  let regchar = satisfy (\c -> isAlphaNum c || c == '_')
  let internal p = try $ p <* lookAhead regchar
  rest <- many $ regchar <|> internal (oneOf ":.#$%&-+?<>~/") <|>
                 try (oneOf ":/" <* lookAhead (char '/'))
  let key = firstChar:rest
  return (suppress_author, T.pack key)


token :: (Stream s m t)
      => (t -> Text)
      -> (t -> SourcePos)
      -> (t -> Maybe a)
      -> ParsecT s st m a
token pp pos match = tokenPrim (T.unpack . pp) (\_ t _ -> pos t) match

infixr 5 <+?>
(<+?>) :: (Monoid a) => ParserT s st m a -> ParserT s st m a -> ParserT s st m a
a <+?> b = a >>= flip fmap (try b <|> return mempty) . mappend

extractIdClass :: Attr -> Attr
extractIdClass (ident, cls, kvs) = (ident', cls', kvs')
  where
    ident' = fromMaybe ident (lookup "id" kvs)
    cls'   = case lookup "class" kvs of
               Just cl -> T.words cl
               Nothing -> cls
    kvs'  = filter (\(k,_) -> k /= "id" || k /= "class") kvs

insertIncludedFile' :: (PandocMonad m, HasIncludeFiles st)
                    => ParserT a st m (mf Blocks)
                    -> (Text -> a)
                    -> [FilePath] -> FilePath
                    -> ParserT a st m (mf Blocks)
insertIncludedFile' blocks totoks dirs f = do
  oldPos <- getPosition
  oldInput <- getInput
  containers <- getIncludeFiles <$> getState
  when (T.pack f `elem` containers) $
    throwError $ PandocParseError $ T.pack $ "Include file loop at " ++ show oldPos
  updateState $ addIncludeFile $ T.pack f
  mbcontents <- readFileFromDirs dirs f
  contents <- case mbcontents of
                   Just s -> return s
                   Nothing -> do
                     report $ CouldNotLoadIncludeFile (T.pack f) oldPos
                     return ""
  setPosition $ newPos f 1 1
  setInput $ totoks contents
  bs <- blocks
  setInput oldInput
  setPosition oldPos
  updateState dropLatestIncludeFile
  return bs

-- | Parse content of include file as blocks. Circular includes result in an
-- @PandocParseError@.
insertIncludedFile :: (PandocMonad m, HasIncludeFiles st)
                   => ParserT [a] st m Blocks
                   -> (Text -> [a])
                   -> [FilePath] -> FilePath
                   -> ParserT [a] st m Blocks
insertIncludedFile blocks totoks dirs f =
  runIdentity <$> insertIncludedFile' (Identity <$> blocks) totoks dirs f

-- | Parse content of include file as future blocks. Circular includes result in
-- an @PandocParseError@.
insertIncludedFileF :: (PandocMonad m, HasIncludeFiles st)
                    => ParserT Text st m (Future st Blocks)
                    -> [FilePath] -> FilePath
                    -> ParserT Text st m (Future st Blocks)
insertIncludedFileF p = insertIncludedFile' p id
