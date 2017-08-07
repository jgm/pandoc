{-# LANGUAGE
  FlexibleContexts
, GeneralizedNewtypeDeriving
, TypeSynonymInstances
, MultiParamTypeClasses
, FlexibleInstances
, IncoherentInstances #-}

{-
Copyright (C) 2006-2017 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Parsing
   Copyright   : Copyright (C) 2006-2017 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

A utility library with parsers used in pandoc readers.
-}
module Text.Pandoc.Parsing ( takeWhileP,
                             takeP,
                             anyLine,
                             anyLineNewline,
                             indentWith,
                             many1Till,
                             notFollowedBy',
                             oneOfStrings,
                             oneOfStringsCI,
                             spaceChar,
                             nonspaceChar,
                             skipSpaces,
                             blankline,
                             blanklines,
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
                             HasHeaderMap (..),
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
                             -- * Re-exports from Text.Pandoc.Parsec
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
                             newPos,
                             Line,
                             Column
                             )
where

import Data.Text (Text)
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Builder (Blocks, Inlines, HasMeta(..), trimInlines)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.XML (fromEntities)
import qualified Text.Pandoc.UTF8 as UTF8 (putStrLn)
import Text.Parsec hiding (token)
import Text.Parsec.Pos (newPos, initialPos, updatePosString)
import Data.Char ( toLower, toUpper, ord, chr, isAscii, isAlphaNum,
                   isHexDigit, isSpace, isPunctuation )
import Data.List ( intercalate, transpose, isSuffixOf )
import Text.Pandoc.Shared
import qualified Data.Map as M
import Text.Pandoc.Readers.LaTeX.Types (Macro)
import Text.HTML.TagSoup.Entity ( lookupEntity )
import Text.Pandoc.Asciify (toAsciiChar)
import Data.Monoid ((<>))
import Text.Pandoc.Class (PandocMonad, readFileFromDirs, report)
import Text.Pandoc.Logging
import Data.Default
import qualified Data.Set as Set
import Control.Monad.Reader
import Control.Monad.Identity
import Data.Maybe (catMaybes)

import Text.Pandoc.Error
import Control.Monad.Except

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

instance Monoid a => Monoid (Future s a) where
  mempty = return mempty
  mappend = liftM2 mappend
  mconcat = liftM mconcat . sequence

-- | Parse characters while a predicate is true.
takeWhileP :: Monad m
           => (Char -> Bool)
           -> ParserT [Char] st m [Char]
takeWhileP f = do
  -- faster than 'many (satisfy f)'
  inp <- getInput
  pos <- getPosition
  let (xs, rest) = span f inp
  -- needed to persuade parsec that this won't match an empty string:
  anyChar
  setInput rest
  setPosition $ updatePosString pos xs
  return xs

-- Parse n characters of input (or the rest of the input if
-- there aren't n characters).
takeP :: Monad m => Int -> ParserT [Char] st m [Char]
takeP n = do
  guard (n > 0)
  -- faster than 'count n anyChar'
  inp <- getInput
  pos <- getPosition
  let (xs, rest) = splitAt n inp
  -- needed to persuade parsec that this won't match an empty string:
  anyChar
  setInput rest
  setPosition $ updatePosString pos xs
  return xs

-- | Parse any line of text
anyLine :: Monad m => ParserT [Char] st m [Char]
anyLine = do
  -- This is much faster than:
  -- manyTill anyChar newline
  inp <- getInput
  pos <- getPosition
  case break (=='\n') inp of
       (this, '\n':rest) -> do
         -- needed to persuade parsec that this won't match an empty string:
         anyChar
         setInput rest
         setPosition $ incSourceLine (setSourceColumn pos 1) 1
         return this
       _ -> mzero

-- | Parse any line, include the final newline in the output
anyLineNewline :: Monad m => ParserT [Char] st m [Char]
anyLineNewline = (++ "\n") <$> anyLine

-- | Parse indent by specified number of spaces (or equiv. tabs)
indentWith :: Stream s m Char
           => HasReaderOptions st
           => Int -> ParserT s st m [Char]
indentWith num = do
  tabStop <- getOption readerTabStop
  if (num < tabStop)
     then count num (char ' ')
     else choice [ try (count num (char ' '))
                 , try (char '\t' >> indentWith (num - tabStop)) ]

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

-- | A more general form of @notFollowedBy@.  This one allows any
-- type of parser to be specified, and succeeds only if that parser fails.
-- It does not consume any input.
notFollowedBy' :: (Show b, Stream s m a) => ParserT s st m b -> ParserT s st m ()
notFollowedBy' p  = try $ join $  do  a <- try p
                                      return (unexpected (show a))
                                  <|>
                                  return (return ())
-- (This version due to Andrew Pimlott on the Haskell mailing list.)

oneOfStrings' :: Stream s m Char => (Char -> Char -> Bool) -> [String] -> ParserT s st m String
oneOfStrings' _ []   = fail "no strings"
oneOfStrings' matches strs = try $ do
  c <- anyChar
  let strs' = [xs | (x:xs) <- strs, x `matches` c]
  case strs' of
       []  -> fail "not found"
       _   -> (c:) <$> oneOfStrings' matches strs'
               <|> if "" `elem` strs'
                      then return [c]
                      else fail "not found"

-- | Parses one of a list of strings.  If the list contains
-- two strings one of which is a prefix of the other, the longer
-- string will be matched if possible.
oneOfStrings :: Stream s m Char => [String] -> ParserT s st m String
oneOfStrings = oneOfStrings' (==)

-- | Parses one of a list of strings (tried in order), case insensitive.
oneOfStringsCI :: Stream s m Char => [String] -> ParserT s st m String
oneOfStringsCI = oneOfStrings' ciMatch
  where ciMatch x y = toLower' x == toLower' y
        -- this optimizes toLower by checking common ASCII case
        -- first, before calling the expensive unicode-aware
        -- function:
        toLower' c | c >= 'A' && c <= 'Z' = chr (ord c + 32)
                   | isAscii c = c
                   | otherwise = toLower c

-- | Parses a space or tab.
spaceChar :: Stream s m Char => ParserT s st m Char
spaceChar = satisfy $ \c -> c == ' ' || c == '\t'

-- | Parses a nonspace, nonnewline character.
nonspaceChar :: Stream s m Char => ParserT s st m Char
nonspaceChar = satisfy $ flip notElem ['\t', '\n', ' ', '\r']

-- | Skips zero or more spaces or tabs.
skipSpaces :: Stream s m Char => ParserT s st m ()
skipSpaces = skipMany spaceChar

-- | Skips zero or more spaces or tabs, then reads a newline.
blankline :: Stream s m Char => ParserT s st m Char
blankline = try $ skipSpaces >> newline

-- | Parses one or more blank lines and returns a string of newlines.
blanklines :: Stream s m Char => ParserT s st m [Char]
blanklines = many1 blankline

-- | Parses material enclosed between start and end parsers.
enclosed :: (Show end, Stream s  m Char) => ParserT s st m t   -- ^ start parser
         -> ParserT s st m end  -- ^ end parser
         -> ParserT s st m a    -- ^ content parser (to be used repeatedly)
         -> ParserT s st m [a]
enclosed start end parser = try $
  start >> notFollowedBy space >> many1Till parser end

-- | Parse string, case insensitive.
stringAnyCase :: Stream s m Char => [Char] -> ParserT s st m String
stringAnyCase [] = string ""
stringAnyCase (x:xs) = do
  firstChar <- char (toUpper x) <|> char (toLower x)
  rest <- stringAnyCase xs
  return (firstChar:rest)

-- | Parse contents of 'str' using 'parser' and return result.
parseFromString :: Monad m
                => ParserT [Char] st m a
                -> String
                -> ParserT [Char] st m a
parseFromString parser str = do
  oldPos <- getPosition
  setPosition $ initialPos "chunk"
  oldInput <- getInput
  setInput str
  result <- parser
  spaces
  eof
  setInput oldInput
  setPosition oldPos
  return result

-- | Like 'parseFromString' but specialized for 'ParserState'.
-- This resets 'stateLastStrPos', which is almost always what we want.
parseFromString' :: Monad m
                 => ParserT String ParserState m a
                 -> String
                 -> ParserT String ParserState m a
parseFromString' parser str = do
  oldStrPos <- stateLastStrPos <$> getState
  res <- parseFromString parser str
  updateState $ \st -> st{ stateLastStrPos = oldStrPos }
  return res

-- | Parse raw line block up to and including blank lines.
lineClump :: Monad m => ParserT [Char] st m String
lineClump = blanklines
          <|> (unlines <$> many1 (notFollowedBy blankline >> anyLine))

-- | Parse a string of characters between an open character
-- and a close character, including text between balanced
-- pairs of open and close, which must be different. For example,
-- @charsInBalanced '(' ')' anyChar@ will parse "(hello (there))"
-- and return "hello (there)".
charsInBalanced :: Stream s m Char => Char -> Char -> ParserT s st m Char
                -> ParserT s st m String
charsInBalanced open close parser = try $ do
  char open
  let isDelim c = c == open || c == close
  raw <- many $  many1 (notFollowedBy (satisfy isDelim) >> parser)
             <|> (do res <- charsInBalanced open close parser
                     return $ [open] ++ res ++ [close])
  char close
  return $ concat raw

-- old charsInBalanced would be:
-- charsInBalanced open close (noneOf "\n" <|> char '\n' >> notFollowedBy blankline)
-- old charsInBalanced' would be:
-- charsInBalanced open close anyChar

-- Auxiliary functions for romanNumeral:

lowercaseRomanDigits :: [Char]
lowercaseRomanDigits = ['i','v','x','l','c','d','m']

uppercaseRomanDigits :: [Char]
uppercaseRomanDigits = map toUpper lowercaseRomanDigits

-- | Parses a roman numeral (uppercase or lowercase), returns number.
romanNumeral :: Stream s m Char => Bool                  -- ^ Uppercase if true
             -> ParserT s st m Int
romanNumeral upperCase = do
    let romanDigits = if upperCase
                         then uppercaseRomanDigits
                         else lowercaseRomanDigits
    lookAhead $ oneOf romanDigits
    let [one, five, ten, fifty, hundred, fivehundred, thousand] =
          map char romanDigits
    thousands <- many thousand >>= (return . (1000 *) . length)
    ninehundreds <- option 0 $ try $ hundred >> thousand >> return 900
    fivehundreds <- many fivehundred >>= (return . (500 *) . length)
    fourhundreds <- option 0 $ try $ hundred >> fivehundred >> return 400
    hundreds <- many hundred >>= (return . (100 *) . length)
    nineties <- option 0 $ try $ ten >> hundred >> return 90
    fifties <- many fifty >>= (return . (50 *) . length)
    forties <- option 0 $ try $ ten >> fifty >> return 40
    tens <- many ten >>= (return . (10 *) . length)
    nines <- option 0 $ try $ one >> ten >> return 9
    fives <- many five >>= (return . (5 *) . length)
    fours <- option 0 $ try $ one >> five >> return 4
    ones <- many one >>= (return . length)
    let total = thousands + ninehundreds + fivehundreds + fourhundreds +
                hundreds + nineties + fifties + forties + tens + nines +
                fives + fours + ones
    if total == 0
       then fail "not a roman numeral"
       else return total

-- Parsers for email addresses and URIs

-- | Parses an email address; returns original and corresponding
-- escaped mailto: URI.
emailAddress :: Stream s m Char => ParserT s st m (String, String)
emailAddress = try $ toResult <$> mailbox <*> (char '@' *> domain)
 where toResult mbox dom = let full = fromEntities $ mbox ++ '@':dom
                           in  (full, escapeURI $ "mailto:" ++ full)
       mailbox           = intercalate "." <$> (emailWord `sepby1` dot)
       domain            = intercalate "." <$> (subdomain `sepby1` dot)
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
       isEmailPunct c    = c `elem` "!\"#$%&'*+-/=?^_{|}~;"
       -- note: sepBy1 from parsec consumes input when sep
       -- succeeds and p fails, so we use this variant here.
       sepby1 p sep      = (:) <$> p <*> (many (try $ sep >> p))


uriScheme :: Stream s m Char => ParserT s st m String
uriScheme = oneOfStringsCI (Set.toList schemes)

-- | Parses a URI. Returns pair of original and URI-escaped version.
uri :: Monad m => ParserT [Char] st m (String, String)
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
  let isWordChar c = isAlphaNum c || c `elem` "#$%*+/@\\_-"
  let wordChar = satisfy isWordChar
  let percentEscaped = try $ char '%' >> skipMany1 (satisfy isHexDigit)
  let entity = () <$ characterReference
  let punct = skipMany1 (char ',')
          <|> () <$ (satisfy (\c -> not (isSpace c) && c /= '<' && c /= '>'))
  let uriChunk =  skipMany1 wordChar
              <|> percentEscaped
              <|> entity
              <|> (try $ punct >>
                    lookAhead (void (satisfy isWordChar) <|> percentEscaped))
  str <- snd <$> withRaw (skipMany1 ( () <$
                                         (enclosed (char '(') (char ')') uriChunk
                                         <|> enclosed (char '{') (char '}') uriChunk
                                         <|> enclosed (char '[') (char ']') uriChunk)
                                         <|> uriChunk))
  str' <- option str $ char '/' >> return (str ++ "/")
  let uri' = scheme ++ ":" ++ fromEntities str'
  return (uri', escapeURI uri')

mathInlineWith :: Stream s m Char  => String -> String -> ParserT s st m String
mathInlineWith op cl = try $ do
  string op
  notFollowedBy space
  words' <- many1Till (count 1 (noneOf " \t\n\\")
                   <|> (char '\\' >>
                           -- This next clause is needed because \text{..} can
                           -- contain $, \(\), etc.
                           (try (string "text" >>
                                 (("\\text" ++) <$> inBalancedBraces 0 ""))
                            <|>  (\c -> ['\\',c]) <$> anyChar))
                   <|> do (blankline <* notFollowedBy' blankline) <|>
                             (oneOf " \t" <* skipMany (oneOf " \t"))
                          notFollowedBy (char '$')
                          return " "
                    ) (try $ string cl)
  notFollowedBy digit  -- to prevent capture of $5
  return $ concat words'
 where
  inBalancedBraces :: Stream s m Char => Int -> String -> ParserT s st m String
  inBalancedBraces 0 "" = do
    c <- anyChar
    if c == '{'
       then inBalancedBraces 1 "{"
       else mzero
  inBalancedBraces 0 s = return $ reverse s
  inBalancedBraces numOpen ('\\':xs) = do
    c <- anyChar
    inBalancedBraces numOpen (c:'\\':xs)
  inBalancedBraces numOpen xs = do
    c <- anyChar
    case c of
         '}' -> inBalancedBraces (numOpen - 1) (c:xs)
         '{' -> inBalancedBraces (numOpen + 1) (c:xs)
         _   -> inBalancedBraces numOpen (c:xs)

mathDisplayWith :: Stream s m Char => String -> String -> ParserT s st m String
mathDisplayWith op cl = try $ do
  string op
  many1Till (noneOf "\n" <|> (newline <* notFollowedBy' blankline)) (try $ string cl)

mathDisplay :: (HasReaderOptions st, Stream s m Char)
            => ParserT s st m String
mathDisplay =
      (guardEnabled Ext_tex_math_dollars >> mathDisplayWith "$$" "$$")
  <|> (guardEnabled Ext_tex_math_single_backslash >>
       mathDisplayWith "\\[" "\\]")
  <|> (guardEnabled Ext_tex_math_double_backslash >>
       mathDisplayWith "\\\\[" "\\\\]")

mathInline :: (HasReaderOptions st , Stream s m Char)
           => ParserT s st m String
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
        => ParsecT [Char] st m a
        -> ParsecT [Char] st m (a, [Char])
withRaw parser = do
  pos1 <- getPosition
  inp <- getInput
  result <- parser
  pos2 <- getPosition
  let (l1,c1) = (sourceLine pos1, sourceColumn pos1)
  let (l2,c2) = (sourceLine pos2, sourceColumn pos2)
  let inplines = take ((l2 - l1) + 1) $ lines inp
  let raw = case inplines of
                []   -> ""
                [l]  -> take (c2 - c1) l
                ls   -> unlines (init ls) ++ take (c2 - 1) (last ls)
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
                  '#':_  -> ent
                  _      -> ent ++ ";"
  case lookupEntity ent' of
       Just (c : _)  -> return c
       _             -> fail "entity not found"

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
  return (Decimal, read num)

-- | Parses a '@' and optional label and
-- returns (DefaultStyle, [next example number]).  The next
-- example number is incremented in parser state, and the label
-- (if present) is added to the label table.
exampleNum :: Stream s m Char
           => ParserT s ParserState m (ListNumberStyle, Int)
exampleNum = do
  char '@'
  lab <- many (alphaNum <|> satisfy (\c -> c == '_' || c == '-'))
  st <- getState
  let num = stateNextExample st
  let newlabels = if null lab
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
anyOrderedListMarker = choice $
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
charRef = do
  c <- characterReference
  return $ Str [c]

lineBlockLine :: Monad m => ParserT [Char] st m String
lineBlockLine = try $ do
  char '|'
  char ' '
  white <- many (spaceChar >> return '\160')
  notFollowedBy newline
  line <- anyLine
  continuations <- many (try $ char ' ' >> anyLine)
  return $ white ++ unwords (line : continuations)

blankLineBlockLine :: Stream s m Char => ParserT s st m Char
blankLineBlockLine = try (char '|' >> blankline)

-- | Parses an RST-style line block and returns a list of strings.
lineBlockLines :: Monad m => ParserT [Char] st m [String]
lineBlockLines = try $ do
  lines' <- many1 (lineBlockLine <|> ((:[]) <$> blankLineBlockLine))
  skipMany1 $ blankline <|> blankLineBlockLine
  return lines'

-- | Parse a table using 'headerParser', 'rowParser',
-- 'lineParser', and 'footerParser'.
tableWith :: (Stream s m Char, HasReaderOptions st,
              Functor mf, Applicative mf, Monad mf)
          => ParserT s st m (mf [Blocks], [Alignment], [Int])
          -> ([Int] -> ParserT s st m (mf [Blocks]))
          -> ParserT s st m sep
          -> ParserT s st m end
          -> ParserT s st m (mf Blocks)
tableWith headerParser rowParser lineParser footerParser = try $ do
  (aligns, widths, heads, rows) <- tableWith' headerParser rowParser
                                                lineParser footerParser
  return $ B.table mempty (zip aligns widths) <$> heads <*> rows

type TableComponents mf = ([Alignment], [Double], mf [Blocks], mf [[Blocks]])

tableWith' :: (Stream s m Char, HasReaderOptions st,
               Functor mf, Applicative mf, Monad mf)
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
    let widths = if (indices == [])
                    then replicate (length aligns) 0.0
                    else widthsFromIndices numColumns indices
    return $ (aligns, widths, heads, lines')

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
      fracs = map (\l -> (fromIntegral l) / quotient) lengths in
  tail fracs

---

-- Parse a grid table:  starts with row of '-' on top, then header
-- (which may be grid), then the rows,
-- which may be grid, separated by blank lines, and
-- ending with a footer (dashed line followed by blank line).
gridTableWith :: (Monad m, HasReaderOptions st,
                  Functor mf, Applicative mf, Monad mf)
              => ParserT [Char] st m (mf Blocks)  -- ^ Block list parser
              -> Bool                             -- ^ Headerless table
              -> ParserT [Char] st m (mf Blocks)
gridTableWith blocks headless =
  tableWith (gridTableHeader headless blocks) (gridTableRow blocks)
            (gridTableSep '-') gridTableFooter

gridTableWith' :: (Monad m, HasReaderOptions st,
                   Functor mf, Applicative mf, Monad mf)
               => ParserT [Char] st m (mf Blocks)  -- ^ Block list parser
               -> Bool                             -- ^ Headerless table
               -> ParserT [Char] st m (TableComponents mf)
gridTableWith' blocks headless =
  tableWith' (gridTableHeader headless blocks) (gridTableRow blocks)
             (gridTableSep '-') gridTableFooter

gridTableSplitLine :: [Int] -> String -> [String]
gridTableSplitLine indices line = map removeFinalBar $ tail $
  splitStringByIndices (init indices) $ trimr line

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

removeFinalBar :: String -> String
removeFinalBar =
  reverse . dropWhile (`elem` " \t") . dropWhile (=='|') . reverse

-- | Separator between rows of grid table.
gridTableSep :: Stream s m Char => Char -> ParserT s st m Char
gridTableSep ch = try $ gridDashedLines ch >> return '\n'

-- | Parse header for a grid table.
gridTableHeader :: (Monad m, Functor mf, Applicative mf, Monad mf)
                => Bool -- ^ Headerless table
                -> ParserT [Char] st m (mf Blocks)
                -> ParserT [Char] st m (mf [Blocks], [Alignment], [Int])
gridTableHeader headless blocks = try $ do
  optional blanklines
  dashes <- gridDashedLines '-'
  rawContent  <- if headless
                    then return $ repeat ""
                    else many1
                         (notFollowedBy (gridTableSep '=') >> char '|' >>
                           many1Till anyChar newline)
  underDashes <- if headless
                    then return dashes
                    else gridDashedLines '='
  guard $ length dashes == length underDashes
  let lines'   = map (snd . fst) underDashes
  let indices  = scanl (+) 0 lines'
  let aligns   = map snd underDashes
  let rawHeads = if headless
                    then replicate (length underDashes) ""
                    else map (unlines . map trim) $ transpose
                       $ map (gridTableSplitLine indices) rawContent
  heads <- fmap sequence $ mapM (parseFromString blocks . trim) rawHeads
  return (heads, aligns, indices)

gridTableRawLine :: Stream s m Char => [Int] -> ParserT s st m [String]
gridTableRawLine indices = do
  char '|'
  line <- many1Till anyChar newline
  return (gridTableSplitLine indices line)

-- | Parse row of grid table.
gridTableRow :: (Monad m, Functor mf, Applicative mf, Monad mf)
             => ParserT [Char] st m (mf Blocks)
             -> [Int]
             -> ParserT [Char] st m (mf [Blocks])
gridTableRow blocks indices = do
  colLines <- many1 (gridTableRawLine indices)
  let cols = map ((++ "\n") . unlines . removeOneLeadingSpace) $
               transpose colLines
      compactifyCell bs = case compactify [bs] of
                            []  -> mempty
                            x:_ -> x
  cells <- sequence <$> mapM (parseFromString blocks) cols
  return $ fmap (map compactifyCell) cells

removeOneLeadingSpace :: [String] -> [String]
removeOneLeadingSpace xs =
  if all startsWithSpace xs
     then map (drop 1) xs
     else xs
   where startsWithSpace ""     = True
         startsWithSpace (y:_) = y == ' '

-- | Parse footer for a grid table.
gridTableFooter :: Stream s m Char => ParserT s st m [Char]
gridTableFooter = blanklines

---

-- | Removes the ParsecT layer from the monad transformer stack
readWithM :: Monad m
          => ParserT [Char] st m a    -- ^ parser
          -> st                       -- ^ initial state
          -> String                   -- ^ input
          -> m (Either PandocError a)
readWithM parser state input =
    mapLeft (PandocParsecError input) `liftM` runParserT parser state "source" input


-- | Parse a string with a given parser and state
readWith :: Parser [Char] st a
         -> st
         -> String
         -> Either PandocError a
readWith p t inp = runIdentity $ readWithM p t inp

-- | Parse a string with @parser@ (for testing).
testStringWith :: Show a
               => ParserT [Char] ParserState Identity a
               -> [Char]
               -> IO ()
testStringWith parser str = UTF8.putStrLn $ show $
                            readWith parser defaultParserState str

-- | Parsing options.
data ParserState = ParserState
    { stateOptions         :: ReaderOptions, -- ^ User options
      stateParserContext   :: ParserContext, -- ^ Inside list?
      stateQuoteContext    :: QuoteContext,  -- ^ Inside quoted environment?
      stateAllowLinks      :: Bool,          -- ^ Allow parsing of links
      stateMaxNestingLevel :: Int,           -- ^ Max # of nested Strong/Emph
      stateLastStrPos      :: Maybe SourcePos, -- ^ Position after last str parsed
      stateKeys            :: KeyTable,      -- ^ List of reference keys
      stateHeaderKeys      :: KeyTable,      -- ^ List of implicit header ref keys
      stateSubstitutions   :: SubstTable,    -- ^ List of substitution references
      stateNotes           :: NoteTable,     -- ^ List of notes (raw bodies)
      stateNotes'          :: NoteTable',    -- ^ List of notes (parsed bodies)
      stateNoteRefs        :: Set.Set String, -- ^ List of note references used
      stateMeta            :: Meta,          -- ^ Document metadata
      stateMeta'           :: F Meta,        -- ^ Document metadata
      stateCitations       :: M.Map String String, -- ^ RST-style citations
      stateHeaderTable     :: [HeaderType],  -- ^ Ordered list of header types used
      stateHeaders         :: M.Map Inlines String, -- ^ List of headers and ids (used for implicit ref links)
      stateIdentifiers     :: Set.Set String, -- ^ Header identifiers used
      stateNextExample     :: Int,           -- ^ Number of next example
      stateExamples        :: M.Map String Int, -- ^ Map from example labels to numbers
      stateMacros          :: M.Map Text Macro, -- ^ Table of macros defined so far
      stateRstDefaultRole  :: String,        -- ^ Current rST default interpreted text role
      stateRstCustomRoles  :: M.Map String (String, Maybe String, Attr), -- ^ Current rST custom text roles
      -- Triple represents: 1) Base role, 2) Optional format (only for :raw:
      -- roles), 3) Additional classes (rest of Attr is unused)).
      stateCaption         :: Maybe Inlines, -- ^ Caption in current environment
      stateInHtmlBlock     :: Maybe String,  -- ^ Tag type of HTML block being parsed
      stateContainers      :: [String],      -- ^ parent include files
      stateLogMessages     :: [LogMessage],  -- ^ log messages
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

class HasHeaderMap st where
  extractHeaderMap  :: st -> M.Map Inlines String
  updateHeaderMap   :: (M.Map Inlines String -> M.Map Inlines String) ->
                       st -> st

instance HasHeaderMap ParserState where
  extractHeaderMap     = stateHeaders
  updateHeaderMap f st = st{ stateHeaders = f $ stateHeaders st }

class HasIdentifierList st where
  extractIdentifierList  :: st -> Set.Set String
  updateIdentifierList   :: (Set.Set String -> Set.Set String) -> st -> st

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
  setLastStrPos  :: SourcePos -> st -> st
  getLastStrPos  :: st -> Maybe SourcePos

instance HasLastStrPosition ParserState where
  setLastStrPos pos st = st{ stateLastStrPos = Just pos }
  getLastStrPos st     = stateLastStrPos st

class HasLogMessages st where
  addLogMessage :: LogMessage -> st -> st
  getLogMessages :: st -> [LogMessage]

instance HasLogMessages ParserState where
  addLogMessage msg st = st{ stateLogMessages = msg : stateLogMessages st }
  getLogMessages st = reverse $ stateLogMessages st

class HasIncludeFiles st where
  getIncludeFiles :: st -> [String]
  addIncludeFile :: String -> st -> st
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
                  stateHeaders         = M.empty,
                  stateIdentifiers     = Set.empty,
                  stateNextExample     = 1,
                  stateExamples        = M.empty,
                  stateMacros          = M.empty,
                  stateRstDefaultRole  = "title-reference",
                  stateRstCustomRoles  = M.empty,
                  stateCaption         = Nothing,
                  stateInHtmlBlock     = Nothing,
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
updateLastStrPos = getPosition >>= updateState . setLastStrPos

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

type NoteTable = [(String, String)]

type NoteTable' = M.Map String (SourcePos, F Blocks)
-- used in markdown reader

newtype Key = Key String deriving (Show, Read, Eq, Ord)

toKey :: String -> Key
toKey = Key . map toLower . unwords . words . unbracket
  where unbracket ('[':xs) | "]" `isSuffixOf` xs = take (length xs - 1) xs
        unbracket xs       = xs

type KeyTable = M.Map Key (Target, Attr)

type SubstTable = M.Map Key Inlines

--  | Add header to the list of headers in state, together
--  with its associated identifier.  If the identifier is null
--  and the auto_identifers extension is set, generate a new
--  unique identifier, and update the list of identifiers
--  in state.  Issue a warning if an explicit identifier
--  is encountered that duplicates an earlier identifier
--  (explict or automatically generated).
registerHeader :: (Stream s m a, HasReaderOptions st,
                    HasHeaderMap st, HasLogMessages st, HasIdentifierList st)
               => Attr -> Inlines -> ParserT s st m Attr
registerHeader (ident,classes,kvs) header' = do
  ids <- extractIdentifierList <$> getState
  exts <- getOption readerExtensions
  let insert' = M.insertWith (\_new old -> old)
  if null ident && Ext_auto_identifiers `extensionEnabled` exts
     then do
       let id' = uniqueIdent (B.toList header') ids
       let id'' = if Ext_ascii_identifiers `extensionEnabled` exts
                     then catMaybes $ map toAsciiChar id'
                     else id'
       updateState $ updateIdentifierList $ Set.insert id'
       updateState $ updateIdentifierList $ Set.insert id''
       updateState $ updateHeaderMap $ insert' header' id'
       return (id'',classes,kvs)
     else do
        unless (null ident) $ do
          when (ident `Set.member` ids) $ do
            pos <- getPosition
            logMessage $ DuplicateIdentifier ident pos
          updateState $ updateIdentifierList $ Set.insert ident
          updateState $ updateHeaderMap $ insert' header' ident
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
singleQuoted inlineParser = try $ do
  singleQuoteStart
  withQuoteContext InSingleQuote $ many1Till inlineParser singleQuoteEnd >>=
    return . B.singleQuoted . mconcat

doubleQuoted :: (HasQuoteContext st m, Stream s m Char)
             => ParserT s st m Inlines
             -> ParserT s st m Inlines
doubleQuoted inlineParser = try $ do
  doubleQuoteStart
  withQuoteContext InDoubleQuote $ manyTill inlineParser doubleQuoteEnd >>=
    return . B.doubleQuoted . mconcat

failIfInQuoteContext :: (HasQuoteContext st m, Stream s m t)
                     => QuoteContext
                     -> ParserT s st m ()
failIfInQuoteContext context = do
  context' <- getQuoteContext
  if context' == context
     then fail "already inside quotes"
     else return ()

charOrRef :: Stream s m Char => String -> ParserT s st m Char
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
  () <$ charOrRef "'\8216\145"

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
           notFollowedBy . satisfy $ flip elem [' ', '\t', '\n']

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
        => ParserT s st m (Bool, String)
citeKey = try $ do
  guard =<< notAfterString
  suppress_author <- option False (char '-' *> return True)
  char '@'
  firstChar <- alphaNum <|> char '_' <|> char '*' -- @* for wildcard in nocite
  let regchar = satisfy (\c -> isAlphaNum c || c == '_')
  let internal p = try $ p <* lookAhead regchar
  rest <- many $ regchar <|> internal (oneOf ":.#$%&-+?<>~/") <|>
                 try (oneOf ":/" <* lookAhead (char '/'))
  let key = firstChar:rest
  return (suppress_author, key)


token :: (Stream s m t)
      => (t -> String)
      -> (t -> SourcePos)
      -> (t -> Maybe a)
      -> ParsecT s st m a
token pp pos match = tokenPrim pp (\_ t _ -> pos t) match

infixr 5 <+?>
(<+?>) :: (Monoid a) => ParserT s st m a -> ParserT s st m a -> ParserT s st m a
a <+?> b = a >>= flip fmap (try b <|> return mempty) . (<>)

extractIdClass :: Attr -> Attr
extractIdClass (ident, cls, kvs) = (ident', cls', kvs')
  where
    ident' = case (lookup "id" kvs) of
               Just v  -> v
               Nothing -> ident
    cls'   = case (lookup "class" kvs) of
               Just cl -> words cl
               Nothing -> cls
    kvs'  = filter (\(k,_) -> k /= "id" || k /= "class") kvs

insertIncludedFile' :: (PandocMonad m, HasIncludeFiles st,
                        Functor mf, Applicative mf, Monad mf)
                    => ParserT [a] st m (mf Blocks)
                    -> (String -> [a])
                    -> [FilePath] -> FilePath
                    -> ParserT [a] st m (mf Blocks)
insertIncludedFile' blocks totoks dirs f = do
  oldPos <- getPosition
  oldInput <- getInput
  containers <- getIncludeFiles <$> getState
  when (f `elem` containers) $
    throwError $ PandocParseError $ "Include file loop at " ++ show oldPos
  updateState $ addIncludeFile f
  mbcontents <- readFileFromDirs dirs f
  contents <- case mbcontents of
                   Just s -> return s
                   Nothing -> do
                     report $ CouldNotLoadIncludeFile f oldPos
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
                   -> (String -> [a])
                   -> [FilePath] -> FilePath
                   -> ParserT [a] st m Blocks
insertIncludedFile blocks totoks dirs f =
  runIdentity <$> insertIncludedFile' (Identity <$> blocks) totoks dirs f

-- | Parse content of include file as future blocks. Circular includes result in
-- an @PandocParseError@.
insertIncludedFileF :: (PandocMonad m, HasIncludeFiles st)
                    => ParserT String st m (Future st Blocks)
                    -> [FilePath] -> FilePath
                    -> ParserT String st m (Future st Blocks)
insertIncludedFileF p = insertIncludedFile' p id
