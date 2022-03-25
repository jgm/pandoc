{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{- |
Module      : Text.Pandoc.Parsing.Lists
Copyright   : © 2006-2022 John MacFarlane
License     : GPL-2.0-or-later
Maintainer  : John MacFarlane <jgm@berkeley.edu>

Parsers for list markers.
-}

module Text.Pandoc.Parsing.Lists
  ( anyOrderedListMarker
  , decimal
  , lowerAlpha
  , lowerRoman
  , orderedListMarker
  , romanNumeral
  , upperAlpha
  , upperRoman
  )
where

import Data.Char
  ( isAsciiUpper
  , isAsciiLower
  , ord
  , toLower
  )
import Data.Maybe (fromMaybe)
import Text.Pandoc.Definition
  ( ListNumberDelim(..)
  , ListAttributes
  , ListNumberStyle(..)
  )
import Text.Pandoc.Shared (safeRead)
import Text.Pandoc.Sources
import Text.Parsec
  ( (<|>)
  , Stream(..)
  , choice
  , getState
  , lookAhead
  , many
  , many1
  , option
  , try
  , updateState
  )
import Text.Pandoc.Parsing.State
import Text.Pandoc.Parsing.Types (ParserT)

import qualified Data.Map as M
import qualified Data.Text as T

-- | Parses a roman numeral (uppercase or lowercase), returns number.
romanNumeral :: (Stream s m Char, UpdateSourcePos s Char)
             => Bool                  -- ^ Uppercase if true
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
    thousands <- (1000 *) . length <$> many thousand
    ninehundreds <- option 0 $ try $ hundred >> thousand >> return 900
    fivehundreds <- option 0 $ 500 <$ fivehundred
    fourhundreds <- option 0 $ try $ hundred >> fivehundred >> return 400
    hundreds <- (100 *) . length <$> many hundred
    nineties <- option 0 $ try $ ten >> hundred >> return 90
    fifties <- option 0 (50 <$ fifty)
    forties <- option 0 $ try $ ten >> fifty >> return 40
    tens <- (10 *) . length <$> many ten
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

-- | Parses an uppercase roman numeral and returns (UpperRoman, number).
upperRoman :: (Stream s m Char, UpdateSourcePos s Char) => ParserT s st m (ListNumberStyle, Int)
upperRoman = do
  num <- romanNumeral True
  return (UpperRoman, num)

-- | Parses a lowercase roman numeral and returns (LowerRoman, number).
lowerRoman :: (Stream s m Char, UpdateSourcePos s Char) => ParserT s st m (ListNumberStyle, Int)
lowerRoman = do
  num <- romanNumeral False
  return (LowerRoman, num)

-- | Parses a decimal numeral and returns (Decimal, number).
decimal :: (Stream s m Char, UpdateSourcePos s Char) => ParserT s st m (ListNumberStyle, Int)
decimal = do
  num <- many1 digit
  return (Decimal, fromMaybe 1 $ safeRead $ T.pack num)

-- | Parses a '@' and optional label and
-- returns (DefaultStyle, [next example number]).  The next
-- example number is incremented in parser state, and the label
-- (if present) is added to the label table.
exampleNum :: (Stream s m Char, UpdateSourcePos s Char)
           => ParserT s ParserState m (ListNumberStyle, Int)
exampleNum = do
  char '@'
  lab <- mconcat . map T.pack <$>
                    many (many1 alphaNum <|>
                          try (do c <- char '_' <|> char '-'
                                  cs <- many1 alphaNum
                                  return (c:cs)))
  st <- getState
  let num = stateNextExample st
  let newlabels = if T.null lab
                     then stateExamples st
                     else M.insert lab num $ stateExamples st
  updateState $ \s -> s{ stateNextExample = num + 1
                       , stateExamples    = newlabels }
  return (Example, num)

-- | Parses a '#' returns (DefaultStyle, 1).
defaultNum :: (Stream s m Char, UpdateSourcePos s Char) => ParserT s st m (ListNumberStyle, Int)
defaultNum = do
  char '#'
  return (DefaultStyle, 1)

-- | Parses a lowercase letter and returns (LowerAlpha, number).
lowerAlpha :: (Stream s m Char, UpdateSourcePos s Char) => ParserT s st m (ListNumberStyle, Int)
lowerAlpha = do
  ch <- satisfy isAsciiLower
  return (LowerAlpha, ord ch - ord 'a' + 1)

-- | Parses an uppercase letter and returns (UpperAlpha, number).
upperAlpha :: (Stream s m Char, UpdateSourcePos s Char) => ParserT s st m (ListNumberStyle, Int)
upperAlpha = do
  ch <- satisfy isAsciiUpper
  return (UpperAlpha, ord ch - ord 'A' + 1)

-- | Parses a roman numeral i or I
romanOne :: (Stream s m Char, UpdateSourcePos s Char) => ParserT s st m (ListNumberStyle, Int)
romanOne = (char 'i' >> return (LowerRoman, 1)) <|>
           (char 'I' >> return (UpperRoman, 1))

-- | Parses an ordered list marker and returns list attributes.
anyOrderedListMarker :: (Stream s m Char, UpdateSourcePos s Char) => ParserT s ParserState m ListAttributes
anyOrderedListMarker = choice
  [delimParser numParser | delimParser <- [inPeriod, inOneParen, inTwoParens],
                           numParser <- [decimal, exampleNum, defaultNum, romanOne,
                           lowerAlpha, lowerRoman, upperAlpha, upperRoman]]

-- | Parses a list number (num) followed by a period, returns list attributes.
inPeriod :: (Stream s m Char, UpdateSourcePos s Char)
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
inOneParen :: (Stream s m Char, UpdateSourcePos s Char)
           => ParserT s st m (ListNumberStyle, Int)
           -> ParserT s st m ListAttributes
inOneParen num = try $ do
  (style, start) <- num
  char ')'
  return (start, style, OneParen)

-- | Parses a list number (num) enclosed in parens, returns list attributes.
inTwoParens :: (Stream s m Char, UpdateSourcePos s Char)
            => ParserT s st m (ListNumberStyle, Int)
            -> ParserT s st m ListAttributes
inTwoParens num = try $ do
  char '('
  (style, start) <- num
  char ')'
  return (start, style, TwoParens)

-- | Parses an ordered list marker with a given style and delimiter,
-- returns number.
orderedListMarker :: (Stream s m Char, UpdateSourcePos s Char)
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
