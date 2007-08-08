{-
Copyright (C) 2006-7 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.ParserCombinators
   Copyright   : Copyright (C) 2006-7 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Parser combinators used in Pandoc readers.
-}
module Text.Pandoc.ParserCombinators ( 
                                      anyLine,
                                      many1Till,
                                      notFollowedBy',
                                      oneOfStrings,
                                      spaceChar,
                                      skipSpaces,
                                      blankline,
                                      blanklines,
                                      enclosed,
                                      stringAnyCase,
                                      parseFromString,
                                      lineClump,
                                      charsInBalanced,
                                      charsInBalanced',
                                      romanNumeral,
                                      withHorizDisplacement
                                     ) where
import Text.ParserCombinators.Parsec
import Data.Char ( toUpper, toLower )

--- | Parse any line of text
anyLine :: GenParser Char st [Char]
anyLine = try (manyTill anyChar newline) <|> many1 anyChar
          -- second alternative is for a line ending with eof

-- | Parses a space or tab.
spaceChar :: CharParser st Char
spaceChar = oneOf " \t"

-- | Skips zero or more spaces or tabs.
skipSpaces :: GenParser Char st ()
skipSpaces = skipMany spaceChar

-- | Skips zero or more spaces or tabs, then reads a newline.
blankline :: GenParser Char st Char
blankline = try (do
                   skipSpaces
                   newline)

-- | Parses one or more blank lines and returns a string of newlines.
blanklines :: GenParser Char st [Char]
blanklines = try (many1 blankline)

-- | Parses material enclosed between start and end parsers.
enclosed :: GenParser Char st t   -- ^ start parser
	    -> GenParser Char st end  -- ^ end parser
	    -> GenParser Char st a    -- ^ content parser (to be used repeatedly)
	    -> GenParser Char st [a]
enclosed start end parser = try (do
                                   start
                                   notFollowedBy space
                                   result <- many1Till parser (try end)
                                   return result)

-- | Like @manyTill@, but reads at least one item.
many1Till :: GenParser tok st a
	     -> GenParser tok st end
	     -> GenParser tok st [a]
many1Till p end = try (do
         first <- p
         rest <- manyTill p end
         return (first:rest))

-- | A more general form of @notFollowedBy@.  This one allows any 
-- type of parser to be specified, and succeeds only if that parser fails.
-- It does not consume any input.
notFollowedBy' :: Show b => GenParser a st b -> GenParser a st ()
notFollowedBy' parser = try (do { c <- try parser; unexpected (show c) }
                           <|> return ())

-- | Parses one of a list of strings (tried in order).  
oneOfStrings :: [String] -> GenParser Char st String
oneOfStrings listOfStrings = choice $ map (try . string) listOfStrings

-- | Parse string, case insensitive.
stringAnyCase :: [Char] -> CharParser st String
stringAnyCase [] = string ""
stringAnyCase (x:xs) = try (do
  firstChar <- choice [ char (toUpper x), char (toLower x) ]
  rest <- stringAnyCase xs
  return (firstChar:rest))

-- | Parse contents of 'str' using 'parser' and return result.
parseFromString :: GenParser tok st a -> [tok] -> GenParser tok st a
parseFromString parser str = try $ do
  oldInput <- getInput
  setInput str
  result <- parser
  setInput oldInput
  return result

-- | Parse raw line block up to and including blank lines.
lineClump :: GenParser Char st String
lineClump = do
  lines <- many1 (do{notFollowedBy blankline; anyLine})
  blanks <- blanklines <|> (do{eof; return "\n"})
  return ((unlines lines) ++ blanks)

-- | Parse a string of characters between an open character
-- and a close character, including text between balanced
-- pairs of open and close. For example,
-- @charsInBalanced '(' ')'@ will parse "(hello (there))"
-- and return "hello (there)".  Stop if a blank line is
-- encountered.
charsInBalanced :: Char -> Char -> GenParser Char st String
charsInBalanced open close = try $ do
  char open
  raw <- manyTill (   (do res <- charsInBalanced open close
                          return $ [open] ++ res ++ [close])
                  <|> (do notFollowedBy' (blankline >> blanklines)
                          count 1 anyChar))
                  (char close)
  return $ concat raw

-- | Like charsInBalanced, but allow blank lines in the content.
charsInBalanced' :: Char -> Char -> GenParser Char st String
charsInBalanced' open close = try $ do
  char open
  raw <- manyTill (   (do res <- charsInBalanced open close
                          return $ [open] ++ res ++ [close])
                  <|> count 1 anyChar)
                  (char close)
  return $ concat raw

-- | Parses a roman numeral (uppercase or lowercase), returns number.
romanNumeral :: Bool ->  -- ^ Uppercase if true
                GenParser Char st Int
romanNumeral upper = try $ do
    let char' c = char (if upper then toUpper c else c)
    let one = char' 'i'
    let five = char' 'v'
    let ten = char' 'x'
    let fifty = char' 'l'
    let hundred = char' 'c'
    let fivehundred = char' 'd'
    let thousand = char' 'm'
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
    fives <- many five >>= (return . (5*) . length)
    fours <- option 0 $ try $ one >> five >> return 4
    ones <- many one >>= (return . length)
    let total = thousands + ninehundreds + fivehundreds + fourhundreds +
                hundreds + nineties + fifties + forties + tens + nines +
                fives + fours + ones
    if total == 0
       then fail "not a roman numeral"
       else return total

-- | Applies a parser, returns tuple of its results and its horizontal
-- displacement (the difference between the source column at the end
-- and the source column at the beginning). Vertical displacement
-- (source row) is ignored.
withHorizDisplacement :: GenParser Char st a  -- ^ Parser to apply
                      -> GenParser Char st (a, Int) -- ^ (result, displacement)
withHorizDisplacement parser = do
  pos1 <- getPosition
  result <- parser
  pos2 <- getPosition
  return (result, sourceColumn pos2 - sourceColumn pos1)

