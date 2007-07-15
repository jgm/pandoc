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
                                      parseFromStr,
                                      lineClump,
                                      charsInBalanced
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
                                   result <- many1Till parser end
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
parseFromStr :: GenParser tok st a -> [tok] -> GenParser tok st a
parseFromStr parser str = try $ do
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
-- and return "hello (there)". 
charsInBalanced :: Char -> Char -> GenParser Char st String
charsInBalanced open close = try $ do
  char open
  raw <- manyTill (   (do res <- charsInBalanced open close
                          return $ [open] ++ res ++ [close])
                  <|> (do notFollowedBy' (blankline >> blanklines)
                          count 1 anyChar))
                  (char close)
  return $ concat raw

