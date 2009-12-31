{-# LANGUAGE DeriveDataTypeable #-}
{-
Copyright (C) 2006-8 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Shared
   Copyright   : Copyright (C) 2006-8 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Utility functions and definitions used by the various Pandoc modules.
-}
module Text.Pandoc.Shared ( 
                     -- * List processing
                     splitBy,
                     splitByIndices,
                     substitute,
                     -- * Text processing
                     backslashEscapes,
                     escapeStringUsing,
                     stripTrailingNewlines,
                     removeLeadingTrailingSpace,
                     removeLeadingSpace,
                     removeTrailingSpace,
                     stripFirstAndLast,
                     camelCaseToHyphenated,
                     toRomanNumeral,
                     wrapped,
                     wrapIfNeeded,
                     wrappedTeX,
                     wrapTeXIfNeeded,
                     BlockWrapper (..),
                     wrappedBlocksToDoc,
                     tabFilter,
                     -- * Parsing
                     (>>~),
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
                     emailAddress,
                     uri,
                     withHorizDisplacement,
                     nullBlock,
                     failIfStrict,
                     failUnlessLHS,
                     escaped,
                     anyOrderedListMarker,
                     orderedListMarker,
                     charRef,
                     readWith,
                     testStringWith,
                     ParserState (..),
                     defaultParserState,
                     HeaderType (..),
                     ParserContext (..),
                     QuoteContext (..),
                     NoteTable,
                     KeyTable,
                     lookupKeySrc,
                     refsMatch,
                     -- * Prettyprinting
                     hang',
                     prettyPandoc,
                     -- * Pandoc block and inline list processing
                     orderedListMarkers,
                     normalizeSpaces,
                     compactify,
                     Element (..),
                     hierarchicalize,
                     isHeaderBlock,
                     -- * Writer options
                     HTMLMathMethod (..),
                     ObfuscationMethod (..),
                     WriterOptions (..),
                     defaultWriterOptions,
                     -- * File handling
                     inDirectory,
                     readDataFile
                    ) where

import Text.Pandoc.Definition
import Text.ParserCombinators.Parsec
import Text.PrettyPrint.HughesPJ ( Doc, fsep, ($$), (<>), empty, isEmpty, text, nest )
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.Pandoc.CharacterReferences ( characterReference )
import Data.Char ( toLower, toUpper, ord, isLower, isUpper, isAlpha,
                   isPunctuation )
import Data.List ( find, isPrefixOf, intercalate )
import Network.URI ( parseURI, URI (..), isAllowedInURI )
import System.Directory
import System.FilePath ( (</>) )
-- Note: ghc >= 6.12 (base >=4.2) supports unicode through iconv
-- So we use System.IO.UTF8 only if we have an earlier version
#if MIN_VERSION_base(4,2,0)
#else
import Prelude hiding ( putStr, putStrLn, writeFile, readFile, getContents )
import System.IO.UTF8
#endif
import Data.Generics
import qualified Control.Monad.State as S
import Control.Monad (join)
import Paths_pandoc (getDataFileName)
--
-- List processing
--

-- | Split list by groups of one or more sep.
splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy _ [] = []
splitBy sep lst = 
  let (first, rest) = break (== sep) lst
      rest'         = dropWhile (== sep) rest
  in  first:(splitBy sep rest')

-- | Split list into chunks divided at specified indices.
splitByIndices :: [Int] -> [a] -> [[a]]
splitByIndices [] lst = [lst]
splitByIndices (x:xs) lst =
    let (first, rest) = splitAt x lst in
    first:(splitByIndices (map (\y -> y - x)  xs) rest)

-- | Replace each occurrence of one sublist in a list with another.
substitute :: (Eq a) => [a] -> [a] -> [a] -> [a]
substitute _ _ [] = []
substitute [] _ lst = lst
substitute target replacement lst = 
    if target `isPrefixOf` lst
       then replacement ++ (substitute target replacement $ drop (length target) lst)
       else (head lst):(substitute target replacement $ tail lst)

--
-- Text processing
--

-- | Returns an association list of backslash escapes for the
-- designated characters.
backslashEscapes :: [Char]    -- ^ list of special characters to escape
                 -> [(Char, String)]
backslashEscapes = map (\ch -> (ch, ['\\',ch]))

-- | Escape a string of characters, using an association list of
-- characters and strings.
escapeStringUsing :: [(Char, String)] -> String -> String
escapeStringUsing _ [] = ""
escapeStringUsing escapeTable (x:xs) = 
  case (lookup x escapeTable) of
       Just str  -> str ++ rest
       Nothing   -> x:rest
  where rest = escapeStringUsing escapeTable xs

-- | Strip trailing newlines from string.
stripTrailingNewlines :: String -> String
stripTrailingNewlines = reverse . dropWhile (== '\n') . reverse

-- | Remove leading and trailing space (including newlines) from string.
removeLeadingTrailingSpace :: String -> String
removeLeadingTrailingSpace = removeLeadingSpace . removeTrailingSpace

-- | Remove leading space (including newlines) from string.
removeLeadingSpace :: String -> String
removeLeadingSpace = dropWhile (`elem` " \n\t")

-- | Remove trailing space (including newlines) from string.
removeTrailingSpace :: String -> String
removeTrailingSpace = reverse . removeLeadingSpace . reverse

-- | Strip leading and trailing characters from string
stripFirstAndLast :: String -> String
stripFirstAndLast str =
  drop 1 $ take ((length str) - 1) str

-- | Change CamelCase word to hyphenated lowercase (e.g., camel-case). 
camelCaseToHyphenated :: String -> String
camelCaseToHyphenated [] = ""
camelCaseToHyphenated (a:b:rest) | isLower a && isUpper b =
  a:'-':(toLower b):(camelCaseToHyphenated rest)
camelCaseToHyphenated (a:rest) = (toLower a):(camelCaseToHyphenated rest)

-- | Convert number < 4000 to uppercase roman numeral.
toRomanNumeral :: Int -> String
toRomanNumeral x =
  if x >= 4000 || x < 0
     then "?"
     else case x of
              _ | x >= 1000 -> "M" ++ toRomanNumeral (x - 1000)
              _ | x >= 900  -> "CM" ++ toRomanNumeral (x - 900)
              _ | x >= 500  -> "D" ++ toRomanNumeral (x - 500)
              _ | x >= 400  -> "CD" ++ toRomanNumeral (x - 400)
              _ | x >= 100  -> "C" ++ toRomanNumeral (x - 100)
              _ | x >= 90   -> "XC" ++ toRomanNumeral (x - 90)
              _ | x >= 50   -> "L"  ++ toRomanNumeral (x - 50)
              _ | x >= 40   -> "XL" ++ toRomanNumeral (x - 40)
              _ | x >= 10   -> "X" ++ toRomanNumeral (x - 10)
              _ | x >= 9    -> "IX" ++ toRomanNumeral (x - 5)
              _ | x >= 5    -> "V" ++ toRomanNumeral (x - 5)
              _ | x >= 4    -> "IV" ++ toRomanNumeral (x - 4)
              _ | x >= 1    -> "I" ++ toRomanNumeral (x - 1)
              _             -> ""

-- | Wrap inlines to line length.
wrapped :: Monad m => ([Inline] -> m Doc) -> [Inline] -> m Doc
wrapped listWriter sect = (mapM listWriter $ splitBy Space sect) >>= 
                          return . fsep

-- | Wrap inlines if the text wrap option is selected.
wrapIfNeeded :: Monad m => WriterOptions -> ([Inline] -> m Doc) -> 
                           [Inline] -> m Doc
wrapIfNeeded opts = if writerWrapText opts
                       then wrapped 
                       else ($)

-- auxiliary function for wrappedTeX
isNote :: Inline -> Bool
isNote (Note _) = True
isNote _ = False

-- | Wrap inlines to line length, treating footnotes in a way that
-- makes sense in LaTeX and ConTeXt.
wrappedTeX :: Monad m 
           => Bool
           -> ([Inline] -> m Doc) 
           -> [Inline] 
           -> m Doc
wrappedTeX includePercent listWriter sect = do
  let (firstpart, rest) = break isNote sect
  firstpartWrapped <- wrapped listWriter firstpart
  if null rest
     then return firstpartWrapped
     else do let (note:rest') = rest
             let (rest1, rest2) = break (== Space) rest'
             -- rest1 is whatever comes between the note and a Space.
             -- if the note is followed directly by a Space, rest1 is null.
             -- rest1 is printed after the note but before the line break,
             -- to avoid spurious blank space the note and immediately
             -- following punctuation.
             rest1Out <- if null rest1
                            then return empty
                            else listWriter rest1
             rest2Wrapped <- if null rest2
                                then return empty
                                else wrappedTeX includePercent listWriter (tail rest2)
             noteText <- listWriter [note]
             return $ (firstpartWrapped <> if includePercent then PP.char '%' else empty) $$ 
                      (noteText <> rest1Out) $$
                      rest2Wrapped

-- | Wrap inlines if the text wrap option is selected, specialized 
-- for LaTeX and ConTeXt.
wrapTeXIfNeeded :: Monad m 
                => WriterOptions
                -> Bool
                -> ([Inline] -> m Doc) 
                -> [Inline] 
                -> m Doc
wrapTeXIfNeeded opts includePercent = if writerWrapText opts
                                         then wrappedTeX includePercent
                                         else ($)

-- | Indicates whether block should be surrounded by blank lines (@Pad@) or not (@Reg@).
data BlockWrapper = Pad Doc | Reg Doc

-- | Converts a list of wrapped blocks to a Doc, with appropriate spaces around blocks.
wrappedBlocksToDoc :: [BlockWrapper] -> Doc
wrappedBlocksToDoc = foldr addBlock empty
     where addBlock (Pad d) accum | isEmpty accum = d
           addBlock (Pad d) accum = d $$ text "" $$ accum
           addBlock (Reg d) accum = d $$ accum

-- | Convert tabs to spaces and filter out DOS line endings.
-- Tabs will be preserved if tab stop is set to 0.
tabFilter :: Int       -- ^ Tab stop
          -> String    -- ^ Input
          -> String
tabFilter tabStop =
  let go _ [] = ""
      go _ ('\n':xs) = '\n' : go tabStop xs
      go _ ('\r':'\n':xs) = '\n' : go tabStop xs
      go _ ('\r':xs) = '\n' : go tabStop xs
      go spsToNextStop ('\t':xs) =
        if tabStop == 0
           then '\t' : go tabStop xs
           else replicate spsToNextStop ' ' ++ go tabStop xs
      go 1 (x:xs) =
        x : go tabStop xs
      go spsToNextStop (x:xs) =
        x : go (spsToNextStop - 1) xs
  in  go tabStop

--
-- Parsing
--

-- | Like >>, but returns the operation on the left.
-- (Suggested by Tillmann Rendel on Haskell-cafe list.)
(>>~) :: (Monad m) => m a -> m b -> m a
a >>~ b = a >>= \x -> b >> return x

-- | Parse any line of text
anyLine :: GenParser Char st [Char]
anyLine = manyTill anyChar newline

-- | Like @manyTill@, but reads at least one item.
many1Till :: GenParser tok st a
	     -> GenParser tok st end
	     -> GenParser tok st [a]
many1Till p end = do
         first <- p
         rest <- manyTill p end
         return (first:rest)

-- | A more general form of @notFollowedBy@.  This one allows any 
-- type of parser to be specified, and succeeds only if that parser fails.
-- It does not consume any input.
notFollowedBy' :: Show b => GenParser a st b -> GenParser a st ()
notFollowedBy' p  = try $ join $  do  a <- try p
                                      return (unexpected (show a))
                                  <|>
                                  return (return ())
-- (This version due to Andrew Pimlott on the Haskell mailing list.)

-- | Parses one of a list of strings (tried in order).  
oneOfStrings :: [String] -> GenParser Char st String
oneOfStrings listOfStrings = choice $ map (try . string) listOfStrings

-- | Parses a space or tab.
spaceChar :: CharParser st Char
spaceChar = char ' ' <|> char '\t'

-- | Skips zero or more spaces or tabs.
skipSpaces :: GenParser Char st ()
skipSpaces = skipMany spaceChar

-- | Skips zero or more spaces or tabs, then reads a newline.
blankline :: GenParser Char st Char
blankline = try $ skipSpaces >> newline

-- | Parses one or more blank lines and returns a string of newlines.
blanklines :: GenParser Char st [Char]
blanklines = many1 blankline

-- | Parses material enclosed between start and end parsers.
enclosed :: GenParser Char st t   -- ^ start parser
	    -> GenParser Char st end  -- ^ end parser
	    -> GenParser Char st a    -- ^ content parser (to be used repeatedly)
	    -> GenParser Char st [a]
enclosed start end parser = try $ 
  start >> notFollowedBy space >> many1Till parser end

-- | Parse string, case insensitive.
stringAnyCase :: [Char] -> CharParser st String
stringAnyCase [] = string ""
stringAnyCase (x:xs) = do
  firstChar <- char (toUpper x) <|> char (toLower x)
  rest <- stringAnyCase xs
  return (firstChar:rest)

-- | Parse contents of 'str' using 'parser' and return result.
parseFromString :: GenParser tok st a -> [tok] -> GenParser tok st a
parseFromString parser str = do
  oldPos <- getPosition
  oldInput <- getInput
  setInput str
  result <- parser
  setInput oldInput
  setPosition oldPos
  return result

-- | Parse raw line block up to and including blank lines.
lineClump :: GenParser Char st String
lineClump = blanklines 
          <|> (many1 (notFollowedBy blankline >> anyLine) >>= return . unlines)

-- | Parse a string of characters between an open character
-- and a close character, including text between balanced
-- pairs of open and close, which must be different. For example,
-- @charsInBalanced '(' ')'@ will parse "(hello (there))"
-- and return "hello (there)".  Stop if a blank line is
-- encountered.
charsInBalanced :: Char -> Char -> GenParser Char st String
charsInBalanced open close = try $ do
  char open
  raw <- many $     (many1 (noneOf [open, close, '\n']))
                <|> (do res <- charsInBalanced open close
                        return $ [open] ++ res ++ [close])
                <|> try (string "\n" >>~ notFollowedBy' blanklines)
  char close
  return $ concat raw

-- | Like @charsInBalanced@, but allow blank lines in the content.
charsInBalanced' :: Char -> Char -> GenParser Char st String
charsInBalanced' open close = try $ do
  char open
  raw <- many $       (many1 (noneOf [open, close]))
                  <|> (do res <- charsInBalanced' open close
                          return $ [open] ++ res ++ [close])
  char close
  return $ concat raw

-- Auxiliary functions for romanNumeral:

lowercaseRomanDigits :: [Char]
lowercaseRomanDigits = ['i','v','x','l','c','d','m']

uppercaseRomanDigits :: [Char]
uppercaseRomanDigits = map toUpper lowercaseRomanDigits

-- | Parses a roman numeral (uppercase or lowercase), returns number.
romanNumeral :: Bool                  -- ^ Uppercase if true
             -> GenParser Char st Int
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

emailChar :: GenParser Char st Char
emailChar = alphaNum <|> oneOf "-+_."

domainChar :: GenParser Char st Char
domainChar = alphaNum <|> char '-'

domain :: GenParser Char st [Char]
domain = do
  first <- many1 domainChar
  dom <- many1 $ try (char '.' >> many1 domainChar )
  return $ intercalate "." (first:dom)

-- | Parses an email address; returns string.
emailAddress :: GenParser Char st [Char]
emailAddress = try $ do
  firstLetter <- alphaNum
  restAddr <- many emailChar
  let addr = firstLetter:restAddr
  char '@'
  dom <- domain
  return $ addr ++ '@':dom

-- | Parses a URI.
uri :: GenParser Char st String
uri = try $ do
  str <- many1 $ satisfy isAllowedInURI
  case parseURI str of
       Just uri' -> if uriScheme uri' `elem` [ "http:", "https:", "ftp:",
                                               "file:", "mailto:",
                                               "news:", "telnet:" ]
                       then return $ show uri'
                       else fail "not a URI"
       Nothing   -> fail "not a URI"

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

-- | Parses a character and returns 'Null' (so that the parser can move on
-- if it gets stuck).
nullBlock :: GenParser Char st Block
nullBlock = anyChar >> return Null

-- | Fail if reader is in strict markdown syntax mode.
failIfStrict :: GenParser Char ParserState ()
failIfStrict = do
  state <- getState
  if stateStrict state then fail "strict mode" else return ()

-- | Fail unless we're in literate haskell mode.
failUnlessLHS :: GenParser tok ParserState ()
failUnlessLHS = do
  state <- getState
  if stateLiterateHaskell state then return () else fail "Literate haskell feature"

-- | Parses backslash, then applies character parser.
escaped :: GenParser Char st Char  -- ^ Parser for character to escape
        -> GenParser Char st Inline
escaped parser = try $ do
  char '\\'
  result <- parser
  return (Str [result])

-- | Parses an uppercase roman numeral and returns (UpperRoman, number).
upperRoman :: GenParser Char st (ListNumberStyle, Int)
upperRoman = do
  num <- romanNumeral True
  return (UpperRoman, num)

-- | Parses a lowercase roman numeral and returns (LowerRoman, number).
lowerRoman :: GenParser Char st (ListNumberStyle, Int)
lowerRoman = do
  num <- romanNumeral False
  return (LowerRoman, num)

-- | Parses a decimal numeral and returns (Decimal, number).
decimal :: GenParser Char st (ListNumberStyle, Int)
decimal = do
  num <- many1 digit
  return (Decimal, read num)

-- | Parses a '#' returns (DefaultStyle, 1).
defaultNum :: GenParser Char st (ListNumberStyle, Int)
defaultNum = do
  char '#'
  return (DefaultStyle, 1)

-- | Parses a lowercase letter and returns (LowerAlpha, number).
lowerAlpha :: GenParser Char st (ListNumberStyle, Int)
lowerAlpha = do
  ch <- oneOf ['a'..'z']
  return (LowerAlpha, ord ch - ord 'a' + 1)

-- | Parses an uppercase letter and returns (UpperAlpha, number).
upperAlpha :: GenParser Char st (ListNumberStyle, Int)
upperAlpha = do
  ch <- oneOf ['A'..'Z']
  return (UpperAlpha, ord ch - ord 'A' + 1)

-- | Parses a roman numeral i or I
romanOne :: GenParser Char st (ListNumberStyle, Int)
romanOne = (char 'i' >> return (LowerRoman, 1)) <|>
           (char 'I' >> return (UpperRoman, 1))

-- | Parses an ordered list marker and returns list attributes.
anyOrderedListMarker :: GenParser Char st ListAttributes 
anyOrderedListMarker = choice $ 
  [delimParser numParser | delimParser <- [inPeriod, inOneParen, inTwoParens],
                           numParser <- [decimal, defaultNum, romanOne,
                           lowerAlpha, lowerRoman, upperAlpha, upperRoman]]

-- | Parses a list number (num) followed by a period, returns list attributes.
inPeriod :: GenParser Char st (ListNumberStyle, Int)
         -> GenParser Char st ListAttributes 
inPeriod num = try $ do
  (style, start) <- num
  char '.'
  let delim = if style == DefaultStyle
                 then DefaultDelim
                 else Period
  return (start, style, delim)
 
-- | Parses a list number (num) followed by a paren, returns list attributes.
inOneParen :: GenParser Char st (ListNumberStyle, Int)
           -> GenParser Char st ListAttributes 
inOneParen num = try $ do
  (style, start) <- num
  char ')'
  return (start, style, OneParen)

-- | Parses a list number (num) enclosed in parens, returns list attributes.
inTwoParens :: GenParser Char st (ListNumberStyle, Int)
            -> GenParser Char st ListAttributes 
inTwoParens num = try $ do
  char '('
  (style, start) <- num
  char ')'
  return (start, style, TwoParens)

-- | Parses an ordered list marker with a given style and delimiter,
-- returns number.
orderedListMarker :: ListNumberStyle 
                  -> ListNumberDelim 
                  -> GenParser Char st Int
orderedListMarker style delim = do
  let num = defaultNum <|>  -- # can continue any kind of list
            case style of
               DefaultStyle -> decimal
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
charRef :: GenParser Char st Inline
charRef = do
  c <- characterReference
  return $ Str [c]

-- | Parse a string with a given parser and state.
readWith :: GenParser Char ParserState a      -- ^ parser
         -> ParserState                    -- ^ initial state
         -> String                         -- ^ input string
         -> a
readWith parser state input = 
    case runParser parser state "source" input of
      Left err     -> error $ "\nError:\n" ++ show err
      Right result -> result

-- | Parse a string with @parser@ (for testing).
testStringWith :: (Show a) => GenParser Char ParserState a
               -> String
               -> IO ()
testStringWith parser str = putStrLn $ show $
                            readWith parser defaultParserState str

-- | Parsing options.
data ParserState = ParserState
    { stateParseRaw        :: Bool,          -- ^ Parse raw HTML and LaTeX?
      stateParserContext   :: ParserContext, -- ^ Inside list?
      stateQuoteContext    :: QuoteContext,  -- ^ Inside quoted environment?
      stateSanitizeHTML    :: Bool,          -- ^ Sanitize HTML?
      stateKeys            :: KeyTable,      -- ^ List of reference keys
#ifdef _CITEPROC
      stateCitations       :: [String],      -- ^ List of available citations
#endif
      stateNotes           :: NoteTable,     -- ^ List of notes
      stateTabStop         :: Int,           -- ^ Tab stop
      stateStandalone      :: Bool,          -- ^ Parse bibliographic info?
      stateTitle           :: [Inline],      -- ^ Title of document
      stateAuthors         :: [[Inline]],    -- ^ Authors of document
      stateDate            :: [Inline],      -- ^ Date of document
      stateStrict          :: Bool,          -- ^ Use strict markdown syntax?
      stateSmart           :: Bool,          -- ^ Use smart typography?
      stateLiterateHaskell :: Bool,          -- ^ Treat input as literate haskell
      stateColumns         :: Int,           -- ^ Number of columns in terminal
      stateHeaderTable     :: [HeaderType],  -- ^ Ordered list of header types used
      stateIndentedCodeClasses :: [String]   -- ^ Classes to use for indented code blocks
    }
    deriving Show

defaultParserState :: ParserState
defaultParserState = 
    ParserState { stateParseRaw        = False,
                  stateParserContext   = NullState,
                  stateQuoteContext    = NoQuote,
                  stateSanitizeHTML    = False,
                  stateKeys            = [],
#ifdef _CITEPROC
                  stateCitations       = [],
#endif
                  stateNotes           = [],
                  stateTabStop         = 4,
                  stateStandalone      = False,
                  stateTitle           = [],
                  stateAuthors         = [],
                  stateDate            = [],
                  stateStrict          = False,
                  stateSmart           = False,
                  stateLiterateHaskell = False,
                  stateColumns         = 80,
                  stateHeaderTable     = [],
                  stateIndentedCodeClasses = [] }

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

type KeyTable = [([Inline], Target)]

-- | Look up key in key table and return target object.
lookupKeySrc :: KeyTable  -- ^ Key table
             -> [Inline]  -- ^ Key
             -> Maybe Target
lookupKeySrc table key = case find (refsMatch key . fst) table of
                           Nothing       -> Nothing
                           Just (_, src) -> Just src

-- | Returns @True@ if keys match (case insensitive).
refsMatch :: [Inline] -> [Inline] -> Bool
refsMatch ((Str x):restx) ((Str y):resty) = 
    ((map toLower x) == (map toLower y)) && refsMatch restx resty
refsMatch ((Emph x):restx) ((Emph y):resty) = 
    refsMatch x y && refsMatch restx resty
refsMatch ((Strong x):restx) ((Strong y):resty) = 
    refsMatch x y && refsMatch restx resty
refsMatch ((Strikeout x):restx) ((Strikeout y):resty) = 
    refsMatch x y && refsMatch restx resty
refsMatch ((Superscript x):restx) ((Superscript y):resty) = 
    refsMatch x y && refsMatch restx resty
refsMatch ((Subscript x):restx) ((Subscript y):resty) = 
    refsMatch x y && refsMatch restx resty
refsMatch ((SmallCaps x):restx) ((SmallCaps y):resty) = 
    refsMatch x y && refsMatch restx resty
refsMatch ((Quoted t x):restx) ((Quoted u y):resty) = 
    t == u && refsMatch x y && refsMatch restx resty
refsMatch ((Code x):restx) ((Code y):resty) = 
    ((map toLower x) == (map toLower y)) && refsMatch restx resty
refsMatch ((Math t x):restx) ((Math u y):resty) = 
    ((map toLower x) == (map toLower y)) && t == u && refsMatch restx resty
refsMatch ((TeX x):restx) ((TeX y):resty) = 
    ((map toLower x) == (map toLower y)) && refsMatch restx resty
refsMatch ((HtmlInline x):restx) ((HtmlInline y):resty) = 
    ((map toLower x) == (map toLower y)) && refsMatch restx resty
refsMatch (x:restx) (y:resty) = (x == y) && refsMatch restx resty
refsMatch [] x = null x
refsMatch x [] = null x

--
-- Prettyprinting
--

-- | A version of hang that works like the version in pretty-1.0.0.0
hang' :: Doc -> Int -> Doc -> Doc
hang' d1 n d2 = d1 $$ (nest n d2)

-- | Indent string as a block.
indentBy :: Int    -- ^ Number of spaces to indent the block 
         -> Int    -- ^ Number of spaces (rel to block) to indent first line
         -> String -- ^ Contents of block to indent
         -> String
indentBy _ _ [] = ""
indentBy num first str = 
  let (firstLine:restLines) = lines str 
      firstLineIndent = num + first
  in  (replicate firstLineIndent ' ') ++ firstLine ++ "\n" ++ 
      (intercalate "\n" $ map ((replicate num ' ') ++ ) restLines)

-- | Prettyprint list of Pandoc blocks elements.
prettyBlockList :: Int       -- ^ Number of spaces to indent list of blocks
                -> [Block]   -- ^ List of blocks
                -> String
prettyBlockList indent [] = indentBy indent 0 "[]"
prettyBlockList indent blocks = indentBy indent (-2) $ "[ " ++ 
  (intercalate "\n, " (map prettyBlock blocks)) ++ " ]"

-- | Prettyprint Pandoc block element.
prettyBlock :: Block -> String
prettyBlock (BlockQuote blocks) = "BlockQuote\n  " ++ 
                                  (prettyBlockList 2 blocks) 
prettyBlock (OrderedList attribs blockLists) = 
  "OrderedList " ++ show attribs ++ "\n" ++ indentBy 2 0 ("[ " ++ 
  (intercalate ", " $ map (\blocks -> prettyBlockList 2 blocks)
  blockLists)) ++ " ]"
prettyBlock (BulletList blockLists) = "BulletList\n" ++ 
  indentBy 2 0 ("[ " ++ (intercalate ", "
  (map (\blocks -> prettyBlockList 2 blocks) blockLists))) ++ " ]" 
prettyBlock (DefinitionList items) = "DefinitionList\n" ++ 
  indentBy 2 0 ("[ " ++ (intercalate "\n, "
  (map (\(term, defs) -> "(" ++ show term ++ ",\n" ++ 
  indentBy 3 0 ("[ " ++ (intercalate ", "
  (map (\blocks -> prettyBlockList 2 blocks) defs)) ++ "]") ++
   ")") items))) ++ " ]" 
prettyBlock (Table caption aligns widths header rows) = 
  "Table " ++ show caption ++ " " ++ show aligns ++ " " ++ 
  show widths ++ "\n" ++ prettyRow header ++ " [\n" ++  
  (intercalate ",\n" (map prettyRow rows)) ++ " ]"
  where prettyRow cols = indentBy 2 0 ("[ " ++ (intercalate ", "
                         (map (\blocks -> prettyBlockList 2 blocks) 
                         cols))) ++ " ]"
prettyBlock block = show block

-- | Prettyprint Pandoc document.
prettyPandoc :: Pandoc -> String
prettyPandoc (Pandoc meta blocks) = "Pandoc " ++ "(" ++ show meta ++ 
  ")\n" ++ (prettyBlockList 0 blocks) ++ "\n"

--
-- Pandoc block and inline list processing
--

-- | Generate infinite lazy list of markers for an ordered list,
-- depending on list attributes.
orderedListMarkers :: (Int, ListNumberStyle, ListNumberDelim) -> [String]
orderedListMarkers (start, numstyle, numdelim) = 
  let singleton c = [c]
      nums = case numstyle of
                     DefaultStyle -> map show [start..]
                     Decimal      -> map show [start..]
                     UpperAlpha   -> drop (start - 1) $ cycle $ 
                                     map singleton ['A'..'Z']
                     LowerAlpha   -> drop (start - 1) $ cycle $
                                     map singleton ['a'..'z']
                     UpperRoman   -> map toRomanNumeral [start..]
                     LowerRoman   -> map (map toLower . toRomanNumeral) [start..]
      inDelim str = case numdelim of
                            DefaultDelim -> str ++ "."
                            Period       -> str ++ "."
                            OneParen     -> str ++ ")"
                            TwoParens    -> "(" ++ str ++ ")"
  in  map inDelim nums

-- | Normalize a list of inline elements: remove leading and trailing
-- @Space@ elements, collapse double @Space@s into singles, and
-- remove empty Str elements.
normalizeSpaces :: [Inline] -> [Inline]
normalizeSpaces [] = []
normalizeSpaces list = 
    let removeDoubles [] = []
        removeDoubles (Space:Space:rest) = removeDoubles (Space:rest)
        removeDoubles (Space:(Str ""):Space:rest) = removeDoubles (Space:rest)
        removeDoubles ((Str ""):rest) = removeDoubles rest 
        removeDoubles (x:rest) = x:(removeDoubles rest)
        removeLeading (Space:xs) = removeLeading xs
        removeLeading x = x
        removeTrailing [] = []
        removeTrailing lst = if (last lst == Space)
                                then init lst
                                else lst
    in  removeLeading $ removeTrailing $ removeDoubles list

-- | Change final list item from @Para@ to @Plain@ if the list contains
-- no other @Para@ blocks.
compactify :: [[Block]]  -- ^ List of list items (each a list of blocks)
           -> [[Block]]
compactify [] = []
compactify items =
  case (init items, last items) of
       (_,[])          -> items
       (others, final) ->
            case last final of
                 Para a -> case (filter isPara $ concat items) of
                                -- if this is only Para, change to Plain
                                [_] -> others ++ [init final ++ [Plain a]]
                                _   -> items
                 _      -> items

isPara :: Block -> Bool
isPara (Para _) = True
isPara _        = False

-- | Data structure for defining hierarchical Pandoc documents
data Element = Blk Block 
             | Sec Int [Int] String [Inline] [Element]
             --    lvl  num ident  label    contents
             deriving (Eq, Read, Show, Typeable, Data)

-- | Convert Pandoc inline list to plain text identifier.
inlineListToIdentifier :: [Inline] -> String
inlineListToIdentifier = dropWhile (not . isAlpha) . inlineListToIdentifier'

inlineListToIdentifier' :: [Inline] -> [Char]
inlineListToIdentifier' [] = ""
inlineListToIdentifier' (x:xs) =
  xAsText ++ inlineListToIdentifier' xs
  where xAsText = case x of
          Str s          -> filter (\c -> c `elem` "_-.~" || not (isPunctuation c)) $
                            intercalate "-" $ words $ map toLower s
          Emph lst       -> inlineListToIdentifier' lst
          Strikeout lst  -> inlineListToIdentifier' lst
          Superscript lst -> inlineListToIdentifier' lst
          SmallCaps   lst -> inlineListToIdentifier' lst
          Subscript lst  -> inlineListToIdentifier' lst
          Strong lst     -> inlineListToIdentifier' lst
          Quoted _ lst   -> inlineListToIdentifier' lst
          Cite   _ lst   -> inlineListToIdentifier' lst
          Code s         -> s
          Space          -> "-"
          EmDash         -> "-"
          EnDash         -> "-"
          Apostrophe     -> ""
          Ellipses       -> ""
          LineBreak      -> "-"
          Math _ _       -> ""
          TeX _          -> ""
          HtmlInline _   -> ""
          Link lst _     -> inlineListToIdentifier' lst
          Image lst _    -> inlineListToIdentifier' lst
          Note _         -> ""

-- | Convert list of Pandoc blocks into (hierarchical) list of Elements
hierarchicalize :: [Block] -> [Element]
hierarchicalize blocks = S.evalState (hierarchicalizeWithIds blocks) ([],[])

hierarchicalizeWithIds :: [Block] -> S.State ([Int],[String]) [Element]
hierarchicalizeWithIds [] = return []
hierarchicalizeWithIds ((Header level title'):xs) = do
  (lastnum, usedIdents) <- S.get
  let ident = uniqueIdent title' usedIdents
  let lastnum' = take level lastnum
  let newnum = if length lastnum' >= level
                  then init lastnum' ++ [last lastnum' + 1] 
                  else lastnum ++ replicate (level - length lastnum - 1) 0 ++ [1]
  S.put (newnum, (ident : usedIdents))
  let (sectionContents, rest) = break (headerLtEq level) xs
  sectionContents' <- hierarchicalizeWithIds sectionContents
  rest' <- hierarchicalizeWithIds rest
  return $ Sec level newnum ident title' sectionContents' : rest'
hierarchicalizeWithIds (x:rest) = do
  rest' <- hierarchicalizeWithIds rest
  return $ (Blk x) : rest'

headerLtEq :: Int -> Block -> Bool
headerLtEq level (Header l _) = l <= level
headerLtEq _ _ = False

uniqueIdent :: [Inline] -> [String] -> String
uniqueIdent title' usedIdents =
  let baseIdent = inlineListToIdentifier title'
      numIdent n = baseIdent ++ "-" ++ show n
  in  if baseIdent `elem` usedIdents
        then case find (\x -> numIdent x `notElem` usedIdents) ([1..60000] :: [Int]) of
                  Just x  -> numIdent x
                  Nothing -> baseIdent   -- if we have more than 60,000, allow repeats
        else baseIdent

-- | True if block is a Header block.
isHeaderBlock :: Block -> Bool
isHeaderBlock (Header _ _) = True
isHeaderBlock _ = False

--
-- Writer options
--

data HTMLMathMethod = PlainMath 
                    | LaTeXMathML (Maybe String)  -- url of LaTeXMathML.js
                    | JsMath (Maybe String)       -- url of jsMath load script
                    | GladTeX
                    | MimeTeX String              -- url of mimetex.cgi 
                    deriving (Show, Read, Eq)

-- | Methods for obfuscating email addresses in HTML.
data ObfuscationMethod = NoObfuscation
                       | ReferenceObfuscation
                       | JavascriptObfuscation
                       deriving (Show, Read, Eq)

-- | Options for writers
data WriterOptions = WriterOptions
  { writerStandalone       :: Bool   -- ^ Include header and footer
  , writerTemplate         :: String -- ^ Template to use in standalone mode
  , writerVariables        :: [(String, String)] -- ^ Variables to set in template
  , writerIncludeBefore    :: String -- ^ Text to include before the body
  , writerIncludeAfter     :: String -- ^ Text to include after the body
  , writerTabStop          :: Int    -- ^ Tabstop for conversion btw spaces and tabs
  , writerTableOfContents  :: Bool   -- ^ Include table of contents
  , writerS5               :: Bool   -- ^ We're writing S5 
  , writerXeTeX            :: Bool   -- ^ Create latex suitable for use by xetex
  , writerHTMLMathMethod   :: HTMLMathMethod  -- ^ How to print math in HTML
  , writerIgnoreNotes      :: Bool   -- ^ Ignore footnotes (used in making toc)
  , writerIncremental      :: Bool   -- ^ Incremental S5 lists
  , writerNumberSections   :: Bool   -- ^ Number sections in LaTeX
  , writerStrictMarkdown   :: Bool   -- ^ Use strict markdown syntax
  , writerReferenceLinks   :: Bool   -- ^ Use reference links in writing markdown, rst
  , writerWrapText         :: Bool   -- ^ Wrap text to line length
  , writerLiterateHaskell  :: Bool   -- ^ Write as literate haskell
  , writerEmailObfuscation :: ObfuscationMethod -- ^ How to obfuscate emails
  , writerIdentifierPrefix :: String -- ^ Prefix for section & note ids in HTML
  } deriving Show

-- | Default writer options.
defaultWriterOptions :: WriterOptions
defaultWriterOptions = 
  WriterOptions { writerStandalone       = False
                , writerTemplate         = ""
                , writerVariables        = []
                , writerIncludeBefore    = ""
                , writerIncludeAfter     = ""
                , writerTabStop          = 4
                , writerTableOfContents  = False
                , writerS5               = False
                , writerXeTeX            = True
                , writerHTMLMathMethod   = PlainMath
                , writerIgnoreNotes      = False
                , writerIncremental      = False
                , writerNumberSections   = False
                , writerStrictMarkdown   = False
                , writerReferenceLinks   = False
                , writerWrapText         = True
                , writerLiterateHaskell  = False
                , writerEmailObfuscation = JavascriptObfuscation
                , writerIdentifierPrefix = ""
                }

--
-- File handling
--

-- | Perform an IO action in a directory, returning to starting directory.
inDirectory :: FilePath -> IO a -> IO a
inDirectory path action = do
  oldDir <- getCurrentDirectory
  setCurrentDirectory path
  result <- action
  setCurrentDirectory oldDir
  return result

-- | Read file from user data directory or, if not found there, from
-- Cabal data directory.  On unix the user data directory is @$HOME/.pandoc@.
readDataFile :: FilePath -> IO String
readDataFile fname = do
  userDir <- getAppUserDataDirectory "pandoc"
  catch (readFile $ userDir </> fname) (\_ -> getDataFileName fname >>= readFile) 
