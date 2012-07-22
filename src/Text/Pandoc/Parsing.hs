{-
Copyright (C) 2006-2010 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-2010 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

A utility library with parsers used in pandoc readers.
-}
module Text.Pandoc.Parsing ( (>>~),
                             anyLine,
                             many1Till,
                             notFollowedBy',
                             oneOfStrings,
                             spaceChar,
                             nonspaceChar,
                             skipSpaces,
                             blankline,
                             blanklines,
                             enclosed,
                             stringAnyCase,
                             parseFromString,
                             lineClump,
                             charsInBalanced,
                             romanNumeral,
                             emailAddress,
                             uri,
                             withHorizDisplacement,
                             withRaw,
                             nullBlock,
                             failIfStrict,
                             failUnlessLHS,
                             escaped,
                             characterReference,
                             updateLastStrPos,
                             anyOrderedListMarker,
                             orderedListMarker,
                             charRef,
                             tableWith,
                             extraTableWith,
                             gridTableWith,
                             readWith,
                             testStringWith,
                             ParserState (..),
                             defaultParserState,
                             HeaderType (..),
                             ParserContext (..),
                             QuoteContext (..),
                             NoteTable,
                             KeyTable,
                             Key,
                             toKey,
                             fromKey,
                             lookupKeySrc,
                             smartPunctuation,
                             macro,
                             applyMacros',
                             -- * Re-exports from Text.Pandoc.Parsec
                             Parser,
                             runParser,
                             parse,
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
                             sepBy1,
                             sepBy,
                             sepEndBy,
                             endBy1,
                             option,
                             optional,
                             optionMaybe,
                             getState,
                             setState,
                             updateState,
                             getPosition,
                             setPosition,
                             sourceColumn,
                             sourceLine,
                             newPos,
                             token
                             )
where

import Text.Pandoc.Definition
import Text.Pandoc.Generic
import qualified Text.Pandoc.UTF8 as UTF8 (putStrLn)
import Text.Parsec
import Text.Parsec.Pos (newPos)
import Data.Char ( toLower, toUpper, ord, isAscii, isAlphaNum, isDigit, isPunctuation )
import Data.List ( intercalate, transpose )
import Network.URI ( parseURI, URI (..), isAllowedInURI )
import Control.Monad ( join, liftM, guard, mzero )
import Text.Pandoc.Shared
import qualified Data.Map as M
import Text.TeXMath.Macros (applyMacros, Macro, parseMacroDefinitions)
import Text.HTML.TagSoup.Entity ( lookupEntity )
import Data.Default

type Parser t s = Parsec t s

-- | Like >>, but returns the operation on the left.
-- (Suggested by Tillmann Rendel on Haskell-cafe list.)
(>>~) :: (Monad m) => m a -> m b -> m a
a >>~ b = a >>= \x -> b >> return x

-- | Parse any line of text
anyLine :: Parsec [Char] st [Char]
anyLine = manyTill anyChar newline

-- | Like @manyTill@, but reads at least one item.
many1Till :: Parsec [tok] st a
          -> Parsec [tok] st end
          -> Parsec [tok] st [a]
many1Till p end = do
         first <- p
         rest <- manyTill p end
         return (first:rest)

-- | A more general form of @notFollowedBy@.  This one allows any 
-- type of parser to be specified, and succeeds only if that parser fails.
-- It does not consume any input.
notFollowedBy' :: Show b => Parsec [a] st b -> Parsec [a] st ()
notFollowedBy' p  = try $ join $  do  a <- try p
                                      return (unexpected (show a))
                                  <|>
                                  return (return ())
-- (This version due to Andrew Pimlott on the Haskell mailing list.)

-- | Parses one of a list of strings (tried in order).  
oneOfStrings :: [String] -> Parsec [Char] st String
oneOfStrings listOfStrings = choice $ map (try . string) listOfStrings

-- | Parses a space or tab.
spaceChar :: Parsec [Char] st Char
spaceChar = satisfy $ \c -> c == ' ' || c == '\t'

-- | Parses a nonspace, nonnewline character.
nonspaceChar :: Parsec [Char] st Char
nonspaceChar = satisfy $ \x -> x /= '\t' && x /= '\n' && x /= ' ' && x /= '\r'

-- | Skips zero or more spaces or tabs.
skipSpaces :: Parsec [Char] st ()
skipSpaces = skipMany spaceChar

-- | Skips zero or more spaces or tabs, then reads a newline.
blankline :: Parsec [Char] st Char
blankline = try $ skipSpaces >> newline

-- | Parses one or more blank lines and returns a string of newlines.
blanklines :: Parsec [Char] st [Char]
blanklines = many1 blankline

-- | Parses material enclosed between start and end parsers.
enclosed :: Parsec [Char] st t   -- ^ start parser
         -> Parsec [Char] st end  -- ^ end parser
         -> Parsec [Char] st a    -- ^ content parser (to be used repeatedly)
         -> Parsec [Char] st [a]
enclosed start end parser = try $ 
  start >> notFollowedBy space >> many1Till parser end

-- | Parse string, case insensitive.
stringAnyCase :: [Char] -> Parsec [Char] st String
stringAnyCase [] = string ""
stringAnyCase (x:xs) = do
  firstChar <- char (toUpper x) <|> char (toLower x)
  rest <- stringAnyCase xs
  return (firstChar:rest)

-- | Parse contents of 'str' using 'parser' and return result.
parseFromString :: Parsec [tok] st a -> [tok] -> Parsec [tok] st a
parseFromString parser str = do
  oldPos <- getPosition
  oldInput <- getInput
  setInput str
  result <- parser
  setInput oldInput
  setPosition oldPos
  return result

-- | Parse raw line block up to and including blank lines.
lineClump :: Parsec [Char] st String
lineClump = blanklines 
          <|> (many1 (notFollowedBy blankline >> anyLine) >>= return . unlines)

-- | Parse a string of characters between an open character
-- and a close character, including text between balanced
-- pairs of open and close, which must be different. For example,
-- @charsInBalanced '(' ')' anyChar@ will parse "(hello (there))"
-- and return "hello (there)".
charsInBalanced :: Char -> Char -> Parsec [Char] st Char
                -> Parsec [Char] st String
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
romanNumeral :: Bool                  -- ^ Uppercase if true
             -> Parsec [Char] st Int
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

emailChar :: Parsec [Char] st Char
emailChar = alphaNum <|>
            satisfy (\c -> c == '-' || c == '+' || c == '_' || c == '.')

domainChar :: Parsec [Char] st Char
domainChar = alphaNum <|> char '-'

domain :: Parsec [Char] st [Char]
domain = do
  first <- many1 domainChar
  dom <- many1 $ try (char '.' >> many1 domainChar )
  return $ intercalate "." (first:dom)

-- | Parses an email address; returns original and corresponding
-- escaped mailto: URI.
emailAddress :: Parsec [Char] st (String, String)
emailAddress = try $ do
  firstLetter <- alphaNum
  restAddr <- many emailChar
  let addr = firstLetter:restAddr
  char '@'
  dom <- domain
  let full = addr ++ '@':dom
  return (full, escapeURI $ "mailto:" ++ full)

-- | Parses a URI. Returns pair of original and URI-escaped version.
uri :: Parsec [Char] st (String, String)
uri = try $ do
  let protocols = [ "http:", "https:", "ftp:", "file:", "mailto:",
                    "news:", "telnet:" ]
  lookAhead $ oneOfStrings protocols
  -- Scan non-ascii characters and ascii characters allowed in a URI.
  -- We allow punctuation except when followed by a space, since
  -- we don't want the trailing '.' in 'http://google.com.'
  let innerPunct = try $ satisfy isPunctuation >>~
                         notFollowedBy (newline <|> spaceChar)
  let uriChar = innerPunct <|>
                satisfy (\c -> not (isPunctuation c) &&
                            (not (isAscii c) || isAllowedInURI c))
  -- We want to allow
  -- http://en.wikipedia.org/wiki/State_of_emergency_(disambiguation)
  -- as a URL, while NOT picking up the closing paren in
  -- (http://wikipedia.org)
  -- So we include balanced parens in the URL.
  let inParens = try $ do char '('
                          res <- many uriChar
                          char ')'
                          return $ '(' : res ++ ")"
  str <- liftM concat $ many1 $ inParens <|> count 1 (innerPunct <|> uriChar)
  -- now see if they amount to an absolute URI
  case parseURI (escapeURI str) of
       Just uri' -> if uriScheme uri' `elem` protocols
                       then return (str, show uri')
                       else fail "not a URI"
       Nothing   -> fail "not a URI"

-- | Applies a parser, returns tuple of its results and its horizontal
-- displacement (the difference between the source column at the end
-- and the source column at the beginning). Vertical displacement
-- (source row) is ignored.
withHorizDisplacement :: Parsec [Char] st a  -- ^ Parser to apply
                      -> Parsec [Char] st (a, Int) -- ^ (result, displacement)
withHorizDisplacement parser = do
  pos1 <- getPosition
  result <- parser
  pos2 <- getPosition
  return (result, sourceColumn pos2 - sourceColumn pos1)

-- | Applies a parser and returns the raw string that was parsed,
-- along with the value produced by the parser.
withRaw :: Parsec [Char] st a -> Parsec [Char] st (a, [Char])
withRaw parser = do
  pos1 <- getPosition
  inp <- getInput
  result <- parser
  pos2 <- getPosition
  let (l1,c1) = (sourceLine pos1, sourceColumn pos1)
  let (l2,c2) = (sourceLine pos2, sourceColumn pos2)
  let inplines = take ((l2 - l1) + 1) $ lines inp
  let raw = case inplines of
                []   -> error "raw: inplines is null" -- shouldn't happen
                [l]  -> take (c2 - c1) l
                ls   -> unlines (init ls) ++ take (c2 - 1) (last ls)
  return (result, raw)

-- | Parses a character and returns 'Null' (so that the parser can move on
-- if it gets stuck).
nullBlock :: Parsec [Char] st Block
nullBlock = anyChar >> return Null

-- | Fail if reader is in strict markdown syntax mode.
failIfStrict :: Parsec [a] ParserState ()
failIfStrict = do
  state <- getState
  if stateStrict state then fail "strict mode" else return ()

-- | Fail unless we're in literate haskell mode.
failUnlessLHS :: Parsec [tok] ParserState ()
failUnlessLHS = getState >>= guard . stateLiterateHaskell

-- | Parses backslash, then applies character parser.
escaped :: Parsec [Char] st Char  -- ^ Parser for character to escape
        -> Parsec [Char] st Char
escaped parser = try $ char '\\' >> parser

-- | Parse character entity.
characterReference :: Parsec [Char] st Char
characterReference = try $ do
  char '&'
  ent <- many1Till nonspaceChar (char ';')
  case lookupEntity ent of
       Just c  -> return c
       Nothing -> fail "entity not found"

-- | Parses an uppercase roman numeral and returns (UpperRoman, number).
upperRoman :: Parsec [Char] st (ListNumberStyle, Int)
upperRoman = do
  num <- romanNumeral True
  return (UpperRoman, num)

-- | Parses a lowercase roman numeral and returns (LowerRoman, number).
lowerRoman :: Parsec [Char] st (ListNumberStyle, Int)
lowerRoman = do
  num <- romanNumeral False
  return (LowerRoman, num)

-- | Parses a decimal numeral and returns (Decimal, number).
decimal :: Parsec [Char] st (ListNumberStyle, Int)
decimal = do
  num <- many1 digit
  return (Decimal, read num)

-- | Parses a '@' and optional label and
-- returns (DefaultStyle, [next example number]).  The next
-- example number is incremented in parser state, and the label
-- (if present) is added to the label table.
exampleNum :: Parsec [Char] ParserState (ListNumberStyle, Int)
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
defaultNum :: Parsec [Char] st (ListNumberStyle, Int)
defaultNum = do
  char '#'
  return (DefaultStyle, 1)

-- | Parses a lowercase letter and returns (LowerAlpha, number).
lowerAlpha :: Parsec [Char] st (ListNumberStyle, Int)
lowerAlpha = do
  ch <- oneOf ['a'..'z']
  return (LowerAlpha, ord ch - ord 'a' + 1)

-- | Parses an uppercase letter and returns (UpperAlpha, number).
upperAlpha :: Parsec [Char] st (ListNumberStyle, Int)
upperAlpha = do
  ch <- oneOf ['A'..'Z']
  return (UpperAlpha, ord ch - ord 'A' + 1)

-- | Parses a roman numeral i or I
romanOne :: Parsec [Char] st (ListNumberStyle, Int)
romanOne = (char 'i' >> return (LowerRoman, 1)) <|>
           (char 'I' >> return (UpperRoman, 1))

-- | Parses an ordered list marker and returns list attributes.
anyOrderedListMarker :: Parsec [Char] ParserState ListAttributes 
anyOrderedListMarker = choice $ 
  [delimParser numParser | delimParser <- [inPeriod, inOneParen, inTwoParens],
                           numParser <- [decimal, exampleNum, defaultNum, romanOne,
                           lowerAlpha, lowerRoman, upperAlpha, upperRoman]]

-- | Parses a list number (num) followed by a period, returns list attributes.
inPeriod :: Parsec [Char] st (ListNumberStyle, Int)
         -> Parsec [Char] st ListAttributes 
inPeriod num = try $ do
  (style, start) <- num
  char '.'
  let delim = if style == DefaultStyle
                 then DefaultDelim
                 else Period
  return (start, style, delim)
 
-- | Parses a list number (num) followed by a paren, returns list attributes.
inOneParen :: Parsec [Char] st (ListNumberStyle, Int)
           -> Parsec [Char] st ListAttributes 
inOneParen num = try $ do
  (style, start) <- num
  char ')'
  return (start, style, OneParen)

-- | Parses a list number (num) enclosed in parens, returns list attributes.
inTwoParens :: Parsec [Char] st (ListNumberStyle, Int)
            -> Parsec [Char] st ListAttributes 
inTwoParens num = try $ do
  char '('
  (style, start) <- num
  char ')'
  return (start, style, TwoParens)

-- | Parses an ordered list marker with a given style and delimiter,
-- returns number.
orderedListMarker :: ListNumberStyle 
                  -> ListNumberDelim 
                  -> Parsec [Char] ParserState Int
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
charRef :: Parsec [Char] st Inline
charRef = do
  c <- characterReference
  return $ Str [c]

-- | Parse a table using 'headerParser', 'rowParser',
-- 'lineParser', and 'footerParser'.
tableWith :: Parsec [Char] ParserState ([[Block]], [Alignment], [Int])
          -> ([Int] -> Parsec [Char] ParserState [[Block]])
          -> Parsec [Char] ParserState sep
          -> Parsec [Char] ParserState end
          -> Parsec [Char] ParserState [Inline]
          -> Parsec [Char] ParserState Block
tableWith headerParser rowParser lineParser footerParser captionParser = try $ do
    caption' <- option [] captionParser
    (heads, aligns, indices) <- headerParser
    lines' <- rowParser indices `sepEndBy` lineParser
    footerParser
    caption <- if null caption'
                  then option [] captionParser
                  else return caption'
    state <- getState
    let numColumns = stateColumns state
    let widths = if (indices == [])
                    then replicate (length aligns) 0.0
                    else widthsFromIndices numColumns indices
    return $ Table caption aligns widths heads lines'

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


-- Parse an extra table (php-markdown): each line starts and ends with '|',
-- with a mandatory line of '--' to separate the (optionnal) headers from content.
extraTableWith :: GenParser Char ParserState Block    -- ^ Block parser
              -> GenParser Char ParserState [Inline] -- ^ Caption parser
              -> Bool                                -- ^ Headerless table
              -> GenParser Char ParserState Block
extraTableWith block tableCaption headless =
  tableWith (extraTableHeader headless block) (extraTableRow block) (extraTableSep '-') extraTableFooter tableCaption

-- | Parse header for an extra table.
extraTableHeader :: Bool -- ^ Headerless table
                -> GenParser Char ParserState Block
                -> GenParser Char ParserState ([[Block]], [Alignment], [Int])
extraTableHeader headless block = try $ do
  optional blanklines
  rawContent  <- if headless
                    then return $ repeat "" 
                    else many1
                         (notFollowedBy (extraTableHeaderSep) >> char '|' >>
                           many1Till anyChar newline)
  aligns <- extraTableHeaderDashedLine
  let indices  = []
  let rawHeads = if headless
                    then replicate (length aligns) ""
                    else map (intercalate " ") $ transpose
                       $ map (extraTableSplitLine ) 
                       $ map (trimOnceBy '|') rawContent
  heads <- mapM (parseFromString $ many block) $
               map removeLeadingTrailingSpace rawHeads
  return (heads, aligns, indices)

extraTableHeaderPart :: GenParser Char st Alignment
extraTableHeaderPart = do
  left <- optionMaybe (char ':')
  many1 (char '-')
  right <- optionMaybe (char ':')
  char '|'
  return $
    case (left,right) of
      (Nothing,Nothing) -> AlignDefault
      (Just _,Nothing)  -> AlignLeft
      (Nothing,Just _)  -> AlignRight
      (Just _,Just _)   -> AlignCenter

extraTableHeaderDashedLine :: GenParser Char st [Alignment]
extraTableHeaderDashedLine = try $ char '|' >> many1 (extraTableHeaderPart) >>~ blankline

extraTableHeaderSep :: GenParser Char ParserState Char
extraTableHeaderSep = try $ extraTableHeaderDashedLine >> return '\n'

-- | Split a header or data line in an extra table.
-- | The line must contain only *inside* separators.
extraTableSplitLine :: String -> [String]
extraTableSplitLine line = map removeLeadingSpace $
  splitBy (== '|') $ removeTrailingSpace line

-- Remove, if present, a character from both ends of a string
trimOnceBy :: Char -> String -> String
trimOnceBy ch s =
   if (head s == ch) && (last s == ch)
     then init $ tail s
     else s
trimEndOnceBy :: Char -> String -> String
trimEndOnceBy ch s =
   if (last s == ch)
     then init s
     else s

-- | Parse row of an extra table.
extraTableRow :: GenParser Char ParserState Block
             -> [Int]
             -> GenParser Char ParserState [[Block]]
extraTableRow block indices = do
  cols <- extraTableRawLine
  mapM (liftM compactifyCell . parseFromString (many block)) cols

extraTableRawLine :: GenParser Char ParserState [String]
extraTableRawLine = do
  char '|'
  line <- many1Till anyChar newline
  return (extraTableSplitLine $ trimEndOnceBy '|' line)

-- | Separator between rows of an extra table.
extraTableSep :: Char -> GenParser Char ParserState Char
extraTableSep ch = do return '\n'

-- | Parse footer for an extra table.
extraTableFooter :: GenParser Char ParserState [Char]
extraTableFooter = blanklines

---

-- Parse a grid table:  starts with row of '-' on top, then header
-- (which may be grid), then the rows,
-- which may be grid, separated by blank lines, and
-- ending with a footer (dashed line followed by blank line).
gridTableWith :: Parsec [Char] ParserState Block    -- ^ Block parser
              -> Parsec [Char] ParserState [Inline] -- ^ Caption parser
              -> Bool                                -- ^ Headerless table
              -> Parsec [Char] ParserState Block
gridTableWith block tableCaption headless =
  tableWith (gridTableHeader headless block) (gridTableRow block) (gridTableSep '-') gridTableFooter tableCaption

gridTableSplitLine :: [Int] -> String -> [String]
gridTableSplitLine indices line = map removeFinalBar $ tail $
  splitStringByIndices (init indices) $ removeTrailingSpace line

gridPart :: Char -> Parsec [Char] st (Int, Int)
gridPart ch = do
  dashes <- many1 (char ch)
  char '+'
  return (length dashes, length dashes + 1)

gridDashedLines :: Char -> Parsec [Char] st [(Int,Int)]
gridDashedLines ch = try $ char '+' >> many1 (gridPart ch) >>~ blankline

removeFinalBar :: String -> String
removeFinalBar =
  reverse . dropWhile (`elem` " \t") . dropWhile (=='|') . reverse

-- | Separator between rows of grid table.
gridTableSep :: Char -> Parsec [Char] ParserState Char
gridTableSep ch = try $ gridDashedLines ch >> return '\n'

-- | Parse header for a grid table.
gridTableHeader :: Bool -- ^ Headerless table
                -> Parsec [Char] ParserState Block
                -> Parsec [Char] ParserState ([[Block]], [Alignment], [Int])
gridTableHeader headless block = try $ do
  optional blanklines
  dashes <- gridDashedLines '-'
  rawContent  <- if headless
                    then return $ repeat "" 
                    else many1
                         (notFollowedBy (gridTableSep '=') >> char '|' >>
                           many1Till anyChar newline)
  if headless
     then return ()
     else gridTableSep '=' >> return ()
  let lines'   = map snd dashes
  let indices  = scanl (+) 0 lines'
  let aligns   = replicate (length lines') AlignDefault
  -- RST does not have a notion of alignments
  let rawHeads = if headless
                    then replicate (length dashes) ""
                    else map (intercalate " ") $ transpose
                       $ map (gridTableSplitLine indices) rawContent
  heads <- mapM (parseFromString $ many block) $
               map removeLeadingTrailingSpace rawHeads
  return (heads, aligns, indices)

gridTableRawLine :: [Int] -> Parsec [Char] ParserState [String]
gridTableRawLine indices = do
  char '|'
  line <- many1Till anyChar newline
  return (gridTableSplitLine indices line)

-- | Parse row of grid table.
gridTableRow :: Parsec [Char] ParserState Block
             -> [Int]
             -> Parsec [Char] ParserState [[Block]]
gridTableRow block indices = do
  colLines <- many1 (gridTableRawLine indices)
  let cols = map ((++ "\n") . unlines . removeOneLeadingSpace) $
               transpose colLines
  mapM (liftM compactifyCell . parseFromString (many block)) cols

removeOneLeadingSpace :: [String] -> [String]
removeOneLeadingSpace xs =
  if all startsWithSpace xs
     then map (drop 1) xs
     else xs
   where startsWithSpace ""     = True
         startsWithSpace (y:_) = y == ' '

compactifyCell :: [Block] -> [Block]
compactifyCell bs = head $ compactify [bs]

-- | Parse footer for a grid table.
gridTableFooter :: Parsec [Char] ParserState [Char]
gridTableFooter = blanklines

---

-- | Parse a string with a given parser and state.
readWith :: Parsec [t] ParserState a      -- ^ parser
         -> ParserState                    -- ^ initial state
         -> [t]                            -- ^ input
         -> a
readWith parser state input = 
    case runParser parser state "source" input of
      Left err'    -> error $ "\nError:\n" ++ show err'
      Right result -> result

-- | Parse a string with @parser@ (for testing).
testStringWith :: (Show a) => Parsec [Char] ParserState a
               -> String
               -> IO ()
testStringWith parser str = UTF8.putStrLn $ show $
                            readWith parser defaultParserState str

-- | Parsing options.
data ParserState = ParserState
    { stateParseRaw        :: Bool,          -- ^ Parse raw HTML and LaTeX?
      stateParserContext   :: ParserContext, -- ^ Inside list?
      stateQuoteContext    :: QuoteContext,  -- ^ Inside quoted environment?
      stateMaxNestingLevel :: Int,           -- ^ Max # of nested Strong/Emph
      stateLastStrPos      :: Maybe SourcePos, -- ^ Position after last str parsed
      stateKeys            :: KeyTable,      -- ^ List of reference keys
      stateCitations       :: [String],      -- ^ List of available citations
      stateNotes           :: NoteTable,     -- ^ List of notes
      stateTabStop         :: Int,           -- ^ Tab stop
      stateStandalone      :: Bool,          -- ^ Parse bibliographic info?
      stateTitle           :: [Inline],      -- ^ Title of document
      stateAuthors         :: [[Inline]],    -- ^ Authors of document
      stateDate            :: [Inline],      -- ^ Date of document
      stateStrict          :: Bool,          -- ^ Use strict markdown syntax?
      stateSmart           :: Bool,          -- ^ Use smart typography?
      stateOldDashes       :: Bool,          -- ^ Use pandoc <= 1.8.2.1 behavior
                                             --   in parsing dashes; -- is em-dash;
                                             --   before numeral is en-dash
      stateLiterateHaskell :: Bool,          -- ^ Treat input as literate haskell
      stateColumns         :: Int,           -- ^ Number of columns in terminal
      stateHeaderTable     :: [HeaderType],  -- ^ Ordered list of header types used
      stateIndentedCodeClasses :: [String],  -- ^ Classes to use for indented code blocks
      stateNextExample     :: Int,           -- ^ Number of next example
      stateExamples        :: M.Map String Int, -- ^ Map from example labels to numbers 
      stateHasChapters     :: Bool,          -- ^ True if \chapter encountered
      stateApplyMacros     :: Bool,          -- ^ Apply LaTeX macros?
      stateMacros          :: [Macro],       -- ^ List of macros defined so far
      stateRstDefaultRole  :: String         -- ^ Current rST default interpreted text role
    }
    deriving Show

instance Default ParserState where
  def = defaultParserState

defaultParserState :: ParserState
defaultParserState = 
    ParserState { stateParseRaw        = False,
                  stateParserContext   = NullState,
                  stateQuoteContext    = NoQuote,
                  stateMaxNestingLevel = 6,
                  stateLastStrPos      = Nothing,
                  stateKeys            = M.empty,
                  stateCitations       = [],
                  stateNotes           = [],
                  stateTabStop         = 4,
                  stateStandalone      = False,
                  stateTitle           = [],
                  stateAuthors         = [],
                  stateDate            = [],
                  stateStrict          = False,
                  stateSmart           = False,
                  stateOldDashes       = False,
                  stateLiterateHaskell = False,
                  stateColumns         = 80,
                  stateHeaderTable     = [],
                  stateIndentedCodeClasses = [],
                  stateNextExample     = 1,
                  stateExamples        = M.empty,
                  stateHasChapters     = False,
                  stateApplyMacros     = True,
                  stateMacros          = [],
                  stateRstDefaultRole  = "title-reference"}

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

newtype Key = Key [Inline] deriving (Show, Read, Eq, Ord)

toKey :: [Inline] -> Key
toKey = Key . bottomUp lowercase
  where lowercase :: Inline -> Inline
        lowercase (Str xs)          = Str (map toLower xs)
        lowercase (Math t xs)       = Math t (map toLower xs)
        lowercase (Code attr xs)    = Code attr (map toLower xs)
        lowercase (RawInline f xs)  = RawInline f (map toLower xs)
        lowercase LineBreak         = Space
        lowercase x                 = x

fromKey :: Key -> [Inline]
fromKey (Key xs) = xs

type KeyTable = M.Map Key Target

-- | Look up key in key table and return target object.
lookupKeySrc :: KeyTable  -- ^ Key table
             -> Key       -- ^ Key
             -> Maybe Target
lookupKeySrc table key = case M.lookup key table of
                           Nothing  -> Nothing
                           Just src -> Just src

-- | Fail unless we're in "smart typography" mode.
failUnlessSmart :: Parsec [tok] ParserState ()
failUnlessSmart = getState >>= guard . stateSmart

smartPunctuation :: Parsec [Char] ParserState Inline
                 -> Parsec [Char] ParserState Inline
smartPunctuation inlineParser = do
  failUnlessSmart
  choice [ quoted inlineParser, apostrophe, dash, ellipses ]

apostrophe :: Parsec [Char] ParserState Inline
apostrophe = (char '\'' <|> char '\8217') >> return (Str "\x2019")

quoted :: Parsec [Char] ParserState Inline
       -> Parsec [Char] ParserState Inline
quoted inlineParser = doubleQuoted inlineParser <|> singleQuoted inlineParser

withQuoteContext :: QuoteContext
                 -> (Parsec [Char] ParserState Inline)
                 -> Parsec [Char] ParserState Inline
withQuoteContext context parser = do
  oldState <- getState
  let oldQuoteContext = stateQuoteContext oldState
  setState oldState { stateQuoteContext = context }
  result <- parser
  newState <- getState
  setState newState { stateQuoteContext = oldQuoteContext }
  return result

singleQuoted :: Parsec [Char] ParserState Inline
             -> Parsec [Char] ParserState Inline
singleQuoted inlineParser = try $ do
  singleQuoteStart
  withQuoteContext InSingleQuote $ many1Till inlineParser singleQuoteEnd >>=
    return . Quoted SingleQuote . normalizeSpaces

doubleQuoted :: Parsec [Char] ParserState Inline
             -> Parsec [Char] ParserState Inline
doubleQuoted inlineParser = try $ do
  doubleQuoteStart
  withQuoteContext InDoubleQuote $ do
    contents <- manyTill inlineParser doubleQuoteEnd
    return . Quoted DoubleQuote . normalizeSpaces $ contents

failIfInQuoteContext :: QuoteContext -> Parsec [tok] ParserState ()
failIfInQuoteContext context = do
  st <- getState
  if stateQuoteContext st == context
     then fail "already inside quotes"
     else return ()

charOrRef :: [Char] -> Parsec [Char] st Char
charOrRef cs =
  oneOf cs <|> try (do c <- characterReference
                       guard (c `elem` cs)
                       return c)

updateLastStrPos :: Parsec [Char] ParserState ()
updateLastStrPos = getPosition >>= \p -> 
  updateState $ \s -> s{ stateLastStrPos = Just p }

singleQuoteStart :: Parsec [Char] ParserState ()
singleQuoteStart = do
  failIfInQuoteContext InSingleQuote
  pos <- getPosition
  st <- getState
  -- single quote start can't be right after str
  guard $ stateLastStrPos st /= Just pos
  try $ do charOrRef "'\8216\145"
           notFollowedBy (oneOf ")!],;:-? \t\n")
           notFollowedBy (char '.') <|> lookAhead (string "..." >> return ())
           notFollowedBy (try (oneOfStrings ["s","t","m","ve","ll","re"] >>
                               satisfy (not . isAlphaNum))) 
                               -- possess/contraction
           return ()

singleQuoteEnd :: Parsec [Char] st ()
singleQuoteEnd = try $ do
  charOrRef "'\8217\146"
  notFollowedBy alphaNum

doubleQuoteStart :: Parsec [Char] ParserState ()
doubleQuoteStart = do
  failIfInQuoteContext InDoubleQuote
  try $ do charOrRef "\"\8220\147"
           notFollowedBy (satisfy (\c -> c == ' ' || c == '\t' || c == '\n'))

doubleQuoteEnd :: Parsec [Char] st ()
doubleQuoteEnd = do
  charOrRef "\"\8221\148"
  return ()

ellipses :: Parsec [Char] st Inline
ellipses = do
  try (charOrRef "\8230\133") <|> try (string "..." >> return '…')
  return (Str "\8230")

dash :: Parsec [Char] ParserState Inline
dash = do
  oldDashes <- stateOldDashes `fmap` getState
  if oldDashes
     then emDashOld <|> enDashOld
     else Str `fmap` (hyphenDash <|> emDash <|> enDash)

-- Two hyphens = en-dash, three = em-dash
hyphenDash :: Parsec [Char] st String
hyphenDash = do
  try $ string "--"
  option "\8211" (char '-' >> return "\8212")

emDash :: Parsec [Char] st String
emDash = do
  try (charOrRef "\8212\151")
  return "\8212"

enDash :: Parsec [Char] st String
enDash = do
  try (charOrRef "\8212\151")
  return "\8211"

enDashOld :: Parsec [Char] st Inline
enDashOld = do
  try (charOrRef "\8211\150") <|>
    try (char '-' >> lookAhead (satisfy isDigit) >> return '–')
  return (Str "\8211")

emDashOld :: Parsec [Char] st Inline
emDashOld = do
  try (charOrRef "\8212\151") <|> (try $ string "--" >> optional (char '-') >> return '-')
  return (Str "\8212")

--
-- Macros
--

-- | Parse a \newcommand or \renewcommand macro definition.
macro :: Parsec [Char] ParserState Block
macro = do
  apply <- stateApplyMacros `fmap` getState
  inp <- getInput
  case parseMacroDefinitions inp of
       ([], _)    -> mzero
       (ms, rest) -> do def' <- count (length inp - length rest) anyChar
                        if apply
                           then do
                             updateState $ \st ->
                               st { stateMacros = ms ++ stateMacros st }
                             return Null
                           else return $ RawBlock "latex" def'

-- | Apply current macros to string.
applyMacros' :: String -> Parsec [Char] ParserState String
applyMacros' target = do
  apply <- liftM stateApplyMacros getState
  if apply
     then do macros <- liftM stateMacros getState
             return $ applyMacros macros target
     else return target

