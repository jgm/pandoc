{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances,
    FlexibleInstances#-}
{-
Copyright (C) 2006-2014 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-2014 John MacFarlane
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
                             oneOfStringsCI,
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
                             readWith,
                             testStringWith,
                             guardEnabled,
                             guardDisabled,
                             updateLastStrPos,
                             notAfterString,
                             ParserState (..),
                             HasReaderOptions (..),
                             HasHeaderMap (..),
                             HasIdentifierList (..),
                             HasMacros (..),
                             HasLastStrPosition (..),
                             defaultParserState,
                             HeaderType (..),
                             ParserContext (..),
                             QuoteContext (..),
                             NoteTable,
                             NoteTable',
                             KeyTable,
                             SubstTable,
                             Key (..),
                             toKey,
                             registerHeader,
                             smartPunctuation,
                             withQuoteContext,
                             singleQuoteStart,
                             singleQuoteEnd,
                             doubleQuoteStart,
                             doubleQuoteEnd,
                             ellipses,
                             apostrophe,
                             dash,
                             nested,
                             citeKey,
                             macro,
                             applyMacros',
                             Parser,
                             F(..),
                             runF,
                             askF,
                             asksF,
                             -- * Re-exports from Text.Pandoc.Parsec
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
                             token
                             )
where

import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Builder (Blocks, Inlines, rawBlock, HasMeta(..))
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.XML (fromEntities)
import qualified Text.Pandoc.UTF8 as UTF8 (putStrLn)
import Text.Parsec
import Text.Parsec.Pos (newPos)
import Data.Char ( toLower, toUpper, ord, chr, isAscii, isAlphaNum, isDigit,
                   isHexDigit, isSpace )
import Data.List ( intercalate, transpose )
import Text.Pandoc.Shared
import qualified Data.Map as M
import Text.TeXMath.Macros (applyMacros, Macro, parseMacroDefinitions)
import Text.Pandoc.Compat.TagSoupEntity ( lookupEntity )
import Text.Pandoc.Asciify (toAsciiChar)
import Data.Default
import qualified Data.Set as Set
import Control.Monad.Reader
import Control.Applicative ((*>), (<*), (<$), liftA2, Applicative)
import Data.Monoid
import Data.Maybe (catMaybes)

type Parser t s = Parsec t s

newtype F a = F { unF :: Reader ParserState a } deriving (Monad, Applicative, Functor)

runF :: F a -> ParserState -> a
runF = runReader . unF

askF :: F ParserState
askF = F ask

asksF :: (ParserState -> a) -> F a
asksF f = F $ asks f

instance Monoid a => Monoid (F a) where
  mempty = return mempty
  mappend = liftM2 mappend
  mconcat = liftM mconcat . sequence

-- | Like >>, but returns the operation on the left.
-- (Suggested by Tillmann Rendel on Haskell-cafe list.)
(>>~) :: (Monad m) => m a -> m b -> m a
a >>~ b = a >>= \x -> b >> return x

-- | Parse any line of text
anyLine :: Parser [Char] st [Char]
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

-- | Like @manyTill@, but reads at least one item.
many1Till :: Parser [tok] st a
          -> Parser [tok] st end
          -> Parser [tok] st [a]
many1Till p end = do
         first <- p
         rest <- manyTill p end
         return (first:rest)

-- | A more general form of @notFollowedBy@.  This one allows any
-- type of parser to be specified, and succeeds only if that parser fails.
-- It does not consume any input.
notFollowedBy' :: Show b => Parser [a] st b -> Parser [a] st ()
notFollowedBy' p  = try $ join $  do  a <- try p
                                      return (unexpected (show a))
                                  <|>
                                  return (return ())
-- (This version due to Andrew Pimlott on the Haskell mailing list.)

oneOfStrings' :: (Char -> Char -> Bool) -> [String] -> Parser [Char] st String
oneOfStrings' _ []   = fail "no strings"
oneOfStrings' matches strs = try $ do
  c <- anyChar
  let strs' = [xs | (x:xs) <- strs, x `matches` c]
  case strs' of
       []  -> fail "not found"
       _   -> (c:) `fmap` oneOfStrings' matches strs'
               <|> if "" `elem` strs'
                      then return [c]
                      else fail "not found"

-- | Parses one of a list of strings.  If the list contains
-- two strings one of which is a prefix of the other, the longer
-- string will be matched if possible.
oneOfStrings :: [String] -> Parser [Char] st String
oneOfStrings = oneOfStrings' (==)

-- | Parses one of a list of strings (tried in order), case insensitive.
oneOfStringsCI :: [String] -> Parser [Char] st String
oneOfStringsCI = oneOfStrings' ciMatch
  where ciMatch x y = toLower' x == toLower' y
        -- this optimizes toLower by checking common ASCII case
        -- first, before calling the expensive unicode-aware
        -- function:
        toLower' c | c >= 'A' && c <= 'Z' = chr (ord c + 32)
                   | isAscii c = c
                   | otherwise = toLower c

-- | Parses a space or tab.
spaceChar :: Parser [Char] st Char
spaceChar = satisfy $ \c -> c == ' ' || c == '\t'

-- | Parses a nonspace, nonnewline character.
nonspaceChar :: Parser [Char] st Char
nonspaceChar = satisfy $ flip notElem ['\t', '\n', ' ', '\r']

-- | Skips zero or more spaces or tabs.
skipSpaces :: Parser [Char] st ()
skipSpaces = skipMany spaceChar

-- | Skips zero or more spaces or tabs, then reads a newline.
blankline :: Parser [Char] st Char
blankline = try $ skipSpaces >> newline

-- | Parses one or more blank lines and returns a string of newlines.
blanklines :: Parser [Char] st [Char]
blanklines = many1 blankline

-- | Parses material enclosed between start and end parsers.
enclosed :: Parser [Char] st t   -- ^ start parser
         -> Parser [Char] st end  -- ^ end parser
         -> Parser [Char] st a    -- ^ content parser (to be used repeatedly)
         -> Parser [Char] st [a]
enclosed start end parser = try $
  start >> notFollowedBy space >> many1Till parser end

-- | Parse string, case insensitive.
stringAnyCase :: [Char] -> Parser [Char] st String
stringAnyCase [] = string ""
stringAnyCase (x:xs) = do
  firstChar <- char (toUpper x) <|> char (toLower x)
  rest <- stringAnyCase xs
  return (firstChar:rest)

-- | Parse contents of 'str' using 'parser' and return result.
parseFromString :: Parser [tok] st a -> [tok] -> Parser [tok] st a
parseFromString parser str = do
  oldPos <- getPosition
  oldInput <- getInput
  setInput str
  result <- parser
  setInput oldInput
  setPosition oldPos
  return result

-- | Parse raw line block up to and including blank lines.
lineClump :: Parser [Char] st String
lineClump = blanklines
          <|> (many1 (notFollowedBy blankline >> anyLine) >>= return . unlines)

-- | Parse a string of characters between an open character
-- and a close character, including text between balanced
-- pairs of open and close, which must be different. For example,
-- @charsInBalanced '(' ')' anyChar@ will parse "(hello (there))"
-- and return "hello (there)".
charsInBalanced :: Char -> Char -> Parser [Char] st Char
                -> Parser [Char] st String
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
             -> Parser [Char] st Int
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
emailAddress :: Parser [Char] st (String, String)
emailAddress = try $ liftA2 toResult mailbox (char '@' *> domain)
 where toResult mbox dom = let full = fromEntities $ mbox ++ '@':dom
                           in  (full, escapeURI $ "mailto:" ++ full)
       mailbox           = intercalate "." `fmap` (emailWord `sepby1` dot)
       domain            = intercalate "." `fmap` (subdomain `sepby1` dot)
       dot               = char '.'
       subdomain         = many1 $ alphaNum <|> innerPunct
       innerPunct        = try (satisfy (\c -> isEmailPunct c || c == '@') <*
                                 notFollowedBy space)
       emailWord         = many1 $ satisfy isEmailChar
       isEmailChar c     = isAlphaNum c || isEmailPunct c
       isEmailPunct c    = c `elem` "!\"#$%&'*+-/=?^_{|}~;"
       -- note: sepBy1 from parsec consumes input when sep
       -- succeeds and p fails, so we use this variant here.
       sepby1 p sep      = liftA2 (:) p (many (try $ sep >> p))


-- Schemes from http://www.iana.org/assignments/uri-schemes.html plus
-- the unofficial schemes coap, doi, javascript.
schemes :: [String]
schemes = ["coap","doi","javascript","aaa","aaas","about","acap","cap","cid",
           "crid","data","dav","dict","dns","file","ftp","geo","go","gopher",
           "h323","http","https","iax","icap","im","imap","info","ipp","iris",
           "iris.beep","iris.xpc","iris.xpcs","iris.lwz","ldap","mailto","mid",
           "msrp","msrps","mtqp","mupdate","news","nfs","ni","nih","nntp",
           "opaquelocktoken","pop","pres","rtsp","service","session","shttp","sieve",
           "sip","sips","sms","snmp","soap.beep","soap.beeps","tag","tel","telnet",
           "tftp","thismessage","tn3270","tip","tv","urn","vemmi","ws","wss","xcon",
           "xcon-userid","xmlrpc.beep","xmlrpc.beeps","xmpp","z39.50r","z39.50s",
           "adiumxtra","afp","afs","aim","apt","attachment","aw","beshare","bitcoin",
           "bolo","callto","chrome","chrome-extension","com-eventbrite-attendee",
           "content", "cvs","dlna-playsingle","dlna-playcontainer","dtn","dvb",
           "ed2k","facetime","feed","finger","fish","gg","git","gizmoproject",
           "gtalk","hcp","icon","ipn","irc","irc6","ircs","itms","jar","jms",
           "keyparc","lastfm","ldaps","magnet","maps","market","message","mms",
           "ms-help","msnim","mumble","mvn","notes","oid","palm","paparazzi",
           "platform","proxy","psyc","query","res","resource","rmi","rsync",
           "rtmp","secondlife","sftp","sgn","skype","smb","soldat","spotify",
           "ssh","steam","svn","teamspeak","things","udp","unreal","ut2004",
           "ventrilo","view-source","webcal","wtai","wyciwyg","xfire","xri",
           "ymsgr"]

uriScheme :: Parser [Char] st String
uriScheme = oneOfStringsCI schemes

-- | Parses a URI. Returns pair of original and URI-escaped version.
uri :: Parser [Char] st (String, String)
uri = try $ do
  scheme <- uriScheme
  char ':'
  -- We allow punctuation except at the end, since
  -- we don't want the trailing '.' in 'http://google.com.' We want to allow
  -- http://en.wikipedia.org/wiki/State_of_emergency_(disambiguation)
  -- as a URL, while NOT picking up the closing paren in
  -- (http://wikipedia.org). So we include balanced parens in the URL.
  let isWordChar c = isAlphaNum c || c == '_' || c == '/' || c == '+' ||
                         not (isAscii c)
  let wordChar = satisfy isWordChar
  let percentEscaped = try $ char '%' >> skipMany1 (satisfy isHexDigit)
  let entity = () <$ characterReference
  let punct = skipMany1 (char ',')
          <|> () <$ (satisfy (\c -> not (isSpace c) && c /= '<'))
  let uriChunk =  skipMany1 wordChar
              <|> percentEscaped
              <|> entity
              <|> (try $ punct >>
                    lookAhead (void (satisfy isWordChar) <|> percentEscaped))
  str <- snd `fmap` withRaw (skipMany1 ( () <$
                                         (enclosed (char '(') (char ')') uriChunk
                                         <|> enclosed (char '{') (char '}') uriChunk
                                         <|> enclosed (char '[') (char ']') uriChunk)
                                         <|> uriChunk))
  str' <- option str $ char '/' >> return (str ++ "/")
  let uri' = scheme ++ ":" ++ fromEntities str'
  return (uri', escapeURI uri')

mathInlineWith :: String -> String -> Parser [Char] st String
mathInlineWith op cl = try $ do
  string op
  notFollowedBy space
  words' <- many1Till (count 1 (noneOf "\n\\")
                   <|> (char '\\' >> anyChar >>= \c -> return ['\\',c])
                   <|> count 1 newline <* notFollowedBy' blankline
                       *> return " ")
              (try $ string cl)
  notFollowedBy digit  -- to prevent capture of $5
  return $ concat words'

mathDisplayWith :: String -> String -> Parser [Char] st String
mathDisplayWith op cl = try $ do
  string op
  many1Till (noneOf "\n" <|> (newline >>~ notFollowedBy' blankline)) (try $ string cl)

mathDisplay :: Parser [Char] ParserState String
mathDisplay =
      (guardEnabled Ext_tex_math_dollars >> mathDisplayWith "$$" "$$")
  <|> (guardEnabled Ext_tex_math_single_backslash >>
       mathDisplayWith "\\[" "\\]")
  <|> (guardEnabled Ext_tex_math_double_backslash >>
       mathDisplayWith "\\\\[" "\\\\]")

mathInline :: Parser [Char] ParserState String
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
withHorizDisplacement :: Parser [Char] st a  -- ^ Parser to apply
                      -> Parser [Char] st (a, Int) -- ^ (result, displacement)
withHorizDisplacement parser = do
  pos1 <- getPosition
  result <- parser
  pos2 <- getPosition
  return (result, sourceColumn pos2 - sourceColumn pos1)

-- | Applies a parser and returns the raw string that was parsed,
-- along with the value produced by the parser.
withRaw :: Monad m => ParsecT [Char] st m a -> ParsecT [Char] st m (a, [Char])
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
escaped :: Parser [Char] st Char  -- ^ Parser for character to escape
        -> Parser [Char] st Char
escaped parser = try $ char '\\' >> parser

-- | Parse character entity.
characterReference :: Parser [Char] st Char
characterReference = try $ do
  char '&'
  ent <- many1Till nonspaceChar (char ';')
  case lookupEntity ent of
       Just c  -> return c
       Nothing -> fail "entity not found"

-- | Parses an uppercase roman numeral and returns (UpperRoman, number).
upperRoman :: Parser [Char] st (ListNumberStyle, Int)
upperRoman = do
  num <- romanNumeral True
  return (UpperRoman, num)

-- | Parses a lowercase roman numeral and returns (LowerRoman, number).
lowerRoman :: Parser [Char] st (ListNumberStyle, Int)
lowerRoman = do
  num <- romanNumeral False
  return (LowerRoman, num)

-- | Parses a decimal numeral and returns (Decimal, number).
decimal :: Parser [Char] st (ListNumberStyle, Int)
decimal = do
  num <- many1 digit
  return (Decimal, read num)

-- | Parses a '@' and optional label and
-- returns (DefaultStyle, [next example number]).  The next
-- example number is incremented in parser state, and the label
-- (if present) is added to the label table.
exampleNum :: Parser [Char] ParserState (ListNumberStyle, Int)
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
defaultNum :: Parser [Char] st (ListNumberStyle, Int)
defaultNum = do
  char '#'
  return (DefaultStyle, 1)

-- | Parses a lowercase letter and returns (LowerAlpha, number).
lowerAlpha :: Parser [Char] st (ListNumberStyle, Int)
lowerAlpha = do
  ch <- oneOf ['a'..'z']
  return (LowerAlpha, ord ch - ord 'a' + 1)

-- | Parses an uppercase letter and returns (UpperAlpha, number).
upperAlpha :: Parser [Char] st (ListNumberStyle, Int)
upperAlpha = do
  ch <- oneOf ['A'..'Z']
  return (UpperAlpha, ord ch - ord 'A' + 1)

-- | Parses a roman numeral i or I
romanOne :: Parser [Char] st (ListNumberStyle, Int)
romanOne = (char 'i' >> return (LowerRoman, 1)) <|>
           (char 'I' >> return (UpperRoman, 1))

-- | Parses an ordered list marker and returns list attributes.
anyOrderedListMarker :: Parser [Char] ParserState ListAttributes
anyOrderedListMarker = choice $
  [delimParser numParser | delimParser <- [inPeriod, inOneParen, inTwoParens],
                           numParser <- [decimal, exampleNum, defaultNum, romanOne,
                           lowerAlpha, lowerRoman, upperAlpha, upperRoman]]

-- | Parses a list number (num) followed by a period, returns list attributes.
inPeriod :: Parser [Char] st (ListNumberStyle, Int)
         -> Parser [Char] st ListAttributes
inPeriod num = try $ do
  (style, start) <- num
  char '.'
  let delim = if style == DefaultStyle
                 then DefaultDelim
                 else Period
  return (start, style, delim)

-- | Parses a list number (num) followed by a paren, returns list attributes.
inOneParen :: Parser [Char] st (ListNumberStyle, Int)
           -> Parser [Char] st ListAttributes
inOneParen num = try $ do
  (style, start) <- num
  char ')'
  return (start, style, OneParen)

-- | Parses a list number (num) enclosed in parens, returns list attributes.
inTwoParens :: Parser [Char] st (ListNumberStyle, Int)
            -> Parser [Char] st ListAttributes
inTwoParens num = try $ do
  char '('
  (style, start) <- num
  char ')'
  return (start, style, TwoParens)

-- | Parses an ordered list marker with a given style and delimiter,
-- returns number.
orderedListMarker :: ListNumberStyle
                  -> ListNumberDelim
                  -> Parser [Char] ParserState Int
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
charRef :: Parser [Char] st Inline
charRef = do
  c <- characterReference
  return $ Str [c]

lineBlockLine :: Parser [Char] st String
lineBlockLine = try $ do
  char '|'
  char ' '
  white <- many (spaceChar >> return '\160')
  notFollowedBy newline
  line <- anyLine
  continuations <- many (try $ char ' ' >> anyLine)
  return $ white ++ unwords (line : continuations)

-- | Parses an RST-style line block and returns a list of strings.
lineBlockLines :: Parser [Char] st [String]
lineBlockLines = try $ do
  lines' <- many1 lineBlockLine
  skipMany1 $ blankline <|> try (char '|' >> blankline)
  return lines'

-- | Parse a table using 'headerParser', 'rowParser',
-- 'lineParser', and 'footerParser'.
tableWith :: Parser [Char] ParserState ([[Block]], [Alignment], [Int])
          -> ([Int] -> Parser [Char] ParserState [[Block]])
          -> Parser [Char] ParserState sep
          -> Parser [Char] ParserState end
          -> Parser [Char] ParserState Block
tableWith headerParser rowParser lineParser footerParser = try $ do
    (heads, aligns, indices) <- headerParser
    lines' <- rowParser indices `sepEndBy1` lineParser
    footerParser
    numColumns <- getOption readerColumns
    let widths = if (indices == [])
                    then replicate (length aligns) 0.0
                    else widthsFromIndices numColumns indices
    return $ Table [] aligns widths heads lines'

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
gridTableWith :: Parser [Char] ParserState [Block]   -- ^ Block list parser
              -> Bool                                -- ^ Headerless table
              -> Parser [Char] ParserState Block
gridTableWith blocks headless =
  tableWith (gridTableHeader headless blocks) (gridTableRow blocks)
            (gridTableSep '-') gridTableFooter

gridTableSplitLine :: [Int] -> String -> [String]
gridTableSplitLine indices line = map removeFinalBar $ tail $
  splitStringByIndices (init indices) $ trimr line

gridPart :: Char -> Parser [Char] st (Int, Int)
gridPart ch = do
  dashes <- many1 (char ch)
  char '+'
  return (length dashes, length dashes + 1)

gridDashedLines :: Char -> Parser [Char] st [(Int,Int)]
gridDashedLines ch = try $ char '+' >> many1 (gridPart ch) >>~ blankline

removeFinalBar :: String -> String
removeFinalBar =
  reverse . dropWhile (`elem` " \t") . dropWhile (=='|') . reverse

-- | Separator between rows of grid table.
gridTableSep :: Char -> Parser [Char] ParserState Char
gridTableSep ch = try $ gridDashedLines ch >> return '\n'

-- | Parse header for a grid table.
gridTableHeader :: Bool -- ^ Headerless table
                -> Parser [Char] ParserState [Block]
                -> Parser [Char] ParserState ([[Block]], [Alignment], [Int])
gridTableHeader headless blocks = try $ do
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
  heads <- mapM (parseFromString blocks) $ map trim rawHeads
  return (heads, aligns, indices)

gridTableRawLine :: [Int] -> Parser [Char] ParserState [String]
gridTableRawLine indices = do
  char '|'
  line <- many1Till anyChar newline
  return (gridTableSplitLine indices line)

-- | Parse row of grid table.
gridTableRow :: Parser [Char] ParserState [Block]
             -> [Int]
             -> Parser [Char] ParserState [[Block]]
gridTableRow blocks indices = do
  colLines <- many1 (gridTableRawLine indices)
  let cols = map ((++ "\n") . unlines . removeOneLeadingSpace) $
               transpose colLines
  mapM (liftM compactifyCell . parseFromString blocks) cols

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
gridTableFooter :: Parser [Char] ParserState [Char]
gridTableFooter = blanklines

---

-- | Parse a string with a given parser and state.
readWith :: Parser [Char] st a       -- ^ parser
         -> st                       -- ^ initial state
         -> [Char]                   -- ^ input
         -> a
readWith parser state input =
    case runParser parser state "source" input of
      Left err'    ->
        let errPos = errorPos err'
            errLine = sourceLine errPos
            errColumn = sourceColumn errPos
            theline = (lines input ++ [""]) !! (errLine - 1)
        in  error $ "\nError at " ++ show  err' ++ "\n" ++
                theline ++ "\n" ++ replicate (errColumn - 1) ' ' ++
                "^"
      Right result -> result

-- | Parse a string with @parser@ (for testing).
testStringWith :: (Show a) => Parser [Char] ParserState a
               -> String
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
      stateKeys            :: KeyTable,      -- ^ List of reference keys (with fallbacks)
      stateSubstitutions   :: SubstTable,    -- ^ List of substitution references
      stateNotes           :: NoteTable,     -- ^ List of notes (raw bodies)
      stateNotes'          :: NoteTable',    -- ^ List of notes (parsed bodies)
      stateMeta            :: Meta,          -- ^ Document metadata
      stateMeta'           :: F Meta,        -- ^ Document metadata
      stateHeaderTable     :: [HeaderType],  -- ^ Ordered list of header types used
      stateHeaders         :: M.Map Inlines String, -- ^ List of headers and ids (used for implicit ref links)
      stateIdentifiers     :: [String],      -- ^ List of header identifiers used
      stateNextExample     :: Int,           -- ^ Number of next example
      stateExamples        :: M.Map String Int, -- ^ Map from example labels to numbers
      stateHasChapters     :: Bool,          -- ^ True if \chapter encountered
      stateMacros          :: [Macro],       -- ^ List of macros defined so far
      stateRstDefaultRole  :: String,        -- ^ Current rST default interpreted text role
      stateRstCustomRoles  :: M.Map String (String, Maybe String, Attr -> (String, Attr)), -- ^ Current rST custom text roles
      -- Triple represents: 1) Base role, 2) Optional format (only for :raw:
      -- roles), 3) Source language annotation for code (could be used to
      -- annotate role classes too).
      stateCaption         :: Maybe Inlines, -- ^ Caption in current environment
      stateWarnings        :: [String]       -- ^ Warnings generated by the parser
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
  getOption            :: (ReaderOptions -> b) -> Parser s st b
  -- default
  getOption  f         = (f . extractReaderOptions) `fmap` getState

instance HasReaderOptions ParserState where
  extractReaderOptions = stateOptions

class HasHeaderMap st where
  extractHeaderMap  :: st -> M.Map Inlines String
  updateHeaderMap   :: (M.Map Inlines String -> M.Map Inlines String) ->
                       st -> st

instance HasHeaderMap ParserState where
  extractHeaderMap     = stateHeaders
  updateHeaderMap f st = st{ stateHeaders = f $ stateHeaders st }

class HasIdentifierList st where
  extractIdentifierList  :: st -> [String]
  updateIdentifierList   :: ([String] -> [String]) -> st -> st

instance HasIdentifierList ParserState where
  extractIdentifierList     = stateIdentifiers
  updateIdentifierList f st = st{ stateIdentifiers = f $ stateIdentifiers st }

class HasMacros st where
  extractMacros         :: st -> [Macro]
  updateMacros          :: ([Macro] -> [Macro]) -> st -> st

instance HasMacros ParserState where
  extractMacros        = stateMacros
  updateMacros f st    = st{ stateMacros = f $ stateMacros st }

class HasLastStrPosition st where
  setLastStrPos  :: SourcePos -> st -> st
  getLastStrPos  :: st -> Maybe SourcePos

instance HasLastStrPosition ParserState where
  setLastStrPos pos st = st{ stateLastStrPos = Just pos }
  getLastStrPos st     = stateLastStrPos st

defaultParserState :: ParserState
defaultParserState =
    ParserState { stateOptions         = def,
                  stateParserContext   = NullState,
                  stateQuoteContext    = NoQuote,
                  stateAllowLinks      = True,
                  stateMaxNestingLevel = 6,
                  stateLastStrPos      = Nothing,
                  stateKeys            = M.empty,
                  stateSubstitutions   = M.empty,
                  stateNotes           = [],
                  stateNotes'          = [],
                  stateMeta            = nullMeta,
                  stateMeta'           = return nullMeta,
                  stateHeaderTable     = [],
                  stateHeaders         = M.empty,
                  stateIdentifiers     = [],
                  stateNextExample     = 1,
                  stateExamples        = M.empty,
                  stateHasChapters     = False,
                  stateMacros          = [],
                  stateRstDefaultRole  = "title-reference",
                  stateRstCustomRoles  = M.empty,
                  stateCaption         = Nothing,
                  stateWarnings        = []}

-- | Succeed only if the extension is enabled.
guardEnabled :: HasReaderOptions st => Extension -> Parser s st ()
guardEnabled ext = getOption readerExtensions >>= guard . Set.member ext

-- | Succeed only if the extension is disabled.
guardDisabled :: HasReaderOptions st => Extension -> Parser s st ()
guardDisabled ext = getOption readerExtensions >>= guard . not . Set.member ext

-- | Update the position on which the last string ended.
updateLastStrPos :: HasLastStrPosition st => Parser s st ()
updateLastStrPos = getPosition >>= updateState . setLastStrPos

-- | Whether we are right after the end of a string.
notAfterString :: HasLastStrPosition st => Parser s st Bool
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

type NoteTable' = [(String, F Blocks)]  -- used in markdown reader

newtype Key = Key String deriving (Show, Read, Eq, Ord)

toKey :: String -> Key
toKey = Key . map toLower . unwords . words

type KeyTable = M.Map Key Target

type SubstTable = M.Map Key Inlines

--  | Add header to the list of headers in state, together
--  with its associated identifier.  If the identifier is null
--  and the auto_identifers extension is set, generate a new
--  unique identifier, and update the list of identifiers
--  in state.
registerHeader :: (HasReaderOptions st, HasHeaderMap st, HasIdentifierList st)
               => Attr -> Inlines -> Parser s st Attr
registerHeader (ident,classes,kvs) header' = do
  ids <- extractIdentifierList `fmap` getState
  exts <- getOption readerExtensions
  let insert' = M.insertWith (\_new old -> old)
  if null ident && Ext_auto_identifiers `Set.member` exts
     then do
       let id' = uniqueIdent (B.toList header') ids
       let id'' = if Ext_ascii_identifiers `Set.member` exts
                     then catMaybes $ map toAsciiChar id'
                     else id'
       updateState $ updateIdentifierList $
         if id' == id'' then (id' :) else ([id', id''] ++)
       updateState $ updateHeaderMap $ insert' header' id'
       return (id'',classes,kvs)
     else do
        unless (null ident) $
          updateState $ updateHeaderMap $ insert' header' ident
        return (ident,classes,kvs)

-- | Fail unless we're in "smart typography" mode.
failUnlessSmart :: HasReaderOptions st => Parser s st ()
failUnlessSmart = getOption readerSmart >>= guard

smartPunctuation :: Parser [Char] ParserState Inlines
                 -> Parser [Char] ParserState Inlines
smartPunctuation inlineParser = do
  failUnlessSmart
  choice [ quoted inlineParser, apostrophe, dash, ellipses ]

apostrophe :: Parser [Char] ParserState Inlines
apostrophe = (char '\'' <|> char '\8217') >> return (B.str "\x2019")

quoted :: Parser [Char] ParserState Inlines
       -> Parser [Char] ParserState Inlines
quoted inlineParser = doubleQuoted inlineParser <|> singleQuoted inlineParser

withQuoteContext :: QuoteContext
                 -> Parser [tok] ParserState a
                 -> Parser [tok] ParserState a
withQuoteContext context parser = do
  oldState <- getState
  let oldQuoteContext = stateQuoteContext oldState
  setState oldState { stateQuoteContext = context }
  result <- parser
  newState <- getState
  setState newState { stateQuoteContext = oldQuoteContext }
  return result

singleQuoted :: Parser [Char] ParserState Inlines
             -> Parser [Char] ParserState Inlines
singleQuoted inlineParser = try $ do
  singleQuoteStart
  withQuoteContext InSingleQuote $ many1Till inlineParser singleQuoteEnd >>=
    return . B.singleQuoted . mconcat

doubleQuoted :: Parser [Char] ParserState Inlines
             -> Parser [Char] ParserState Inlines
doubleQuoted inlineParser = try $ do
  doubleQuoteStart
  withQuoteContext InDoubleQuote $ manyTill inlineParser doubleQuoteEnd >>=
    return . B.doubleQuoted . mconcat

failIfInQuoteContext :: QuoteContext -> Parser [tok] ParserState ()
failIfInQuoteContext context = do
  st <- getState
  if stateQuoteContext st == context
     then fail "already inside quotes"
     else return ()

charOrRef :: [Char] -> Parser [Char] st Char
charOrRef cs =
  oneOf cs <|> try (do c <- characterReference
                       guard (c `elem` cs)
                       return c)

singleQuoteStart :: Parser [Char] ParserState ()
singleQuoteStart = do
  failIfInQuoteContext InSingleQuote
  -- single quote start can't be right after str
  guard =<< notAfterString
  () <$ charOrRef "'\8216\145"

singleQuoteEnd :: Parser [Char] st ()
singleQuoteEnd = try $ do
  charOrRef "'\8217\146"
  notFollowedBy alphaNum

doubleQuoteStart :: Parser [Char] ParserState ()
doubleQuoteStart = do
  failIfInQuoteContext InDoubleQuote
  try $ do charOrRef "\"\8220\147"
           notFollowedBy . satisfy $ flip elem [' ', '\t', '\n']

doubleQuoteEnd :: Parser [Char] st ()
doubleQuoteEnd = do
  charOrRef "\"\8221\148"
  return ()

ellipses :: Parser [Char] st Inlines
ellipses = do
  try (charOrRef "\8230\133") <|> try (string "..." >> return '…')
  return (B.str "\8230")

dash :: Parser [Char] ParserState Inlines
dash = do
  oldDashes <- getOption readerOldDashes
  if oldDashes
     then emDashOld <|> enDashOld
     else B.str `fmap` (hyphenDash <|> emDash <|> enDash)

-- Two hyphens = en-dash, three = em-dash
hyphenDash :: Parser [Char] st String
hyphenDash = do
  try $ string "--"
  option "\8211" (char '-' >> return "\8212")

emDash :: Parser [Char] st String
emDash = do
  try (charOrRef "\8212\151")
  return "\8212"

enDash :: Parser [Char] st String
enDash = do
  try (charOrRef "\8212\151")
  return "\8211"

enDashOld :: Parser [Char] st Inlines
enDashOld = do
  try (charOrRef "\8211\150") <|>
    try (char '-' >> lookAhead (satisfy isDigit) >> return '–')
  return (B.str "\8211")

emDashOld :: Parser [Char] st Inlines
emDashOld = do
  try (charOrRef "\8212\151") <|> (try $ string "--" >> optional (char '-') >> return '-')
  return (B.str "\8212")

-- This is used to prevent exponential blowups for things like:
-- a**a*a**a*a**a*a**a*a**a*a**a*a**
nested :: Parser s ParserState a
       -> Parser s ParserState a
nested p = do
  nestlevel <- stateMaxNestingLevel `fmap` getState
  guard $ nestlevel > 0
  updateState $ \st -> st{ stateMaxNestingLevel = stateMaxNestingLevel st - 1 }
  res <- p
  updateState $ \st -> st{ stateMaxNestingLevel = nestlevel }
  return res

citeKey :: HasLastStrPosition st => Parser [Char] st (Bool, String)
citeKey = try $ do
  guard =<< notAfterString
  suppress_author <- option False (char '-' *> return True)
  char '@'
  firstChar <- letter <|> char '_'
  let regchar = satisfy (\c -> isAlphaNum c || c == '_')
  let internal p = try $ p <* lookAhead regchar
  rest <- many $ regchar <|> internal (oneOf ":.#$%&-+?<>~/")
  let key = firstChar:rest
  return (suppress_author, key)

--
-- Macros
--

-- | Parse a \newcommand or \renewcommand macro definition.
macro :: (HasMacros st, HasReaderOptions st) => Parser [Char] st Blocks
macro = do
  apply <- getOption readerApplyMacros
  inp <- getInput
  case parseMacroDefinitions inp of
       ([], _)    -> mzero
       (ms, rest) -> do def' <- count (length inp - length rest) anyChar
                        if apply
                           then do
                             updateState $ \st ->
                               updateMacros (ms ++) st
                             return mempty
                           else return $ rawBlock "latex" def'

-- | Apply current macros to string.
applyMacros' :: String -> Parser [Char] ParserState String
applyMacros' target = do
  apply <- getOption readerApplyMacros
  if apply
     then do macros <- extractMacros `fmap` getState
             return $ applyMacros macros target
     else return target
