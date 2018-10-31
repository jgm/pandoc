{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-
  Copyright (C) 2018 Yan Pashkovsky <yanp.bugz@gmail.com>
                     and John MacFarlane

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
   Module      : Text.Pandoc.Readers.Roff
   Copyright   : Copyright (C) 2018 Yan Pashkovsky and John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : Yan Pashkovsky <yanp.bugz@gmail.com>
   Stability   : WIP
   Portability : portable

Tokenizer for groff formats (man, ms).
-}
module Text.Pandoc.Readers.Roff
  ( FontSpec(..)
  , defaultFontSpec
  , LinePart(..)
  , Arg
  , TableOption
  , CellFormat(..)
  , TableRow
  , RoffToken(..)
  , RoffTokens(..)
  , linePartsToString
  , lexRoff
  )
where

import Prelude
import Safe (lastDef)
import Control.Monad (void, mzero, guard, when, mplus)
import Control.Monad.Except (throwError)
import Text.Pandoc.Class
       (getResourcePath, readFileFromDirs, PandocMonad(..), report)
import Data.Char (isLower, toLower, toUpper, chr,
                  isAscii, isAlphaNum, isSpace)
import Data.Default (Default)
import qualified Data.Map as M
import Data.List (intercalate, isSuffixOf)
import qualified Data.Text as T
import Text.Pandoc.Logging (LogMessage(..))
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import Text.Pandoc.Shared (safeRead, substitute)
import Text.Parsec hiding (tokenPrim)
import qualified Text.Parsec as Parsec
import Text.Pandoc.RoffChar (characterCodes, combiningAccents)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable
import qualified Data.Text.Normalize as Normalize

-- import Debug.Trace (traceShowId)

--
-- Data Types
--
data FontSpec = FontSpec{ fontBold      :: Bool
                        , fontItalic    :: Bool
                        , fontMonospace :: Bool
                        } deriving (Show, Eq, Ord)

defaultFontSpec :: FontSpec
defaultFontSpec = FontSpec False False False

data LinePart = RoffStr String
              | Font FontSpec
              | MacroArg Int
              deriving Show

type Arg = [LinePart]

type TableOption = (String, String)

data CellFormat =
  CellFormat
  { columnType     :: Char
  , pipePrefix     :: Bool
  , pipeSuffix     :: Bool
  , columnSuffixes :: [String]
  } deriving (Show, Eq, Ord)

type TableRow = ([CellFormat], [RoffTokens])

data RoffToken = RoffTextLine [LinePart]
               | RoffEmptyLine
               | RoffControlLine String [Arg] SourcePos
               | RoffTable [TableOption] [TableRow] SourcePos
               deriving Show

newtype RoffTokens = RoffTokens { unRoffTokens :: Seq.Seq RoffToken }
        deriving (Show, Semigroup, Monoid)

singleTok :: RoffToken -> RoffTokens
singleTok t = RoffTokens (Seq.singleton t)

data RoffMode = NormalMode
              | CopyMode
              deriving Show

data RoffState = RoffState { customMacros :: M.Map String RoffTokens
                           , prevFont     :: FontSpec
                           , currentFont  :: FontSpec
                           , tableTabChar :: Char
                           , roffMode     :: RoffMode
                           } deriving Show

instance Default RoffState where
  def = RoffState { customMacros = M.fromList
                       $ map (\(n, s) ->
                                (n, singleTok
                                  (RoffTextLine [RoffStr s])))
                       [ ("Tm", "\x2122")
                       , ("lq", "\x201C")
                       , ("rq", "\x201D")
                       , ("R",  "\x00AE") ]
                  , prevFont = defaultFontSpec
                  , currentFont = defaultFontSpec
                  , tableTabChar = '\t'
                  , roffMode = NormalMode
                  }

type RoffLexer m = ParserT [Char] RoffState m

--
-- Lexer: String -> RoffToken
--

eofline :: Stream s m Char => ParsecT s u m ()
eofline = void newline <|> eof

spacetab :: Stream s m Char => ParsecT s u m Char
spacetab = char ' ' <|> char '\t'

characterCodeMap :: M.Map String Char
characterCodeMap =
  M.fromList $ map (\(x,y) -> (y,x)) characterCodes

combiningAccentsMap :: M.Map String Char
combiningAccentsMap =
  M.fromList $ map (\(x,y) -> (y,x)) combiningAccents

escape :: PandocMonad m => RoffLexer m [LinePart]
escape = do
  backslash
  escapeGlyph <|> escapeNormal

escapeGlyph :: PandocMonad m => RoffLexer m [LinePart]
escapeGlyph = do
  c <- lookAhead (oneOf ['[','('])
  escapeArg >>= resolveGlyph c

resolveGlyph :: PandocMonad m => Char -> String -> RoffLexer m [LinePart]
resolveGlyph delimChar glyph = do
  let cs = substitute "_u" " u" glyph -- unicode glyphs separated by _
  (case words cs of
      []  -> mzero
      [s] -> case M.lookup s characterCodeMap `mplus` readUnicodeChar s of
               Nothing -> mzero
               Just c  -> return [RoffStr [c]]
      (s:ss) -> do
        basechar <- case M.lookup s characterCodeMap `mplus`
                         readUnicodeChar s of
                      Nothing ->
                        case s of
                          [ch] | isAscii ch && isAlphaNum ch ->
                                 return ch
                          _ -> mzero
                      Just c  -> return c
        let addAccents [] xs = return $ T.unpack .
                                 Normalize.normalize Normalize.NFC .
                                 T.pack $ reverse xs
            addAccents (a:as) xs =
              case M.lookup a combiningAccentsMap `mplus` readUnicodeChar a of
                Just x  -> addAccents as (x:xs)
                Nothing -> mzero
        addAccents ss [basechar] >>= \xs -> return [RoffStr xs])
      <|> case delimChar of
            '['  -> escUnknown ("\\[" ++ glyph ++ "]")
            '('  -> escUnknown ("\\(" ++ glyph)
            '\'' -> escUnknown ("\\C'" ++ glyph ++ "'")
            _    -> fail "resolveGlyph: unknown glyph delimiter"

readUnicodeChar :: String -> Maybe Char
readUnicodeChar ('u':cs@(_:_:_:_:_)) =
  case safeRead ('0':'x':cs) of
    Just i  -> Just (chr i)
    Nothing -> Nothing
readUnicodeChar _ = Nothing

escapeNormal :: PandocMonad m => RoffLexer m [LinePart]
escapeNormal = do
  c <- anyChar
  case c of
    ' ' -> return [RoffStr " "]
    '"' -> mempty <$ skipMany (satisfy (/='\n')) -- line comment
    '#' -> mempty <$ manyTill anyChar newline
    '%' -> return mempty  -- optional hyphenation
    '&' -> return mempty  -- nonprintable zero-width
    ')' -> return mempty  -- nonprintable zero-width
    '*' -> escString
    ',' -> return mempty  -- to fix spacing after roman
    '-' -> return [RoffStr "-"]
    '.' -> return [RoffStr "`"]
    '/' -> return mempty  -- to fix spacing before roman
    '0' -> return [RoffStr "\x2007"] -- digit-width space
    ':' -> return mempty  -- zero-width break
    'A' -> quoteArg >>= checkDefined
    'B' -> escIgnore 'B' [quoteArg]
    'C' -> quoteArg >>= resolveGlyph '\''
    'D' -> escIgnore 'D' [quoteArg]
    'E' -> do
      mode <- roffMode <$> getState
      case mode of
        CopyMode   -> return mempty
        NormalMode -> return [RoffStr "\\"]
    'H' -> escIgnore 'H' [quoteArg]
    'L' -> escIgnore 'L' [quoteArg]
    'M' -> escIgnore 'M' [escapeArg, count 1 (satisfy (/='\n'))]
    'N' -> escIgnore 'N' [quoteArg]
    'O' -> escIgnore 'O' [count 1 (oneOf ['0','1'])]
    'R' -> escIgnore 'R' [quoteArg]
    'S' -> escIgnore 'S' [quoteArg]
    'V' -> escIgnore 'V' [escapeArg, count 1 alphaNum]
    'X' -> escIgnore 'X' [quoteArg]
    'Y' -> escIgnore 'Y' [escapeArg, count 1 (satisfy (/='\n'))]
    'Z' -> escIgnore 'Z' [quoteArg]
    '\'' -> return [RoffStr "`"]
    '\n' -> return mempty  -- line continuation
    '^' -> return [RoffStr "\x200A"] -- 1/12 em space
    '_' -> return [RoffStr "_"]
    '`' -> return [RoffStr "`"]
    'a' -> return mempty  -- "non-interpreted leader character"
    'b' -> escIgnore 'b' [quoteArg]
    'c' -> return mempty  -- interrupt text processing
    'd' -> escIgnore 'd' [] -- forward down 1/2em
    'e' -> return [RoffStr "\\"]
    'f' -> escFont
    'g' -> escIgnore 'g' [escapeArg, count 1 (satisfy (/='\n'))]
    'h' -> escIgnore 'h' [quoteArg]
    'k' -> escIgnore 'k' [escapeArg, count 1 (satisfy (/='\n'))]
    'l' -> escIgnore 'l' [quoteArg]
    'm' -> escIgnore 'm' [escapeArg, count 1 (satisfy (/='\n'))]
    'n' -> escIgnore 'm' [escapeArg, count 1 (satisfy (/='\n'))]
    'o' -> escIgnore 'o' [quoteArg]
    'p' -> escIgnore 'p' []
    'r' -> escIgnore 'r' []
    's' -> escIgnore 's' [escapeArg, signedNumber]
    't' -> return [RoffStr "\t"]
    'u' -> escIgnore 'u' []
    'v' -> escIgnore 'v' [quoteArg]
    'w' -> escIgnore 'w' [quoteArg]
    'x' -> escIgnore 'x' [quoteArg]
    'z' -> escIgnore 'z' [count 1 anyChar]
    '{' -> return mempty
    '|' -> return [RoffStr "\x2006"] --1/6 em space
    '}' -> return mempty
    '~' -> return [RoffStr "\160"] -- nonbreaking space
    '\\' -> do
      mode <- roffMode <$> getState
      case mode of
        CopyMode   -> char '\\'
        NormalMode -> return '\\'
      return [RoffStr "\\"]
    _   -> return [RoffStr [c]]
    -- man 7 groff: "If  a  backslash  is followed by a character that
    -- does not constitute a defined escape sequence, the backslash
    -- is  silently  ignored  and  the character maps to itself."

escIgnore :: PandocMonad m
          => Char
          -> [RoffLexer m String]
          -> RoffLexer m [LinePart]
escIgnore c argparsers = do
  pos <- getPosition
  arg <- snd <$> withRaw (choice argparsers) <|> return ""
  report $ SkippedContent ('\\':c:arg) pos
  return mempty

escUnknown :: PandocMonad m => String -> RoffLexer m [LinePart]
escUnknown s = do
  pos <- getPosition
  report $ SkippedContent s pos
  return [RoffStr "\xFFFD"]

signedNumber :: PandocMonad m => RoffLexer m String
signedNumber = try $ do
  sign <- option "" ("-" <$ char '-' <|> "" <$ char '+')
  ds <- many1 digit
  return (sign ++ ds)

-- Parses: [..] or (..
escapeArg :: PandocMonad m => RoffLexer m String
escapeArg = choice
    [ char '[' *> manyTill (noneOf ['\n',']']) (char ']')
    , char '(' *> count 2 (satisfy (/='\n'))
    ]

-- Parses: '..'
quoteArg :: PandocMonad m => RoffLexer m String
quoteArg = char '\'' *> manyTill (noneOf ['\n','\'']) (char '\'')

escFont :: PandocMonad m => RoffLexer m [LinePart]
escFont = do
  font <- escapeArg <|> count 1 alphaNum
  font' <- if null font || font == "P"
              then prevFont <$> getState
              else return $ foldr processFontLetter defaultFontSpec font
  modifyState $ \st -> st{ prevFont = currentFont st
                         , currentFont = font' }
  return [Font font']
  where
    processFontLetter c fs
              | isLower c    = processFontLetter (toUpper c) fs
    processFontLetter 'B' fs = fs{ fontBold = True }
    processFontLetter 'I' fs = fs{ fontItalic = True }
    processFontLetter 'C' fs = fs{ fontMonospace = True }
    processFontLetter _   fs = fs -- do nothing

-- separate function from lexMacro since real man files sometimes do not
-- follow the rules
lexComment :: PandocMonad m => RoffLexer m RoffTokens
lexComment = do
  try $ string ".\\\""
  many Parsec.space
  skipMany $ noneOf "\n"
  eofline
  return mempty

lexMacro :: PandocMonad m => RoffLexer m RoffTokens
lexMacro = do
  pos <- getPosition
  guard $ sourceColumn pos == 1
  char '.' <|> char '\''
  skipMany spacetab
  macroName <- many (satisfy (not . isSpace))
  case macroName of
    "nop" -> return mempty
    "ie"  -> lexConditional
    "if"  -> lexConditional
    "el"  -> skipConditional
    "TS"  -> lexTable pos

    _ -> do
       args <- lexArgs
       case macroName of
         ""     -> return mempty
         "\\\"" -> return mempty
         "\\#"  -> return mempty
         "de"   -> lexMacroDef args
         "de1"  -> lexMacroDef args
         "ds"   -> lexStringDef args
         "ds1"  -> lexStringDef args
         "sp"   -> return $ singleTok RoffEmptyLine
         "so"   -> lexIncludeFile args
         _      -> resolveMacro macroName args pos

lexTable :: PandocMonad m => SourcePos -> RoffLexer m RoffTokens
lexTable pos = do
  skipMany lexComment
  spaces
  opts <- option [] tableOptions
  case lookup "tab" opts of
    Just (c:_) -> modifyState $ \st -> st{ tableTabChar = c }
    _          -> modifyState $ \st -> st{ tableTabChar = '\t' }
  spaces
  skipMany lexComment
  spaces
  rows <- lexTableRows
  morerows <- many $ try $ do
    string ".T&"
    skipMany spacetab
    newline
    lexTableRows
  string ".TE"
  skipMany spacetab
  eofline
  return $ singleTok $ RoffTable opts (rows ++ concat morerows) pos

lexTableRows :: PandocMonad m => RoffLexer m [TableRow]
lexTableRows = do
  aligns <- tableFormatSpec
  spaces
  skipMany $ lexComment
          <|> try (mempty <$ (string ".sp" >> skipMany spaceChar >> newline))
  spaces
  rows <- many (notFollowedBy (try (string ".TE") <|> try (string ".T&")) >>
                  tableRow)
  return $ zip aligns rows

tableCell :: PandocMonad m => RoffLexer m RoffTokens
tableCell = do
  pos <- getPosition
  (enclosedCell <|> simpleCell) >>= lexRoff pos . T.pack
  where
  enclosedCell = do
    try (string "T{")
    manyTill anyChar (try (string "T}"))
  simpleCell = do
    tabChar <- tableTabChar <$> getState
    many (notFollowedBy (char tabChar <|> newline) >> anyChar)

tableRow :: PandocMonad m => RoffLexer m [RoffTokens]
tableRow = do
  tabChar <- tableTabChar <$> getState
  c <- tableCell
  cs <- many $ try (char tabChar >> tableCell)
  skipMany spacetab
  eofline
  skipMany lexComment
  return (c:cs)

tableOptions :: PandocMonad m => RoffLexer m [TableOption]
tableOptions = try $ many tableOption <* spaces <* char ';'

tableOption :: PandocMonad m => RoffLexer m TableOption
tableOption = do
  k <- many1 letter
  v <- option "" $ try $ do
         skipMany spacetab
         char '('
         manyTill anyChar (char ')')
  skipMany spacetab
  optional (char ',')
  skipMany spacetab
  return (k,v)

tableFormatSpec :: PandocMonad m => RoffLexer m [[CellFormat]]
tableFormatSpec = do
  first <- tableFormatSpecLine
  rest <- many $ try $ (newline <|> char ',') *> tableFormatSpecLine
  let speclines = first:rest
  spaces
  char '.'
  return $ speclines ++ repeat (lastDef [] speclines) -- last line is default

tableFormatSpecLine :: PandocMonad m => RoffLexer m [CellFormat]
tableFormatSpecLine =
  many1 $ try $ skipMany spacetab *> tableColFormat <* skipMany spacetab

tableColFormat :: PandocMonad m => RoffLexer m CellFormat
tableColFormat = do
    pipePrefix' <- option False
                   $ True <$ try (string "|" <* notFollowedBy spacetab)
    c <- oneOf ['a','A','c','C','l','L','n','N','r','R','s','S','^','_','-',
                '=','|']
    suffixes <- many $ try (skipMany spacetab *> count 1 digit) <|>
      (do x <- oneOf ['b','B','d','D','e','E','f','F','i','I','m','M',
                  'p','P','t','T','u','U','v','V','w','W','x','X', 'z','Z']
          num <- case toLower x of
                   'w' -> many1 digit <|>
                           do char '('
                              xs <- manyTill anyChar (char ')')
                              return ("(" ++ xs ++ ")")
                   'f' -> count 1 alphaNum <* skipMany spacetab
                   'm' -> count 1 alphaNum <* skipMany spacetab
                   _   -> return ""
          return $ x : num)
    pipeSuffix' <- option False $ True <$ string "|"
    return $ CellFormat
             { columnType     = c
             , pipePrefix     = pipePrefix'
             , pipeSuffix     = pipeSuffix'
             , columnSuffixes = suffixes }

-- We don't fully handle the conditional.  But we do
-- include everything under '.ie n', which occurs commonly
-- in man pages.  We always skip the '.el' part.
lexConditional :: PandocMonad m => RoffLexer m RoffTokens
lexConditional = do
  skipMany spacetab
  lexNCond <|> skipConditional

-- n means nroff mode
lexNCond :: PandocMonad m => RoffLexer m RoffTokens
lexNCond = do
  newline
  many1 spacetab
  lexGroup <|> manToken

lexGroup :: PandocMonad m => RoffLexer m RoffTokens
lexGroup = do
  groupstart
  mconcat <$> manyTill manToken groupend
  where
    groupstart = try $ string "\\{\\" >> newline
    groupend   = try $ string "\\}" >> eofline

skipConditional :: PandocMonad m => RoffLexer m RoffTokens
skipConditional = do
  rest <- anyLine
  when ("\\{\\" `isSuffixOf` rest) $
    void $ manyTill anyChar (try (string "\\}"))
  return mempty

lexIncludeFile :: PandocMonad m => [Arg] -> RoffLexer m RoffTokens
lexIncludeFile args = do
  pos <- getPosition
  case args of
    (f:_) -> do
      let fp = linePartsToString f
      dirs <- getResourcePath
      result <- readFileFromDirs dirs fp
      case result of
        Nothing  -> report $ CouldNotLoadIncludeFile fp pos
        Just s   -> getInput >>= setInput . (s ++)
      return mempty
    []    -> return mempty

resolveMacro :: PandocMonad m
             => String -> [Arg] -> SourcePos -> RoffLexer m RoffTokens
resolveMacro macroName args pos = do
  macros <- customMacros <$> getState
  case M.lookup macroName macros of
    Nothing -> return $ singleTok $ RoffControlLine macroName args pos
    Just ts -> do
      let fillLP (MacroArg i)    zs =
            case drop (i - 1) args of
              []     -> zs
              (ys:_) -> ys ++ zs
          fillLP z zs = z : zs
      let fillMacroArg (RoffTextLine lineparts) =
            RoffTextLine (foldr fillLP [] lineparts)
          fillMacroArg x = x
      return $ RoffTokens . fmap fillMacroArg . unRoffTokens $ ts

lexStringDef :: PandocMonad m => [Arg] -> RoffLexer m RoffTokens
lexStringDef args = do -- string definition
   case args of
     []     -> fail "No argument to .ds"
     (x:ys) -> do
       let ts = singleTok $ RoffTextLine (intercalate [RoffStr " " ] ys)
       let stringName = linePartsToString x
       modifyState $ \st ->
         st{ customMacros = M.insert stringName ts (customMacros st) }
   return mempty

lexMacroDef :: PandocMonad m => [Arg] -> RoffLexer m RoffTokens
lexMacroDef args = do -- macro definition
   modifyState $ \st -> st{ roffMode = CopyMode }
   (macroName, stopMacro) <-
     case args of
       (x : y : _) -> return (linePartsToString x, linePartsToString y)
                      -- optional second arg
       (x:_)       -> return (linePartsToString x, ".")
       []          -> fail "No argument to .de"
   let stop = try $ do
         char '.' <|> char '\''
         skipMany spacetab
         string stopMacro
         _ <- lexArgs
         return ()
   ts <- mconcat <$> manyTill manToken stop
   modifyState $ \st ->
     st{ customMacros = M.insert macroName ts (customMacros st)
       , roffMode = NormalMode }
   return mempty

lexArgs :: PandocMonad m => RoffLexer m [Arg]
lexArgs = do
  args <- many $ try oneArg
  skipMany spacetab
  eofline
  return args

  where

  oneArg :: PandocMonad m => RoffLexer m [LinePart]
  oneArg = do
    skipMany $ try $ string "\\\n"  -- continuation line
    try quotedArg <|> plainArg
    -- try, because there are some erroneous files, e.g. linux/bpf.2

  plainArg :: PandocMonad m => RoffLexer m [LinePart]
  plainArg = do
    skipMany spacetab
    mconcat <$> many1 (macroArg <|> escape <|> regularText <|> unescapedQuote)
    where
      unescapedQuote = char '"' >> return [RoffStr "\""]

  quotedArg :: PandocMonad m => RoffLexer m [LinePart]
  quotedArg = do
    skipMany spacetab
    char '"'
    xs <- mconcat <$>
           many (macroArg <|> escape <|> regularText
                 <|> spaceTabChar <|> escapedQuote)
    char '"'
    return xs
    where
      escapedQuote = try $ do
        char '"'
        char '"'
        return [RoffStr "\""]

checkDefined :: PandocMonad m => String -> RoffLexer m [LinePart]
checkDefined name = do
  macros <- customMacros <$> getState
  case M.lookup name macros of
    Just _  -> return [RoffStr "1"]
    Nothing -> return [RoffStr "0"]

escString :: PandocMonad m => RoffLexer m [LinePart]
escString = try $ do
  pos <- getPosition
  (do cs <- escapeArg <|> count 1 anyChar
      resolveString cs pos)
    <|> mempty <$ char 'S'

  where
  -- strings and macros share namespace
  resolveString stringname pos = do
    RoffTokens ts <- resolveMacro stringname [] pos
    case Foldable.toList ts of
      [RoffTextLine xs] -> return xs
      _          -> do
        report $ SkippedContent ("unknown string " ++ stringname) pos
        return mempty

lexLine :: PandocMonad m => RoffLexer m RoffTokens
lexLine = do
  mode <- roffMode <$> getState
  case mode of
    CopyMode   -> optional $ try $ string "\\&"
    NormalMode -> return ()
  lnparts <- mconcat <$> many1 linePart
  eofline
  go lnparts
  where  -- return empty line if we only have empty strings;
         -- this can happen if the line just contains \f[C], for example.
    go [] = return mempty
    go (RoffStr "" : xs) = go xs
    go xs = return $ singleTok $ RoffTextLine xs

linePart :: PandocMonad m => RoffLexer m [LinePart]
linePart = macroArg <|> escape <|>
           regularText <|> quoteChar <|> spaceTabChar

backslash :: PandocMonad m => RoffLexer m ()
backslash = do
  char '\\'
  mode <- roffMode <$> getState
  case mode of
    -- experimentally, it seems you don't always need to double
    -- the backslash in macro defs.  It's essential with \\$1,
    -- but not with \\f[I].  So we make the second one optional.
    CopyMode   -> optional $ char '\\'
    NormalMode -> return ()

macroArg :: PandocMonad m => RoffLexer m [LinePart]
macroArg = try $ do
  pos <- getPosition
  backslash
  char '$'
  x <- escapeArg <|> count 1 digit
  case safeRead x of
    Just i  -> return [MacroArg i]
    Nothing -> do
      report $ SkippedContent ("illegal macro argument " ++ x) pos
      return []

regularText :: PandocMonad m => RoffLexer m [LinePart]
regularText = do
  s <- many1 $ noneOf "\n\r\t \\\""
  return [RoffStr s]

quoteChar :: PandocMonad m => RoffLexer m [LinePart]
quoteChar = do
  char '"'
  return [RoffStr "\""]

spaceTabChar :: PandocMonad m => RoffLexer m [LinePart]
spaceTabChar = do
  c <- spacetab
  return [RoffStr [c]]

lexEmptyLine :: PandocMonad m => RoffLexer m RoffTokens
lexEmptyLine = newline >> return (singleTok RoffEmptyLine)

manToken :: PandocMonad m => RoffLexer m RoffTokens
manToken = lexComment <|> lexMacro <|> lexLine <|> lexEmptyLine

linePartsToString :: [LinePart] -> String
linePartsToString = mconcat . map go
  where
  go (RoffStr s) = s
  go _ = mempty

-- | Tokenize a string as a sequence of groff tokens.
lexRoff :: PandocMonad m => SourcePos -> T.Text -> m RoffTokens
lexRoff pos txt = do
  eithertokens <- readWithM (do setPosition pos
                                mconcat <$> many manToken) def (T.unpack txt)
  case eithertokens of
    Left e       -> throwError e
    Right tokenz -> return tokenz
