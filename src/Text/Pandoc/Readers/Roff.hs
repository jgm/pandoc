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
  ( MacroKind
  , FontSpec(..)
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

type MacroKind = String

data LinePart = RoffStr String
              | Font FontSpec
              | FontSize Int
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

data RoffToken = MLine [LinePart]
               | MEmptyLine
               | MMacro MacroKind [Arg] SourcePos
               | MTable [TableOption] [TableRow] SourcePos
               deriving Show

newtype RoffTokens = RoffTokens { unRoffTokens :: Seq.Seq RoffToken }
        deriving (Show, Semigroup, Monoid)

singleTok :: RoffToken -> RoffTokens
singleTok t = RoffTokens (Seq.singleton t)

data RoffState = RoffState { customMacros :: M.Map String RoffTokens
                           , prevFont     :: FontSpec
                           , currentFont  :: FontSpec
                           , tableTabChar :: Char
                           } deriving Show

instance Default RoffState where
  def = RoffState { customMacros = M.fromList
                       $ map (\(n, s) ->
                                (n, singleTok
                                  (MLine [RoffStr s])))
                       [ ("Tm", "\x2122")
                       , ("lq", "\x201C")
                       , ("rq", "\x201D")
                       , ("R",  "\x00AE") ]
                  , prevFont = defaultFontSpec
                  , currentFont = defaultFontSpec
                  , tableTabChar = '\t'
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
  char '\\'
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
    'C' -> quoteArg >>= resolveGlyph '\''
    'f' -> escFont
    's' -> escFontSize
    '*' -> escString
    '"' -> mempty <$ skipMany (satisfy (/='\n')) -- line comment
    '#' -> mempty <$ manyTill anyChar newline
    '%' -> return mempty
    '{' -> return mempty
    '}' -> return mempty
    '&' -> return mempty
    '\n' -> return mempty
    ':' -> return mempty
    '0' -> return mempty
    'c' -> return mempty
    '-' -> return [RoffStr "-"]
    '_' -> return [RoffStr "_"]
    ' ' -> return [RoffStr " "]
    '\\' -> return [RoffStr "\\"]
    't' -> return [RoffStr "\t"]
    'e' -> return [RoffStr "\\"]
    '`' -> return [RoffStr "`"]
    '^' -> return [RoffStr " "]
    '|' -> return [RoffStr " "]
    '\'' -> return [RoffStr "`"]
    '.' -> return [RoffStr "`"]
    '~' -> return [RoffStr "\160"] -- nonbreaking space
    _   -> escUnknown ['\\',c]

escUnknown :: PandocMonad m => String -> RoffLexer m [LinePart]
escUnknown s = do
  pos <- getPosition
  report $ SkippedContent s pos
  return [RoffStr "\xFFFD"]

-- \s-1 \s0
escFontSize :: PandocMonad m => RoffLexer m [LinePart]
escFontSize = do
  let sign = option "" ("-" <$ char '-' <|> "" <$ char '+')
  let toFontSize xs =
        case safeRead xs of
          Nothing  -> mzero
          Just n   -> return [FontSize n]
  choice
    [ do char '('
         s <- sign
         ds <- count 2 digit
         toFontSize (s ++ ds)
    , do char '['
         s <- sign
         ds <- many1 digit
         char ']'
         toFontSize (s ++ ds)
    , do s <- sign
         ds <- count 1 digit
         toFontSize (s ++ ds)
    ]

-- Parses: [..] or (..
escapeArg :: PandocMonad m => RoffLexer m String
escapeArg = choice
    [ char '[' *> manyTill (noneOf ['\n',']']) (char ']')
    , char '(' *> count 2 anyChar
    ]

-- Parses: '..'
quoteArg :: PandocMonad m => RoffLexer m String
quoteArg = char '\'' *> manyTill (noneOf ['\n','\'']) (char '\'')

escFont :: PandocMonad m => RoffLexer m [LinePart]
escFont = do
  font <- escapeArg <|> count 1 alphaNum
  font' <- if null font
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
         "sp"   -> return $ singleTok MEmptyLine
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
  return $ singleTok $ MTable opts (rows ++ concat morerows) pos

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
    Nothing -> return $ singleTok $ MMacro macroName args pos
    Just ts -> do
      let fillLP (MacroArg i)    zs =
            case drop (i - 1) args of
              []     -> zs
              (ys:_) -> ys ++ zs
          fillLP z zs = z : zs
      let fillMacroArg (MLine lineparts) =
            MLine (foldr fillLP [] lineparts)
          fillMacroArg x = x
      return $ RoffTokens . fmap fillMacroArg . unRoffTokens $ ts

lexStringDef :: PandocMonad m => [Arg] -> RoffLexer m RoffTokens
lexStringDef args = do -- string definition
   case args of
     []     -> fail "No argument to .ds"
     (x:ys) -> do
       let ts = singleTok $ MLine (intercalate [RoffStr " " ] ys)
       let stringName = linePartsToString x
       modifyState $ \st ->
         st{ customMacros = M.insert stringName ts (customMacros st) }
   return mempty

lexMacroDef :: PandocMonad m => [Arg] -> RoffLexer m RoffTokens
lexMacroDef args = do -- macro definition
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
     st{ customMacros = M.insert macroName ts (customMacros st) }
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

escString :: PandocMonad m => RoffLexer m [LinePart]
escString = try $ do
  pos <- getPosition
  (do cs <- escapeArg
      resolveString cs pos)
    <|> mempty <$ char 'S'

  where
  -- strings and macros share namespace
  resolveString stringname pos = do
    RoffTokens ts <- resolveMacro stringname [] pos
    case Foldable.toList ts of
      [MLine xs] -> return xs
      _          -> do
        report $ SkippedContent ("unknown string " ++ stringname) pos
        return mempty

lexLine :: PandocMonad m => RoffLexer m RoffTokens
lexLine = do
  lnparts <- mconcat <$> many1 linePart
  eofline
  go lnparts
  where  -- return empty line if we only have empty strings;
         -- this can happen if the line just contains \f[C], for example.
    go [] = return mempty
    go (RoffStr "" : xs) = go xs
    go xs = return $ singleTok $ MLine xs

linePart :: PandocMonad m => RoffLexer m [LinePart]
linePart = macroArg <|> escape <|>
           regularText <|> quoteChar <|> spaceTabChar

macroArg :: PandocMonad m => RoffLexer m [LinePart]
macroArg = try $ do
  pos <- getPosition
  string "\\\\$"
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
lexEmptyLine = newline >> return (singleTok MEmptyLine)

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
