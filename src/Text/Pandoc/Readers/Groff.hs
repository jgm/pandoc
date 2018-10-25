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
   Module      : Text.Pandoc.Readers.Groff
   Copyright   : Copyright (C) 2018 Yan Pashkovsky and John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : Yan Pashkovsky <yanp.bugz@gmail.com>
   Stability   : WIP
   Portability : portable

Tokenizer for groff formats (man, ms).
-}
module Text.Pandoc.Readers.Groff
  ( MacroKind
  , FontSpec(..)
  , defaultFontSpec
  , LinePart(..)
  , Arg
  , ManToken(..)
  , ManTokens(..)
  , singleTok
  , RoffState(..)
  , ManLexer
  , manToken
  , linePartsToString
  )
where

import Prelude
import Control.Monad (void, mzero, guard, when)
import Text.Pandoc.Class
       (getResourcePath, readFileFromDirs, PandocMonad(..), report)
import Data.Char (isHexDigit, chr, ord, isAscii, isAlphaNum, isSpace)
import Data.Default (Default)
import qualified Data.Map as M
import Data.List (intercalate, isSuffixOf)
import qualified Data.Text as T
import Text.Pandoc.Builder as B
import Text.Pandoc.Logging (LogMessage(..))
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import Text.Pandoc.Shared (safeRead)
import Text.Parsec hiding (tokenPrim)
import qualified Text.Parsec as Parsec
import Text.Pandoc.GroffChar (characterCodes, combiningAccents)
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

-- TODO parse tables (see man tbl)
data ManToken = MLine [LinePart]
              | MEmptyLine
              | MMacro MacroKind [Arg] SourcePos
              | MTable [Alignment] ManTokens [ManTokens] [[ManTokens]]
              deriving Show

newtype ManTokens = ManTokens { unManTokens :: Seq.Seq ManToken }
        deriving (Show, Semigroup, Monoid)

singleTok :: ManToken -> ManTokens
singleTok t = ManTokens (Seq.singleton t)

data RoffState = RoffState { customMacros :: M.Map String ManTokens
                           , prevFont     :: FontSpec
                           , currentFont  :: FontSpec
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
                  }

type ManLexer m = ParserT [Char] RoffState m

--
-- Lexer: String -> ManToken
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

escape :: PandocMonad m => ManLexer m [LinePart]
escape = do
  char '\\'
  c <- anyChar
  case c of
    'f' -> escFont
    's' -> escFontSize
    '*' -> escStar
    '(' -> twoCharGlyph
    '[' -> bracketedGlyph
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

  where

  twoCharGlyph = do
    cs <- count 2 anyChar
    case M.lookup cs characterCodeMap of
      Just c  -> return [RoffStr [c]]
      Nothing -> escUnknown ('\\':'(':cs)

  bracketedGlyph = unicodeGlyph <|> charGlyph

  charGlyph = do
    cs <- manyTill (noneOf ['[',']','\n']) (char ']')
    (case words cs of
        []  -> mzero
        [s] -> case M.lookup s characterCodeMap of
                 Nothing -> mzero
                 Just c  -> return [RoffStr [c]]
        (s:ss) -> do
          basechar <- case M.lookup cs characterCodeMap of
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
                case M.lookup a combiningAccentsMap of
                  Just x  -> addAccents as (x:xs)
                  Nothing -> mzero
          addAccents ss [basechar] >>= \xs -> return [RoffStr xs])
      <|> escUnknown ("\\[" ++ cs ++ "]")

  unicodeGlyph = try $ do
    xs <- ucharCode `sepBy1` (char '_') <* char ']'
    return [RoffStr xs]

  ucharCode = try $ do
    char 'u'
    cs <- many1 (satisfy isHexDigit)
    let lcs = length cs
    guard $ lcs >= 4 && lcs <= 6
    case chr <$> safeRead ('0':'x':cs) of
       Nothing  -> mzero
       Just c   -> return c

  escUnknown :: PandocMonad m => String -> ManLexer m [LinePart]
  escUnknown s = do
    pos <- getPosition
    report $ SkippedContent ("Unknown escape sequence " ++ s) pos
    return [RoffStr "\xFFFD"]

-- \s-1 \s0
escFontSize :: PandocMonad m => ManLexer m [LinePart]
escFontSize = do
  let sign = option "" $ count 1 (oneOf "+-")
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

escFont :: PandocMonad m => ManLexer m [LinePart]
escFont = do
  font <- choice
        [ char 'S' >> return defaultFontSpec
        , digit >> return defaultFontSpec
        , char '(' >> anyChar >> anyChar >> return defaultFontSpec
        , digit >> return defaultFontSpec
        , ($ defaultFontSpec) <$> letterFontKind
        , lettersFont
        ]
  modifyState $ \st -> st{ prevFont = currentFont st
                         , currentFont = font }
  return [Font font]

lettersFont :: PandocMonad m => ManLexer m FontSpec
lettersFont = try $ do
  char '['
  fs <- many letterFontKind
  skipMany letter
  char ']'
  if null fs
     then prevFont <$> getState
     else return $ foldr ($) defaultFontSpec fs

letterFontKind :: PandocMonad m => ManLexer m (FontSpec -> FontSpec)
letterFontKind = choice [
    oneOf ['B','b'] >> return (\fs -> fs{ fontBold = True })
  , oneOf ['I','i'] >> return (\fs -> fs { fontItalic = True })
  , oneOf ['C','c'] >> return (\fs -> fs { fontMonospace = True })
  , oneOf ['P','p','R','r'] >> return id
  ]


-- separate function from lexMacro since real man files sometimes do not
-- follow the rules
lexComment :: PandocMonad m => ManLexer m ManTokens
lexComment = do
  try $ string ".\\\""
  many Parsec.space
  skipMany $ noneOf "\n"
  char '\n'
  return mempty

lexMacro :: PandocMonad m => ManLexer m ManTokens
lexMacro = do
  pos <- getPosition
  char '.' <|> char '\''
  many spacetab
  macroName <- many (satisfy (not . isSpace))
  case macroName of
    "nop" -> return mempty
    "ie"  -> lexConditional
    "if"  -> lexConditional
    "el"  -> skipConditional
    "TS"  -> lexTable

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

-- | TODO placeholder
lexTable :: PandocMonad m => ManLexer m ManTokens
lexTable = do
  pos <- getPosition
  manyTill anyLine (try (string ".TE" >> many spacetab >> eofline))
  report $ SkippedContent "table" pos
  return mempty

-- We don't fully handle the conditional.  But we do
-- include everything under '.ie n', which occurs commonly
-- in man pages.  We always skip the '.el' part.
lexConditional :: PandocMonad m => ManLexer m ManTokens
lexConditional = do
  skipMany spacetab
  lexNCond <|> skipConditional

-- n means nroff mode
lexNCond :: PandocMonad m => ManLexer m ManTokens
lexNCond = do
  char '\n'
  many1 spacetab
  lexGroup <|> manToken

lexGroup :: PandocMonad m => ManLexer m ManTokens
lexGroup = do
  groupstart
  mconcat <$> manyTill manToken groupend
  where
    groupstart = try $ string "\\{\\" >> newline
    groupend   = try $ string "\\}" >> eofline

skipConditional :: PandocMonad m => ManLexer m ManTokens
skipConditional = do
  rest <- anyLine
  when ("\\{\\" `isSuffixOf` rest) $
    void $ manyTill anyChar (try (string "\\}"))
  return mempty

lexIncludeFile :: PandocMonad m => [Arg] -> ManLexer m ManTokens
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
             => String -> [Arg] -> SourcePos -> ManLexer m ManTokens
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
      return $ ManTokens . fmap fillMacroArg . unManTokens $ ts

lexStringDef :: PandocMonad m => [Arg] -> ManLexer m ManTokens
lexStringDef args = do -- string definition
   case args of
     []     -> fail "No argument to .ds"
     (x:ys) -> do
       let ts = singleTok $ MLine (intercalate [RoffStr " " ] ys)
       let stringName = linePartsToString x
       modifyState $ \st ->
         st{ customMacros = M.insert stringName ts (customMacros st) }
   return mempty

lexMacroDef :: PandocMonad m => [Arg] -> ManLexer m ManTokens
lexMacroDef args = do -- macro definition
   (macroName, stopMacro) <-
     case args of
       (x : y : _) -> return (linePartsToString x, linePartsToString y)
                      -- optional second arg
       (x:_)       -> return (linePartsToString x, ".")
       []          -> fail "No argument to .de"
   let stop = try $ do
         char '.' <|> char '\''
         many spacetab
         string stopMacro
         _ <- lexArgs
         return ()
   ts <- mconcat <$> manyTill manToken stop
   modifyState $ \st ->
     st{ customMacros = M.insert macroName ts (customMacros st) }
   return mempty

lexArgs :: PandocMonad m => ManLexer m [Arg]
lexArgs = do
  args <- many $ try oneArg
  skipMany spacetab
  eofline
  return args

  where

  oneArg :: PandocMonad m => ManLexer m [LinePart]
  oneArg = do
    skipMany $ try $ string "\\\n"  -- continuation line
    try quotedArg <|> plainArg
    -- try, because there are some erroneous files, e.g. linux/bpf.2

  plainArg :: PandocMonad m => ManLexer m [LinePart]
  plainArg = do
    skipMany spacetab
    mconcat <$> many1 (macroArg <|> escape <|> regularText <|> unescapedQuote)
    where
      unescapedQuote = char '"' >> return [RoffStr "\""]

  quotedArg :: PandocMonad m => ManLexer m [LinePart]
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

escStar :: PandocMonad m => ManLexer m [LinePart]
escStar = try $ do
  pos <- getPosition
  c <- anyChar
  case c of
    '(' -> do
      cs <- count 2 anyChar
      resolveString cs pos
    '[' -> do
      cs <- many (noneOf "\t\n\r ]")
      char ']'
      resolveString cs pos
    'S' -> return mempty -- switch back to default font size
    _   -> resolveString [c] pos

  where

  -- strings and macros share namespace
  resolveString stringname pos = do
    ManTokens ts <- resolveMacro stringname [] pos
    case Foldable.toList ts of
      [MLine xs] -> return xs
      _          -> do
        report $ SkippedContent ("unknown string " ++ stringname) pos
        return mempty

lexLine :: PandocMonad m => ManLexer m ManTokens
lexLine = do
  lnparts <- mconcat <$> many1 linePart
  eofline
  go lnparts
  where  -- return empty line if we only have empty strings;
         -- this can happen if the line just contains \f[C], for example.
    go [] = return mempty
    go (RoffStr "" : xs) = go xs
    go xs = return $ singleTok $ MLine xs

linePart :: PandocMonad m => ManLexer m [LinePart]
linePart = macroArg <|> escape <|>
           regularText <|> quoteChar <|> spaceTabChar

macroArg :: PandocMonad m => ManLexer m [LinePart]
macroArg = try $ do
  string "\\\\$"
  x <- digit
  return [MacroArg $ ord x - ord '0']

regularText :: PandocMonad m => ManLexer m [LinePart]
regularText = do
  s <- many1 $ noneOf "\n\r\t \\\""
  return [RoffStr s]

quoteChar :: PandocMonad m => ManLexer m [LinePart]
quoteChar = do
  char '"'
  return [RoffStr "\""]

spaceTabChar :: PandocMonad m => ManLexer m [LinePart]
spaceTabChar = do
  c <- spacetab
  return [RoffStr [c]]

lexEmptyLine :: PandocMonad m => ManLexer m ManTokens
lexEmptyLine = char '\n' >> return (singleTok MEmptyLine)

manToken :: PandocMonad m => ManLexer m ManTokens
manToken = lexComment <|> lexMacro <|> lexLine <|> lexEmptyLine

linePartsToString :: [LinePart] -> String
linePartsToString = mconcat . map go
  where
  go (RoffStr s) = s
  go _ = mempty
