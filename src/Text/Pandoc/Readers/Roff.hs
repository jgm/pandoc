{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{- |
   Module      : Text.Pandoc.Readers.Roff
   Copyright   : Copyright (C) 2018-2020 Yan Pashkovsky and John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : Yan Pashkovsky <yanp.bugz@gmail.com>
   Stability   : WIP
   Portability : portable

Tokenizer for roff formats (man, ms).
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
  , linePartsToText
  , lexRoff
  )
where

import Safe (lastDef)
import Control.Monad (void, guard)
import Control.Monad.Except (throwError)
import Text.Pandoc.Class.PandocMonad
       (getResourcePath, readFileFromDirs, PandocMonad(..), report)
import Data.Char (isLower, toLower, toUpper, isAlphaNum)
import Data.Default (Default)
import qualified Data.Map as M
import Data.List (intercalate)
import qualified Data.Text as T
import Text.Pandoc.Logging (LogMessage(..))
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import Text.Pandoc.Shared (safeRead)
import Text.Pandoc.Readers.Roff.Escape
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable

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

data LinePart = RoffStr T.Text
              | Font FontSpec
              | MacroArg Int
              deriving Show

instance RoffLikeLexer RoffTokens where
  -- The token stream is a list of 'LinePart's
  type Token RoffTokens = [LinePart]
  type State RoffTokens = RoffState
  emit t = [RoffStr t]
  expandString = try $ do
    pos <- getPosition
    char '\\'
    char '*'
    cs <- escapeArg <|> countChar 1 anyChar
    s <- linePartsToText <$> resolveText cs pos
    addToInput s
  escString = try $ do
    pos <- getPosition
    (do cs <- escapeArg <|> countChar 1 anyChar
        resolveText cs pos)
      <|> mempty <$ char 'S'
  backslash = do
    char '\\'
    mode <- roffMode <$> getState
    case mode of
      -- experimentally, it seems you don't always need to double
      -- the backslash in macro defs.  It's essential with \\$1,
      -- but not with \\f[I].  So we make the second one optional.
      CopyMode   -> optional $ char '\\'
      NormalMode -> return ()
  checkDefined name = do
    macros <- customMacros <$> getState
    case M.lookup name macros of
      Just _  -> return [RoffStr "1"]
      Nothing -> return [RoffStr "0"]
  -- \E is ignored in copy mode
  escE = do
      mode <- roffMode <$> getState
      case mode of
        CopyMode   -> return mempty
        NormalMode -> return [RoffStr "\\"]
  escFont = do
    font <- escapeArg <|> countChar 1 alphaNum
    font' <- if T.null font || font == "P"
                then prevFont <$> getState
                else return $ foldr processFontLetter defaultFontSpec $ T.unpack font
    updateState $ \st -> st{ prevFont = currentFont st
                           , currentFont = font' }
    return [Font font']
    where
      processFontLetter c fs
                | isLower c    = processFontLetter (toUpper c) fs
      processFontLetter 'B' fs = fs{ fontBold = True }
      processFontLetter 'I' fs = fs{ fontItalic = True }
      processFontLetter 'C' fs = fs{ fontMonospace = True }
      processFontLetter _   fs = fs -- do nothing

type Arg = [LinePart]

type TableOption = (T.Text, T.Text)

data CellFormat =
  CellFormat
  { columnType     :: Char
  , pipePrefix     :: Bool
  , pipeSuffix     :: Bool
  , columnSuffixes :: [T.Text]
  } deriving (Show, Eq, Ord)

type TableRow = ([CellFormat], [RoffTokens])

data RoffToken = TextLine [LinePart]
               | EmptyLine
               | ControlLine T.Text [Arg] SourcePos
               | Tbl [TableOption] [TableRow] SourcePos
               deriving Show

newtype RoffTokens = RoffTokens { unRoffTokens :: Seq.Seq RoffToken }
        deriving (Show, Semigroup, Monoid)

singleTok :: RoffToken -> RoffTokens
singleTok t = RoffTokens (Seq.singleton t)

data RoffMode = NormalMode
              | CopyMode
              deriving Show

data RoffState = RoffState { customMacros     :: M.Map T.Text RoffTokens
                           , prevFont         :: FontSpec
                           , currentFont      :: FontSpec
                           , tableTabChar     :: Char
                           , roffMode         :: RoffMode
                           , lastExpression   :: Maybe Bool
                           , afterConditional :: Bool
                           } deriving Show

instance Default RoffState where
  def = RoffState { customMacros = M.fromList
                       $ map (\(n, s) ->
                                (n, singleTok
                                  (TextLine [RoffStr s])))
                       [ ("Tm", "\x2122")
                       , ("lq", "\x201C")
                       , ("rq", "\x201D")
                       , ("R",  "\x00AE") ]
                  , prevFont = defaultFontSpec
                  , currentFont = defaultFontSpec
                  , tableTabChar = '\t'
                  , roffMode = NormalMode
                  , lastExpression = Nothing
                  , afterConditional = False
                  }

type RoffLexer m = ParsecT Sources RoffState m

--
-- Lexer: T.Text -> RoffToken
--

eofline :: (Stream s m Char, UpdateSourcePos s Char) => ParsecT s u m ()
eofline = void newline <|> eof <|> () <$ lookAhead (string "\\}")

spacetab :: (Stream s m Char, UpdateSourcePos s Char) => ParsecT s u m Char
spacetab = char ' ' <|> char '\t'

-- separate function from lexMacro since real man files sometimes do not
-- follow the rules
lexComment :: PandocMonad m => RoffLexer m RoffTokens
lexComment = do
  try $ string ".\\\""
  skipMany $ noneOf "\n"
  eofline
  return mempty

lexMacro :: PandocMonad m => RoffLexer m RoffTokens
lexMacro = do
  pos <- getPosition
  st <- getState
  guard $ sourceColumn pos == 1 || afterConditional st
  char '.' <|> char '\''
  skipMany spacetab
  macroName <- manyChar (satisfy isAlphaNum)
  case macroName of
    "nop" -> return mempty
    "ie"  -> lexConditional "ie"
    "if"  -> lexConditional "if"
    "el"  -> lexConditional "el"
    "while" -> lexConditional "while"
               -- this doesn't get the semantics right but
               -- avoids parse errors

    _ -> do
       args <- lexArgs
       case macroName of
         ""     -> return mempty
         "TS"   -> lexTable pos
         "de"   -> lexMacroDef args
         "de1"  -> lexMacroDef args
         "ds"   -> lexStringDef args
         "ds1"  -> lexStringDef args
         "sp"   -> return $ singleTok EmptyLine
         "so"   -> lexIncludeFile args
         _      -> resolveMacro macroName args pos

lexTable :: PandocMonad m => SourcePos -> RoffLexer m RoffTokens
lexTable pos = do
  skipMany lexComment
  spaces
  opts <- try tableOptions <|> [] <$ optional (char ';')
  case lookup "tab" opts of
    Just (T.uncons -> Just (c, _)) -> updateState $ \st -> st{ tableTabChar = c }
    _                              -> updateState $ \st -> st{ tableTabChar = '\t' }
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
  return $ singleTok $ Tbl opts (rows <> concat morerows) pos

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
tableOptions = many1 tableOption <* spaces <* char ';'

tableOption :: PandocMonad m => RoffLexer m TableOption
tableOption = do
  k <- many1Char letter
  v <- option "" $ try $ do
         skipMany spacetab
         char '('
         manyTillChar anyChar (char ')')
  skipMany spacetab
  optional (char ',' >> skipMany spacetab)
  return (k,v)

tableFormatSpec :: PandocMonad m => RoffLexer m [[CellFormat]]
tableFormatSpec = do
  first <- tableFormatSpecLine
  rest <- many $ try $ (newline <|> char ',') *> tableFormatSpecLine
  let speclines = first:rest
  spaces
  char '.'
  return $ speclines <> repeat (lastDef [] speclines) -- last line is default

tableFormatSpecLine :: PandocMonad m => RoffLexer m [CellFormat]
tableFormatSpecLine =
  many1 $ skipMany spacetab *> tableColFormat <* skipMany spacetab

tableColFormat :: PandocMonad m => RoffLexer m CellFormat
tableColFormat = do
    pipePrefix' <- option False
                   $ True <$ try (string "|" <* notFollowedBy spacetab)
    c <- oneOf ['a','A','c','C','l','L','n','N','r','R','s','S','^','_','-',
                '=','|']
    suffixes <- many $ try (skipMany spacetab *> countChar 1 digit) <|>
      (do x <- oneOf ['b','B','d','D','e','E','f','F','i','I','m','M',
                  'p','P','t','T','u','U','v','V','w','W','x','X', 'z','Z']
          num <- case toLower x of
                   'w' -> many1 digit <|>
                           (do char '('
                               xs <- manyTill anyChar (char ')')
                               return ("(" <> xs <> ")")) <|>
                           return ""
                   'f' -> count 1 alphaNum <* skipMany spacetab
                   'm' -> count 1 alphaNum <* skipMany spacetab
                   _   -> return ""
          return $ T.pack $ x : num)
    pipeSuffix' <- option False $ True <$ string "|"
    return $ CellFormat
             { columnType     = c
             , pipePrefix     = pipePrefix'
             , pipeSuffix     = pipeSuffix'
             , columnSuffixes = suffixes }

-- We don't fully handle the conditional.  But we do
-- include everything under '.ie n', which occurs commonly
-- in man pages.
lexConditional :: PandocMonad m => T.Text -> RoffLexer m RoffTokens
lexConditional mname = do
  pos <- getPosition
  skipMany spacetab
  mbtest <- if mname == "el"
               then fmap not . lastExpression <$> getState
               else expression
  skipMany spacetab
  st <- getState -- save state, so we can reset it
  ifPart <- do
      optional $ try $ char '\\' >> newline
      lexGroup
       <|> do updateState $ \s -> s{ afterConditional = True }
              t <- manToken
              updateState $ \s -> s{ afterConditional = False }
              return t
  case mbtest of
    Nothing    -> do
      setState st  -- reset state, so we don't record macros in skipped section
      report $ SkippedContent (T.cons '.' mname) pos
      return mempty
    Just True  -> return ifPart
    Just False -> do
      setState st
      return mempty

expression :: PandocMonad m => RoffLexer m (Maybe Bool)
expression = do
  raw <- charsInBalanced '(' ')' (T.singleton <$> (satisfy (/= '\n')))
      <|> many1Char nonspaceChar
  returnValue $
    case raw of
      "1"  -> Just True
      "n"  -> Just True  -- nroff mode
      "t"  -> Just False -- troff mode
      _    -> Nothing
  where
    returnValue v = do
      updateState $ \st -> st{ lastExpression = v }
      return v

lexGroup :: PandocMonad m => RoffLexer m RoffTokens
lexGroup = do
  groupstart
  mconcat <$> manyTill manToken groupend
  where
    groupstart = try $ string "\\{" <* optional (try (string "\\\n"))
    groupend   = try $ string "\\}"

lexIncludeFile :: PandocMonad m => [Arg] -> RoffLexer m RoffTokens
lexIncludeFile args = do
  pos <- getPosition
  case args of
    (f:_) -> do
      let fp = linePartsToText f
      dirs <- getResourcePath
      result <- readFileFromDirs dirs $ T.unpack fp
      case result of
        Nothing  -> report $ CouldNotLoadIncludeFile fp pos
        Just s   -> addToInput s
      return mempty
    []    -> return mempty

resolveMacro :: PandocMonad m
             => T.Text -> [Arg] -> SourcePos -> RoffLexer m RoffTokens
resolveMacro macroName args pos = do
  macros <- customMacros <$> getState
  case M.lookup macroName macros of
    Nothing -> return $ singleTok $ ControlLine macroName args pos
    Just ts -> do
      let fillLP (MacroArg i)    zs =
            case drop (i - 1) args of
              []     -> zs
              (ys:_) -> ys <> zs
          fillLP z zs = z : zs
      let fillMacroArg (TextLine lineparts) =
            TextLine (foldr fillLP [] lineparts)
          fillMacroArg x = x
      return $ RoffTokens . fmap fillMacroArg . unRoffTokens $ ts

lexStringDef :: PandocMonad m => [Arg] -> RoffLexer m RoffTokens
lexStringDef args = do -- string definition
   case args of
     []     -> Prelude.fail "No argument to .ds"
     (x:ys) -> do
       let ts = singleTok $ TextLine (intercalate [RoffStr " " ] ys)
       let stringName = linePartsToText x
       updateState $ \st ->
         st{ customMacros = M.insert stringName ts (customMacros st) }
   return mempty

lexMacroDef :: PandocMonad m => [Arg] -> RoffLexer m RoffTokens
lexMacroDef args = do -- macro definition
   updateState $ \st -> st{ roffMode = CopyMode }
   (macroName, stopMacro) <-
     case args of
       (x : y : _) -> return (linePartsToText x, linePartsToText y)
                      -- optional second arg
       (x:_)       -> return (linePartsToText x, ".")
       []          -> Prelude.fail "No argument to .de"
   let stop = try $ do
         char '.' <|> char '\''
         skipMany spacetab
         textStr stopMacro
         _ <- lexArgs
         return ()
   ts <- mconcat <$> manyTill manToken stop
   updateState $ \st ->
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

-- strings and macros share namespace
resolveText :: PandocMonad m
              => T.Text -> SourcePos -> RoffLexer m [LinePart]
resolveText stringname pos = do
  RoffTokens ts <- resolveMacro stringname [] pos
  case Foldable.toList ts of
    [TextLine xs] -> return xs
    _          -> do
      report $ SkippedContent ("unknown string " <> stringname) pos
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
    go xs = return $ singleTok $ TextLine xs

linePart :: PandocMonad m => RoffLexer m [LinePart]
linePart = macroArg <|> escape <|>
           regularText <|> quoteChar <|> spaceTabChar

macroArg :: PandocMonad m => RoffLexer m [LinePart]
macroArg = try $ do
  pos <- getPosition
  backslash
  char '$'
  x <- escapeArg <|> countChar 1 digit
  case safeRead x of
    Just i  -> return [MacroArg i]
    Nothing -> do
      report $ SkippedContent ("illegal macro argument " <> x) pos
      return []

regularText :: PandocMonad m => RoffLexer m [LinePart]
regularText = do
  s <- many1Char $ noneOf "\n\r\t \\\""
  return [RoffStr s]

quoteChar :: PandocMonad m => RoffLexer m [LinePart]
quoteChar = do
  char '"'
  return [RoffStr "\""]

spaceTabChar :: PandocMonad m => RoffLexer m [LinePart]
spaceTabChar = do
  c <- spacetab
  return [RoffStr $ T.singleton c]

lexEmptyLine :: PandocMonad m => RoffLexer m RoffTokens
lexEmptyLine = newline >> return (singleTok EmptyLine)

manToken :: PandocMonad m => RoffLexer m RoffTokens
manToken = lexComment <|> lexMacro <|> lexLine <|> lexEmptyLine

linePartsToText :: [LinePart] -> T.Text
linePartsToText = mconcat . map go
  where
  go (RoffStr s) = s
  go _ = mempty

-- | Tokenize a string as a sequence of roff tokens.
lexRoff :: PandocMonad m => SourcePos -> T.Text -> m RoffTokens
lexRoff pos txt = do
  eithertokens <- readWithM (do setPosition pos
                                mconcat <$> many manToken) def txt
  case eithertokens of
    Left e       -> throwError e
    Right tokenz -> return tokenz
