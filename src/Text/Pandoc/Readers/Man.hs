{-# LANGUAGE FlexibleContexts #-}
{-
  Copyright (C) 2018 Yan Pashkovsky <yanp.bugz@gmail.com>

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
   Module      : Text.Pandoc.Readers.Man
   Copyright   : Copyright (C) 2018 Yan Pashkovsky and John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : Yan Pashkovsky <yanp.bugz@gmail.com>
   Stability   : WIP
   Portability : portable

Conversion of man to 'Pandoc' document.
-}
module Text.Pandoc.Readers.Man (readMan) where

import Prelude
import Control.Monad (liftM, void, mzero, guard)
import Control.Monad.Except (throwError)
import Data.Char (isHexDigit, chr, ord)
import Data.Default (Default)
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import Data.Set (Set, singleton)
import qualified Data.Set as S (fromList, toList, union)
import Data.List (intersperse, intercalate)
import qualified Data.Text as T
import Text.Pandoc.Class (PandocMonad(..), report)
import Text.Pandoc.Builder as B hiding (singleton)
import Text.Pandoc.Error (PandocError (PandocParsecError))
import Text.Pandoc.Logging (LogMessage(..))
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import Text.Pandoc.Shared (crFilter, safeRead)
import Text.Parsec hiding (tokenPrim, space)
import qualified Text.Parsec as Parsec
import Text.Parsec.Pos (updatePosString)
import Text.Pandoc.GroffChar (characterCodes, combiningAccents)

--
-- Data Types
--
data FontKind = Bold | Italic | Monospace | Regular deriving (Show, Eq, Ord)

type MacroKind = String

type Font = Set FontKind

data LinePart = RoffStr (String, Font)
              | MacroArg Int
              deriving Show

-- TODO parse tables (see man tbl)
data ManToken = MLine [LinePart]
              | MEmptyLine
              | MMacro MacroKind [[LinePart]]
              | MComment
              deriving Show

data RoffState = RoffState { fontKind      :: Font
                           } deriving Show

instance Default RoffState where
  def = RoffState { fontKind = singleton Regular }

data ManState = ManState { customMacros  :: M.Map String [ManToken]
                         , readerOptions :: ReaderOptions
                         , metadata      :: Meta
                         } deriving Show

instance Default ManState where
  def = ManState { customMacros = mempty
                 , readerOptions = def
                 , metadata = nullMeta }

type ManLexer m = ParserT [Char] RoffState m
type ManParser m = ParserT [ManToken] ManState m


-- | Read man (troff) from an input string and return a Pandoc document.
readMan :: PandocMonad m => ReaderOptions -> T.Text -> m Pandoc
readMan opts txt = do
  eithertokens <- readWithM (many manToken) def (T.unpack $ crFilter txt)
  case eithertokens of
    Left e       -> throwError e
    Right tokenz -> do
      let state = def {readerOptions = opts} :: ManState
      eitherdoc <- readWithMTokens parseMan state tokenz
      either throwError return eitherdoc

  where

  readWithMTokens :: PandocMonad m
          => ParserT [ManToken] ManState m a  -- ^ parser
          -> ManState                         -- ^ initial state
          -> [ManToken]                       -- ^ input
          -> m (Either PandocError a)
  readWithMTokens parser state input =
    let leftF = PandocParsecError . (intercalate "\n") $ show <$> input
    in mapLeft leftF `liftM` runParserT parser state "source" input

  mapLeft :: (a -> c) -> Either a b -> Either c b
  mapLeft f (Left x) = Left $ f x
  mapLeft _ (Right r) = Right r

--
-- String -> ManToken function
--

manToken :: PandocMonad m => ManLexer m ManToken
manToken = lexComment <|> lexMacro <|> lexLine <|> lexEmptyLine

parseMan :: PandocMonad m => ManParser m Pandoc
parseMan = do
  bs <- many parseBlock <* eof
  meta <- metadata <$> getState
  let (Pandoc _ blocks) = doc $ mconcat bs
  return $ Pandoc meta blocks

parseBlock :: PandocMonad m => ManParser m Blocks
parseBlock = choice [ parseList
                    , parseDefinitionList
                    , parseBlockQuote
                    , parseTitle
                    , parsePara
                    , parseSkippedContent
                    , parseCodeBlock
                    , parseHeader
                    , parseMacroDef
                    , parseUnkownMacro
                    ]

eofline :: Stream s m Char => ParsecT s u m ()
eofline = void newline <|> eof

spacetab :: Stream s m Char => ParsecT s u m Char
spacetab = char ' ' <|> char '\t'

characterCodeMap :: M.Map String Char
characterCodeMap =
  M.fromList $ map (\(x,y) -> (y,x)) $ characterCodes ++ combiningAccents

escapeLexer :: PandocMonad m => ManLexer m String
escapeLexer = do
  char '\\'
  twoCharGlyph <|> bracketedGlyph <|> escFont <|> escStar <|> escSingle
  where

  twoCharGlyph = do
    char '('
    cs <- count 2 anyChar
    case M.lookup cs characterCodeMap of
      Just c  -> return [c]
      Nothing -> escUnknown ('\\':'(':cs) "\xFFFD"

  bracketedGlyph =
    char '[' *>
     (    ucharCode `sepBy1` (char '_')
      <|> charCode `sepBy1` (many1 Parsec.space)
     ) <* char ']'

  ucharCode = do
    char 'u'
    cs <- many1 (satisfy isHexDigit)
    case chr <$> safeRead ('0':'x':cs) of
       Nothing  -> mzero
       Just c   -> return c

  charCode = do
    cs <- many1 (noneOf ['[',']',' ','\t','\n'])
    case M.lookup cs characterCodeMap of
       Nothing -> escUnknown ("\\[" ++ cs ++ "]") '\xFFFD'
       Just c  -> return c

  escStar = do
    char '*'
    choice
      [ ("\xae" <$ char 'R')
      , ("" <$ char 'S') -- switch back to default font size
      , ("\x201c" <$ try (string "(lq") <|> try (string "[lq]"))
      , ("\x201d" <$ try (string "(rq") <|> try (string "[rq]"))
      , ("" <$ try (string "(HF" >>
                     modifyState (\r -> r {fontKind = singleton Bold})))
      , ("\x2122" <$ try (string "(Tm"))
      ]

  escSingle = do
    c <- anyChar
    case c of
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
      '-' -> return "-"
      '_' -> return "_"
      ' ' -> return " "
      '\\' -> return "\\"
      't' -> return "\t"
      'e' -> return "\\"
      '`' -> return "`"
      '^' -> return " "
      '|' -> return " "
      '\'' -> return "`"
      '.' -> return "`"
      _   -> escUnknown [c] "\xFFFD"

  escFont :: PandocMonad m => ManLexer m String
  escFont = do
    char 'f'
    font <- choice [ singleton <$> letterFontKind
          , char '(' >> anyChar >> anyChar >> return (singleton Regular)
          , try lettersFont
          , digit >> return (singleton Regular)
          ]
    modifyState (\r -> r {fontKind = font})
    return mempty

  lettersFont :: PandocMonad m => ManLexer m Font
  lettersFont = do
    char '['
    fs <- many letterFontKind
    many letter
    char ']'
    return $ S.fromList fs

  letterFontKind :: PandocMonad m => ManLexer m FontKind
  letterFontKind = choice [
      char 'B' >> return Bold
    , char 'I' >> return Italic
    , char 'C' >> return Monospace
    , (char 'P' <|> char 'R') >> return Regular
    ]

  escUnknown :: PandocMonad m => String -> a -> ManLexer m a
  escUnknown s x = do
    pos <- getPosition
    report $ SkippedContent ("Unknown escape sequence " ++ s) pos
    return x

currentFont :: PandocMonad m => ManLexer m Font
currentFont = fontKind <$> getState

-- separate function from lexMacro since real man files sometimes do not follow the rules
lexComment :: PandocMonad m => ManLexer m ManToken
lexComment = do
  try $ string ".\\\""
  many Parsec.space
  skipMany $ noneOf "\n"
  char '\n'
  return MComment

lexMacro :: PandocMonad m => ManLexer m ManToken
lexMacro = do
  char '.' <|> char '\''
  many spacetab
  macroName <- many (letter <|> oneOf ['\\', '"', '&', '.'])
  args <- lexArgs
  let addFonts fs = map (addFontsToRoffStr fs)
      addFontsToRoffStr fs (RoffStr (s, fs')) = RoffStr (s, fs `S.union` fs')
      addFontsToRoffStr _  x                  = x

      tok = case macroName of
              ""     -> MComment
              x | x `elem` ["\\\"", "\\#"] -> MComment
              "B"    -> MLine $ concatMap (addFonts (singleton Bold)) args
              "BR"   -> MLine $ concat args -- TODO
              x | x `elem` ["BI", "IB"] -> MLine $ -- TODO FIXME!
                        concatMap (addFonts (S.fromList [Italic, Bold])) args
              x | x `elem` ["I", "IR", "RI"]  -> MLine $
                        concatMap (addFonts (singleton Italic)) args
              x | x `elem` [ "P", "PP", "LP", "sp"] -> MEmptyLine
              _      -> MMacro macroName args
  return tok

  where

  lexArgs :: PandocMonad m => ManLexer m [[LinePart]]
  lexArgs = do
    args <- many $ try oneArg
    skipMany spacetab
    eofline
    return args

    where

    oneArg :: PandocMonad m => ManLexer m [LinePart]
    oneArg = do
      many1 spacetab
      skipMany $ try $ string "\\\n"  -- TODO why is this here?
      try quotedArg <|> plainArg
      -- try, because there are some erroneous files, e.g. linux/bpf.2

    plainArg :: PandocMonad m => ManLexer m [LinePart]
    plainArg = do
      -- TODO skip initial spaces, then parse many linePart til a spaec
      skipMany spacetab
      many (macroArg <|> esc <|> regularText)

    quotedArg :: PandocMonad m => ManLexer m [LinePart]
    quotedArg = do
        char '"'
        xs <- many (macroArg <|> esc <|> regularText <|> spaceTabChar
                    <|> escapedQuote)
        char '"'
        return xs
      where escapedQuote = try $ do
              char '"'
              char '"'
              fonts <- currentFont
              return $ RoffStr ("\"", fonts)

lexLine :: PandocMonad m => ManLexer m ManToken
lexLine = do
  lnparts <- many1 linePart
  eofline
  return $ MLine lnparts
  where

linePart :: PandocMonad m => ManLexer m LinePart
linePart = macroArg <|> esc <|> regularText <|> quoteChar <|> spaceTabChar

macroArg :: PandocMonad m => ManLexer m LinePart
macroArg = try $ do
  char '\\'
  char '$'
  x <- digit
  return $ MacroArg $ ord x - ord '0'

esc :: PandocMonad m => ManLexer m LinePart
esc = do
  s <- escapeLexer
  font <- currentFont
  return $ RoffStr (s, font)

regularText :: PandocMonad m => ManLexer m LinePart
regularText = do
  s <- many1 $ noneOf "\n\r\t \\\""
  font <- currentFont
  return $ RoffStr (s, font)

quoteChar :: PandocMonad m => ManLexer m LinePart
quoteChar = do
  char '"'
  font <- currentFont
  return $ RoffStr ("\"", font)

spaceTabChar :: PandocMonad m => ManLexer m LinePart
spaceTabChar = do
  c <- spacetab
  font <- currentFont
  return $ RoffStr ([c], font)

lexEmptyLine :: PandocMonad m => ManLexer m ManToken
lexEmptyLine = char '\n' >> return MEmptyLine

--
-- ManToken parsec functions
--

msatisfy :: (Show t, Stream s m t) => (t -> Bool) -> ParserT s st m t
msatisfy predic = tokenPrim show nextPos testTok
  where
    testTok t     = if predic t then Just t else Nothing
    nextPos pos _x _xs  = updatePosString
                             (setSourceColumn
                               (setSourceLine pos $ sourceLine pos + 1) 1) ("")

mline :: PandocMonad m => ManParser m ManToken
mline = msatisfy isMLine where
  isMLine (MLine _) = True
  isMLine _ = False

memptyLine :: PandocMonad m => ManParser m ManToken
memptyLine = msatisfy isMEmptyLine where
  isMEmptyLine MEmptyLine = True
  isMEmptyLine _ = False

mmacro :: PandocMonad m => MacroKind -> ManParser m ManToken
mmacro mk = msatisfy isMMacro where
  isMMacro (MMacro mk' _) | mk == mk' = True
                          | otherwise = False
  isMMacro _ = False

mmacroAny :: PandocMonad m => ManParser m ManToken
mmacroAny = msatisfy isMMacro where
  isMMacro (MMacro _ _) = True
  isMMacro _ = False

mcomment :: PandocMonad m => ManParser m ManToken
mcomment = msatisfy isMComment where
  isMComment MComment = True
  isMComment _        = False

--
-- ManToken -> Block functions
--

parseTitle :: PandocMonad m => ManParser m Blocks
parseTitle = do
  (MMacro _ args) <- mmacro "TH"
  let adjustMeta =
       case args of
         (x:y:z:_) -> setMeta "title" (linePartsToInlines x) .
                      setMeta "section" (linePartsToInlines y) .
                      setMeta "date" (linePartsToInlines z)
         [x,y]     -> setMeta "title" (linePartsToInlines x) .
                      setMeta "section" (linePartsToInlines y)
         [x]       -> setMeta "title" (linePartsToInlines x)
         []        -> id
  modifyState $ \st -> st{ metadata = adjustMeta $ metadata st }
  return mempty

parseSkippedContent :: PandocMonad m => ManParser m Blocks
parseSkippedContent = mempty <$ (mcomment <|> memptyLine)

linePartsToInlines :: [LinePart] -> Inlines
linePartsToInlines = mconcat . map go
  where
  go (RoffStr (s, fonts)) = inner (S.toList fonts) s
  go _ = mempty
  inner :: [FontKind] -> String -> Inlines
  inner [] s = text s
  inner (Bold:fs) s = strong $ inner fs s
  inner (Italic:fs) s = emph $ inner fs s
  -- Monospace goes after Bold and Italic in ordered set
  inner (Monospace:_) s = code s
  inner (Regular:fs) s = inner fs s

linePartsToString :: [LinePart] -> String
linePartsToString = mconcat . map go
  where
  go (RoffStr (s, _)) = s
  go _ = mempty

parsePara :: PandocMonad m => ManParser m Blocks
parsePara = para . trimInlines <$> parseInlines

parseInlines :: PandocMonad m => ManParser m Inlines
parseInlines = do
  inls <- many1 (lineInl <|> comment)
  let withspaces = intersperse B.space inls
  return $ mconcat withspaces

lineInl :: PandocMonad m => ManParser m Inlines
lineInl = do
  (MLine fragments) <- mline
  return $ linePartsToInlines $ fragments

comment :: PandocMonad m => ManParser m Inlines
comment = mcomment >> return mempty

bareIP :: PandocMonad m => ManParser m ManToken
bareIP = msatisfy isBareIP where
  isBareIP (MMacro "IP" []) = True
  isBareIP _                = False

parseCodeBlock :: PandocMonad m => ManParser m Blocks
parseCodeBlock = try $ do
  optional bareIP -- some people indent their code
  mmacro "nf"
  toks <- many (mline <|> memptyLine <|> mcomment)
  mmacro "fi"
  return $ codeBlock (removeFinalNewline $
                      intercalate "\n" . catMaybes $
                      extractText <$> toks)

  where

  removeFinalNewline [] = []
  removeFinalNewline xs = if last xs == '\n' then init xs else xs
  extractText :: ManToken -> Maybe String
  extractText (MLine ss) = Just $ linePartsToString ss
  extractText MEmptyLine = Just ""
  -- string are intercalated with '\n', this prevents double '\n'
  extractText _ = Nothing

parseHeader :: PandocMonad m => ManParser m Blocks
parseHeader = do
  MMacro name args <- mmacro "SH" <|> mmacro "SS"
  contents <- if null args
                 then do
                   lineInl
                 else do
                   return $
                     mconcat $ intersperse B.space $ map linePartsToInlines args
  let lvl = if name == "SH" then 1 else 2
  return $ header lvl contents

parseBlockQuote :: PandocMonad m => ManParser m Blocks
parseBlockQuote = blockQuote <$> continuation

data ListType = Ordered ListAttributes
              | Bullet

listTypeMatches :: Maybe ListType -> ListType -> Bool
listTypeMatches Nothing _            = True
listTypeMatches (Just Bullet) Bullet = True
listTypeMatches (Just (Ordered (_,x,y))) (Ordered (_,x',y'))
                                     = x == x' && y == y'
listTypeMatches (Just _) _           = False

listItem :: PandocMonad m => Maybe ListType -> ManParser m (ListType, Blocks)
listItem mbListType = try $ do
  (MMacro _ args) <- mmacro "IP"
  case args of
    (arg1 : _)  -> do
      let cs = linePartsToString arg1
      let cs' = if not ('.' `elem` cs || ')' `elem` cs) then cs ++ "." else cs
      let lt = case Parsec.runParser anyOrderedListMarker defaultParserState
                     "list marker" cs' of
                  Right (start, listtype, listdelim)
                    | cs == cs' -> Ordered (start, listtype, listdelim)
                    | otherwise -> Ordered (start, listtype, DefaultDelim)
                  Left _        -> Bullet
      guard $ listTypeMatches mbListType lt
      inls <- parseInlines
      continuations <- mconcat <$> many continuation
      return $ (lt, para inls <> continuations)
    []          -> mzero

parseList :: PandocMonad m => ManParser m Blocks
parseList = try $ do
  (lt, x) <- listItem Nothing
  xs <- map snd <$> many (listItem (Just lt))
  return $ case lt of
             Bullet        -> bulletList (x:xs)
             Ordered lattr -> orderedListWith lattr (x:xs)

continuation :: PandocMonad m => ManParser m Blocks
continuation = do
  mmacro "RS"
  bs <- mconcat <$> many (notFollowedBy (mmacro "RE") >> parseBlock)
  mmacro "RE"
  return bs

definitionListItem :: PandocMonad m
                   => ManParser m (Inlines, [Blocks])
definitionListItem = try $ do
  (MMacro _ _) <- mmacro "TP"  -- args specify indent level, can ignore
  term <- lineInl
  inls <- parseInlines
  continuations <- mconcat <$> many continuation
  return $ (term, [para inls <> continuations])

parseDefinitionList :: PandocMonad m => ManParser m Blocks
parseDefinitionList = definitionList <$> many1 definitionListItem

parseMacroDef :: PandocMonad m => ManParser m Blocks
parseMacroDef = do
  MMacro _ args <- mmacro "de"
  (macroName, stopMacro) <-
    case args of
       (x : y : _) -> return (linePartsToString x, linePartsToString y)
                      -- optional second arg
       (x:_)       -> return (linePartsToString x, ".")
       []          -> fail "No argument to .de"
  ts <- manyTill (msatisfy (const True)) (mmacro stopMacro)
  modifyState $ \st ->
    st{ customMacros = M.insert macroName ts (customMacros st) }
  return mempty

-- In case of weird man file it will be parsed succesfully
parseUnkownMacro :: PandocMonad m => ManParser m Blocks
parseUnkownMacro = do
  pos <- getPosition
  tok <- mmacroAny
  case tok of
    MMacro mkind args -> do
      macros <- customMacros <$> getState
      case M.lookup mkind macros of
        Nothing -> do
          report $ SkippedContent ('.':mkind) pos
          return mempty
        Just ts -> do
          toks <- getInput
          let fillLP (RoffStr (x,y)) zs = RoffStr (x,y) : zs
              fillLP (MacroArg i)    zs =
                 case drop (i - 1) args of
                   []     -> zs
                   (ys:_) -> ys ++ zs
          let fillMacroArg (MLine lineparts) = MLine (foldr fillLP [] lineparts)
              fillMacroArg x = x
          setInput $ (map fillMacroArg ts) ++ toks
          return mempty
    _              -> fail "the impossible happened"
