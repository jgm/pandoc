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
import Text.Pandoc.Class (getResourcePath, readFileFromDirs)
import Data.Char (isHexDigit, chr, ord)
import Data.Default (Default)
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (intersperse, intercalate)
import qualified Data.Text as T
import Text.Pandoc.Class (PandocMonad(..), report)
import Text.Pandoc.Builder as B
import Text.Pandoc.Error (PandocError (PandocParsecError))
import Text.Pandoc.Logging (LogMessage(..))
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import Text.Pandoc.Shared (crFilter, safeRead)
import Text.Parsec hiding (tokenPrim, space)
import qualified Text.Parsec as Parsec
import Text.Parsec.Pos (updatePosString)
import Text.Pandoc.GroffChar (characterCodes, combiningAccents)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable

-- import Debug.Trace (traceShowId)

--
-- Data Types
--
data FontKind = Bold | Italic | Monospace | Regular deriving (Show, Eq, Ord)

type MacroKind = String

type Font = Set FontKind

data LinePart = RoffStr (String, Font)
              | MacroArg Int
              deriving Show

type Arg = [LinePart]

-- TODO parse tables (see man tbl)
data ManToken = MLine [LinePart]
              | MEmptyLine
              | MMacro MacroKind [Arg]
              deriving Show

newtype ManTokens = ManTokens { unManTokens :: Seq.Seq ManToken }
        deriving (Show, Semigroup, Monoid)

singleTok :: ManToken -> ManTokens
singleTok t = ManTokens (Seq.singleton t)

data RoffState = RoffState { fontKind      :: Font
                           , customMacros  :: M.Map String ManTokens
                           } deriving Show

instance Default RoffState where
  def = RoffState { customMacros = M.fromList
                       $ map (\(n, s) ->
                                (n, singleTok
                                  (MLine [RoffStr (s, mempty)])))
                       [ ("Tm", "\x2122")
                       , ("lq", "\x201C")
                       , ("rq", "\x201D")
                       , ("R",  "\x00AE") ]
                  , fontKind     = S.singleton Regular }

data ManState = ManState { readerOptions :: ReaderOptions
                         , metadata      :: Meta
                         } deriving Show

instance Default ManState where
  def = ManState { readerOptions = def
                 , metadata      = nullMeta }

type ManLexer m = ParserT [Char] RoffState m
type ManParser m = ParserT [ManToken] ManState m


-- | Read man (troff) from an input string and return a Pandoc document.
readMan :: PandocMonad m => ReaderOptions -> T.Text -> m Pandoc
readMan opts txt = do
  eithertokens <- readWithM
    (Foldable.toList . unManTokens . mconcat <$> many manToken)
    def (T.unpack $ crFilter txt)
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
-- Lexer: String -> ManToken
--

eofline :: Stream s m Char => ParsecT s u m ()
eofline = void newline <|> eof

spacetab :: Stream s m Char => ParsecT s u m Char
spacetab = char ' ' <|> char '\t'

characterCodeMap :: M.Map String Char
characterCodeMap =
  M.fromList $ map (\(x,y) -> (y,x)) $ characterCodes ++ combiningAccents

escapeLexer :: PandocMonad m => ManLexer m String
escapeLexer = try $ do
  char '\\'
  c <- noneOf ['*','$'] -- see escStar, macroArg
  case c of
    '(' -> twoCharGlyph
    '[' -> bracketedGlyph
    'f' -> escFont
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

  where

  twoCharGlyph = do
    cs <- count 2 anyChar
    case M.lookup cs characterCodeMap of
      Just c  -> return [c]
      Nothing -> escUnknown ('\\':'(':cs) "\xFFFD"

  bracketedGlyph =
     (    ucharCode `sepBy1` (char '_')
      <|> charCode `sepBy1` (many1 Parsec.space)
     ) <* char ']'

  ucharCode = do
    char 'u'
    cs <- many1 (satisfy isHexDigit)
    case chr <$> safeRead ('0':'x':cs) of
       Nothing  -> escUnknown ("\\[u" ++ cs ++ "]") '\xFFFD'
       Just c   -> return c

  charCode = do
    cs <- many1 (noneOf ['[',']',' ','\t','\n'])
    case M.lookup cs characterCodeMap of
       Nothing -> escUnknown ("\\[" ++ cs ++ "]") '\xFFFD'
       Just c  -> return c


  escFont :: PandocMonad m => ManLexer m String
  escFont = do
    font <- choice [ S.singleton <$> letterFontKind
          , char '(' >> anyChar >> anyChar >> return (S.singleton Regular)
          , try lettersFont
          , digit >> return (S.singleton Regular)
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
lexComment :: PandocMonad m => ManLexer m ManTokens
lexComment = do
  try $ string ".\\\""
  many Parsec.space
  skipMany $ noneOf "\n"
  char '\n'
  return mempty

lexMacro :: PandocMonad m => ManLexer m ManTokens
lexMacro = do
  char '.' <|> char '\''
  many spacetab
  macroName <- many (letter <|> oneOf ['\\', '"', '&', '.'])
  args <- lexArgs

  case macroName of
    ""     -> return mempty
    "\\\"" -> return mempty
    "\\#"  -> return mempty
    "de"   -> lexMacroDef args
    "sp"   -> return $ singleTok MEmptyLine
    "so"   -> lexIncludeFile args
    _      -> resolveMacro macroName args


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
             => String -> [Arg] -> ManLexer m ManTokens
resolveMacro macroName args = do
  macros <- customMacros <$> getState
  case M.lookup macroName macros of
    Nothing -> return $ singleTok $ MMacro macroName args
    Just ts -> do
      let fillLP (RoffStr (x,y)) zs = RoffStr (x,y) : zs
          fillLP (MacroArg i)    zs =
            case drop (i - 1) args of
              []     -> zs
              (ys:_) -> ys ++ zs
      let fillMacroArg (MLine lineparts) =
            MLine (foldr fillLP [] lineparts)
          fillMacroArg x = x
      return $ ManTokens . fmap fillMacroArg . unManTokens $ ts

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
    many1 spacetab
    skipMany $ try $ string "\\\n"  -- continuation line
    try quotedArg <|> plainArg
    -- try, because there are some erroneous files, e.g. linux/bpf.2

  plainArg :: PandocMonad m => ManLexer m [LinePart]
  plainArg = do
    skipMany spacetab
    mconcat <$>
      many (macroArg <|> esc <|> regularText <|> unescapedQuote <|> escStar)
    where unescapedQuote = do
            char '"'
            fonts <- currentFont
            return [RoffStr ("\"", fonts)]


  quotedArg :: PandocMonad m => ManLexer m [LinePart]
  quotedArg = do
      char '"'
      xs <- mconcat <$>
             many (macroArg <|> esc <|> escStar <|> regularText
                   <|> spaceTabChar <|> escapedQuote)
      char '"'
      return xs
    where escapedQuote = try $ do
            char '"'
            char '"'
            fonts <- currentFont
            return [RoffStr ("\"", fonts)]

escStar :: PandocMonad m => ManLexer m [LinePart]
escStar = try $ do
  char '\\'
  char '*'
  c <- anyChar
  case c of
    '(' -> do
      cs <- count 2 anyChar
      resolveString cs
    '[' -> do
      cs <- many (noneOf "\t\n\r ]")
      char ']'
      resolveString cs
    'S' -> return mempty -- switch back to default font size
    _   -> resolveString [c]

  where

  -- strings and macros share namespace
  resolveString stringname = do
    ManTokens ts <- resolveMacro stringname []
    case Foldable.toList ts of
      [MLine xs] -> return xs
      _          -> do
        pos <- getPosition
        report $ SkippedContent ("unknown string " ++ stringname) pos
        return mempty

lexLine :: PandocMonad m => ManLexer m ManTokens
lexLine = do
  lnparts <- mconcat <$> many1 linePart
  eofline
  return $ singleTok $ MLine lnparts
  where

linePart :: PandocMonad m => ManLexer m [LinePart]
linePart = macroArg <|> esc <|> escStar <|>
           regularText <|> quoteChar <|> spaceTabChar

macroArg :: PandocMonad m => ManLexer m [LinePart]
macroArg = try $ do
  char '\\'
  char '$'
  x <- digit
  return [MacroArg $ ord x - ord '0']

esc :: PandocMonad m => ManLexer m [LinePart]
esc = do
  s <- escapeLexer
  font <- currentFont
  return [RoffStr (s, font)]

regularText :: PandocMonad m => ManLexer m [LinePart]
regularText = do
  s <- many1 $ noneOf "\n\r\t \\\""
  font <- currentFont
  return [RoffStr (s, font)]

quoteChar :: PandocMonad m => ManLexer m [LinePart]
quoteChar = do
  char '"'
  font <- currentFont
  return [RoffStr ("\"", font)]

spaceTabChar :: PandocMonad m => ManLexer m [LinePart]
spaceTabChar = do
  c <- spacetab
  font <- currentFont
  return [RoffStr ([c], font)]

lexEmptyLine :: PandocMonad m => ManLexer m ManTokens
lexEmptyLine = char '\n' >> return (singleTok MEmptyLine)

manToken :: PandocMonad m => ManLexer m ManTokens
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
                    , parseNewParagraph
                    , parsePara
                    , parseCodeBlock
                    , parseHeader
                    , skipUnkownMacro
                    ]

parseNewParagraph :: PandocMonad m => ManParser m Blocks
parseNewParagraph = do
  mmacro "P" <|> mmacro "PP" <|> mmacro "LP" <|> memptyLine
  return mempty

--
-- Parser: [ManToken] -> Pandoc
--

msatisfy :: (Show t, Stream s m t) => (t -> Bool) -> ParserT s st m t
msatisfy predic = tokenPrim show nextPos testTok
  where
    testTok t     = if predic t then Just t else Nothing
    nextPos pos _x _xs  = updatePosString
                             (setSourceColumn
                               (setSourceLine pos $ sourceLine pos + 1) 1) ("")

mtoken :: PandocMonad m => ManParser m ManToken
mtoken = msatisfy (const True)

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

linePartsToInlines :: [LinePart] -> Inlines
linePartsToInlines = go

  where
  go [] = mempty
  go (MacroArg _:xs) = go xs -- shouldn't happen
  go xs@(RoffStr{} : _) =
    if lb > 0 && lb >= li
       then strong (go (removeFont Bold bolds)) <> go (drop lb xs)
       else if li > 0
            then emph (go (removeFont Italic italics)) <> go (drop li xs)
            else text (linePartsToString regulars) <> go (drop lr xs)

    where
    (lb, li, lr) = (length bolds, length italics, length regulars)

    removeFont font = map (removeFont' font)
    removeFont' font (RoffStr (s,f)) = RoffStr (s, S.delete font f)
    removeFont' _ x = x

    bolds = takeWhile isBold xs
    italics = takeWhile isItalic xs
    regulars = takeWhile (\x -> not (isBold x || isItalic x)) xs

    isBold (RoffStr (_,f)) = Bold `S.member` f
    isBold _ = False

    isItalic (RoffStr (_,f)) = Italic `S.member` f
    isItalic _ = False

linePartsToString :: [LinePart] -> String
linePartsToString = mconcat . map go
  where
  go (RoffStr (s, _)) = s
  go _ = mempty

parsePara :: PandocMonad m => ManParser m Blocks
parsePara = para . trimInlines <$> parseInlines

parseInlines :: PandocMonad m => ManParser m Inlines
parseInlines = mconcat . intersperse B.space <$> many1 parseInline

parseInline :: PandocMonad m => ManParser m Inlines
parseInline = try $ do
  tok <- mtoken
  case tok of
    MLine lparts -> return $ linePartsToInlines lparts
    MMacro mname args ->
      case mname of
        "UR" -> parseLink args
        "MT" -> parseEmailLink args
        "B"  -> parseBold args
        "I"  -> parseItalic args
        "br" -> return linebreak
        "BI" -> parseAlternatingFonts [strong, emph] args
        "IB" -> parseAlternatingFonts [emph, strong] args
        "IR" -> parseAlternatingFonts [emph, id] args
        "RI" -> parseAlternatingFonts [id, emph] args
        "BR" -> parseAlternatingFonts [strong, id] args
        "RB" -> parseAlternatingFonts [id, strong] args
        "SY" -> return $ strong $ mconcat $ intersperse B.space
                       $ map linePartsToInlines args
        "YS" -> return mempty
        "OP" -> case args of
                  (x:ys) -> return $ B.space <> str "[" <> B.space <>
                             mconcat (strong (linePartsToInlines x) :
                               map ((B.space <>) . linePartsToInlines) ys)
                             <> B.space <> str "]"
                  []     -> return mempty
        _ -> mzero
    _ -> mzero

parseBold :: PandocMonad m => [Arg] -> ManParser m Inlines
parseBold [] = do
  MLine lparts <- mline
  return $ strong $ linePartsToInlines lparts
parseBold args = return $
  strong $ mconcat $ intersperse B.space $ map linePartsToInlines args

parseItalic :: PandocMonad m => [Arg] -> ManParser m Inlines
parseItalic [] = do
  MLine lparts <- mline
  return $ emph $ linePartsToInlines lparts
parseItalic args = return $
  emph $ mconcat $ intersperse B.space $ map linePartsToInlines args

parseAlternatingFonts :: PandocMonad m
                      => [Inlines -> Inlines]
                      -> [Arg]
                      -> ManParser m Inlines
parseAlternatingFonts constructors args = return $ mconcat $
  zipWith (\f arg -> f (linePartsToInlines arg)) (cycle constructors) args

lineInl :: PandocMonad m => ManParser m Inlines
lineInl = do
  (MLine fragments) <- mline
  return $ linePartsToInlines $ fragments

bareIP :: PandocMonad m => ManParser m ManToken
bareIP = msatisfy isBareIP where
  isBareIP (MMacro "IP" []) = True
  isBareIP _                = False

parseCodeBlock :: PandocMonad m => ManParser m Blocks
parseCodeBlock = try $ do
  optional bareIP -- some people indent their code
  mmacro "nf"
  toks <- many (mline <|> memptyLine)
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
  term <- parseInline
  inls <- parseInlines
  continuations <- mconcat <$> many continuation
  return $ (term, [para inls <> continuations])

parseDefinitionList :: PandocMonad m => ManParser m Blocks
parseDefinitionList = definitionList <$> many1 definitionListItem

parseLink :: PandocMonad m => [Arg] -> ManParser m Inlines
parseLink args = do
  contents <- mconcat <$> many lineInl
  mmacro "UE"
  let url = case args of
              [] -> ""
              (x:_) -> linePartsToString x
  return $ link url "" contents

parseEmailLink :: PandocMonad m => [Arg] -> ManParser m Inlines
parseEmailLink args = do
  contents <- mconcat <$> many lineInl
  mmacro "ME"
  let url = case args of
              [] -> ""
              (x:_) -> "mailto:" ++ linePartsToString x
  return $ link url "" contents

skipUnkownMacro :: PandocMonad m => ManParser m Blocks
skipUnkownMacro = do
  pos <- getPosition
  tok <- mmacroAny
  case tok of
    MMacro mkind _ -> do
      report $ SkippedContent ('.':mkind) pos
      return mempty
    _                 -> fail "the impossible happened"
