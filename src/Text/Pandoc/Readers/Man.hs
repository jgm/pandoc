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
   Copyright   : Copyright (C) 2018 Yan Pashkovsky
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
import Data.Char (isHexDigit, chr)
import Data.Default (Default)
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import Data.Set (Set, singleton)
import qualified Data.Set as S (fromList, toList)
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

type RoffStr = (String, Font)

-- TODO parse tables (see man tbl)
data ManToken = MStr RoffStr
              | MLine [RoffStr]
              | MMaybeLink String
              | MEmptyLine
              | MMacro MacroKind [RoffStr]
              | MComment String
              deriving Show

data RoffState = RoffState { fontKind :: Font
                           } deriving Show

instance Default RoffState where
  def = RoffState {fontKind = singleton Regular}

type ManLexer m = ParserT [Char] RoffState m
type ManParser m = ParserT [ManToken] ParserState m


-- | Read man (troff) from an input string and return a Pandoc document.
readMan :: PandocMonad m => ReaderOptions -> T.Text -> m Pandoc
readMan opts txt = do
  eithertokens <- readWithM lexMan def (T.unpack $ crFilter txt)
  case eithertokens of
    Left e       -> throwError e
    Right tokenz -> do
      let state = def {stateOptions = opts} :: ParserState
      eitherdoc <- readWithMTokens parseMan state tokenz
      either throwError return eitherdoc

  where

  readWithMTokens :: PandocMonad m
          => ParserT [ManToken] ParserState m a  -- ^ parser
          -> ParserState                         -- ^ initial state
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

lexMan :: PandocMonad m => ManLexer m [ManToken]
lexMan = many (lexComment <|> lexMacro <|> lexLine <|> lexEmptyLine)

parseMan :: PandocMonad m => ManParser m Pandoc
parseMan = do
  bs <- many parseBlock
  meta <- stateMeta <$> getState
  let (Pandoc _ blocks) = doc $ mconcat bs
  return $ Pandoc meta blocks

parseBlock :: PandocMonad m => ManParser m Blocks
parseBlock = choice [ parseList
                    , parseDefinitionList
                    , parseTitle
                    , parsePara
                    , parseSkippedContent
                    , parseCodeBlock
                    , parseHeader
                    , parseSkipMacro
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
      Nothing -> escUnknown ('(':cs)

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
       Nothing -> mzero
       Just c  -> return c

  escStar = do
    char '*'
    choice
      [ ("\xae" <$ char 'R')
      , ("" <$ char 'S') -- switch back to default font size
      , ("\x201c" <$ try (string "(lq"))
      , ("\x201d" <$ try (string "(rq"))
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
      _   -> escUnknown [c]

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

  escUnknown :: PandocMonad m => String -> ManLexer m String
  escUnknown s = do
    pos <- getPosition
    report $ SkippedContent ("Unknown escape sequence " ++ s) pos
    return mempty

currentFont :: PandocMonad m => ManLexer m Font
currentFont = fontKind <$> getState

-- separate function from lexMacro since real man files sometimes do not follow the rules
lexComment :: PandocMonad m => ManLexer m ManToken
lexComment = do
  try $ string ".\\\""
  many Parsec.space
  body <- many $ noneOf "\n"
  char '\n'
  return $ MComment body

lexMacro :: PandocMonad m => ManLexer m ManToken
lexMacro = do
  char '.' <|> char '\''
  many spacetab
  macroName <- many1 (letter <|> oneOf ['\\', '"', '&'])
  args <- lexArgs
  let joinedArgs = unwords $ fst <$> args

      tok = case macroName of
              x | x `elem` ["\\\"", "\\#"] -> MComment joinedArgs
              "B"    -> MStr (joinedArgs, singleton Bold)
              "BR"   -> MMaybeLink joinedArgs
              x | x `elem` ["BI", "IB"] -> MStr (joinedArgs, S.fromList [Italic, Bold])
              x | x `elem` ["I", "IR", "RI"]  -> MStr (joinedArgs, singleton Italic)
              x | x `elem` [ "P", "PP", "LP", "sp"] -> MEmptyLine
              _      -> MMacro macroName args
  return tok

  where

  -- TODO better would be [[RoffStr]], since one arg may have different fonts
  lexArgs :: PandocMonad m => ManLexer m [RoffStr]
  lexArgs = do
    args <- many $ try oneArg
    many spacetab
    eofline
    return args

    where

    oneArg :: PandocMonad m => ManLexer m RoffStr
    oneArg = do
      many1 spacetab
      many $ try $ string "\\\n"
      try quotedArg <|> plainArg -- try, because there are some erroneous files, e.g. linux/bpf.2

    plainArg :: PandocMonad m => ManLexer m RoffStr
    plainArg = do
      indents <- many spacetab
      arg <- many1 $ escapeLexer <|> many1 (noneOf " \t\n\\")
      f <- currentFont
      return (indents ++ mconcat arg, f)

    quotedArg :: PandocMonad m => ManLexer m RoffStr
    quotedArg = do
      char '"'
      val <- mconcat <$> many quotedChar
      char '"'
      val2 <- mconcat <$> many (escapeLexer <|> many1 (noneOf " \t\n"))
      f <- currentFont
      return (val ++ val2, f)

    quotedChar :: PandocMonad m => ManLexer m String
    quotedChar = escapeLexer
              <|> many1 (noneOf "\"\n\\")
              <|> try (string "\"\"" >> return "\"")

lexLine :: PandocMonad m => ManLexer m ManToken
lexLine = do
  lnparts <- many1 (esc <|> linePart)
  eofline
  return $ MLine $ catMaybes lnparts
  where

  esc :: PandocMonad m => ManLexer m (Maybe (String, Font))
  esc = do
    someesc <- escapeLexer
    font <- currentFont
    return $ if null someesc
                then Nothing
                else Just (someesc, font)

  linePart :: PandocMonad m => ManLexer m (Maybe (String, Font))
  linePart = do
    lnpart <- many1 $ noneOf "\n\\"
    font <- currentFont
    return $ Just (lnpart, font)


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

mstr :: PandocMonad m => ManParser m ManToken
mstr = msatisfy isMStr where
  isMStr (MStr _) = True
  isMStr _ = False

mline :: PandocMonad m => ManParser m ManToken
mline = msatisfy isMLine where
  isMLine (MLine _) = True
  isMLine _ = False

mmaybeLink :: PandocMonad m => ManParser m ManToken
mmaybeLink = msatisfy isMMaybeLink where
  isMMaybeLink (MMaybeLink _) = True
  isMMaybeLink _ = False

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
  isMComment (MComment _) = True
  isMComment _ = False

--
-- ManToken -> Block functions
--

parseTitle :: PandocMonad m => ManParser m Blocks
parseTitle = do
  (MMacro _ args) <- mmacro "TH"
  let adjustMeta =
       case map fst args of
         (x:y:z:_) -> setMeta "title" x . setMeta "section" y . setMeta "date" z
         [x,y]     -> setMeta "title" x . setMeta "section" y
         [x]       -> setMeta "title" x
         []        -> id
  modifyState $ \st -> st{ stateMeta = adjustMeta $ stateMeta st }
  return mempty

parseSkippedContent :: PandocMonad m => ManParser m Blocks
parseSkippedContent = mempty <$ (mcomment <|> memptyLine)

strToInlines :: RoffStr -> Inlines
strToInlines (s, fonts) = inner $ S.toList fonts where
  inner :: [FontKind] -> Inlines
  inner [] = text s
  inner (Bold:fs) = strong $ inner fs
  inner (Italic:fs) = emph $ inner fs

  -- Monospace goes after Bold and Italic in ordered set
  inner (Monospace:_) = code s
  inner (Regular:fs) = inner fs

parsePara :: PandocMonad m => ManParser m Blocks
parsePara = para . trimInlines <$> parseInlines

parseInlines :: PandocMonad m => ManParser m Inlines
parseInlines = do
  inls <- many1 (strInl <|> lineInl <|> linkInl <|> comment)
  let withspaces = intersperse B.space inls
  return $ mconcat withspaces

strInl :: PandocMonad m => ManParser m Inlines
strInl = do
  (MStr rstr) <- mstr
  return $ strToInlines rstr

lineInl :: PandocMonad m => ManParser m Inlines
lineInl = do
  (MLine fragments) <- mline
  return $ mconcat $ strToInlines <$> fragments

linkInl :: PandocMonad m => ManParser m Inlines
linkInl = do
  (MMaybeLink txt) <- mmaybeLink
  let inls = case runParser linkParser () "" txt of
                Right lnk -> lnk
                Left _ -> strong $ text txt
  return inls

    where

    -- assuming man pages are generated from Linux-like repository
    linkParser :: Parsec String () Inlines
    linkParser = do
      mpage <- many1 (alphaNum <|> char '_')
      spacetab
      char '('
      mansect <- digit
      char ')'
      other <- many anyChar
      let manurl pagename section = "../"++section++"/"++pagename++"."++section
          lnkInls = link (manurl mpage [mansect]) mpage (strong $ str mpage)
      return $ lnkInls <> strong (str (" ("++[mansect] ++ ")") <> text other)

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
  toks <- many (mstr <|> mline <|> mmaybeLink <|> memptyLine <|> mcomment)
  mmacro "fi"
  return $ codeBlock (removeFinalNewline $
                      intercalate "\n" . catMaybes $
                      extractText <$> toks)

  where

  removeFinalNewline [] = []
  removeFinalNewline xs = if last xs == '\n' then init xs else xs
  extractText :: ManToken -> Maybe String
  extractText (MStr (s, _)) = Just s
  extractText (MLine ss) = Just . concat $ map fst ss -- TODO maybe unwords?
  extractText (MMaybeLink s) = Just s
  extractText MEmptyLine = Just "" -- string are intercalated with '\n', this prevents double '\n'
  extractText _ = Nothing

parseHeader :: PandocMonad m => ManParser m Blocks
parseHeader = do
  MMacro name args <- mmacro "SH" <|> mmacro "SS"
  contents <- if null args
                 then do
                   strInl <|> lineInl
                 else do
                   return $
                     mconcat $ intersperse B.space $ map strToInlines args
  let lvl = if name == "SH" then 1 else 2
  return $ header lvl contents

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
    []          -> mzero
    ((cs,_):_)  -> do
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
  term <- strInl <|> lineInl
  inls <- parseInlines
  continuations <- mconcat <$> many continuation
  return $ (term, [para inls <> continuations])

parseDefinitionList :: PandocMonad m => ManParser m Blocks
parseDefinitionList = definitionList <$> many1 definitionListItem

-- In case of weird man file it will be parsed succesfully
parseSkipMacro :: PandocMonad m => ManParser m Blocks
parseSkipMacro = do
  pos <- getPosition
  tok <- mmacroAny
  case tok of
    MMacro mkind _ -> report $ SkippedContent ('.':mkind) pos
    _              -> return ()  -- shouldn't happen
  return mempty
