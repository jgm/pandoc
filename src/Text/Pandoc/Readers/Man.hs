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
module Text.Pandoc.Readers.Man (readMan, testFile) where

import Prelude
import Control.Monad (liftM)
import Control.Monad.Except (throwError)
import Data.Default (Default)
import Data.Map (insert)
import Data.Maybe (isJust, catMaybes)
import Data.List (intersperse, intercalate)
import qualified Data.Text as T

import Text.Pandoc.Class (PandocMonad(..), runPure, runIOorExplode)
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError (PandocParsecError))
import Text.Pandoc.Logging (LogMessage(..))
import Text.Pandoc.Options
import Text.Pandoc.Parsing
import Text.Pandoc.Shared (crFilter)
import Text.Parsec hiding (tokenPrim)
import Text.Parsec.Char ()
import Text.Parsec.Pos (updatePosString)

--
-- Data Types
--

data FontKind = Regular | Italic | Bold | ItalicBold deriving Show

data MacroKind = KTitle
               | KCodeBlStart
               | KCodeBlEnd
               | KTab
               | KTabEnd
               deriving (Show, Eq)

data ManToken = MStr String FontKind
              | MLine [(String, FontKind)]
              | MLink String Target
              | MEmptyLine
              | MHeader Integer String
              | MMacro MacroKind [String]
              | MUnknownMacro String [String]
              | MComment String
              deriving Show

data EscapeThing = EFont FontKind
                 | EChar Char
                 | ENothing
                 deriving Show

data RoffState = RoffState { inCodeBlock :: Bool
                           , fontKind :: FontKind
                           } deriving Show

instance Default RoffState where
  def = RoffState {inCodeBlock = False, fontKind = Regular}

data ManState = ManState {pState :: ParserState, rState :: RoffState}

type ManParser m = ParserT [Char] ManState m
type ManCompiler m = ParserT [ManToken] ManState m

instance HasLogMessages ManState where
  addLogMessage lm mst  = mst {pState = addLogMessage lm (pState mst)}
  getLogMessages mst = getLogMessages $ pState mst

----
testStrr :: [Char] -> Either PandocError Pandoc
testStrr s = runPure $ readMan def (T.pack s)

printPandoc :: Pandoc -> [Char]
printPandoc (Pandoc m content) =
  let ttl = "Pandoc: " ++ (show $ unMeta m)
      cnt = intercalate "\n" $ map show content
  in ttl ++ "\n" ++ cnt

strrepr :: Either PandocError Pandoc -> [Char]
strrepr obj = case obj of
  Right x -> printPandoc x
  Left y -> show y

testFile :: FilePath -> IO ()
testFile fname = do
  cont <- readFile fname
  pand <- runIOorExplode $ readMan def (T.pack cont)
  putStrLn $ printPandoc pand
----


-- | Read man (troff) from an input string and return a Pandoc document.
readMan :: PandocMonad m => ReaderOptions -> T.Text -> m Pandoc
readMan opts txt = do
  let state = ManState { pState = def{ stateOptions = opts }, rState = def}
  eithertokens <- readWithM parseMan state (T.unpack $ crFilter txt)
  case eithertokens of
    Right tokenz -> do
      eitherdoc <- readWithMTokens compileMan state tokenz
      case eitherdoc of
        Right doc -> return doc
        Left e -> throwError e
    Left e       -> throwError e

  where

  readWithMTokens :: PandocMonad m
          => ParserT [ManToken] ManState m a  -- ^ parser
          -> ManState                         -- ^ initial state
          -> [ManToken]                       -- ^ input
          -> m (Either PandocError a)
  readWithMTokens parser state input =
    mapLeft (PandocParsecError . concat $ show <$> input) `liftM` runParserT parser state "source" input

  mapLeft :: (a -> c) -> Either a b -> Either c b
  mapLeft f (Left x) = Left $ f x
  mapLeft _ (Right r) = Right r

--
-- String -> ManToken function
--

parseMan :: PandocMonad m => ManParser m [ManToken]
parseMan = many (parseMacro <|> parseLine <|> parseEmptyLine)

compileMan :: PandocMonad m => ManCompiler m Pandoc
compileMan = do
  let compilers = [compileTitle, compilePara, compileSkippedContent]
  blocks <- many $ choice compilers
  parserst <- pState <$> getState
  return $ Pandoc (stateMeta parserst) blocks

modifyRoffState :: PandocMonad m => (RoffState -> RoffState) -> ParsecT a ManState m ()
modifyRoffState f = do
  mst <- getState
  setState mst { rState = f $ rState mst }

parseMacro :: PandocMonad m => ManParser m ManToken
parseMacro = do
  char '.' <|> char '\''
  many space
  macroName <- many1 (letter <|> oneOf ['\\', '"'])
  args <- parseArgs
  let joinedArgs = concat $ intersperse " " args

  let tok = case macroName of
              x | x `elem` ["\\\"", "\\#"] -> MComment joinedArgs
              "TH"   -> MMacro KTitle args
              "TP"   -> MMacro KTab []
              "PP"   -> MMacro KTabEnd []
              "nf"   -> MMacro KCodeBlStart []
              "fi"   -> MMacro KCodeBlEnd []
              x | x `elem` ["B", "BR"]    -> MStr joinedArgs Bold -- "BR" is often used as a link to another man
              x | x `elem` ["BI", "IB"] -> MStr joinedArgs ItalicBold
              x | x `elem` ["I", "IR", "RI"]  -> MStr joinedArgs Italic
              "SH"   -> MHeader 2 joinedArgs
              "sp"   -> MEmptyLine
              _      -> MUnknownMacro macroName args
  return tok

  where

  linkToMan :: String -> Maybe Block
  linkToMan txt = case runParser linkParser () "" txt of
      Right lnk -> Just $ Plain [lnk]
      Left _ -> Nothing
    where
    linkParser :: Parsec String () Inline
    linkParser = do
      mpage <- many1 alphaNum
      space
      char '('
      mansect <- digit
      char ')'
      -- assuming man pages are generated from Linux-like repository
      let manurl pagename section = "../"++section++"/"++pagename++"."++section
      return $ Link nullAttr [Str txt] (manurl mpage [mansect], mpage)
   
  parseArgs :: PandocMonad m => ManParser m [String]
  parseArgs = do
    eolOpt <- optionMaybe $ char '\n'
    if isJust eolOpt
      then return []
      else do
        many1 space
        arg <- try quotedArg <|> plainArg
        otherargs <- parseArgs
        return $ arg : otherargs

    where

    plainArg :: PandocMonad m => ManParser m String
    plainArg = many1 $ noneOf " \t\n"

    quotedArg :: PandocMonad m => ManParser m String
    quotedArg = do
      char '"'
      val <- many1 quotedChar
      char '"'
      return val

    quotedChar :: PandocMonad m => ManParser m Char
    quotedChar = noneOf "\"\n" <|> try (string "\"\"" >> return '"')

escapeParser :: PandocMonad m => ManParser m EscapeThing
escapeParser = do
  char '\\'
  choice [escChar, escFont]
  where

  escChar :: PandocMonad m => ManParser m EscapeThing
  escChar = choice [ char '-' >> return (EChar '-')
                  , oneOf ['%', '{', '}'] >> return ENothing
                  ]

  escFont :: PandocMonad m => ManParser m EscapeThing
  escFont = do
    char 'f'
    font <- choice [ char 'B' >> return Bold
          , char 'I' >> return Italic
          , (char 'P' <|> anyChar) >> return Regular
          , char '(' >> anyChar >> anyChar >> return Regular
          , string "[]"  >> return Regular
          , char '[' >> many1 letter >> char ']'  >> return Regular
          ]
    modifyRoffState (\r -> r {fontKind = font})
    return $ EFont font

parseLine :: PandocMonad m => ManParser m ManToken
parseLine = do
  lnparts <- many1 (esc <|> linePart)
  newline
  return $ MLine $ catMaybes lnparts
  where

  esc :: PandocMonad m => ManParser m (Maybe (String, FontKind))
  esc = do
    someesc <- escapeParser
    font <- currentFont
    let rv = case someesc of
               EChar c -> Just ([c], font)
               _ -> Nothing
    return rv

  linePart :: PandocMonad m => ManParser m (Maybe (String, FontKind))
  linePart = do
    lnpart <- many1 $ noneOf "\n\\"
    font <- currentFont
    return $ Just (lnpart, font)
  
  currentFont :: PandocMonad m => ManParser m FontKind
  currentFont = do
    RoffState {fontKind = fk} <- rState <$> getState
    return fk

    
parseEmptyLine :: PandocMonad m => ManParser m ManToken
parseEmptyLine = char '\n' >> return MEmptyLine

--
-- ManToken parsec functions
--

msatisfy :: (Show t, Stream s m t) => (t -> Bool) -> ParserT s st m t
msatisfy predic = tokenPrim show nextPos testTok
  where
    testTok t     = if predic t then Just t else Nothing
    nextPos pos x _xs  = updatePosString pos (show x)

mstr :: PandocMonad m => ManCompiler m ManToken
mstr = msatisfy isMStr where
  isMStr (MStr _ _) = True
  isMStr _ = False

mline :: PandocMonad m => ManCompiler m ManToken
mline = msatisfy isMLine where
  isMLine (MLine _) = True
  isMLine _ = False

mlink :: PandocMonad m => ManCompiler m ManToken
mlink = msatisfy isMLink where
  isMLink (MLink _ _) = True
  isMLink _ = False

memplyLine :: PandocMonad m => ManCompiler m ManToken
memplyLine = msatisfy isMEmptyLine where
  isMEmptyLine MEmptyLine = True
  isMEmptyLine _ = False

mheader :: PandocMonad m => ManCompiler m ManToken
mheader = msatisfy isMHeader where
  isMHeader (MHeader _ _) = True
  isMHeader _ = False

mmacro :: PandocMonad m => MacroKind -> ManCompiler m ManToken
mmacro mk = msatisfy isMMacro where
  isMMacro (MMacro mk' _) | mk == mk' = True
                          | otherwise = False
  isMMacro _ = False

munknownMacro :: PandocMonad m => ManCompiler m ManToken
munknownMacro = msatisfy isMUnknownMacro where
  isMUnknownMacro (MUnknownMacro _ _) = True
  isMUnknownMacro _ = False

mcomment :: PandocMonad m => ManCompiler m ManToken
mcomment = msatisfy isMComment where
  isMComment (MComment _) = True
  isMComment _ = False

--
-- ManToken -> Block functions
--

compileTitle :: PandocMonad m => ManCompiler m Block
compileTitle = do
  (MMacro _ args) <- mmacro KTitle
  if null args
    then return Null
    else do
         let mantitle = head args
         modifyState (changeTitle mantitle)
         return $ Header 1 nullAttr [Str mantitle]
  where
  changeTitle title mst@ManState{ pState = pst} =
    let meta = stateMeta pst
        metaUp = Meta $ insert "title" (MetaString title) (unMeta meta)
    in
    mst { pState = pst {stateMeta = metaUp} }
      
compileSkippedContent :: PandocMonad m => ManCompiler m Block
compileSkippedContent = do
  tok <- munknownMacro <|> mcomment <|> memplyLine
  onToken tok
  return Null

  where

  onToken :: PandocMonad m => ManToken -> ManCompiler m ()
  onToken (MUnknownMacro mname _) = do
    pos <- getPosition
    logMessage $ SkippedContent ("Unknown macro: " ++ mname) pos
  onToken _ = return ()

strToInline :: String -> FontKind -> Inline
strToInline s Regular = Str s
strToInline s Italic = Emph [Str s]
strToInline s Bold = Strong [Str s]
strToInline s ItalicBold = Strong [Emph [Str s]]

compilePara :: PandocMonad m => ManCompiler m Block
compilePara = do
  inls <- many1 (strInl <|> lineInl)
  let withspaces = intersperse [Str " "] inls
  return $ Para (concat withspaces)

  where

  strInl :: PandocMonad m => ManCompiler m [Inline]
  strInl = do
    (MStr str fk) <- mstr
    return [strToInline str fk]

  lineInl :: PandocMonad m => ManCompiler m [Inline]
  lineInl = do
    (MLine fragments) <- mline
    return $ fmap (\(s,f) -> strToInline s f) fragments
