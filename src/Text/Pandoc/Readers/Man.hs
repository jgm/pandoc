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
import Control.Monad.Except (throwError)
import Data.Default (Default)
import Data.Functor.Identity (Identity)
import Data.Map (insert)
import Data.Maybe (isJust, fromMaybe)
import Data.List (intersperse, intercalate)
import qualified Data.Text as T

import Text.Pandoc.Class (PandocMonad(..))
import Text.Pandoc.Definition
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
               deriving Show

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

-- | Read man (troff) from an input string and return a Pandoc document.
readMan :: PandocMonad m => ReaderOptions -> T.Text -> m Pandoc
readMan opts txt = do
  let state = ManState { pState = def{ stateOptions = opts }, rState = def}
  parsed <- readWithM parseMan state (T.unpack $ crFilter txt)
  case parsed of
    Right result -> return result
    Left e       -> throwError e

--
-- String -> ManToken function
--

parseMan :: PandocMonad m => ManParser m Pandoc
parseMan = do
  tokens <- many (parseMacro <|> parseLine <|> parseEmptyLine)
  let blocks = []
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

  macroTitle :: PandocMonad m => String -> ManParser m Block
  macroTitle mantitle = do
    modifyState (changeTitle mantitle)
    if null mantitle
      then return Null
      else return $ Header 1 nullAttr [Str mantitle]
    where
    changeTitle title mst@ManState{ pState = pst} =
      let meta = stateMeta pst
          metaUp = Meta $ insert "title" (MetaString title) (unMeta meta)
      in
      mst { pState = pst {stateMeta = metaUp} }

  macroCodeBlock :: PandocMonad m => Bool -> ManParser m ()
  macroCodeBlock insideCB = modifyRoffState (\rst -> rst{inCodeBlock = insideCB}) >> return ()

  macroBR :: String -> Bool -> Block
  macroBR txt inCode | inCode    = Plain [Code nullAttr txt]
                     | otherwise = fromMaybe (Plain [Strong [Str txt]]) (linkToMan txt)

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

    
  unkownMacro :: PandocMonad m => String -> ManParser m Block
  unkownMacro mname = do
    pos <- getPosition
    logMessage $ SkippedContent ("Unknown macro: " ++ mname) pos
    return Null
   
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
    modifyRoffState (\r -> RoffState {fontKind = font})
    return $ EFont font

parseLine :: PandocMonad m => ManParser m ManToken
parseLine = do
  lnparts <- many1 (esc <|> linePart)
  return $ MLine lnparts
  where

  esc :: PandocMonad m => ManParser m (String, FontKind)
  esc = do
    someesc <- escapeParser
    font <- currentFont
    let rv = case someesc of
               EChar c -> ([c], font)
               _ -> ("", font)
    return rv

  linePart :: PandocMonad m => ManParser m (String, FontKind)
  linePart = do
    lnpart <- many1 $ noneOf "\n\\"
    font <- currentFont
    return (lnpart, font)
  
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
msatisfy pred = tokenPrim show nextPos testTok
  where
    posFromTok (pos,t)  = pos
    testTok t     = if pred t then Just t else Nothing
    nextPos pos x xs  = updatePosString pos (show x)

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

mmacro :: PandocMonad m => ManCompiler m ManToken
mmacro = msatisfy isMMacro where
  isMMacro (MMacro _ _) = True
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

compileHeader :: PandocMonad m => ManCompiler m Block
compileHeader = undefined --do

