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
import Text.Parsec
import Text.Parsec.Char ()

data FontKind = Regular | Italic | Bold | ItalicBold deriving Show

data RoffState = RoffState { inCodeBlock :: Bool
                           , fontKind :: FontKind
                           } deriving Show

instance Default RoffState where
  def = RoffState {inCodeBlock = False, fontKind = Regular}

data ManState = ManState {pState :: ParserState, rState :: RoffState}

instance HasLogMessages ManState where
  addLogMessage lm mst  = mst {pState = addLogMessage lm (pState mst)}
  getLogMessages mst = getLogMessages $ pState mst

modifyRoffState :: PandocMonad m => (RoffState -> RoffState) -> ParsecT a ManState m ()
modifyRoffState f = do
  mst <- getState
  setState mst { rState = f $ rState mst }

type ManParser m = ParserT [Char] ManState m

parseMacro :: PandocMonad m => ManParser m Block
parseMacro = do
  char '.' <|> char '\''
  many space
  macroName <- many1 (letter <|> oneOf ['\\', '"'])
  args <- parseArgs
  let joinedArgs = concat $ intersperse " " args
  ManState { rState = rst } <- getState
  let toTextF transf = if inCodeBlock rst then [Code nullAttr joinedArgs] else transf [Str joinedArgs]
  let toText = return . Plain . toTextF
  let toBold       = toText (\s -> [Strong s])
  let toItalic     = toText (\s -> [Emph s])
  let toBoldItalic = toText (\s -> [Strong [Emph s]])
  
  case macroName of
    "\\\"" -> return Null -- comment
    "TH"   -> macroTitle (if null args then "" else head args) -- man-title
    "TP"   -> return Null -- tab-indented paragraph
    "PP"   -> return Null -- end of tab-indented paragraphs
    "nf"   -> macroCodeBlock True >> return Null
    "fi"   -> macroCodeBlock False >> return Null
    "B"    -> toBold
    "BR"   -> return $ macroBR joinedArgs (inCodeBlock rst)
    "BI"   -> toBoldItalic
    "IB"   -> toBoldItalic
    "I"    -> toItalic
    "IR"   -> toItalic
    "RI"   -> toItalic
    "SH"   -> return $ Header 2 nullAttr [Str joinedArgs]
    "sp"   -> return $ if inCodeBlock rst then Null else Plain [LineBreak]
    _      -> unkownMacro macroName

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

roffInline :: RoffState -> String -> [Inline]
roffInline rst str
  | null str && (not $ inCodeBlock rst) = []
  | inCodeBlock rst = [Code nullAttr str]
  | otherwise       = case fontKind rst of
    Regular    -> [Str str]
    Italic     -> [Emph [Str str]]
    Bold       -> [Strong [Str str]]
    ItalicBold -> [Emph [Strong [Str str]]]

parseLine :: PandocMonad m => ManParser m Block
parseLine = do
  parts <- parseLineParts
  newline
  return $ if null parts
    then Null
    else Plain parts
  where
    parseLineParts :: PandocMonad m => ManParser m [Inline]
    parseLineParts = do
      lnpart <- many $ noneOf "\n\\"
      ManState {rState = roffSt} <- getState
      let inls = roffInline roffSt lnpart
      others <- backSlash <|> return []
      return $ inls ++ others
    
    backSlash :: PandocMonad m => ManParser m [Inline]
    backSlash = do
      char '\\'
      esc <- choice [ char 'f' >> fEscape
                    , char '-' >> return (Just '-')
                    , char '%' >> return Nothing
                    , Just <$> noneOf "\n"
                    ]
      ManState {rState = roffSt} <- getState
      case esc of
        Just c -> let inls = roffInline roffSt [c]
                  in parseLineParts >>= (\oth -> return $ inls ++ oth)
        Nothing -> parseLineParts
      where
      
      fEscape :: PandocMonad m => ManParser m (Maybe Char)
      fEscape = choice [ char 'B' >> modifyRoffState (\rst -> rst {fontKind = Bold})
                       , char 'I' >> modifyRoffState (\rst -> rst {fontKind = Italic})
                       , (char 'P' <|> anyChar) >> modifyRoffState (\rst -> rst {fontKind = Regular})
                       ]
                >> return Nothing

finds :: (a -> Bool) -> [a] -> ([a], [a])
finds predic els = let matched = finds' els
  in (matched, drop (length matched) els) where
  finds' [] = []
  finds' (e:es) | predic e = e : finds' es
                | otherwise = []

-- | return (matched, notmatched, others)
findsBoth :: (a -> Bool) -> [a] -> ([a], [a], [a])
findsBoth predic els =
  let (matched, els')     = finds predic els
      (notmatched, els'') = finds (not . predic) els'
  in (matched, notmatched, els'')

createParas :: [Block] -> [Block]
createParas bs = inner bs [] where
  inner :: [Block] -> [Inline] -> [Block]
  inner [] inls = plainInlinesToPara inls
  inner (Plain einls : oth) inls = inner oth (inls ++ joinCode einls)
  inner (block : oth) inls = (plainInlinesToPara inls ++ [block]) ++ inner oth []

  joinCode :: [Inline] -> [Inline]
  joinCode inls =
    let (codes, notcodes) = finds isCode inls
        codeStr (Code _ s) = s
        codeStr _          = ""
        joined = Code nullAttr (concat $ codeStr <$> codes)
    in if null codes
         then notcodes
         else joined : notcodes

  plainInlinesToPara :: [Inline] -> [Block]
  plainInlinesToPara []   = []
  plainInlinesToPara inls =
    let (cds, ncds, oth) = findsBoth isCode inls
        codeToStr (Code _ s) = s
        codeToStr _ = ""
        cbs = if null cds
                then []
                else [CodeBlock nullAttr (intercalate "\n" $ codeToStr <$> cds)]
        paras = [Para (intersperse (Str " ") ncds)]
    in cbs ++ paras ++ plainInlinesToPara oth

  isCode (Code _ _) = True
  isCode _ = False

parseMan :: PandocMonad m => ManParser m Pandoc
parseMan = do
  blocks <- createParas <$> many (parseMacro <|> parseLine)
  parserst <- pState <$> getState
  return $ Pandoc (stateMeta parserst) blocks

-- | Read man (troff) from an input string and return a Pandoc document.
readMan :: PandocMonad m => ReaderOptions -> T.Text -> m Pandoc
readMan opts txt = do
  let state = ManState { pState = def{ stateOptions = opts }, rState = def}
  parsed <- readWithM parseMan state (T.unpack $ crFilter txt)
  case parsed of
    Right result -> return result
    Left e       -> throwError e
