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
module Text.Pandoc.Readers.Man where

import Control.Monad.Except (liftM2, throwError, guard)
import Text.Pandoc.Class (PandocMonad(..))
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Parsing hiding (enclosed)
import Text.Pandoc.Shared (crFilter)
import Text.Parsec
import Text.Parsec.Char
import Data.Text (Text)
import Data.Map (empty)
import qualified Data.Text as T


-- | Read man (troff) from an input string and return a Pandoc document.
readMan :: PandocMonad m
          => ReaderOptions
          -> Text
          -> m Pandoc
readMan opts s = do
  parsed <- readWithM parseMan def{ stateOptions = opts } (T.unpack s)
  case parsed of
    Right result -> return result
    Left e       -> throwError e

type ManParser m = ParserT [Char] ParserState m

comment :: PandocMonad m => ManParser m String
comment = do
  string ".\\\" "
  many anyChar

data Macro = Macro { macroName :: String
                   , macroArgs :: [String]
                   }

parseMacro :: PandocMonad m => ManParser m Block
parseMacro = do
  m <- macro
  return $ Plain (map Str $ (macroName m : macroArgs m))

macro :: PandocMonad m => ManParser m Macro
macro = do
  char '.' <|> char '\''
  many space
  name <- many1 letter
  --args <- many parseArg
  return $ Macro { macroName = name, macroArgs = [] }
  
  where

  parseArg :: PandocMonad m => ManParser m String
  parseArg = do
    many1 space
    plainArg
  
  quotedArg :: PandocMonad m => ManParser m String
  quotedArg = do
    char '"'
    val <- many1 quotedChar
    char '"'
    return val

  plainArg :: PandocMonad m => ManParser m String
  plainArg = do
    many1 $ noneOf " \t"
  
  quotedChar :: PandocMonad m => ManParser m Char
  quotedChar = do
        noneOf "\""
    <|> try (string "\"\"" >> return '"')

parseLine :: PandocMonad m => ManParser m Block
parseLine = do
  str <- many anyChar
  return $ Plain [Str str]

parseBlock :: PandocMonad m => ManParser m Block
parseBlock = do
  choice [ parseMacro
         , parseLine
         ]

parseMan :: PandocMonad m => ManParser m Pandoc
parseMan = do
  blocks <- parseBlock `sepBy` newline
  
  return $ Pandoc Meta{unMeta = empty} blocks