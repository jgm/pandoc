{-
Copyright (C) 2006-2010 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.CharacterReferences
   Copyright   : Copyright (C) 2006-2010 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Functions for parsing character references.
-}
module Text.Pandoc.CharacterReferences (
                     characterReference,
                     decodeCharacterReferences,
                    ) where
import Text.ParserCombinators.Parsec
import Text.HTML.TagSoup.Entity ( lookupNamedEntity, lookupNumericEntity )
import Data.Maybe ( fromMaybe )

-- | Parse character entity.
characterReference :: GenParser Char st Char
characterReference = try $ do
  char '&'
  character <- numRef <|> entity
  char ';'
  return character  

numRef :: GenParser Char st Char
numRef = do
  char '#'
  num <- hexNum <|> decNum
  return $ fromMaybe '?' $ lookupNumericEntity num

hexNum :: GenParser Char st [Char]
hexNum = do
  x <- oneOf "Xx"
  num <- many1 hexDigit
  return (x:num)

decNum :: GenParser Char st [Char]
decNum = many1 digit

entity :: GenParser Char st Char
entity = do
  body <- many1 alphaNum
  return $ fromMaybe '?' $ lookupNamedEntity body

-- | Convert entities in a string to characters.
decodeCharacterReferences :: String -> String
decodeCharacterReferences str = 
  case parse (many (characterReference <|> anyChar)) str str of
	Left err        -> error $ "\nError: " ++ show err
	Right result    -> result

