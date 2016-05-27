{-
Copyright (C) 2014-2016 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

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
   Module      : Text.Pandoc.Readers.Org.Options
   Copyright   : Copyright (C) 2014-2016 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Org-mode parsing utilities.

Most functions are simply re-exports from @Text.Pandoc.Parsing@, some
functions are adapted to Org-mode specific functionality.
-}
module Text.Pandoc.Readers.Org.Parsing
  ( OrgParser
  , anyLine
  , blanklines
  , newline
  , parseFromString
  , skipSpaces1
  , inList
  , withContext
  , updateLastForbiddenCharPos
  , updateLastPreCharPos
  , orgArgKey
  , orgArgWord
  , orgArgWordChar
  -- * Re-exports from Text.Pandoc.Parser
  , ParserContext (..)
  , many1Till
  , notFollowedBy'
  , spaceChar
  , nonspaceChar
  , skipSpaces
  , blankline
  , enclosed
  , stringAnyCase
  , charsInBalanced
  , uri
  , withRaw
  , readWithM
  , guardEnabled
  , updateLastStrPos
  , notAfterString
  , ParserState (..)
  , registerHeader
  , QuoteContext (..)
  , singleQuoteStart
  , singleQuoteEnd
  , doubleQuoteStart
  , doubleQuoteEnd
  , dash
  , ellipses
  , citeKey
  -- * Re-exports from Text.Pandoc.Parsec
  , runParser
  , getInput
  , char
  , letter
  , digit
  , alphaNum
  , skipMany1
  , spaces
  , anyChar
  , satisfy
  , string
  , count
  , eof
  , noneOf
  , oneOf
  , lookAhead
  , notFollowedBy
  , many
  , many1
  , manyTill
  , (<|>)
  , (<?>)
  , choice
  , try
  , sepBy
  , sepBy1
  , option
  , optional
  , optionMaybe
  , getState
  , updateState
  , SourcePos
  , getPosition
  ) where

import           Text.Pandoc.Readers.Org.ParserState

import qualified Text.Pandoc.Parsing as P
import           Text.Pandoc.Parsing hiding ( anyLine, blanklines, newline
                                            , parseFromString )

import           Control.Monad ( guard )
import           Control.Monad.Reader ( Reader )

-- | The parser used to read org files.
type OrgParser = ParserT [Char] OrgParserState (Reader OrgParserLocal)

--
-- Adaptions and specializations of parsing utilities
--

-- | Parse any line of text
anyLine :: OrgParser String
anyLine =
  P.anyLine
    <* updateLastPreCharPos
    <* updateLastForbiddenCharPos

-- The version Text.Pandoc.Parsing cannot be used, as we need additional parts
-- of the state saved and restored.
parseFromString :: OrgParser a -> String -> OrgParser a
parseFromString parser str' = do
  oldLastPreCharPos <- orgStateLastPreCharPos <$> getState
  updateState $ \s -> s{ orgStateLastPreCharPos = Nothing }
  result <- P.parseFromString parser str'
  updateState $ \s -> s{ orgStateLastPreCharPos = oldLastPreCharPos }
  return result

-- | Skip one or more tab or space characters.
skipSpaces1 :: OrgParser ()
skipSpaces1 = skipMany1 spaceChar

-- | Like @Text.Parsec.Char.newline@, but causes additional state changes.
newline :: OrgParser Char
newline =
  P.newline
       <* updateLastPreCharPos
       <* updateLastForbiddenCharPos

-- | Like @Text.Parsec.Char.blanklines@, but causes additional state changes.
blanklines :: OrgParser [Char]
blanklines =
  P.blanklines
       <* updateLastPreCharPos
       <* updateLastForbiddenCharPos

-- | Succeeds when we're in list context.
inList :: OrgParser ()
inList = do
  ctx <- orgStateParserContext <$> getState
  guard (ctx == ListItemState)

-- | Parse in different context
withContext :: ParserContext -- ^ New parser context
            -> OrgParser a   -- ^ Parser to run in that context
            -> OrgParser a
withContext context parser = do
  oldContext <- orgStateParserContext <$> getState
  updateState $ \s -> s{ orgStateParserContext = context }
  result <- parser
  updateState $ \s -> s{ orgStateParserContext = oldContext }
  return result

--
-- Parser state update functions
--

-- | Set the current position as the last position at which a forbidden char
-- was found (i.e. a character which is not allowed at the inner border of
-- markup).
updateLastForbiddenCharPos :: OrgParser ()
updateLastForbiddenCharPos = getPosition >>= \p ->
  updateState $ \s -> s{ orgStateLastForbiddenCharPos = Just p}

-- | Set the current parser position as the position at which a character was
-- seen which allows inline markup to follow.
updateLastPreCharPos :: OrgParser ()
updateLastPreCharPos = getPosition >>= \p ->
  updateState $ \s -> s{ orgStateLastPreCharPos = Just p}

orgArgKey :: OrgParser String
orgArgKey = try $
  skipSpaces *> char ':'
             *> many1 orgArgWordChar

orgArgWord :: OrgParser String
orgArgWord = many1 orgArgWordChar

orgArgWordChar :: OrgParser Char
orgArgWordChar = alphaNum <|> oneOf "-_"
