{- |
   Module      : Text.Pandoc.Readers.Org.Parsing
   Copyright   : Copyright (C) 2014-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Org-mode parsing utilities.

Most functions are simply re-exports from @Text.Pandoc.Parsing@, some
functions are adapted to Org-mode specific functionality.
-}
module Text.Pandoc.Readers.Org.Parsing
  ( OrgParser
  , anyLine
  , anyLineNewline
  , indentWith
  , blanklines
  , newline
  , parseFromString
  , skipSpaces1
  , inList
  , withContext
  , getExportSetting
  , updateLastForbiddenCharPos
  , updateLastPreCharPos
  , orgArgKey
  , orgArgWord
  , orgArgWordChar
  , orgTagWord
  , orgTagWordChar
  -- * Re-exports from Text.Pandoc.Parser
  , ParserContext (..)
  , textStr
  , countChar
  , manyChar
  , many1Char
  , manyTillChar
  , many1Till
  , many1TillChar
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
  , gridTableWith
  , insertIncludedFileF
  -- * Re-exports from Text.Pandoc.Parsec
  , runParser
  , runParserT
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
  , sepEndBy1
  , endBy1
  , option
  , optional
  , optionMaybe
  , getState
  , updateState
  , SourcePos
  , getPosition
  ) where

import Data.Text (Text)
import Text.Pandoc.Readers.Org.ParserState

import Text.Pandoc.Parsing hiding (F, anyLine, blanklines, newline,
                            parseFromString)
import qualified Text.Pandoc.Parsing as P

import Control.Monad (guard)
import Control.Monad.Reader (ReaderT)

-- | The parser used to read org files.
type OrgParser m = ParserT Text OrgParserState (ReaderT OrgParserLocal m)

--
-- Adaptions and specializations of parsing utilities
--

-- | Parse any line of text
anyLine :: Monad m => OrgParser m Text
anyLine =
  P.anyLine
    <* updateLastPreCharPos
    <* updateLastForbiddenCharPos

-- | Like @'Text.Pandoc.Parsing'@, but resets the position of the last character
-- allowed before emphasised text.
parseFromString :: Monad m => OrgParser m a -> Text -> OrgParser m a
parseFromString parser str' = do
  updateState $ \s -> s{ orgStateLastPreCharPos = Nothing }
  result <- P.parseFromString parser str'
  updateState $ \s -> s { orgStateLastPreCharPos = Nothing }
  return result

-- | Skip one or more tab or space characters.
skipSpaces1 :: Monad m => OrgParser m ()
skipSpaces1 = skipMany1 spaceChar

-- | Like @Text.Parsec.Char.newline@, but causes additional state changes.
newline :: Monad m => OrgParser m Char
newline =
  P.newline
       <* updateLastPreCharPos
       <* updateLastForbiddenCharPos

-- | Like @Text.Parsec.Char.blanklines@, but causes additional state changes.
blanklines :: Monad m => OrgParser m Text
blanklines =
  P.blanklines
       <* updateLastPreCharPos
       <* updateLastForbiddenCharPos

-- | Succeeds when we're in list context.
inList :: Monad m => OrgParser m ()
inList = do
  ctx <- orgStateParserContext <$> getState
  guard (ctx == ListItemState)

-- | Parse in different context
withContext :: Monad m
            => ParserContext -- ^ New parser context
            -> OrgParser m a   -- ^ Parser to run in that context
            -> OrgParser m a
withContext context parser = do
  oldContext <- orgStateParserContext <$> getState
  updateState $ \s -> s{ orgStateParserContext = context }
  result <- parser
  updateState $ \s -> s{ orgStateParserContext = oldContext }
  return result

--
-- Parser state functions
--

-- | Get an export setting.
getExportSetting :: Monad m =>  (ExportSettings -> a) -> OrgParser m a
getExportSetting s = s . orgStateExportSettings <$> getState

-- | Set the current position as the last position at which a forbidden char
-- was found (i.e. a character which is not allowed at the inner border of
-- markup).
updateLastForbiddenCharPos :: Monad m => OrgParser m ()
updateLastForbiddenCharPos = getPosition >>= \p ->
  updateState $ \s -> s{ orgStateLastForbiddenCharPos = Just p}

-- | Set the current parser position as the position at which a character was
-- seen which allows inline markup to follow.
updateLastPreCharPos :: Monad m => OrgParser m ()
updateLastPreCharPos = getPosition >>= \p ->
  updateState $ \s -> s{ orgStateLastPreCharPos = Just p}

--
-- Org key-value parsing
--

-- | Read the key of a plist style key-value list.
orgArgKey :: Monad m => OrgParser m Text
orgArgKey = try $
  skipSpaces *> char ':'
             *> many1Char orgArgWordChar

-- | Read the value of a plist style key-value list.
orgArgWord :: Monad m => OrgParser m Text
orgArgWord = many1Char orgArgWordChar

-- | Chars treated as part of a word in plists.
orgArgWordChar :: Monad m => OrgParser m Char
orgArgWordChar = alphaNum <|> oneOf "-_"

orgTagWord :: Monad m => OrgParser m Text
orgTagWord = many1Char orgTagWordChar

orgTagWordChar :: Monad m => OrgParser m Char
orgTagWordChar = alphaNum <|> oneOf "@%#_"
