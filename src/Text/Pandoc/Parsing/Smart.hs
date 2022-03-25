{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{- |
Module      : Text.Pandoc.Parsing.Smart
Copyright   : © 2006-2022 John MacFarlane
License     : GPL-2.0-or-later
Maintainer  : John MacFarlane <jgm@berkeley.edu>

Smart parsing of quotes, dashes, and other character combinations.
-}

module Text.Pandoc.Parsing.Smart
  ( apostrophe
  , dash
  , doubleCloseQuote
  , doubleQuoteEnd
  , doubleQuoteStart
  , doubleQuoted
  , ellipses
  , singleQuoteEnd
  , singleQuoteStart
  , singleQuoted
  , smartPunctuation
  )
where

import Control.Monad (guard , void)
import Text.Pandoc.Builder (Inlines)
import Text.Pandoc.Options
  ( extensionEnabled
  , Extension(Ext_old_dashes, Ext_smart)
  , ReaderOptions(readerExtensions) )
import Text.Pandoc.Sources
import Text.Pandoc.Parsing.Capabilities
import Text.Pandoc.Parsing.General
import Text.Pandoc.Parsing.Types (ParserT)
import Text.Parsec
  ( (<|>)
  , Stream(..)
  , choice
  , lookAhead
  , manyTill
  , notFollowedBy
  , try
  )

import qualified Text.Pandoc.Builder as B

-- | Parses various ASCII punctuation, quotes, and apostrophe in a smart
-- way, inferring their semantic meaning.
--
-- Fails unless the 'Ext_smart' extension has been enabled.
smartPunctuation :: (HasReaderOptions st, HasLastStrPosition st,
                     HasQuoteContext st m,
                     Stream s m Char, UpdateSourcePos s Char)
                 => ParserT s st m Inlines
                 -> ParserT s st m Inlines
smartPunctuation inlineParser = do
  guardEnabled Ext_smart
  choice [ quoted inlineParser, apostrophe, doubleCloseQuote, dash, ellipses ]

-- | Parses inline text in single or double quotes, assumes English
-- quoting conventions.
quoted :: (HasLastStrPosition st, HasQuoteContext st m,
           Stream s m Char, UpdateSourcePos s Char)
       => ParserT s st m Inlines
       -> ParserT s st m Inlines
quoted inlineParser = doubleQuoted inlineParser <|> singleQuoted inlineParser

-- | Parses inline text in single quotes, assumes English quoting
-- conventions.
singleQuoted :: (HasLastStrPosition st, HasQuoteContext st m,
                 Stream s m Char, UpdateSourcePos s Char)
             => ParserT s st m Inlines
             -> ParserT s st m Inlines
singleQuoted inlineParser = do
  singleQuoteStart
  (B.singleQuoted . mconcat <$>
    try
     (withQuoteContext InSingleQuote (many1Till inlineParser singleQuoteEnd)))
   <|> pure "\8217"

-- | Parses inline text in double quotes; assumes English quoting
-- conventions.
doubleQuoted :: (HasQuoteContext st m, HasLastStrPosition st,
                 Stream s m Char, UpdateSourcePos s Char)
             => ParserT s st m Inlines
             -> ParserT s st m Inlines
doubleQuoted inlineParser = do
  doubleQuoteStart
  (B.doubleQuoted . mconcat <$>
    try
     (withQuoteContext InDoubleQuote (manyTill inlineParser doubleQuoteEnd)))
   <|> pure (B.str "\8220")

charOrRef :: (Stream s m Char, UpdateSourcePos s Char) => [Char] -> ParserT s st m Char
charOrRef cs =
  oneOf cs <|> try (do c <- characterReference
                       guard (c `elem` cs)
                       return c)

-- | Succeeds if the parser is
--
-- * not within single quoted text;
-- * not directly after a word; and
-- * looking at an opening single quote char that's not followed by a
--   space.
--
-- Gobbles the quote character on success.
singleQuoteStart :: (HasLastStrPosition st, HasQuoteContext st m,
                     Stream s m Char, UpdateSourcePos s Char)
                 => ParserT s st m ()
singleQuoteStart = do
  failIfInQuoteContext InSingleQuote
  -- single quote start can't be right after str
  guard =<< notAfterString
  try $ do
    charOrRef "'\8216\145"
    void $ lookAhead (satisfy (not . isSpaceChar))

singleQuoteEnd :: (Stream s m Char, UpdateSourcePos s Char)
               => ParserT s st m ()
singleQuoteEnd = try $ do
  charOrRef "'\8217\146"
  notFollowedBy alphaNum

-- | Succeeds if the parser is
--
-- * not within a double quoted text;
--
-- * not directly after a word; and
--
-- * looking at an opening double quote char that's not followed by a
--   space.
--
-- Gobbles the quote character on success.
doubleQuoteStart :: (HasLastStrPosition st,
                     HasQuoteContext st m,
                     Stream s m Char, UpdateSourcePos s Char)
                 => ParserT s st m ()
doubleQuoteStart = do
  failIfInQuoteContext InDoubleQuote
  guard =<< notAfterString
  try $ do charOrRef "\"\8220\147"
           void $ lookAhead (satisfy (not . isSpaceChar))

-- | Parses a closing quote character.
doubleQuoteEnd :: (Stream s m Char, UpdateSourcePos s Char)
               => ParserT s st m ()
doubleQuoteEnd = void (charOrRef "\"\8221\148")

-- | Parses an ASCII apostrophe (@'@) or right single quotation mark and
-- returns a RIGHT SINGLE QUOtatiON MARK character.
apostrophe :: (Stream s m Char, UpdateSourcePos s Char)
           => ParserT s st m Inlines
apostrophe = (char '\'' <|> char '\8217') >> return (B.str "\8217")

-- | Parses an ASCII quotation mark character and returns a RIGHT DOUBLE
-- QUOTATION MARK.
doubleCloseQuote :: (Stream s m Char, UpdateSourcePos s Char)
                 => ParserT s st m Inlines
doubleCloseQuote = B.str "\8221" <$ char '"'

-- | Parses three dots as HORIZONTAL ELLIPSIS.
ellipses :: (Stream s m Char, UpdateSourcePos s Char)
         => ParserT s st m Inlines
ellipses = try (string "..." >> return (B.str "\8230"))

-- | Parses two hyphens as EN DASH and three as EM DASH.
--
-- If the extension @'Ext_old_dashes'@ is enabled, then two hyphens are
-- parsed as EM DASH, and one hyphen is parsed as EN DASH if it is
-- followed by a digit.
dash :: (HasReaderOptions st, Stream s m Char, UpdateSourcePos s Char)
     => ParserT s st m Inlines
dash = try $ do
  oldDashes <- extensionEnabled Ext_old_dashes <$> getOption readerExtensions
  if oldDashes
     then do
       char '-'
       (char '-' >> return (B.str "\8212"))
         <|> (lookAhead digit >> return (B.str "\8211"))
     else do
       string "--"
       (char '-' >> return (B.str "\8212"))
         <|> return (B.str "\8211")
