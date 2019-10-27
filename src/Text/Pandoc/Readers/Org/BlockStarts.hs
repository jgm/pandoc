{-# LANGUAGE NoImplicitPrelude #-}
{- |
   Module      : Text.Pandoc.Readers.Org.BlockStarts
   Copyright   : Copyright (C) 2014-2019 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Parsers for Org-mode inline elements.
-}
module Text.Pandoc.Readers.Org.BlockStarts
  ( exampleLineStart
  , hline
  , noteMarker
  , tableStart
  , drawerStart
  , headerStart
  , metaLineStart
  , latexEnvStart
  , commentLineStart
  , bulletListStart
  , orderedListStart
  , endOfBlock
  ) where

import Prelude
import Control.Monad (void)
import Text.Pandoc.Readers.Org.Parsing

-- | Horizontal Line (five -- dashes or more)
hline :: Monad m => OrgParser m ()
hline = try $ do
  skipSpaces
  string "-----"
  many (char '-')
  skipSpaces
  newline
  return ()

-- | Read the start of a header line, return the header level
headerStart :: Monad m => OrgParser m Int
headerStart = try $
  (length <$> many1 (char '*')) <* many1 (char ' ') <* updateLastPreCharPos

tableStart :: Monad m => OrgParser m Char
tableStart = try $ skipSpaces *> char '|'

gridTableStart :: Monad m => OrgParser m ()
gridTableStart = try $ skipSpaces <* char '+' <* char '-'


latexEnvStart :: Monad m => OrgParser m String
latexEnvStart = try $
  skipSpaces *> string "\\begin{"
             *> latexEnvName
             <* string "}"
             <* blankline
 where
   latexEnvName :: Monad m => OrgParser m String
   latexEnvName = try $ mappend <$> many1 alphaNum <*> option "" (string "*")

bulletListStart :: Monad m => OrgParser m Int
bulletListStart = try $ do
  ind <- length <$> many spaceChar
   -- Unindented lists cannot use '*' bullets.
  oneOf (if ind == 0 then "+-" else "*+-")
  skipSpaces1 <|> lookAhead eol
  return (ind + 1)

genericListStart :: Monad m
                 => OrgParser m String
                 -> OrgParser m Int
genericListStart listMarker = try $ do
  ind <- length <$> many spaceChar
  void listMarker
  skipSpaces1 <|> lookAhead eol
  return (ind + 1)

eol :: Monad m => OrgParser m ()
eol = void (char '\n')

orderedListStart :: Monad m => OrgParser m Int
orderedListStart = genericListStart orderedListMarker
  -- Ordered list markers allowed in org-mode
  where orderedListMarker = mappend <$> many1 digit <*> (pure <$> oneOf ".)")

drawerStart :: Monad m => OrgParser m String
drawerStart = try $ skipSpaces *> drawerName <* skipSpaces <* newline
 where drawerName = char ':' *> manyTill nonspaceChar (char ':')

metaLineStart :: Monad m => OrgParser m ()
metaLineStart = try $ skipSpaces <* string "#+"

commentLineStart :: Monad m => OrgParser m ()
commentLineStart = try $
  -- the first char after '#' must be a plain space character or a newline
  skipSpaces <* string "#" <* lookAhead (oneOf " \n")

exampleLineStart :: Monad m => OrgParser m ()
exampleLineStart = () <$ try (skipSpaces *> string ": ")

noteMarker :: Monad m => OrgParser m String
noteMarker = try $ do
  char '['
  choice [ many1Till digit (char ']')
         , (++) <$> string "fn:"
                <*> many1Till (noneOf "\n\r\t ") (char ']')
         ]

-- | Succeeds if the parser is at the end of a block.
endOfBlock :: Monad m => OrgParser m ()
endOfBlock = lookAhead . try $
  void blankline <|> anyBlockStart
 where
   -- Succeeds if there is a new block starting at this position.
   anyBlockStart :: Monad m => OrgParser m ()
   anyBlockStart = try . choice $
     [ exampleLineStart
     , hline
     , metaLineStart
     , commentLineStart
     , gridTableStart
     , void noteMarker
     , void tableStart
     , void drawerStart
     , void headerStart
     , void latexEnvStart
     , void bulletListStart
     , void orderedListStart
     ]
