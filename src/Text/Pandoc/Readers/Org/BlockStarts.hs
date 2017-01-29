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

import Control.Monad ( void )
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

latexEnvStart :: Monad m => OrgParser m String
latexEnvStart = try $ do
  skipSpaces *> string "\\begin{"
             *> latexEnvName
             <* string "}"
             <* blankline
 where
   latexEnvName :: Monad m => OrgParser m String
   latexEnvName = try $ mappend <$> many1 alphaNum <*> option "" (string "*")


-- | Parses bullet list marker.
bulletListStart :: Monad m => OrgParser m ()
bulletListStart = try $
  choice
  [ () <$ skipSpaces  <* oneOf "+-" <* skipSpaces1
  , () <$ skipSpaces1 <* char '*'   <* skipSpaces1
  ]

genericListStart :: Monad m
                 => OrgParser m String
                 -> OrgParser m Int
genericListStart listMarker = try $
  (+) <$> (length <$> many spaceChar)
      <*> (length <$> listMarker <* many1 spaceChar)

orderedListStart :: Monad m => OrgParser m Int
orderedListStart = genericListStart orderedListMarker
  -- Ordered list markers allowed in org-mode
  where orderedListMarker = mappend <$> many1 digit <*> (pure <$> oneOf ".)")

drawerStart :: Monad m => OrgParser m String
drawerStart = try $
  skipSpaces *> drawerName <* skipSpaces <* newline
 where drawerName = char ':' *> manyTill nonspaceChar (char ':')

metaLineStart :: Monad m => OrgParser m ()
metaLineStart = try $ skipSpaces <* string "#+"

commentLineStart :: Monad m => OrgParser m ()
commentLineStart = try $ skipSpaces <* string "# "

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
endOfBlock = lookAhead . try $ do
    void blankline <|> anyBlockStart
 where
   -- Succeeds if there is a new block starting at this position.
   anyBlockStart :: Monad m => OrgParser m ()
   anyBlockStart = try . choice $
     [ exampleLineStart
     , hline
     , metaLineStart
     , commentLineStart
     , void noteMarker
     , void tableStart
     , void drawerStart
     , void headerStart
     , void latexEnvStart
     , void bulletListStart
     , void orderedListStart
     ]

