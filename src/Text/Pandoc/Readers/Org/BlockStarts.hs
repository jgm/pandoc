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
  ) where

import           Text.Pandoc.Readers.Org.Parsing

-- | Horizontal Line (five -- dashes or more)
hline :: OrgParser ()
hline = try $ do
  skipSpaces
  string "-----"
  many (char '-')
  skipSpaces
  newline
  return ()

-- | Read the start of a header line, return the header level
headerStart :: OrgParser Int
headerStart = try $
  (length <$> many1 (char '*')) <* many1 (char ' ') <* updateLastPreCharPos

tableStart :: OrgParser Char
tableStart = try $ skipSpaces *> char '|'

latexEnvStart :: OrgParser String
latexEnvStart = try $ do
  skipSpaces *> string "\\begin{"
             *> latexEnvName
             <* string "}"
             <* blankline
 where
   latexEnvName :: OrgParser String
   latexEnvName = try $ mappend <$> many1 alphaNum <*> option "" (string "*")


-- | Parses bullet list marker.
bulletListStart :: OrgParser ()
bulletListStart = try $
  choice
  [ () <$ skipSpaces  <* oneOf "+-" <* skipSpaces1
  , () <$ skipSpaces1 <* char '*'   <* skipSpaces1
  ]

genericListStart :: OrgParser String
                 -> OrgParser Int
genericListStart listMarker = try $
  (+) <$> (length <$> many spaceChar)
      <*> (length <$> listMarker <* many1 spaceChar)

orderedListStart :: OrgParser Int
orderedListStart = genericListStart orderedListMarker
  -- Ordered list markers allowed in org-mode
  where orderedListMarker = mappend <$> many1 digit <*> (pure <$> oneOf ".)")

drawerStart :: OrgParser String
drawerStart = try $
  skipSpaces *> drawerName <* skipSpaces <* newline
 where drawerName = char ':' *> manyTill nonspaceChar (char ':')

metaLineStart :: OrgParser ()
metaLineStart = try $ skipSpaces <* string "#+"

commentLineStart :: OrgParser ()
commentLineStart = try $ skipSpaces <* string "# "

exampleLineStart :: OrgParser ()
exampleLineStart = () <$ try (skipSpaces *> string ": ")

noteMarker :: OrgParser String
noteMarker = try $ do
  char '['
  choice [ many1Till digit (char ']')
         , (++) <$> string "fn:"
                <*> many1Till (noneOf "\n\r\t ") (char ']')
         ]
