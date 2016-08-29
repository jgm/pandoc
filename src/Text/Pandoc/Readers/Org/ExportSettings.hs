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
   Copyright   : Copyright (C) 2016 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Parsers for Org-mode export options.
-}
module Text.Pandoc.Readers.Org.ExportSettings
  ( exportSettings
  ) where

import           Text.Pandoc.Readers.Org.ParserState
import           Text.Pandoc.Readers.Org.Parsing

import           Control.Monad ( mzero, void )
import           Data.Char ( toLower )
import           Data.Maybe ( listToMaybe )

-- | Read and handle space separated org-mode export settings.
exportSettings :: OrgParser ()
exportSettings = void $ sepBy spaces exportSetting

-- | Setter function for export settings.
type ExportSettingSetter a = a -> ExportSettings -> ExportSettings

-- | Read and process a single org-mode export option.
exportSetting :: OrgParser ()
exportSetting = choice
  [ booleanSetting "^" (\val es -> es { exportSubSuperscripts = val })
  , booleanSetting "'" (\val es -> es { exportSmartQuotes = val })
  , booleanSetting "*" (\val es -> es { exportEmphasizedText = val })
  , booleanSetting "-" (\val es -> es { exportSpecialStrings = val })
  , ignoredSetting ":"
  , ignoredSetting "<"
  , ignoredSetting "\\n"
  , archivedTreeSetting "arch" (\val es -> es { exportArchivedTrees = val })
  , booleanSetting "author" (\val es -> es { exportWithAuthor = val })
  , ignoredSetting "c"
  -- org-mode allows the special value `comment` for creator, which we'll
  -- interpret as true as it doesn't make sense in the context of Pandoc.
  , booleanSetting "creator" (\val es -> es { exportWithCreator = val })
  , complementableListSetting "d" (\val es -> es { exportDrawers = val })
  , ignoredSetting "date"
  , ignoredSetting "e"
  , booleanSetting "email" (\val es -> es { exportWithEmail = val })
  , ignoredSetting "f"
  , integerSetting "H" (\val es -> es { exportHeadlineLevels = val })
  , ignoredSetting "inline"
  , ignoredSetting "num"
  , ignoredSetting "p"
  , ignoredSetting "pri"
  , ignoredSetting "prop"
  , ignoredSetting "stat"
  , ignoredSetting "tags"
  , ignoredSetting "tasks"
  , ignoredSetting "tex"
  , ignoredSetting "timestamp"
  , ignoredSetting "title"
  , ignoredSetting "toc"
  , ignoredSetting "todo"
  , ignoredSetting "|"
  ] <?> "export setting"

genericExportSetting :: OrgParser a
                     -> String
                     -> ExportSettingSetter a
                     -> OrgParser ()
genericExportSetting optionParser settingIdentifier setter = try $ do
  _     <- string settingIdentifier *> char ':'
  value <- optionParser
  updateState $ modifyExportSettings value
 where
   modifyExportSettings val st =
     st { orgStateExportSettings = setter val . orgStateExportSettings $ st }

-- | A boolean option, either nil (False) or non-nil (True).
booleanSetting :: String ->  ExportSettingSetter Bool -> OrgParser ()
booleanSetting = genericExportSetting elispBoolean

-- | An integer-valued option.
integerSetting :: String -> ExportSettingSetter Int -> OrgParser ()
integerSetting = genericExportSetting parseInt
 where
   parseInt = try $
     many1 digit >>= maybe mzero (return . fst) . listToMaybe . reads

-- | Either the string "headline" or an elisp boolean and treated as an
-- @ArchivedTreesOption@.
archivedTreeSetting :: String
                    -> ExportSettingSetter ArchivedTreesOption
                    -> OrgParser ()
archivedTreeSetting =
  genericExportSetting $ archivedTreesHeadlineSetting <|> archivedTreesBoolean
 where
   archivedTreesHeadlineSetting = try $ do
     _ <- string "headline"
     lookAhead (newline <|> spaceChar)
     return ArchivedTreesHeadlineOnly

   archivedTreesBoolean = try $ do
     exportBool <- elispBoolean
     return $
       if exportBool
       then ArchivedTreesExport
       else ArchivedTreesNoExport

-- | A list or a complement list (i.e. a list starting with `not`).
complementableListSetting :: String
                          -> ExportSettingSetter (Either [String] [String])
                          -> OrgParser ()
complementableListSetting = genericExportSetting $ choice
  [ Left  <$> complementStringList
  , Right <$> stringList
  , (\b -> if b then Left [] else Right []) <$> elispBoolean
  ]
 where
   -- Read a plain list of strings.
   stringList :: OrgParser [String]
   stringList = try $
     char '('
       *> sepBy elispString spaces
       <* char ')'

   -- Read an emacs lisp list specifying a complement set.
   complementStringList :: OrgParser [String]
   complementStringList = try $
     string "(not "
       *> sepBy elispString spaces
       <* char ')'

   elispString :: OrgParser String
   elispString = try $
     char '"'
       *> manyTill alphaNum (char '"')

-- | Read but ignore the export setting.
ignoredSetting :: String -> OrgParser ()
ignoredSetting s = try (() <$ string s <* char ':' <* many1 nonspaceChar)

-- | Read an elisp boolean.  Only NIL is treated as false, non-NIL values are
-- interpreted as true.
elispBoolean :: OrgParser Bool
elispBoolean = try $ do
  value <- many1 nonspaceChar
  return $ case map toLower value of
             "nil" -> False
             "{}"  -> False
             "()"  -> False
             _     -> True
