{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Org.ExportSettings
   Copyright   : © 2016–2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Parsers for Org-mode export options.
-}
module Text.Pandoc.Readers.Org.ExportSettings
  ( exportSettings
  ) where

import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Logging (LogMessage (UnknownOrgExportOption))
import Text.Pandoc.Readers.Org.ParserState
import Text.Pandoc.Readers.Org.Parsing

import Control.Monad (mzero, void)
import Data.Char (toLower)
import Data.Maybe (listToMaybe)
import Data.Text (Text, unpack)

-- | Read and handle space separated org-mode export settings.
exportSettings :: PandocMonad m => OrgParser m ()
exportSettings = void $ sepBy skipSpaces exportSetting

-- | Setter function for export settings.
type ExportSettingSetter a = a -> ExportSettings -> ExportSettings

-- | Read and process a single org-mode export option.
exportSetting :: PandocMonad m => OrgParser m ()
exportSetting = choice
  [ booleanSetting "^" (\val es -> es { exportSubSuperscripts = val })
  , booleanSetting "'" (\val es -> es { exportSmartQuotes = val })
  , booleanSetting "*" (\val es -> es { exportEmphasizedText = val })
  , booleanSetting "-" (\val es -> es { exportSpecialStrings = val })
  , ignoredSetting ":"
  , ignoredSetting "<"
  , booleanSetting "\\n" (\val es -> es { exportPreserveBreaks = val })
  , archivedTreeSetting "arch" (\val es -> es { exportArchivedTrees = val })
  , booleanSetting "author" (\val es -> es { exportWithAuthor = val })
  , ignoredSetting "c"
  -- org-mode allows the special value `comment` for creator, which we'll
  -- interpret as true as it doesn't make sense in the context of Pandoc.
  , booleanSetting "creator" (\val es -> es { exportWithCreator = val })
  , complementableListSetting "d" (\val es -> es { exportDrawers = val })
  , ignoredSetting "date"
  , booleanSetting "e" (\val es -> es { exportWithEntities = val })
  , booleanSetting "email" (\val es -> es { exportWithEmail = val })
  , booleanSetting "f" (\val es -> es { exportWithFootnotes = val })
  , integerSetting "H" (\val es -> es { exportHeadlineLevels = val })
  , ignoredSetting "inline"
  , ignoredSetting "num"
  , booleanSetting "p" (\val es -> es { exportWithPlanning = val })
  , ignoredSetting "pri"
  , ignoredSetting "prop"
  , ignoredSetting "stat"
  , booleanSetting "tags" (\val es -> es { exportWithTags = val })
  , ignoredSetting "tasks"
  , texSetting     "tex" (\val es -> es { exportWithLatex = val })
  , ignoredSetting "timestamp"
  , ignoredSetting "title"
  , ignoredSetting "toc"
  , booleanSetting "todo" (\val es -> es { exportWithTodoKeywords = val })
  , booleanSetting "|" (\val es -> es { exportWithTables = val })
  , ignoreAndWarn
  ] <?> "export setting"

-- | Generic handler for export settings. Takes a parser which converts
-- the plain option text into a data structure.
genericExportSetting :: Monad m
                     => OrgParser m a
                     -> Text
                     -> ExportSettingSetter a
                     -> OrgParser m ()
genericExportSetting optionParser settingIdentifier setter = try $ do
  _     <- textStr settingIdentifier *> char ':'
  value <- optionParser
  updateState $ modifyExportSettings value
 where
   modifyExportSettings val st =
     st { orgStateExportSettings = setter val . orgStateExportSettings $ st }

-- | A boolean option, either nil (False) or non-nil (True).
booleanSetting :: Monad m => Text ->  ExportSettingSetter Bool -> OrgParser m ()
booleanSetting = genericExportSetting elispBoolean

-- | An integer-valued option.
integerSetting :: Monad m => Text -> ExportSettingSetter Int -> OrgParser m ()
integerSetting = genericExportSetting parseInt
 where
   parseInt = try $
     many1 digit >>= maybe mzero (return . fst) . listToMaybe . reads

-- | Either the string "headline" or an elisp boolean and treated as an
-- @ArchivedTreesOption@.
archivedTreeSetting :: Monad m
                    => Text
                    -> ExportSettingSetter ArchivedTreesOption
                    -> OrgParser m ()
archivedTreeSetting =
  genericExportSetting $ archivedTreesHeadlineSetting <|> archivedTreesBoolean
 where
   archivedTreesHeadlineSetting =
     ArchivedTreesHeadlineOnly <$ optionString "headline"

   archivedTreesBoolean = try $ do
     exportBool <- elispBoolean
     return $
       if exportBool
       then ArchivedTreesExport
       else ArchivedTreesNoExport

-- | A list or a complement list (i.e. a list starting with `not`).
complementableListSetting :: Monad m
                          => Text
                          -> ExportSettingSetter (Either [Text] [Text])
                          -> OrgParser m ()
complementableListSetting = genericExportSetting $ choice
  [ Left  <$> complementTextList
  , Right <$> stringList
  , (\b -> if b then Left [] else Right []) <$> elispBoolean
  ]
 where
   -- Read a plain list of strings.
   stringList :: Monad m => OrgParser m [Text]
   stringList = try $
     char '('
       *> sepBy elispText spaces
       <* char ')'

   -- Read an emacs lisp list specifying a complement set.
   complementTextList :: Monad m => OrgParser m [Text]
   complementTextList = try $
     string "(not "
       *> sepBy elispText spaces
       <* char ')'

   elispText :: Monad m => OrgParser m Text
   elispText = try $
     char '"'
       *> manyTillChar alphaNum (char '"')

-- | Parses either @t@, @nil@, or @verbatim@ into a 'TeXExport' value.
texSetting :: Monad m
           => Text
           -> ExportSettingSetter TeXExport
           -> OrgParser m ()
texSetting = genericExportSetting $ texVerbatim <|> texBoolean
 where
   texVerbatim = TeXVerbatim <$ optionString "verbatim"

   texBoolean = try $ do
     exportBool <- elispBoolean
     return $
       if exportBool
       then TeXExport
       else TeXIgnore

-- | Read but ignore the export setting.
ignoredSetting :: Monad m => Text -> OrgParser m ()
ignoredSetting s = try (() <$ textStr s <* char ':' <* many1 nonspaceChar)

-- | Read any setting string, but ignore it and emit a warning.
ignoreAndWarn :: PandocMonad m => OrgParser m ()
ignoreAndWarn = try $ do
  opt <- many1Char nonspaceChar
  report (UnknownOrgExportOption opt)
  return ()

-- | Read an elisp boolean.  Only NIL is treated as false, non-NIL values are
-- interpreted as true.
elispBoolean :: Monad m => OrgParser m Bool
elispBoolean = try $ do
  value <- many1 nonspaceChar
  return $ case map toLower value of
             "nil" -> False
             "{}"  -> False
             "()"  -> False
             _     -> True

-- | Try to parse a literal string as the option value. Returns the
-- string on success.
optionString :: Monad m => Text -> OrgParser m Text
optionString s = try $ do
  _ <- string (unpack s)
  lookAhead (newline <|> spaceChar)
  return s
