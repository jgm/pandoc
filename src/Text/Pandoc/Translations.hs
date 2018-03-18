{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-
Copyright (C) 2017-2018 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Translations
   Copyright   : Copyright (C) 2017-2018 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Data types for localization.

Translations are stored in @data/translations/langname.trans@,
where langname can be the full BCP47 language specifier, or
just the language part.  File format is:

> # A comment, ignored
> Figure: Figura
> Index: Indeksi

-}
module Text.Pandoc.Translations (
                           Term(..)
                         , Translations
                         , lookupTerm
                         , readTranslations
                         )
where
import Data.Aeson.Types (typeMismatch)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Text as T
import Data.Yaml as Yaml
import GHC.Generics (Generic)
import Text.Pandoc.Shared (safeRead)
import qualified Text.Pandoc.UTF8 as UTF8

data Term =
    Preface
  | References
  | Abstract
  | Bibliography
  | Chapter
  | Appendix
  | Contents
  | ListOfFigures
  | ListOfTables
  | Index
  | Figure
  | Table
  | Part
  | Page
  | See
  | SeeAlso
  | Encl
  | Cc
  | To
  | Proof
  | Glossary
  | Listing
  deriving (Show, Eq, Ord, Generic, Enum, Read)

newtype Translations = Translations (M.Map Term String)
        deriving (Show, Generic, Semigroup, Monoid)

instance FromJSON Term where
  parseJSON (String t) = case safeRead (T.unpack t) of
                               Just t' -> pure t'
                               Nothing -> fail $ "Invalid Term name " ++
                                                 show t
  parseJSON invalid = typeMismatch "Term" invalid

instance FromJSON Translations where
  parseJSON (Object hm) = do
    xs <- mapM addItem (HM.toList hm)
    return $ Translations (M.fromList xs)
    where addItem (k,v) =
            case safeRead (T.unpack k) of
                 Nothing -> fail $ "Invalid Term name " ++ show k
                 Just t  ->
                   case v of
                        (String s) -> return (t, T.unpack $ T.strip s)
                        inv        -> typeMismatch "String" inv
  parseJSON invalid = typeMismatch "Translations" invalid

lookupTerm :: Term -> Translations -> Maybe String
lookupTerm t (Translations tm) = M.lookup t tm

readTranslations :: String -> Either String Translations
readTranslations s =
  case Yaml.decodeEither' $ UTF8.fromString s of
       Left err' -> Left $ prettyPrintParseException err'
       Right t   -> Right t
