{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{- |
   Module      : Text.Pandoc.Translations
   Copyright   : Copyright (C) 2017-2022 John MacFarlane
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
import Data.Aeson.Types (Value(..), FromJSON(..))
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import Text.Pandoc.Shared (safeRead)
import qualified Text.Pandoc.UTF8 as UTF8
import Data.Yaml (prettyPrintParseException)

data Term =
    Abstract
  | Appendix
  | Bibliography
  | Cc
  | Chapter
  | Contents
  | Encl
  | Figure
  | Glossary
  | Index
  | Listing
  | ListOfFigures
  | ListOfTables
  | Page
  | Part
  | Preface
  | Proof
  | References
  | See
  | SeeAlso
  | Table
  | To
  deriving (Show, Eq, Ord, Generic, Enum, Read)

newtype Translations = Translations (M.Map Term T.Text)
        deriving (Show, Generic, Semigroup, Monoid)

instance FromJSON Term where
  parseJSON (String t) = case safeRead t of
                               Just t' -> pure t'
                               Nothing -> Prelude.fail $ "Invalid Term name " ++
                                                 show t
  parseJSON invalid = Aeson.typeMismatch "Term" invalid

instance FromJSON Translations where
  parseJSON o@(Object{}) = do
    xs <- parseJSON o >>= mapM addItem . M.toList
    return $ Translations (M.fromList xs)
    where addItem (k,v) =
            case safeRead k of
                 Nothing -> Prelude.fail $ "Invalid Term name " ++ show k
                 Just t  ->
                   case v of
                        (String s) -> return (t, T.strip s)
                        inv        -> Aeson.typeMismatch "String" inv
  parseJSON invalid = Aeson.typeMismatch "Translations" invalid

lookupTerm :: Term -> Translations -> Maybe T.Text
lookupTerm t (Translations tm) = M.lookup t tm

readTranslations :: T.Text -> Either T.Text Translations
readTranslations s =
  case Yaml.decodeAllEither' $ UTF8.fromText s of
       Left err' -> Left $ T.pack $ prettyPrintParseException err'
       Right (t:_)     -> Right t
       Right []        -> Left "empty YAML document"
