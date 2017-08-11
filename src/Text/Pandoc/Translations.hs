{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-
Copyright (C) 2017 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2017 John MacFarlane
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
                         , Translations(..)
                         , readTranslations
                         )
where
import qualified Data.Map as M
import GHC.Generics (Generic)
import Text.Pandoc.Shared (trim, safeRead)

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
  | Proof
  | See
  | SeeAlso
  | Cc
  | To
  deriving (Show, Eq, Ord, Generic, Read)

newtype Translations = Translations (M.Map Term String)
        deriving (Show, Eq, Ord, Generic, Monoid)

readTranslations :: String -> Either String Translations
readTranslations = foldr parseLine (Right mempty) . lines

parseLine :: String
          -> Either String Translations
          -> Either String Translations
parseLine _ (Left s) = Left s
parseLine ('#':_) x  = x
parseLine []      x  = x
parseLine t (Right (Translations tm)) =
  if null rest
     then Left $ "no colon in " ++ term
     else
       case safeRead term of
            Nothing    -> Left $ term ++ " is not a recognized term name"
            Just term' -> Right (Translations $ (M.insert term' defn) tm)
  where (trm, rest) = break (\c -> c == ':') t
        defn = trim $ drop 1 rest
        term = trim trm
