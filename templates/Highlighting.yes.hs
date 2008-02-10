{-
Copyright (C) 2008 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Highlighting
   Copyright   : Copyright (C) 2008 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Exports functions for syntax highlighting.
-}

module Text.Pandoc.Highlighting ( languages, highlightHtml ) where
import Text.Highlighting.Kate
import Text.XHtml
import Data.List (find, lookup)
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import Text.Pandoc.Definition

highlightHtml :: Attr -> String -> Either String Html
highlightHtml (_, classes, keyvals) rawCode =
  let firstNum = read $ fromMaybe "1" $ lookup "startFrom" keyvals
      fmtOpts = [OptNumberFrom firstNum] ++
                case find (`elem` ["number","numberLines","number-lines"]) classes of
                  Nothing   -> []
                  Just _    -> [OptNumberLines]
      lcLanguages = map (map toLower) languages
  in  case find (\c -> (map toLower c) `elem` lcLanguages) classes of
            Nothing   -> Left "Unknown or unsupported language"
            Just lang -> case highlightAs lang rawCode of
                               Left err -> Left err
                               Right hl -> Right $ formatAsXHtml fmtOpts lang hl

