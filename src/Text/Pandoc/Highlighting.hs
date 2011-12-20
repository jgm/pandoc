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

module Text.Pandoc.Highlighting ( languages
                                , highlightHtml
                                , highlightLaTeX
                                , defaultHighlightingCss
                                , languagesByExtension
                                ) where
import Text.Blaze
import Text.Pandoc.Definition
#ifdef _HIGHLIGHTING
import Text.Highlighting.Kate ( SourceLine, languages, highlightAs, formatAsHtml,
        TokenType(..), formatAsLaTeX, FormatOption (..), defaultHighlightingCss, languagesByExtension )
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import qualified Text.Blaze.Html5.Attributes as A

highlight :: ([FormatOption] -> String -> [SourceLine] -> a) -- ^ Formatter
          -> Bool   -- ^ True if inline
          -> Attr   -- ^ Attributes of the Code or CodeBlock
          -> String -- ^ Raw contents of the Code or CodeBlock
          -> Either String a  -- ^ An error or the formatted result
highlight formatter inline (_, classes, keyvals) rawCode =
  let firstNum = case reads (fromMaybe "1" $ lookup "startFrom" keyvals) of
                      ((n,_):_) -> n
                      []        -> 1
      fmtOpts = [OptNumberFrom firstNum] ++
                [OptInline | inline] ++
                case find (`elem` ["number","numberLines","number-lines"]) classes of
                  Nothing   -> []
                  Just _    -> [OptNumberLines]
      addBirdTracks = "literate" `elem` classes
      lcLanguages = map (map toLower) languages
  in  case find (\c -> (map toLower c) `elem` lcLanguages) classes of
            Nothing        -> Left "Unknown or unsupported language"
            Just language  -> case highlightAs language rawCode of
                                   Left err -> Left err
                                   Right hl -> Right $ formatter fmtOpts language
                                                     $ if addBirdTracks
                                                          then map ((OtherTok,"> "):) hl
                                                          else hl

highlightHtml :: Bool   -- ^ True if inline HTML
              -> Attr   -- ^ Attributes of the Code or CodeBlock
              -> String -- ^ Raw contents of the Code or CodeBlock
              -> Either String Html   -- ^ An error or the formatted Html
highlightHtml inline attr@(id',_,_) = fmap addId . highlight formatAsHtml inline attr
      where addId = if null id' then id else (! A.id (toValue id'))

highlightLaTeX  :: Bool   -- ^ True if inline
                -> Attr   -- ^ Attributes of the Code or CodeBlock
                -> String -- ^ Raw contents of the Code or CodeBlock
                -> Either String String   -- ^ An error or the formatted LaTeX string
highlightLaTeX = highlight formatAsLaTeX

#else
defaultHighlightingCss :: String
defaultHighlightingCss = ""

languages :: [String]
languages = []

languagesByExtension :: String -> [String]
languagesByExtension _ = []

highlightHtml :: Bool -> Attr -> String -> Either String Html
highlightHtml _ _ _ = Left "Pandoc was not compiled with support for highlighting"

highlightLaTeX :: Bool -> Attr -> String -> Either String String
highlightLaTeX _ _ _ = Left "Pandoc was not compiled with support for highlighting"
#endif
