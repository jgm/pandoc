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
                                , languagesByExtension
                                , highlight
                                , formatLaTeXInline
                                , formatLaTeXBlock
                                , styleToLaTeX
                                , formatHtmlInline
                                , formatHtmlBlock
                                , styleToCss
                                , pygments
                                , espresso
                                , tango
                                , kate
                                , monochrome
                                , haddock
                                , Style
                                ) where
import Text.Pandoc.Definition
import Text.Highlighting.Kate
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Char (toLower)

lcLanguages :: [String]
lcLanguages = map (map toLower) languages

highlight :: (FormatOptions -> [SourceLine] -> a) -- ^ Formatter
          -> Attr   -- ^ Attributes of the CodeBlock
          -> String -- ^ Raw contents of the CodeBlock
          -> Maybe a -- ^ Maybe the formatted result
highlight formatter (_, classes, keyvals) rawCode =
  let firstNum = case reads (fromMaybe "1" $ lookup "startFrom" keyvals) of
                      ((n,_):_) -> n
                      []        -> 1
      fmtOpts = defaultFormatOpts{
                  startNumber = firstNum,
                  numberLines = any (`elem`
                        ["number","numberLines", "number-lines"]) classes }
      lcclasses = map (map toLower) classes
  in  case find (`elem` lcLanguages) lcclasses of
            Nothing        -> Nothing
            Just language  -> Just
                              $ formatter fmtOpts{ codeClasses = [language],
                                                   containerClasses = classes }
                              $ highlightAs language rawCode

