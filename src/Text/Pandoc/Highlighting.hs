{-
Copyright (C) 2008-2016 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2008-2016 John MacFarlane
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
                                , zenburn
                                , tango
                                , kate
                                , monochrome
                                , haddock
                                , Style
                                , fromListingsLanguage
                                , toListingsLanguage
                                ) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared (safeRead)
import Text.Highlighting.Kate
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import qualified Data.Map as M
import Control.Applicative ((<|>))

lcLanguages :: [String]
lcLanguages = map (map toLower) languages

highlight :: (FormatOptions -> [SourceLine] -> a) -- ^ Formatter
          -> Attr   -- ^ Attributes of the CodeBlock
          -> String -- ^ Raw contents of the CodeBlock
          -> Maybe a -- ^ Maybe the formatted result
highlight formatter (_, classes, keyvals) rawCode =
  let firstNum = fromMaybe 1 (safeRead (fromMaybe "1" $ lookup "startFrom" keyvals))
      fmtOpts = defaultFormatOpts{
                  startNumber = firstNum,
                  numberLines = any (`elem`
                        ["number","numberLines", "number-lines"]) classes }
      lcclasses = map (map toLower)
                     (classes ++ concatMap languagesByExtension classes)
  in  case find (`elem` lcLanguages) lcclasses of
            Nothing
              | numberLines fmtOpts -> Just
                              $ formatter fmtOpts{ codeClasses = [],
                                                   containerClasses = classes }
                              $ map (\ln -> [(NormalTok, ln)]) $ lines rawCode
              | otherwise  -> Nothing
            Just language  -> Just
                              $ formatter fmtOpts{ codeClasses = [language],
                                                   containerClasses = classes }
                              $ highlightAs language rawCode

-- Functions for correlating latex listings package's language names
-- with highlighting-kate language names:

langToListingsMap :: M.Map String String
langToListingsMap = M.fromList langsList

listingsToLangMap :: M.Map String String
listingsToLangMap = M.fromList $ map switch langsList
  where switch (a,b) = (b,a)

langsList :: [(String, String)]
langsList =    [("ada","Ada")
               ,("java","Java")
               ,("prolog","Prolog")
               ,("python","Python")
               ,("gnuassembler","Assembler")
               ,("commonlisp","Lisp")
               ,("r","R")
               ,("awk","Awk")
               ,("bash","bash")
               ,("makefile","make")
               ,("c","C")
               ,("matlab","Matlab")
               ,("ruby","Ruby")
               ,("cpp","C++")
               ,("ocaml","Caml")
               ,("modula2","Modula-2")
               ,("sql","SQL")
               ,("eiffel","Eiffel")
               ,("tcl","tcl")
               ,("erlang","erlang")
               ,("verilog","Verilog")
               ,("fortran","Fortran")
               ,("vhdl","VHDL")
               ,("pascal","Pascal")
               ,("perl","Perl")
               ,("xml","XML")
               ,("haskell","Haskell")
               ,("php","PHP")
               ,("xslt","XSLT")
               ,("html","HTML")
               ,("gap","GAP")
               ]

listingsLangs :: [String]
listingsLangs = ["Ada","Java","Prolog","Algol","JVMIS","Promela",
                 "Ant","ksh","Python","Assembler","Lisp","R","Awk",
                 "Logo","Reduce","bash","make","Rexx","Basic",
                 "Mathematica","RSL","C","Matlab","Ruby","C++",
                 "Mercury","S","Caml","MetaPost","SAS","Clean",
                 "Miranda","Scilab","Cobol","Mizar","sh","Comal",
                 "ML","SHELXL","csh","Modula-2","Simula","Delphi",
                 "MuPAD","SQL","Eiffel","NASTRAN","tcl","Elan",
                 "Oberon-2","TeX","erlang","OCL","VBScript","Euphoria",
                 "Octave","Verilog","Fortran","Oz","VHDL","GCL",
                 "Pascal","VRML","Gnuplot","Perl","XML","Haskell",
                 "PHP","XSLT","HTML","PL/I","GAP"]

-- Determine listings language name from highlighting-kate language name.
toListingsLanguage :: String -> Maybe String
toListingsLanguage lang = (if lang `elem` listingsLangs
                              then Just lang
                              else Nothing) <|>
                             M.lookup (map toLower lang) langToListingsMap

-- Determine highlighting-kate language name from listings language name.
fromListingsLanguage :: String -> Maybe String
fromListingsLanguage lang = M.lookup lang listingsToLangMap
