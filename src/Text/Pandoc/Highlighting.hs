{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Highlighting
   Copyright   : Copyright (C) 2008-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Exports functions for syntax highlighting.
-}

module Text.Pandoc.Highlighting ( highlightingStyles
                                , languages
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
                                , breezeDark
                                , haddock
                                , Style
                                , fromListingsLanguage
                                , toListingsLanguage
                                ) where
import Control.Monad
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Skylighting
import Text.Pandoc.Definition
import Text.Pandoc.Shared (safeRead)

highlightingStyles :: [(T.Text, Style)]
highlightingStyles =
  [("pygments", pygments),
   ("tango", tango),
   ("espresso", espresso),
   ("zenburn", zenburn),
   ("kate", kate),
   ("monochrome", monochrome),
   ("breezedark", breezeDark),
   ("haddock", haddock)]

languages :: [T.Text]
languages = [T.toLower (sName s) | s <- M.elems defaultSyntaxMap]

languagesByExtension :: T.Text -> [T.Text]
languagesByExtension ext =
  [T.toLower (sName s) | s <- syntaxesByExtension defaultSyntaxMap (T.unpack ext)]

highlight :: SyntaxMap
          -> (FormatOptions -> [SourceLine] -> a) -- ^ Formatter
          -> Attr   -- ^ Attributes of the CodeBlock
          -> T.Text -- ^ Raw contents of the CodeBlock
          -> Either T.Text a
highlight syntaxmap formatter (ident, classes, keyvals) rawCode =
  let firstNum = fromMaybe 1 (safeRead (fromMaybe "1" $ lookup "startFrom" keyvals))
      fmtOpts = defaultFormatOpts{
                  startNumber = firstNum,
                  lineAnchors = any (`elem`
                        ["line-anchors", "lineAnchors"]) classes,
                  numberLines = any (`elem`
                        ["number","numberLines", "number-lines"]) classes,
                  lineIdPrefix = if T.null ident
                                    then mempty
                                    else ident <> "-" }
      tokenizeOpts = TokenizerConfig{ syntaxMap = syntaxmap
                                    , traceOutput = False }
  in  case msum (map (`lookupSyntax` syntaxmap) classes) of
            Nothing
              | numberLines fmtOpts -> Right
                              $ formatter fmtOpts{ codeClasses = [],
                                                   containerClasses = classes }
                              $ map (\ln -> [(NormalTok, ln)])
                              $ T.lines rawCode
              | otherwise  -> Left ""
            Just syntax  -> either (Left . T.pack) Right $
              formatter fmtOpts{ codeClasses =
                                   [T.toLower (sShortname syntax)],
                                  containerClasses = classes } <$>
                tokenize tokenizeOpts syntax rawCode

-- Functions for correlating latex listings package's language names
-- with skylighting language names:

langToListingsMap :: M.Map T.Text T.Text
langToListingsMap = M.fromList langsList

listingsToLangMap :: M.Map T.Text T.Text
listingsToLangMap = M.fromList $ map switch langsList
  where switch (a,b) = (b,a)

langsList :: [(T.Text, T.Text)]
langsList =
  [("abap","ABAP"),
  ("acm","ACM"),
  ("acmscript","ACMscript"),
  ("acsl","ACSL"),
  ("ada","Ada"),
  ("algol","Algol"),
  ("ant","Ant"),
  ("assembler","Assembler"),
  ("gnuassembler","Assembler"),
  ("awk","Awk"),
  ("bash","bash"),
  ("monobasic","Basic"),
  ("purebasic","Basic"),
  ("c","C"),
  ("cs","C"),
  ("objectivec","C"),
  ("cpp","C++"),
  ("c++","C++"),
  ("ocaml","Caml"),
  ("cil","CIL"),
  ("clean","Clean"),
  ("cobol","Cobol"),
  ("comal80","Comal80"),
  ("command.com","command.com"),
  ("comsol","Comsol"),
  ("csh","csh"),
  ("delphi","Delphi"),
  ("eiffel","Eiffel"),
  ("elan","Elan"),
  ("elisp","elisp"),
  ("erlang","erlang"),
  ("euphoria","Euphoria"),
  ("fortran","Fortran"),
  ("gap","GAP"),
  ("gcl","GCL"),
  ("gnuplot","Gnuplot"),
  ("go","Go"),
  ("hansl","hansl"),
  ("haskell","Haskell"),
  ("html","HTML"),
  ("idl","IDL"),
  ("inform","inform"),
  ("java","Java"),
  ("jvmis","JVMIS"),
  ("ksh","ksh"),
  ("lingo","Lingo"),
  ("lisp","Lisp"),
  ("commonlisp","Lisp"),
  ("llvm","LLVM"),
  ("logo","Logo"),
  ("lua","Lua"),
  ("make","make"),
  ("makefile","make"),
  ("mathematica","Mathematica"),
  ("matlab","Matlab"),
  ("mercury","Mercury"),
  ("metapost","MetaPost"),
  ("miranda","Miranda"),
  ("mizar","Mizar"),
  ("ml","ML"),
  ("modula2","Modula-2"),
  ("mupad","MuPAD"),
  ("nastran","NASTRAN"),
  ("oberon2","Oberon-2"),
  ("ocl","OCL"),
  ("octave","Octave"),
  ("oorexx","OORexx"),
  ("oz","Oz"),
  ("pascal","Pascal"),
  ("perl","Perl"),
  ("php","PHP"),
  ("pli","PL/I"),
  ("plasm","Plasm"),
  ("postscript","PostScript"),
  ("pov","POV"),
  ("prolog","Prolog"),
  ("promela","Promela"),
  ("pstricks","PSTricks"),
  ("python","Python"),
  ("r","R"),
  ("reduce","Reduce"),
  ("rexx","Rexx"),
  ("rsl","RSL"),
  ("ruby","Ruby"),
  ("s","S"),
  ("sas","SAS"),
  ("scala","Scala"),
  ("scilab","Scilab"),
  ("sh","sh"),
  ("shelxl","SHELXL"),
  ("simula","Simula"),
  ("sparql","SPARQL"),
  ("sql","SQL"),
  ("swift","Swift"),
  ("tcl","tcl"),
  ("tex","TeX"),
  ("latex","TeX"),
  ("vbscript","VBScript"),
  ("verilog","Verilog"),
  ("vhdl","VHDL"),
  ("vrml","VRML"),
  ("xml","XML"),
  ("xslt","XSLT")]

-- | Determine listings language name from skylighting language name.
toListingsLanguage :: T.Text -> Maybe T.Text
toListingsLanguage lang = M.lookup (T.toLower lang) langToListingsMap

-- | Determine skylighting language name from listings language name.
fromListingsLanguage :: T.Text -> Maybe T.Text
fromListingsLanguage lang = M.lookup lang listingsToLangMap
