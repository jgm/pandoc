{-
Copyright (C) 2006 John MacFarlane <jgm at berkeley dot edu>

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
   Module      : Text.Pandoc.Writers.LaTeX
   Copyright   : Copyright (C) 2006 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm at berkeley dot edu>
   Stability   : alpha 
   Portability : portable

Conversion of 'Pandoc' format into LaTeX.
-}
module Text.Pandoc.Writers.LaTeX ( 
                                  writeLaTeX 
                                 ) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Printf ( printf )
import Data.List ( (\\) )

-- | Convert Pandoc to LaTeX.
writeLaTeX :: WriterOptions -> Pandoc -> String
writeLaTeX options (Pandoc meta blocks) = 
  let body = (writerIncludeBefore options) ++ 
             (concatMap blockToLaTeX blocks) ++
             (writerIncludeAfter options) in
  let head = if writerStandalone options
                then latexHeader options meta
                else "" in
  let foot = if writerStandalone options then "\n\\end{document}\n" else "" in
  head ++ body ++ foot

-- | Insert bibliographic information into LaTeX header.
latexHeader :: WriterOptions -- ^ Options, including LaTeX header
            -> Meta          -- ^ Meta with bibliographic information
            -> String
latexHeader options (Meta title authors date) =
  let titletext = if null title
                     then "" 
                     else "\\title{" ++ inlineListToLaTeX title ++ "}\n"
      authorstext = if null authors
                       then "" 
                       else "\\author{" ++ (joinWithSep "\\\\" 
                            (map stringToLaTeX authors)) ++ "}\n"
      datetext = if date == ""
                    then "" 
                    else "\\date{" ++ stringToLaTeX date ++ "}\n"
      maketitle = if null title then "" else "\\maketitle\n" 
      secnumline = if (writerNumberSections options)
                      then "" 
                      else "\\setcounter{secnumdepth}{0}\n" 
      header     = writerHeader options in
  header ++ secnumline ++ titletext ++ authorstext ++ datetext ++ 
  "\\begin{document}\n" ++ maketitle

-- escape things as needed for LaTeX (also ldots, dashes, quotes, etc.) 

escapeBrackets  = backslashEscape "{}"
escapeSpecial   = backslashEscape "$%&~_#"

escapeBackslash = substitute "\\" "\\textbackslash{}" 
fixBackslash    = substitute "\\textbackslash\\{\\}" "\\textbackslash{}"
escapeHat       = substitute "^" "\\^{}"
escapeBar       = substitute "|" "\\textbar{}"
escapeLt        = substitute "<" "\\textless{}"
escapeGt        = substitute ">" "\\textgreater{}"

-- | Escape string for LaTeX
stringToLaTeX :: String -> String
stringToLaTeX = escapeGt . escapeLt . escapeBar . escapeHat . 
                escapeSpecial . fixBackslash . escapeBrackets . 
                escapeBackslash 

-- | Remove all code elements from list of inline elements
-- (because it's illegal to have a \\verb inside a command argument)
deVerb :: [Inline] -> [Inline]
deVerb [] = []
deVerb ((Code str):rest) = (Str str):(deVerb rest)
deVerb (other:rest) = other:(deVerb rest)

-- | Convert Pandoc block element to LaTeX.
blockToLaTeX :: Block     -- ^ Block to convert
             -> String 
blockToLaTeX Null = ""
blockToLaTeX (Plain lst) = inlineListToLaTeX lst ++ "\n"
blockToLaTeX (Para lst) = (inlineListToLaTeX lst) ++ "\n\n"
blockToLaTeX (BlockQuote lst) = "\\begin{quote}\n" ++ 
    (concatMap blockToLaTeX lst) ++ "\\end{quote}\n"
blockToLaTeX (CodeBlock str) = "\\begin{verbatim}\n" ++ str ++ 
    "\n\\end{verbatim}\n"
blockToLaTeX (RawHtml str) = ""
blockToLaTeX (BulletList lst) = "\\begin{itemize}\n" ++ 
    (concatMap listItemToLaTeX lst) ++ "\\end{itemize}\n"
blockToLaTeX (OrderedList lst) = "\\begin{enumerate}\n" ++ 
    (concatMap listItemToLaTeX lst) ++ "\\end{enumerate}\n"
blockToLaTeX (DefinitionList lst) = 
    let defListItemToLaTeX (term, def) = "\\item[" ++ 
           substitute "]" "\\]" (inlineListToLaTeX term) ++ "] " ++
           concatMap blockToLaTeX def
    in  "\\begin{description}\n" ++ concatMap defListItemToLaTeX lst ++ 
        "\\end{description}\n"
blockToLaTeX HorizontalRule = 
    "\\begin{center}\\rule{3in}{0.4pt}\\end{center}\n\n"
blockToLaTeX (Header level lst) = 
    if (level > 0) && (level <= 3)
       then "\\" ++ (concat (replicate (level - 1) "sub")) ++ "section{" ++ 
            (inlineListToLaTeX (deVerb lst)) ++ "}\n\n"
       else (inlineListToLaTeX lst) ++ "\n\n"
blockToLaTeX (Table caption aligns widths heads rows) =
    let colWidths = map printDecimal widths
        colDescriptors = concat $ zipWith
                                  (\width align -> ">{\\PBS" ++ 
                                  (case align of 
                                         AlignLeft -> "\\raggedright"
                                         AlignRight -> "\\raggedleft"
                                         AlignCenter -> "\\centering"
                                         AlignDefault -> "\\raggedright") ++
                                  "\\hspace{0pt}}p{" ++ width ++ 
                                  "\\textwidth}")
                                  colWidths aligns
        headers        = tableRowToLaTeX heads 
        captionText    = inlineListToLaTeX caption 
        tableBody      = "\\begin{tabular}{" ++ colDescriptors ++ "}\n" ++
                         headers ++ "\\hline\n" ++ 
                         (concatMap tableRowToLaTeX rows) ++ 
                         "\\end{tabular}\n" 
        centered str   = "\\begin{center}\n" ++ str ++ "\\end{center}\n" in
    if null captionText
      then centered tableBody ++ "\n"
      else "\\begin{table}[h]\n" ++ centered tableBody ++ "\\caption{" ++
           captionText ++ "}\n" ++ "\\end{table}\n\n" 

printDecimal :: Float -> String
printDecimal = printf "%.2f" 

tableColumnWidths cols = map (length . (concatMap blockToLaTeX)) cols

tableRowToLaTeX cols = joinWithSep " & " (map (concatMap blockToLaTeX) cols) ++ "\\\\\n"

listItemToLaTeX list = "\\item " ++ 
    (concatMap blockToLaTeX list) 

-- | Convert list of inline elements to LaTeX.
inlineListToLaTeX :: [Inline]  -- ^ Inlines to convert
                  -> String
inlineListToLaTeX lst = 
  concatMap inlineToLaTeX lst

isQuoted :: Inline -> Bool
isQuoted (Quoted _ _) = True
isQuoted Apostrophe = True
isQuoted _ = False

-- | Convert inline element to LaTeX
inlineToLaTeX :: Inline    -- ^ Inline to convert
              -> String
inlineToLaTeX (Emph lst) = "\\emph{" ++ 
    (inlineListToLaTeX (deVerb lst)) ++ "}"
inlineToLaTeX (Strong lst) = "\\textbf{" ++ 
    (inlineListToLaTeX (deVerb lst)) ++ "}"
inlineToLaTeX (Code str) = "\\verb" ++ [chr] ++ stuffing ++ [chr]
                     where stuffing = str 
                           chr      = ((enumFromTo '!' '~') \\ stuffing) !! 0
inlineToLaTeX (Quoted SingleQuote lst) =
  let s1 = if (not (null lst)) && (isQuoted (head lst)) then "\\," else ""
      s2 = if (not (null lst)) && (isQuoted (last lst)) then "\\," else "" in
  "`" ++ s1 ++ inlineListToLaTeX lst ++ s2 ++ "'"
inlineToLaTeX (Quoted DoubleQuote lst) =
  let s1 = if (not (null lst)) && (isQuoted (head lst)) then "\\," else ""
      s2 = if (not (null lst)) && (isQuoted (last lst)) then "\\," else "" in
  "``" ++ s1 ++ inlineListToLaTeX lst ++ s2 ++ "''"
inlineToLaTeX Apostrophe = "'"
inlineToLaTeX EmDash = "---"
inlineToLaTeX EnDash = "--"
inlineToLaTeX Ellipses = "\\ldots{}"
inlineToLaTeX (Str str) = stringToLaTeX str
inlineToLaTeX (TeX str) = str
inlineToLaTeX (HtmlInline str) = ""
inlineToLaTeX (LineBreak) = "\\\\\n"
inlineToLaTeX Space = " "
inlineToLaTeX (Link text (src, tit)) = 
    "\\href{" ++ src ++ "}{" ++ (inlineListToLaTeX (deVerb text)) ++ "}"
inlineToLaTeX (Image alternate (source, tit)) = 
    "\\includegraphics{" ++ source ++ "}" 
inlineToLaTeX (Note contents) = 
    "\\footnote{" ++ (stripTrailingNewlines $ concatMap blockToLaTeX contents)  ++ "}"
