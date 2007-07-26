{-
Copyright (C) 2006-7 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-7 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
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
import Data.List ( (\\), isInfixOf )
import qualified Data.Set as S
import Control.Monad.State

type WriterState = S.Set String  -- set of strings to include in header
                                 -- constructed based on content of document

-- | Add line to header.
addToHeader :: String -> State WriterState ()
addToHeader str = modify (S.insert str)

-- | Convert Pandoc to LaTeX.
writeLaTeX :: WriterOptions -> Pandoc -> String
writeLaTeX options document = 
  evalState (pandocToLaTeX options document) S.empty 

pandocToLaTeX :: WriterOptions -> Pandoc -> State WriterState String
pandocToLaTeX options (Pandoc meta blocks) = do
  main     <- blockListToLaTeX blocks
  head     <- if writerStandalone options
                 then latexHeader options meta
                 else return ""
  let body =  writerIncludeBefore options ++ main ++
              writerIncludeAfter options
  let toc  =  if writerTableOfContents options
                 then "\\tableofcontents\n\n"
                 else "" 
  let foot = if writerStandalone options
                then "\n\\end{document}\n"
                else ""
  return $ head ++ toc ++ body ++ foot

-- | Insert bibliographic information into LaTeX header.
latexHeader :: WriterOptions -- ^ Options, including LaTeX header
            -> Meta          -- ^ Meta with bibliographic information
            -> State WriterState String
latexHeader options (Meta title authors date) = do
  titletext    <- if null title
                     then return "" 
                     else do title' <- inlineListToLaTeX title
                             return $ "\\title{" ++ title' ++ "}\n"
  extras       <- get >>= (return . unlines . S.toList)
  let authorstext = if null authors
                       then "" 
                       else "\\author{" ++ (joinWithSep "\\\\" 
                            (map stringToLaTeX authors)) ++ "}\n"
  let datetext  = if date == ""
                     then "" 
                     else "\\date{" ++ stringToLaTeX date ++ "}\n"
  let maketitle = if null title then "" else "\\maketitle\n" 
  let verbatim  = if "\\usepackage{fancyvrb}" `isInfixOf` extras
                     then "\\VerbatimFootnotes % allows verbatim text in footnotes\n"
                     else ""
  let secnumline = if (writerNumberSections options)
                      then "" 
                      else "\\setcounter{secnumdepth}{0}\n" 
  let baseHeader = writerHeader options
  let header     = baseHeader ++ extras
  return $ header ++ secnumline ++ verbatim ++ titletext ++ authorstext ++
           datetext ++ "\\begin{document}\n" ++ maketitle ++ "\n"

-- escape things as needed for LaTeX

stringToLaTeX :: String -> String
stringToLaTeX = escapeStringUsing latexEscapes
  where latexEscapes = backslashEscapes "{}$%&_#" ++ 
                       [ ('^', "\\^{}")
                       , ('\\', "\\textbackslash{}")
                       , ('~', "\\ensuremath{\\sim}")
                       , ('|', "\\textbar{}")
                       , ('<', "\\textless{}")
                       , ('>', "\\textgreater{}")
                       ]

-- | Remove all code elements from list of inline elements
-- (because it's illegal to have a \\verb inside a command argument)
deVerb :: [Inline] -> [Inline]
deVerb [] = []
deVerb ((Code str):rest) = 
  (Str $ stringToLaTeX str):(deVerb rest)
deVerb (other:rest) = other:(deVerb rest)

-- | Convert Pandoc block element to LaTeX.
blockToLaTeX :: Block     -- ^ Block to convert
             -> State WriterState String 
blockToLaTeX Null = return ""
blockToLaTeX (Plain lst) = (inlineListToLaTeX lst) >>= (return . (++ "\n"))
blockToLaTeX (Para lst) = (inlineListToLaTeX lst) >>= (return . (++ "\n\n"))
blockToLaTeX (BlockQuote lst) = do
  contents <- blockListToLaTeX lst
  return $ "\\begin{quote}\n" ++ contents ++ "\\end{quote}\n"
blockToLaTeX (CodeBlock str) = return $ 
  "\\begin{verbatim}\n" ++ str ++ "\n\\end{verbatim}\n"
blockToLaTeX (RawHtml str) = return ""
blockToLaTeX (BulletList lst) = do
  items <- mapM listItemToLaTeX lst
  return $ "\\begin{itemize}\n" ++ concat items ++ "\\end{itemize}\n"
blockToLaTeX (OrderedList lst) = do
  items <- mapM listItemToLaTeX lst
  return $ "\\begin{enumerate}\n" ++ concat items ++ "\\end{enumerate}\n"
blockToLaTeX (DefinitionList lst) = do
  items <- mapM defListItemToLaTeX lst
  return $ "\\begin{description}\n" ++ concat items ++ "\\end{description}\n"
blockToLaTeX HorizontalRule = return $
    "\\begin{center}\\rule{3in}{0.4pt}\\end{center}\n\n"
blockToLaTeX (Header level lst) = do
  text <- inlineListToLaTeX (deVerb lst)
  return $ if (level > 0) && (level <= 3)
              then "\\" ++ (concat (replicate (level - 1) "sub")) ++ 
                   "section{" ++ text ++ "}\n\n"
              else text ++ "\n\n"
blockToLaTeX (Table caption aligns widths heads rows) = do
  headers <- tableRowToLaTeX heads
  captionText <- inlineListToLaTeX caption
  rows' <- mapM tableRowToLaTeX rows
  let colWidths = map (printf "%.2f") widths
  let colDescriptors = concat $ zipWith
                                (\width align -> ">{\\PBS" ++ 
                                (case align of 
                                       AlignLeft -> "\\raggedright"
                                       AlignRight -> "\\raggedleft"
                                       AlignCenter -> "\\centering"
                                       AlignDefault -> "\\raggedright") ++
                                "\\hspace{0pt}}p{" ++ width ++ 
                                "\\textwidth}")
                                colWidths aligns
  let tableBody = "\\begin{tabular}{" ++ colDescriptors ++ "}\n" ++
                  headers ++ "\\hline\n" ++ concat rows' ++ "\\end{tabular}\n" 
  let centered str   = "\\begin{center}\n" ++ str ++ "\\end{center}\n"
  addToHeader "\\usepackage{array}\n\
      \% This is needed because raggedright in table elements redefines \\\\:\n\
      \\\newcommand{\\PreserveBackslash}[1]{\\let\\temp=\\\\#1\\let\\\\=\\temp}\n\
      \\\let\\PBS=\\PreserveBackslash"
  return $ if null captionText
              then centered tableBody ++ "\n"
              else "\\begin{table}[h]\n" ++ centered tableBody ++ "\\caption{" ++
                   captionText ++ "}\n" ++ "\\end{table}\n\n" 

blockListToLaTeX lst = mapM blockToLaTeX lst >>= (return . concat)

tableRowToLaTeX cols = 
  mapM blockListToLaTeX cols >>= (return . (++ "\\\\\n") . (joinWithSep " & "))

listItemToLaTeX lst = blockListToLaTeX lst >>= (return . ("\\item "++)) 

defListItemToLaTeX (term, def) = do
    term' <- inlineListToLaTeX (deVerb term)
    def'  <- blockListToLaTeX def
    return $ "\\item[" ++ term' ++ "] " ++ def'

-- | Convert list of inline elements to LaTeX.
inlineListToLaTeX :: [Inline]  -- ^ Inlines to convert
                  -> State WriterState String
inlineListToLaTeX lst = 
  mapM inlineToLaTeX lst >>= (return . concat)

isQuoted :: Inline -> Bool
isQuoted (Quoted _ _) = True
isQuoted Apostrophe = True
isQuoted _ = False

-- | Convert inline element to LaTeX
inlineToLaTeX :: Inline    -- ^ Inline to convert
              -> State WriterState String
inlineToLaTeX (Emph lst) = do
  contents <- inlineListToLaTeX (deVerb lst)
  return $ "\\emph{" ++ contents ++ "}"
inlineToLaTeX (Strong lst) = do
  contents <- inlineListToLaTeX (deVerb lst)
  return $ "\\textbf{" ++ contents ++ "}"
inlineToLaTeX (Strikeout lst) = do
  contents <- inlineListToLaTeX (deVerb lst)
  addToHeader "\\usepackage[normalem]{ulem}"
  return $ "\\sout{" ++ contents ++ "}"
inlineToLaTeX (Superscript lst) = do
  contents <- inlineListToLaTeX (deVerb lst)
  return $ "\\textsuperscript{" ++ contents ++ "}"
inlineToLaTeX (Subscript lst) = do
  contents <- inlineListToLaTeX (deVerb lst)
  -- oddly, latex includes \textsuperscript but not \textsubscript
  -- so we have to define it:
  addToHeader "\\newcommand{\\textsubscript}[1]{\\ensuremath{_{\\scriptsize\\textrm{#1}}}}"
  return $ "\\textsubscript{" ++ contents ++ "}"
inlineToLaTeX (Code str) = return $ "\\verb" ++ [chr] ++ stuffing ++ [chr]
                     where stuffing = str 
                           chr      = ((enumFromTo '!' '~') \\ stuffing) !! 0
inlineToLaTeX (Quoted SingleQuote lst) = do
  contents <- inlineListToLaTeX lst
  let s1 = if (not (null lst)) && (isQuoted (head lst)) then "\\," else ""
  let s2 = if (not (null lst)) && (isQuoted (last lst)) then "\\," else ""
  return $ "`" ++ s1 ++ contents ++ s2 ++ "'"
inlineToLaTeX (Quoted DoubleQuote lst) = do
  contents <- inlineListToLaTeX lst
  let s1 = if (not (null lst)) && (isQuoted (head lst)) then "\\," else ""
  let s2 = if (not (null lst)) && (isQuoted (last lst)) then "\\," else ""
  return $ "``" ++ s1 ++ contents ++ s2 ++ "''"
inlineToLaTeX Apostrophe = return "'"
inlineToLaTeX EmDash = return "---"
inlineToLaTeX EnDash = return "--"
inlineToLaTeX Ellipses = return "\\ldots{}"
inlineToLaTeX (Str str) = return $ stringToLaTeX str
inlineToLaTeX (TeX str) = return str
inlineToLaTeX (HtmlInline str) = return ""
inlineToLaTeX (LineBreak) = return "\\\\\n"
inlineToLaTeX Space = return " "
inlineToLaTeX (Link text (src, tit)) = do
  contents <- inlineListToLaTeX (deVerb text)
  addToHeader "\\usepackage[breaklinks=true]{hyperref}"
  return $ "\\href{" ++ src ++ "}{" ++ contents ++ "}"
inlineToLaTeX (Image alternate (source, tit)) = do
  addToHeader "\\usepackage{graphicx}"
  return $ "\\includegraphics{" ++ source ++ "}" 
inlineToLaTeX (Note contents) = do
  addToHeader "\\usepackage{fancyvrb}"
  contents' <- blockListToLaTeX contents
  return $ "\\footnote{" ++ stripTrailingNewlines contents'  ++ "}"

