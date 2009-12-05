{-
Copyright (C) 2006-8 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-8 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha 
   Portability : portable

Conversion of 'Pandoc' format into LaTeX.
-}
module Text.Pandoc.Writers.LaTeX ( writeLaTeX ) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Printf ( printf )
import Data.List ( (\\), isSuffixOf, intercalate )
import Data.Char ( toLower )
import qualified Data.Set as S
import Control.Monad.State
import Text.PrettyPrint.HughesPJ hiding ( Str )

data WriterState = 
  WriterState { stIncludes :: S.Set String  -- strings to include in header
              , stInNote   :: Bool          -- @True@ if we're in a note
              , stOLLevel  :: Int           -- level of ordered list nesting
              , stOptions  :: WriterOptions -- writer options, so they don't have to be parameter 
              }

-- | Add line to header.
addToHeader :: String -> State WriterState ()
addToHeader str = do
  st <- get
  let includes = stIncludes st
  put st {stIncludes = S.insert str includes}

-- | Convert Pandoc to LaTeX.
writeLaTeX :: WriterOptions -> Pandoc -> String
writeLaTeX options document = 
  render $ evalState (pandocToLaTeX options document) $ 
  WriterState { stIncludes = S.empty, stInNote = False, stOLLevel = 1, stOptions = options } 

pandocToLaTeX :: WriterOptions -> Pandoc -> State WriterState Doc
pandocToLaTeX options (Pandoc meta blocks) = do
  main     <- blockListToLaTeX blocks
  head'    <- if writerStandalone options
                 then latexHeader options meta
                 else return empty
  let before = if null (writerIncludeBefore options)
                  then empty
                  else text (writerIncludeBefore options)
  let after  = if null (writerIncludeAfter options)
                  then empty
                  else text (writerIncludeAfter options)
  let body = before $$ main $$ after
  let toc  =  if writerTableOfContents options
                 then text "\\tableofcontents\n"
                 else empty 
  let foot = if writerStandalone options
                then text "\\end{document}"
                else empty 
  return $ head' $$ toc $$ body $$ foot

-- | Insert bibliographic information into LaTeX header.
latexHeader :: WriterOptions -- ^ Options, including LaTeX header
            -> Meta          -- ^ Meta with bibliographic information
            -> State WriterState Doc
latexHeader options (Meta title authors date) = do
  titletext <- if null title
                  then return empty
                  else inlineListToLaTeX title >>= return . inCmd "title"
  headerIncludes <- get >>= return . S.toList . stIncludes
  let extras = text $ unlines headerIncludes
  let verbatim  = if "\\usepackage{fancyvrb}" `elem` headerIncludes
                     then text "\\VerbatimFootnotes % allows verbatim text in footnotes"
                     else empty
  let authorstext = text $ "\\author{" ++ 
                    intercalate "\\\\" (map stringToLaTeX authors) ++ "}"
  let datetext  = if date == ""
                     then empty 
                     else text $ "\\date{" ++ stringToLaTeX date ++ "}"
  let maketitle = if null title then empty else text "\\maketitle"
  let secnumline = if (writerNumberSections options)
                      then empty 
                      else text "\\setcounter{secnumdepth}{0}"
  let baseHeader = text $ writerHeader options
  let header     = baseHeader $$ extras
  return $ header $$ secnumline $$ verbatim $$ titletext $$ authorstext $$
           datetext $$ text "\\begin{document}" $$ maketitle $$ text ""

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
                       , ('\160', "~")
                       ]

-- | Puts contents into LaTeX command.
inCmd :: String -> Doc -> Doc
inCmd cmd contents = char '\\' <> text cmd <> braces contents

-- | Remove all code elements from list of inline elements
-- (because it's illegal to have verbatim inside some command arguments)
deVerb :: [Inline] -> [Inline]
deVerb [] = []
deVerb ((Code str):rest) = 
  (TeX $ "\\texttt{" ++ stringToLaTeX str ++ "}"):(deVerb rest)
deVerb (other:rest) = other:(deVerb rest)

-- | Convert Pandoc block element to LaTeX.
blockToLaTeX :: Block     -- ^ Block to convert
             -> State WriterState Doc
blockToLaTeX Null = return empty
blockToLaTeX (Plain lst) = do
  st <- get
  let opts = stOptions st
  wrapTeXIfNeeded opts True inlineListToLaTeX lst
blockToLaTeX (Para lst) = do
  st <- get
  let opts = stOptions st
  result <- wrapTeXIfNeeded opts True inlineListToLaTeX lst
  return $ result <> char '\n'
blockToLaTeX (BlockQuote lst) = do
  contents <- blockListToLaTeX lst
  return $ text "\\begin{quote}" $$ contents $$ text "\\end{quote}"
blockToLaTeX (CodeBlock (_,classes,_) str) = do
  st <- get
  env <- if writerLiterateHaskell (stOptions st) && "haskell" `elem` classes &&
                    "literate" `elem` classes
            then return "code"
            else if stInNote st
                    then do addToHeader "\\usepackage{fancyvrb}"
                            return "Verbatim"
                    else return "verbatim"
  return $ text ("\\begin{" ++ env ++ "}\n") <> text str <> 
           text ("\n\\end{" ++ env ++ "}")
blockToLaTeX (RawHtml _) = return empty
blockToLaTeX (BulletList lst) = do
  items <- mapM listItemToLaTeX lst
  return $ text "\\begin{itemize}" $$ vcat items $$ text "\\end{itemize}"
blockToLaTeX (OrderedList (start, numstyle, numdelim) lst) = do
  st <- get
  let oldlevel = stOLLevel st
  put $ st {stOLLevel = oldlevel + 1}
  items <- mapM listItemToLaTeX lst
  modify (\s -> s {stOLLevel = oldlevel})
  exemplar <- if numstyle /= DefaultStyle || numdelim /= DefaultDelim
                 then do addToHeader "\\usepackage{enumerate}"
                         return $ char '[' <> 
                                  text (head (orderedListMarkers (1, numstyle,
                                  numdelim))) <> char ']'
                 else return empty
  let resetcounter = if start /= 1 && oldlevel <= 4
                        then text $ "\\setcounter{enum" ++ 
                             map toLower (toRomanNumeral oldlevel) ++
                             "}{" ++ show (start - 1) ++ "}"
                        else empty 
  return $ text "\\begin{enumerate}" <> exemplar $$ resetcounter $$
           vcat items $$ text "\\end{enumerate}"
blockToLaTeX (DefinitionList lst) = do
  items <- mapM defListItemToLaTeX lst
  return $ text "\\begin{description}" $$ vcat items $$
           text "\\end{description}"
blockToLaTeX HorizontalRule = return $ text $
    "\\begin{center}\\rule{3in}{0.4pt}\\end{center}\n"
blockToLaTeX (Header level lst) = do
  let lst' = deVerb lst
  txt <- inlineListToLaTeX lst'
  let noNote (Note _) = Str ""
      noNote x        = x
  let lstNoNotes = processWith noNote lst'
  -- footnotes in sections don't work unless you specify an optional
  -- argument:  \section[mysec]{mysec\footnote{blah}}
  optional <- if lstNoNotes == lst'
                 then return empty
                 else do
                   res <- inlineListToLaTeX lstNoNotes
                   return $ char '[' <> res <> char ']'
  return $ if (level > 0) && (level <= 3)
              then text ("\\" ++ (concat (replicate (level - 1) "sub")) ++ 
                   "section") <> optional <> char '{' <> txt <> text "}\n"
              else txt <> char '\n'
blockToLaTeX (Table caption aligns widths heads rows) = do
  headers <- tableRowToLaTeX heads
  captionText <- inlineListToLaTeX caption
  rows' <- mapM tableRowToLaTeX rows
  let colDescriptors = concat $ zipWith toColDescriptor widths aligns
  let tableBody = text ("\\begin{tabular}{" ++ colDescriptors ++ "}") $$
                  headers $$ text "\\hline" $$ vcat rows' $$ 
                  text "\\end{tabular}" 
  let centered txt = text "\\begin{center}" $$ txt $$ text "\\end{center}"
  addToHeader $ "\\usepackage{array}\n" ++
    "% This is needed because raggedright in table elements redefines \\\\:\n" ++
    "\\newcommand{\\PreserveBackslash}[1]{\\let\\temp=\\\\#1\\let\\\\=\\temp}\n" ++
    "\\let\\PBS=\\PreserveBackslash"
  return $ if isEmpty captionText
              then centered tableBody <> char '\n'
              else text "\\begin{table}[h]" $$ centered tableBody $$ 
                   inCmd "caption" captionText $$ text "\\end{table}\n" 

toColDescriptor :: Double -> Alignment -> String
toColDescriptor 0 align =
  case align of
         AlignLeft    -> "l"
         AlignRight   -> "r"
         AlignCenter  -> "c"
         AlignDefault -> "l"
toColDescriptor width align = ">{\\PBS" ++
  (case align of
         AlignLeft -> "\\raggedright"
         AlignRight -> "\\raggedleft"
         AlignCenter -> "\\centering"
         AlignDefault -> "\\raggedright") ++
  "\\hspace{0pt}}p{" ++ printf "%.2f" width ++
  "\\columnwidth}"

blockListToLaTeX :: [Block] -> State WriterState Doc
blockListToLaTeX lst = mapM blockToLaTeX lst >>= return . vcat

tableRowToLaTeX :: [[Block]] -> State WriterState Doc
tableRowToLaTeX cols = mapM blockListToLaTeX cols >>= 
  return . ($$ text "\\\\") . foldl (\row item -> row $$
  (if isEmpty row then text "" else text " & ") <> item) empty

listItemToLaTeX :: [Block] -> State WriterState Doc
listItemToLaTeX lst = blockListToLaTeX lst >>= return .  (text "\\item" $$) .
                      (nest 2)

defListItemToLaTeX :: ([Inline], [Block]) -> State WriterState Doc
defListItemToLaTeX (term, def) = do
    term' <- inlineListToLaTeX $ deVerb term
    def'  <- blockListToLaTeX def
    return $ text "\\item[" <> term' <> text "]" $$ def'

-- | Convert list of inline elements to LaTeX.
inlineListToLaTeX :: [Inline]  -- ^ Inlines to convert
                  -> State WriterState Doc
inlineListToLaTeX lst = mapM inlineToLaTeX lst >>= return . hcat

isQuoted :: Inline -> Bool
isQuoted (Quoted _ _) = True
isQuoted Apostrophe = True
isQuoted _ = False

-- | Convert inline element to LaTeX
inlineToLaTeX :: Inline    -- ^ Inline to convert
              -> State WriterState Doc
inlineToLaTeX (Emph lst) =
  inlineListToLaTeX (deVerb lst) >>= return . inCmd "emph"
inlineToLaTeX (Strong lst) = 
  inlineListToLaTeX (deVerb lst) >>= return . inCmd "textbf" 
inlineToLaTeX (Strikeout lst) = do
  contents <- inlineListToLaTeX $ deVerb lst
  addToHeader "\\usepackage[normalem]{ulem}"
  return $ inCmd "sout" contents
inlineToLaTeX (Superscript lst) = 
  inlineListToLaTeX (deVerb lst) >>= return . inCmd "textsuperscript"
inlineToLaTeX (Subscript lst) = do
  contents <- inlineListToLaTeX $ deVerb lst
  -- oddly, latex includes \textsuperscript but not \textsubscript
  -- so we have to define it (using a different name so as not to conflict with memoir class):
  addToHeader "\\newcommand{\\textsubscr}[1]{\\ensuremath{_{\\scriptsize\\textrm{#1}}}}"
  return $ inCmd "textsubscr" contents
inlineToLaTeX (SmallCaps lst) =
  inlineListToLaTeX (deVerb lst) >>= return . inCmd "textsc"
inlineToLaTeX (Cite _ lst) =
  inlineListToLaTeX lst
inlineToLaTeX (Code str) = do
  st <- get
  if stInNote st
     then do addToHeader "\\usepackage{fancyvrb}"
     else return ()
  let chr = ((enumFromTo '!' '~') \\ str) !! 0
  return $ text $ "\\verb" ++ [chr] ++ str ++ [chr]
inlineToLaTeX (Quoted SingleQuote lst) = do
  contents <- inlineListToLaTeX lst
  let s1 = if (not (null lst)) && (isQuoted (head lst))
              then text "\\,"
              else empty 
  let s2 = if (not (null lst)) && (isQuoted (last lst))
              then text "\\,"
              else empty
  return $ char '`' <> s1 <> contents <> s2 <> char '\''
inlineToLaTeX (Quoted DoubleQuote lst) = do
  contents <- inlineListToLaTeX lst
  let s1 = if (not (null lst)) && (isQuoted (head lst))
              then text "\\,"
              else empty 
  let s2 = if (not (null lst)) && (isQuoted (last lst))
              then text "\\,"
              else empty
  return $ text "``" <> s1 <> contents <> s2 <> text "''"
inlineToLaTeX Apostrophe = return $ char '\''
inlineToLaTeX EmDash = return $ text "---"
inlineToLaTeX EnDash = return $ text "--"
inlineToLaTeX Ellipses = return $ text "\\ldots{}"
inlineToLaTeX (Str str) = return $ text $ stringToLaTeX str
inlineToLaTeX (Math InlineMath str) = return $ char '$' <> text str <> char '$'
inlineToLaTeX (Math DisplayMath str) = return $ text "\\[" <> text str <> text "\\]"
inlineToLaTeX (TeX str) = return $ text str
inlineToLaTeX (HtmlInline _) = return empty
inlineToLaTeX (LineBreak) = return $ text "\\\\" 
inlineToLaTeX Space = return $ char ' '
inlineToLaTeX (Link txt (src, _)) = do
  addToHeader "\\usepackage[breaklinks=true]{hyperref}"
  case txt of
        [Code x] | x == src ->  -- autolink
             do addToHeader "\\usepackage{url}" 
                return $ text $ "\\url{" ++ x ++ "}"
        _ -> do contents <- inlineListToLaTeX $ deVerb txt
                return $ text ("\\href{" ++ src ++ "}{") <> contents <> 
                         char '}'
inlineToLaTeX (Image _ (source, _)) = do
  addToHeader "\\usepackage{graphicx}"
  return $ text $ "\\includegraphics{" ++ source ++ "}" 
inlineToLaTeX (Note contents) = do
  st <- get
  put (st {stInNote = True})
  contents' <- blockListToLaTeX contents
  modify (\s -> s {stInNote = False})
  let rawnote = stripTrailingNewlines $ render contents'
  -- note: a \n before } is needed when note ends with a Verbatim environment
  let optNewline = "\\end{Verbatim}" `isSuffixOf` rawnote
  return $ text "\\footnote{" <> 
           text rawnote <> (if optNewline then char '\n' else empty) <> char '}'
