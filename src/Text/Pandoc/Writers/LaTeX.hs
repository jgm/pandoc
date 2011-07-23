{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2006-2010 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-2010 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha 
   Portability : portable

Conversion of 'Pandoc' format into LaTeX.
-}
module Text.Pandoc.Writers.LaTeX ( writeLaTeX ) where
import Text.Pandoc.Definition
import Text.Pandoc.Generic
import Text.Pandoc.Shared
import Text.Pandoc.Templates
import Text.Printf ( printf )
import Network.URI ( isAbsoluteURI, unEscapeString )
import Data.List ( (\\), isSuffixOf, isInfixOf,
                   isPrefixOf, intercalate, intersperse )
import Data.Char ( toLower, isPunctuation )
import Control.Monad.State
import Text.Pandoc.Pretty
import System.FilePath (dropExtension)

data WriterState = 
  WriterState { stInNote     :: Bool          -- @True@ if we're in a note
              , stInTable    :: Bool          -- @True@ if we're in a table
              , stTableNotes :: [(Char, Doc)] -- List of markers, notes
                                              -- in current table
              , stOLLevel    :: Int           -- level of ordered list nesting
              , stOptions    :: WriterOptions -- writer options, so they don't have to be parameter 
              , stVerbInNote :: Bool          -- true if document has verbatim text in note
              , stEnumerate  :: Bool          -- true if document needs fancy enumerated lists
              , stTable      :: Bool          -- true if document has a table
              , stStrikeout  :: Bool          -- true if document has strikeout
              , stSubscript  :: Bool          -- true if document has subscript
              , stUrl        :: Bool          -- true if document has visible URL link
              , stGraphics   :: Bool          -- true if document contains images
              , stLHS        :: Bool          -- true if document has literate haskell code
              , stBook       :: Bool          -- true if document uses book or memoir class
              , stCsquotes   :: Bool          -- true if document uses csquotes
              }

-- | Convert Pandoc to LaTeX.
writeLaTeX :: WriterOptions -> Pandoc -> String
writeLaTeX options document = 
  evalState (pandocToLaTeX options document) $ 
  WriterState { stInNote = False, stInTable = False,
                stTableNotes = [], stOLLevel = 1, stOptions = options,
                stVerbInNote = False, stEnumerate = False,
                stTable = False, stStrikeout = False, stSubscript = False,
                stUrl = False, stGraphics = False,
                stLHS = False, stBook = writerChapters options,
                stCsquotes = False }

pandocToLaTeX :: WriterOptions -> Pandoc -> State WriterState String
pandocToLaTeX options (Pandoc (Meta title authors date) blocks) = do
  let template = writerTemplate options
  let usesBookClass x = "\\documentclass" `isPrefixOf` x &&
         ("{memoir}" `isSuffixOf` x || "{book}" `isSuffixOf` x ||
          "{report}" `isSuffixOf` x)
  when (any usesBookClass (lines template)) $
    modify $ \s -> s{stBook = True}
  -- check for \usepackage...{csquotes}; if present, we'll use
  -- \enquote{...} for smart quotes:
  when ("{csquotes}" `isInfixOf` template) $
    modify $ \s -> s{stCsquotes = True}
  opts <- liftM stOptions get
  let colwidth = if writerWrapText opts
                    then Just $ writerColumns opts
                    else Nothing
  titletext <- liftM (render colwidth) $ inlineListToLaTeX title
  authorsText <- mapM (liftM (render colwidth) . inlineListToLaTeX) authors
  dateText <- liftM (render colwidth) $ inlineListToLaTeX date
  let (blocks', lastHeader) = if writerCiteMethod options == Citeproc then
                                (blocks, [])
                              else case last blocks of
                                Header 1 il -> (init blocks, il)
                                _           -> (blocks, [])
  body <- blockListToLaTeX blocks'
  biblioTitle <- liftM (render colwidth) $ inlineListToLaTeX lastHeader
  let main = render colwidth body
  st <- get
  let biblioFiles = intercalate "," $ map dropExtension $  writerBiblioFiles options
      citecontext = case writerCiteMethod options of
                         Natbib   -> [ ("biblio-files", biblioFiles)
                                     , ("biblio-title", biblioTitle)
                                     , ("natbib", "yes")
                                     ]
                         Biblatex -> [ ("biblio-files", biblioFiles)
                                     , ("biblio-title", biblioTitle)
                                     , ("biblatex", "yes")
                                     ]
                         _      -> []
      context  = writerVariables options ++
                 [ ("toc", if writerTableOfContents options then "yes" else "")
                 , ("body", main)
                 , ("title", titletext)
                 , ("date", dateText) ] ++
                 [ ("author", a) | a <- authorsText ] ++
                 [ ("verbatim-in-note", "yes") | stVerbInNote st ] ++
                 [ ("fancy-enums", "yes") | stEnumerate st ] ++
                 [ ("tables", "yes") | stTable st ] ++
                 [ ("strikeout", "yes") | stStrikeout st ] ++
                 [ ("subscript", "yes") | stSubscript st ] ++
                 [ ("url", "yes") | stUrl st ] ++
                 [ ("numbersections", "yes") | writerNumberSections options ] ++
                 [ ("lhs", "yes") | stLHS st ] ++
                 [ ("graphics", "yes") | stGraphics st ] ++
                 [ ("book-class", "yes") | stBook st] ++
                 [ ("listings", "yes") | writerListings options ] ++
                 citecontext
  return $ if writerStandalone options
              then renderTemplate context template
              else main

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
                       , ('[', "{[}")  -- to avoid interpretation as
                       , (']', "{]}")  -- optional arguments
                       , ('\160', "~")
                       , ('\x2018', "`")
                       , ('\x2019', "'")
                       , ('\x201C', "``")
                       , ('\x201D', "''")
                       ]

-- | Puts contents into LaTeX command.
inCmd :: String -> Doc -> Doc
inCmd cmd contents = char '\\' <> text cmd <> braces contents

-- | Convert Pandoc block element to LaTeX.
blockToLaTeX :: Block     -- ^ Block to convert
             -> State WriterState Doc
blockToLaTeX Null = return empty
blockToLaTeX (Plain lst) = inlineListToLaTeX lst
blockToLaTeX (Para [Image txt (src,tit)]) = do
  capt <- inlineListToLaTeX txt
  img <- inlineToLaTeX (Image txt (src,tit))
  return $ "\\begin{figure}[htbp]" $$ "\\centering" $$ img $$
           ("\\caption{" <> capt <> char '}') $$ "\\end{figure}" $$ blankline
blockToLaTeX (Para lst) = do
  result <- inlineListToLaTeX lst
  return $ result <> blankline
blockToLaTeX (BlockQuote lst) = do
  contents <- blockListToLaTeX lst
  return $ "\\begin{quote}" $$ contents $$ "\\end{quote}"
blockToLaTeX (CodeBlock (_,classes,keyvalAttr) str) = do
  st <- get
  env <- if writerLiterateHaskell (stOptions st) && "haskell" `elem` classes &&
                    "literate" `elem` classes
            then do
              modify $ \s -> s{ stLHS = True }
              return "code"
            else if writerListings (stOptions st)
                    then return "lstlisting"
                    else if stInNote st
                            then do
                              modify $ \s -> s{ stVerbInNote = True }
                              return "Verbatim"
                            else return "verbatim"
  let params = if writerListings (stOptions st)
               then take 1
                    [ "language=" ++ lang | lang <- classes
                    , lang `elem` ["ABAP","IDL","Plasm","ACSL","inform"
                                  ,"POV","Ada","Java","Prolog","Algol"
                                  ,"JVMIS","Promela","Ant","ksh","Python"
                                  ,"Assembler","Lisp","R","Awk","Logo"
                                  ,"Reduce","bash","make","Rexx","Basic"
                                  ,"Mathematica","RSL","C","Matlab","Ruby"
                                  ,"C++","Mercury","S","Caml","MetaPost"
                                  ,"SAS","Clean","Miranda","Scilab","Cobol"
                                  ,"Mizar","sh","Comal","ML","SHELXL","csh"
                                  ,"Modula-2","Simula","Delphi","MuPAD"
                                  ,"SQL","Eiffel","NASTRAN","tcl","Elan"
                                  ,"Oberon-2","TeX","erlang","OCL"
                                  ,"VBScript","Euphoria","Octave","Verilog"
                                  ,"Fortran","Oz","VHDL","GCL","Pascal"
                                  ,"VRML","Gnuplot","Perl","XML","Haskell"
	                          ,"PHP","XSLT","HTML","PL/I"]
                    ] ++ 
                    [ key ++ "=" ++ attr | (key,attr) <- keyvalAttr ]
               else []
      printParams 
          | null params = empty
          | otherwise   = "[" <> hsep (intersperse "," (map text params)) <>
                          "]"
  return $ flush ("\\begin{" <> text env <> "}" <> printParams $$ text str $$
           "\\end{" <> text env <> "}") $$ cr
           -- final cr needed because of footnotes
blockToLaTeX (RawBlock "latex" x) = return $ text x <> blankline
blockToLaTeX (RawBlock _ _) = return empty
blockToLaTeX (BulletList lst) = do
  items <- mapM listItemToLaTeX lst
  return $ "\\begin{itemize}" $$ vcat items $$ "\\end{itemize}"
blockToLaTeX (OrderedList (start, numstyle, numdelim) lst) = do
  st <- get
  let oldlevel = stOLLevel st
  put $ st {stOLLevel = oldlevel + 1}
  items <- mapM listItemToLaTeX lst
  modify (\s -> s {stOLLevel = oldlevel})
  exemplar <- if numstyle /= DefaultStyle || numdelim /= DefaultDelim
                 then do
                   modify $ \s -> s{ stEnumerate = True }
                   return $ char '[' <> 
                       text (head (orderedListMarkers (1, numstyle,
                            numdelim))) <> char ']'
                 else return empty
  let resetcounter = if start /= 1 && oldlevel <= 4
                        then text $ "\\setcounter{enum" ++ 
                             map toLower (toRomanNumeral oldlevel) ++
                             "}{" ++ show (start - 1) ++ "}"
                        else empty 
  return $ "\\begin{enumerate}" <> exemplar $$ resetcounter $$
           vcat items $$ "\\end{enumerate}"
blockToLaTeX (DefinitionList lst) = do
  items <- mapM defListItemToLaTeX lst
  return $ "\\begin{description}" $$ vcat items $$ "\\end{description}"
blockToLaTeX HorizontalRule = return $
  "\\begin{center}\\rule{3in}{0.4pt}\\end{center}" $$ blankline
blockToLaTeX (Header level lst) = do
  txt <- inlineListToLaTeX lst
  let noNote (Note _) = Str ""
      noNote x        = x
  let lstNoNotes = bottomUp noNote lst
  -- footnotes in sections don't work unless you specify an optional
  -- argument:  \section[mysec]{mysec\footnote{blah}}
  optional <- if lstNoNotes == lst
                 then return empty
                 else do
                   res <- inlineListToLaTeX lstNoNotes
                   return $ char '[' <> res <> char ']'
  let stuffing = optional <> char '{' <> txt <> char '}'
  book <- liftM stBook get
  let level' = if book then level - 1 else level
  let headerWith x y = text x <> y $$ blankline
  return $ case level' of
                0  -> headerWith "\\chapter" stuffing
                1  -> headerWith "\\section" stuffing
                2  -> headerWith "\\subsection" stuffing
                3  -> headerWith "\\subsubsection" stuffing
                4  -> headerWith "\\paragraph" stuffing
                5  -> headerWith "\\subparagraph" stuffing
                _            -> txt $$ blankline
blockToLaTeX (Table caption aligns widths heads rows) = do
  modify $ \s -> s{ stInTable = True, stTableNotes = [] }
  headers <- if all null heads
                then return empty
                else liftM ($$ "\\ML")
                     $ (tableRowToLaTeX True aligns widths) heads
  captionText <- inlineListToLaTeX caption
  let capt = if isEmpty captionText
                then empty
                else text "caption = " <> captionText <> "," <> space
  rows' <- mapM (tableRowToLaTeX False aligns widths) rows
  let rows'' = intersperse ("\\\\\\noalign{\\medskip}") rows'
  tableNotes <- liftM (reverse . stTableNotes) get
  let toNote (marker, x) = "\\tnote" <> brackets (char marker) <>
                            braces (nest 2 x)
  let notes = vcat $ map toNote tableNotes
  let colDescriptors = text $ concat $ map toColDescriptor aligns
  let tableBody =
         ("\\ctable" <> brackets (capt <> text "pos = H, center, botcap"))
         <> braces colDescriptors
         $$ braces ("% notes" <> cr <> notes <> cr)
         $$ braces (text "% rows" $$ "\\FL" $$
                     vcat (headers : rows'') $$ "\\LL" <> cr)
  modify $ \s -> s{ stTable = True, stInTable = False, stTableNotes = [] }
  return $ tableBody $$ blankline

toColDescriptor :: Alignment -> String
toColDescriptor align =
  case align of
         AlignLeft    -> "l"
         AlignRight   -> "r"
         AlignCenter  -> "c"
         AlignDefault -> "l"

blockListToLaTeX :: [Block] -> State WriterState Doc
blockListToLaTeX lst = mapM blockToLaTeX lst >>= return . vcat

tableRowToLaTeX :: Bool
                -> [Alignment]
                -> [Double]
                -> [[Block]]
                -> State WriterState Doc
tableRowToLaTeX header aligns widths cols = do
  renderedCells <- mapM blockListToLaTeX cols
  let valign = text $ if header then "[b]" else "[t]"
  let halign x = case x of
                  AlignLeft    -> "\\raggedright"
                  AlignRight   -> "\\raggedleft"
                  AlignCenter  -> "\\centering"
                  AlignDefault -> "\\raggedright"
  let toCell 0 _ c = c
      toCell w a c = "\\parbox" <> valign <>
                     braces (text (printf "%.2f\\columnwidth" w)) <>
                     braces (halign a <> cr <> c <> cr)
  let cells = zipWith3 toCell widths aligns renderedCells
  return $ hcat $ intersperse (" & ") cells

listItemToLaTeX :: [Block] -> State WriterState Doc
listItemToLaTeX lst = blockListToLaTeX lst >>= return .  (text "\\item" $$) .
                      (nest 2)

defListItemToLaTeX :: ([Inline], [[Block]]) -> State WriterState Doc
defListItemToLaTeX (term, defs) = do
    term' <- inlineListToLaTeX term
    def'  <- liftM vsep $ mapM blockListToLaTeX defs
    return $ "\\item" <> brackets term' $$ def'

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
  inlineListToLaTeX lst >>= return . inCmd "emph"
inlineToLaTeX (Strong lst) = 
  inlineListToLaTeX lst >>= return . inCmd "textbf" 
inlineToLaTeX (Strikeout lst) = do
  contents <- inlineListToLaTeX lst
  modify $ \s -> s{ stStrikeout = True }
  return $ inCmd "sout" contents
inlineToLaTeX (Superscript lst) =
  inlineListToLaTeX lst >>= return . inCmd "textsuperscript"
inlineToLaTeX (Subscript lst) = do
  modify $ \s -> s{ stSubscript = True }
  contents <- inlineListToLaTeX lst
  -- oddly, latex includes \textsuperscript but not \textsubscript
  -- so we have to define it (using a different name so as not to conflict with memoir class):
  return $ inCmd "textsubscr" contents
inlineToLaTeX (SmallCaps lst) =
  inlineListToLaTeX lst >>= return . inCmd "textsc"
inlineToLaTeX (Cite cits lst) = do
  st <- get
  let opts = stOptions st
  case writerCiteMethod opts of
     Natbib   -> citationsToNatbib cits
     Biblatex -> citationsToBiblatex cits
     _        -> inlineListToLaTeX lst

inlineToLaTeX (Code _ str) = do
  st <- get
  if writerListings (stOptions st)
    then do
      when (stInNote st) $ modify $ \s -> s{ stVerbInNote = True }
      let chr = ((enumFromTo '!' '~') \\ str) !! 0
      return $ text $ "\\lstinline" ++ [chr] ++ str ++ [chr]
    else return $ text $ "\\texttt{" ++ stringToLaTeX str ++ "}"
inlineToLaTeX (Quoted SingleQuote lst) = do
  contents <- inlineListToLaTeX lst
  csquotes <- liftM stCsquotes get
  if csquotes
     then return $ "\\enquote" <> braces contents
     else do
       let s1 = if (not (null lst)) && (isQuoted (head lst))
                   then "\\,"
                   else empty
       let s2 = if (not (null lst)) && (isQuoted (last lst))
                   then "\\,"
                   else empty
       return $ char '`' <> s1 <> contents <> s2 <> char '\''
inlineToLaTeX (Quoted DoubleQuote lst) = do
  contents <- inlineListToLaTeX lst
  csquotes <- liftM stCsquotes get
  if csquotes
     then return $ "\\enquote" <> braces contents
     else do
       let s1 = if (not (null lst)) && (isQuoted (head lst))
                   then "\\,"
                   else empty
       let s2 = if (not (null lst)) && (isQuoted (last lst))
                   then "\\,"
                   else empty
       return $ "``" <> s1 <> contents <> s2 <> "''"
inlineToLaTeX Apostrophe = return $ char '\''
inlineToLaTeX EmDash = return "---"
inlineToLaTeX EnDash = return "--"
inlineToLaTeX Ellipses = return "\\ldots{}"
inlineToLaTeX (Str str) = return $ text $ stringToLaTeX str
inlineToLaTeX (Math InlineMath str) = return $ char '$' <> text str <> char '$'
inlineToLaTeX (Math DisplayMath str) = return $ "\\[" <> text str <> "\\]"
inlineToLaTeX (RawInline "latex" str) = return $ text str
inlineToLaTeX (RawInline "tex" str) = return $ text str
inlineToLaTeX (RawInline _ _) = return empty
inlineToLaTeX (LineBreak) = return "\\\\"
inlineToLaTeX Space = return space
inlineToLaTeX (Link txt (src, _)) =
  case txt of
        [Code _ x] | x == src ->  -- autolink
             do modify $ \s -> s{ stUrl = True }
                return $ text $ "\\url{" ++ x ++ "}"
        _ -> do contents <- inlineListToLaTeX txt
                return $ text ("\\href{" ++ stringToLaTeX src ++ "}{") <>
                         contents <> char '}'
inlineToLaTeX (Image _ (source, _)) = do
  modify $ \s -> s{ stGraphics = True }
  let source' = if isAbsoluteURI source
                   then source
                   else unEscapeString source
  return $ "\\includegraphics" <> braces (text source')
inlineToLaTeX (Note contents) = do
  modify (\s -> s{stInNote = True})
  contents' <- blockListToLaTeX contents
  modify (\s -> s {stInNote = False})
  inTable <- liftM stInTable get
  if inTable
     then do
       curnotes <- liftM stTableNotes get
       let marker = cycle ['a'..'z'] !! length curnotes
       modify $ \s -> s{ stTableNotes = (marker, contents') : curnotes }
       return $ "\\tmark" <> brackets (char marker) <> space
     else return $ "\\footnote" <> braces (nest 2 contents')
     -- note: a \n before } needed when note ends with a Verbatim environment

citationsToNatbib :: [Citation] -> State WriterState Doc
citationsToNatbib (one:[])
  = citeCommand c p s k
  where
    Citation { citationId = k
             , citationPrefix = p
             , citationSuffix = s
             , citationMode = m
             }
      = one
    c = case m of
             AuthorInText     -> "citet"
             SuppressAuthor  -> "citeyearpar"
             NormalCitation -> "citep"

citationsToNatbib cits
  | noPrefix (tail cits) && noSuffix (init cits) && ismode NormalCitation cits
  = citeCommand "citep" p s ks
  where
     noPrefix  = and . map (null . citationPrefix)
     noSuffix  = and . map (null . citationSuffix)
     ismode m  = and . map (((==) m)  . citationMode)
     p         = citationPrefix  $ head $ cits
     s         = citationSuffix  $ last $ cits
     ks        = intercalate ", " $ map citationId cits

citationsToNatbib (c:cs) | citationMode c == AuthorInText = do
     author <- citeCommand "citeauthor" [] [] (citationId c)
     cits   <- citationsToNatbib (c { citationMode = SuppressAuthor } : cs)
     return $ author <+> cits

citationsToNatbib cits = do
  cits' <- mapM convertOne cits
  return $ text "\\citetext{" <> foldl combineTwo empty cits' <> text "}"
  where
    combineTwo a b | isEmpty a = b
                   | otherwise = a <> text "; " <> b
    convertOne Citation { citationId = k
                        , citationPrefix = p
                        , citationSuffix = s
                        , citationMode = m
                        }
        = case m of
               AuthorInText   -> citeCommand "citealt"  p s k
               SuppressAuthor -> citeCommand "citeyear" p s k
               NormalCitation -> citeCommand "citealp"  p s k

citeCommand :: String -> [Inline] -> [Inline] -> String -> State WriterState Doc
citeCommand c p s k = do
  args <- citeArguments p s k
  return $ text ("\\" ++ c) <> args

citeArguments :: [Inline] -> [Inline] -> String -> State WriterState Doc
citeArguments p s k = do
  let s' = case s of
        (Str (x:[]) : r) | isPunctuation x -> dropWhile (== Space) r
        (Str (x:xs) : r) | isPunctuation x -> Str xs : r
        _                                  -> s
  pdoc <- inlineListToLaTeX p
  sdoc <- inlineListToLaTeX s'
  let optargs = case (isEmpty pdoc, isEmpty sdoc) of
                     (True, True ) -> empty
                     (True, False) -> brackets sdoc
                     (_   , _    ) -> brackets pdoc <> brackets sdoc
  return $ optargs <> braces (text k)

citationsToBiblatex :: [Citation] -> State WriterState Doc
citationsToBiblatex (one:[])
  = citeCommand cmd p s k
    where
       Citation { citationId = k
                , citationPrefix = p
                , citationSuffix = s
                , citationMode = m
                } = one
       cmd = case m of
                  SuppressAuthor -> "autocite*"
                  AuthorInText   -> "textcite"
                  NormalCitation -> "autocite"

citationsToBiblatex (c:cs) = do
  args <- mapM convertOne (c:cs)
  return $ text cmd <> foldl (<>) empty args
    where
       cmd = case citationMode c of
                  AuthorInText -> "\\textcites"
                  _            -> "\\autocites"
       convertOne Citation { citationId = k
                           , citationPrefix = p
                           , citationSuffix = s
                           }
              = citeArguments p s k

citationsToBiblatex _ = return empty
