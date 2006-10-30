-- | Convert Pandoc to LaTeX.
module Text.Pandoc.Writers.LaTeX ( 
                                  writeLaTeX 
                                 ) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import List ( (\\) )

-- | Convert Pandoc to LaTeX.
writeLaTeX :: WriterOptions -> Pandoc -> String
writeLaTeX options (Pandoc meta blocks) = 
    let notes = filter isNoteBlock blocks in  -- assumes all notes are at outer level
    let body = (writerIncludeBefore options) ++ 
               (concatMap (blockToLaTeX notes) (replaceReferenceLinks blocks)) ++ 
               (writerIncludeAfter options) in
    let head = if writerStandalone options then 
                   latexHeader notes options meta
               else 
                   ""  in
    let foot = if writerStandalone options then "\n\\end{document}\n" else "" in
    head ++ body ++ foot

-- | Insert bibliographic information into LaTeX header.
latexHeader ::  [Block]   -- ^ List of note blocks to use in resolving note refs
            -> WriterOptions  -- ^ Options, including LaTeX header
            -> Meta       -- ^ Meta with bibliographic information
            -> String
latexHeader notes options (Meta title authors date) =
    let titletext = if null title then 
                        "" 
                    else 
                        "\\title{" ++ inlineListToLaTeX notes title ++ "}\n"
        authorstext = if null authors then 
                          "" 
                      else 
                          "\\author{" ++ (joinWithSep "\\\\" (map stringToLaTeX authors)) ++ "}\n"
        datetext = if date == "" then 
                       "" 
                   else 
                       "\\date{" ++ stringToLaTeX date ++ "}\n"
        maketitle = if null title then 
                        "" 
                    else 
                        "\\maketitle\n" 
        secnumline = if (writerNumberSections options) then 
                         "" 
                     else 
                         "\\setcounter{secnumdepth}{0}\n" 
        header     = writerHeader options in
    header ++ secnumline ++ titletext ++ authorstext ++ datetext ++ "\\begin{document}\n" ++ maketitle


-- escape things as needed for LaTeX (also ldots, dashes, quotes, etc.) 

escapeBrackets  = backslashEscape "{}"
escapeSpecial   = backslashEscape "$%&~_#"

escapeBackslash = gsub "\\\\" "\\\\textbackslash{}" 
fixBackslash    = gsub "\\\\textbackslash\\\\\\{\\\\\\}" "\\\\textbackslash{}"
escapeHat       = gsub "\\^" "\\\\^{}"
escapeBar       = gsub "\\|" "\\\\textbar{}"
escapeLt        = gsub "<" "\\\\textless{}"
escapeGt        = gsub ">" "\\\\textgreater{}"

escapeDoubleQuotes = 
    gsub "\"" "''" . -- rest are right quotes
    gsub "([[:space:]])\"" "\\1``" . -- never right quote after space 
    gsub "\"('|`)([^[:punct:][:space:]])" "``{}`\\2" . -- "'word left
    gsub "\"([^[:punct:][:space:]])" "``\\1"  -- "word left
    
escapeSingleQuotes =
    gsub "('|`)(\"|``)" "`{}``" .  -- '"word left
    gsub "([^[:punct:][:space:]])`(s|S)" "\\1'\\2" . -- catch possessives 
    gsub "^'([^[:punct:][:space:]])" "`\\1" .  -- 'word left 
    gsub "([[:space:]])'" "\\1`" . -- never right quote after space 
    gsub "([[:space:]])'([^[:punct:][:space:]])" "\\1`\\2" -- 'word left (leave possessives)

escapeEllipses = gsub "\\.\\.\\.|\\. \\. \\." "\\ldots{}" 

escapeDashes = gsub "([0-9])-([0-9])" "\\1--\\2" .
               gsub " *--- *" "---" .
               gsub "([^-])--([^-])" "\\1---\\2" 

escapeSmart = escapeDashes . escapeSingleQuotes . escapeDoubleQuotes . escapeEllipses 

-- | Escape string for LaTeX (including smart quotes, dashes, ellipses)
stringToLaTeX :: String -> String
stringToLaTeX = escapeSmart . escapeGt . escapeLt . escapeBar . escapeHat . 
                escapeSpecial . fixBackslash . escapeBrackets . escapeBackslash 

-- | Remove all code elements from list of inline elements
-- (because it's illegal to have a \\verb inside a command argument)
deVerb :: [Inline] -> [Inline]
deVerb [] = []
deVerb ((Code str):rest) = (Str str):(deVerb rest)
deVerb (other:rest) = other:(deVerb rest)

-- | Convert Pandoc block element to LaTeX.
blockToLaTeX :: [Block]   -- ^ List of note blocks to use in resolving note refs
             -> Block     -- ^ Block to convert
             -> String 
blockToLaTeX notes Blank = "\n" 
blockToLaTeX notes Null = ""
blockToLaTeX notes (Plain lst) = inlineListToLaTeX notes lst ++ "\n"
blockToLaTeX notes (Para lst) = (inlineListToLaTeX notes lst) ++ "\n\n"
blockToLaTeX notes (BlockQuote lst) = 
    "\\begin{quote}\n" ++ (concatMap (blockToLaTeX notes) lst) ++ "\\end{quote}\n"
blockToLaTeX notes (Note ref lst) = ""
blockToLaTeX notes (Key _ _) = ""
blockToLaTeX notes (CodeBlock str) = "\\begin{verbatim}\n" ++ str ++ "\\end{verbatim}\n"
blockToLaTeX notes (RawHtml str) = ""
blockToLaTeX notes (BulletList lst) = 
    "\\begin{itemize}\n" ++ (concatMap (listItemToLaTeX notes) lst) ++ "\\end{itemize}\n"
blockToLaTeX notes (OrderedList lst) = 
    "\\begin{enumerate}\n" ++ (concatMap (listItemToLaTeX notes) lst) ++ "\\end{enumerate}\n"
blockToLaTeX notes HorizontalRule = "\\begin{center}\\rule{3in}{0.4pt}\\end{center}\n\n"
blockToLaTeX notes (Header level lst) = 
    if (level > 0) && (level <= 3) then
        "\\" ++ (concat (replicate (level - 1) "sub")) ++ "section{" ++ 
                 (inlineListToLaTeX notes (deVerb lst)) ++ "}\n\n"
    else 
        (inlineListToLaTeX notes lst) ++ "\n\n"
listItemToLaTeX notes list = "\\item " ++ (concatMap (blockToLaTeX notes) list) 

-- | Convert list of inline elements to LaTeX.
inlineListToLaTeX :: [Block]   -- ^ List of note blocks to use in resolving note refs
                  -> [Inline]  -- ^ Inlines to convert
                  -> String
inlineListToLaTeX notes lst = 
    -- first, consolidate Str and Space for more effective smartquotes:
    let lst' = consolidateList lst in
    concatMap (inlineToLaTeX notes) lst'

-- | Convert inline element to LaTeX
inlineToLaTeX :: [Block]   -- ^ List of note blocks to use in resolving note refs
              -> Inline    -- ^ Inline to convert
              -> String
inlineToLaTeX notes (Emph lst) = "\\emph{" ++ (inlineListToLaTeX notes (deVerb lst)) ++ "}"
inlineToLaTeX notes (Strong lst) = "\\textbf{" ++ (inlineListToLaTeX notes (deVerb lst)) ++ "}"
inlineToLaTeX notes (Code str) = "\\verb" ++ [chr] ++ stuffing ++ [chr]
                        where stuffing = str 
                              chr      = ((enumFromTo '!' '~') \\ stuffing) !! 0
inlineToLaTeX notes (Str str) = stringToLaTeX str
inlineToLaTeX notes (TeX str) = str
inlineToLaTeX notes (HtmlInline str) = ""
inlineToLaTeX notes (LineBreak) = "\\\\\n"
inlineToLaTeX notes Space = " "
inlineToLaTeX notes (Link text (Src src tit)) = 
    "\\href{" ++ src ++ "}{" ++ (inlineListToLaTeX notes (deVerb text)) ++ "}"
inlineToLaTeX notes (Link text (Ref [])) = "[" ++ (inlineListToLaTeX notes text) ++ "]"
inlineToLaTeX notes (Link text (Ref ref)) = "[" ++ (inlineListToLaTeX notes text) ++ "][" ++ 
       (inlineListToLaTeX notes ref) ++ "]"  -- this is what markdown does, for better or worse
inlineToLaTeX notes (Image alternate (Src source tit)) = "\\includegraphics{" ++ source ++ "}" 
inlineToLaTeX notes (Image alternate (Ref [])) = 
    "![" ++ (inlineListToLaTeX notes alternate) ++ "]" 
inlineToLaTeX notes (Image alternate (Ref ref)) = 
    "![" ++ (inlineListToLaTeX notes alternate) ++ "][" ++ (inlineListToLaTeX notes ref) ++ "]"
inlineToLaTeX [] (NoteRef ref) = ""
inlineToLaTeX ((Note firstref firstblocks):rest) (NoteRef ref) = 
    if (firstref == ref) then
        "\\footnote{" ++ (stripTrailingNewlines (concatMap (blockToLaTeX rest) firstblocks))  ++ "}"
    else
        inlineToLaTeX rest (NoteRef ref) 

