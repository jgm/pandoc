{-
Copyright (C) 2008 John MacFarlane and Peter Wang

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
   Module      : Text.Pandoc.Writers.Texinfo
   Copyright   : Copyright (C) 2008 John MacFarlane and Peter Wang
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha 
   Portability : portable

Conversion of 'Pandoc' format into Texinfo.
-}
module Text.Pandoc.Writers.Texinfo ( writeTexinfo ) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Printf ( printf )
import Data.List ( (\\), isSuffixOf )
import Data.Char ( toLower, chr, ord )
import qualified Data.Set as S
import Control.Monad.State
import Text.PrettyPrint.HughesPJ hiding ( Str )

data WriterState = 
  WriterState { stIncludes :: S.Set String  -- strings to include in header
              }

{- TODO:
 - internal cross references a la HTML
 - generated .texi files don't work when run through texi2dvi
 -}

-- | Add line to header.
addToHeader :: String -> State WriterState ()
addToHeader str = do
  st <- get
  let includes = stIncludes st
  put st {stIncludes = S.insert str includes}

-- | Convert Pandoc to Texinfo.
writeTexinfo :: WriterOptions -> Pandoc -> String
writeTexinfo options document = 
  render $ evalState (pandocToTexinfo options $ wrapTop document) $ 
  WriterState { stIncludes = S.empty } 

-- | Add a "Top" node around the document, needed by Texinfo.
wrapTop (Pandoc (Meta title authors date) blocks) =
  Pandoc (Meta title authors date) (Header 0 title : blocks)

pandocToTexinfo :: WriterOptions -> Pandoc -> State WriterState Doc
pandocToTexinfo options (Pandoc meta blocks) = do
  main     <- blockListToTexinfo blocks
  head     <- if writerStandalone options
                 then texinfoHeader options meta
                 else return empty
  let before = if null (writerIncludeBefore options)
                  then empty
                  else text (writerIncludeBefore options)
  let after  = if null (writerIncludeAfter options)
                  then empty
                  else text (writerIncludeAfter options)
  let body = before $$ main $$ after
  -- XXX toc untested
  let toc  =  if writerTableOfContents options
                 then text "@contents"
                 else empty 
  let foot = if writerStandalone options
                then text "@bye"
                else empty 
  return $ head $$ toc $$ body $$ foot

-- | Insert bibliographic information into Texinfo header.
texinfoHeader :: WriterOptions -- ^ Options, including Texinfo header
              -> Meta          -- ^ Meta with bibliographic information
              -> State WriterState Doc
texinfoHeader options (Meta title authors date) = do
  titletext <- if null title
                  then return empty
                  else do
                    t <- inlineListToTexinfo title
                    return $ text "@title " <> t
  headerIncludes <- get >>= return . S.toList . stIncludes
  let extras = text $ unlines headerIncludes
  let authorstext = map makeAuthor authors
  let datetext  = if date == ""
                     then empty 
                     else text $ stringToTexinfo date

  let baseHeader = text $ writerHeader options
  let header     = baseHeader $$ extras
  return $ text "\\input texinfo" $$
           header $$
	   text "@ifnottex" $$
	   text "@paragraphindent 0" $$
	   text "@end ifnottex" $$
           text "@titlepage" $$
           titletext $$ vcat authorstext $$
           datetext $$
           text "@end titlepage"

makeAuthor author = text $ "@author " ++ (stringToTexinfo author)

-- | Escape things as needed for Texinfo.
stringToTexinfo :: String -> String
stringToTexinfo = escapeStringUsing texinfoEscapes
  where texinfoEscapes = [ ('{', "@{")
                         , ('}', "@}")
                         , ('@', "@@")
                         , (',', "@comma{}") -- only needed in argument lists
                         ]

-- | Puts contents into Texinfo command.
inCmd :: String -> Doc -> Doc
inCmd cmd contents = char '@' <> text cmd <> braces contents

-- | Remove all code elements from list of inline elements
-- (because it's illegal to have verbatim inside some command arguments)
-- XXX not sure about this
deVerb :: [Inline] -> [Inline]
deVerb [] = []
deVerb ((Code str):rest) = (Code $ stringToTexinfo str):(deVerb rest)
deVerb (other:rest) = other:(deVerb rest)

-- | Convert Pandoc block element to Texinfo.
blockToTexinfo :: Block     -- ^ Block to convert
	       -> State WriterState Doc

blockToTexinfo Null = return empty

blockToTexinfo (Plain lst) =
  inlineListToTexinfo lst

blockToTexinfo (Para lst) = do
  result <- inlineListToTexinfo lst
  return $ result <> char '\n'

blockToTexinfo (BlockQuote lst) = do
  contents <- blockListToTexinfo lst
  return $ text "@quotation" $$
           contents $$
           text "@end quotation"

blockToTexinfo (CodeBlock _ str) = do
  -- XXX a paragraph followed by verbatim looks better if there is no blank
  -- line between the paragraph and verbatim, otherwise there is extra blank
  -- line in makeinfo output.
  return $ text "@verbatim" $$
           vcat (map text (lines str)) $$
           text "@end verbatim\n"

blockToTexinfo (RawHtml str) = return empty

blockToTexinfo (BulletList lst) = do
  items <- mapM listItemToTexinfo lst
  return $ text "@itemize" $$
           vcat items $$
           text "@end itemize\n"

blockToTexinfo (OrderedList (start, numstyle, numdelim) lst) = do
  items <- mapM listItemToTexinfo lst
  return $ text "@enumerate " <> exemplar $$
           vcat items $$
           text "@end enumerate"
  where
    exemplar = case numstyle of
                DefaultStyle -> decimal
                Decimal      -> decimal
                UpperRoman   -> decimal   -- Roman numerals not supported
                LowerRoman   -> decimal
                UpperAlpha   -> upperAlpha
                LowerAlpha   -> lowerAlpha
    decimal = if start == 1
                 then empty
                 else text (show start)
    upperAlpha = text [chr $ ord 'A' + start - 1]
    lowerAlpha = text [chr $ ord 'a' + start - 1]

blockToTexinfo (DefinitionList lst) = do
  items <- mapM defListItemToTexinfo lst
  return $ text "@table @asis" $$
           vcat items $$
           text "@end table"

blockToTexinfo HorizontalRule =
    -- XXX can't get the equivalent from LaTeX.hs to work
    return $ text "@iftex" $$
             text "@bigskip@hrule@bigskip" $$
	     text "@end iftex" $$
             text "@ifnottex" $$
	     text (take 72 $ repeat '-') $$
             text "@end ifnottex"

blockToTexinfo (Header 0 lst) = do
  txt <- if null lst
            then return $ text "Top"
            else inlineListToTexinfo (deVerb lst)
  return $ text "@node Top" $$
           text "@top " <> txt <> char '\n'

blockToTexinfo (Header level lst) = do
  node <- inlineListForNode (deVerb lst)
  txt <- inlineListToTexinfo (deVerb lst)
  return $ if (level > 0) && (level <= 4)
              then text "\n@node " <> node <> char '\n' <>
                   text (seccmd level) <> txt
              else txt
  where
    seccmd 1 = "@chapter "
    seccmd 2 = "@section "
    seccmd 3 = "@subsection "
    seccmd 4 = "@subsubsection "

blockToTexinfo (Table caption aligns widths heads rows) = do
  headers <- tableHeadToTexinfo aligns heads
  captionText <- inlineListToTexinfo (deVerb caption)
  rowsText <- mapM (tableRowToTexinfo aligns) rows
  let colWidths = map (printf "%.2f ") widths
  let colDescriptors = concat colWidths
  let tableBody = text ("@multitable @columnfractions " ++ colDescriptors) $$
                  headers $$
                  vcat rowsText $$ 
                  text "@end multitable" 
  return $ if isEmpty captionText
              then tableBody <> char '\n'
              else text "@float" $$
                   tableBody $$ 
                   inCmd "caption" captionText $$
                   text "@end float"

tableHeadToTexinfo = tableAnyRowToTexinfo "@headitem "

tableRowToTexinfo = tableAnyRowToTexinfo "@item "

tableAnyRowToTexinfo :: String
                     -> [Alignment]
                     -> [[Block]]
                     -> State WriterState Doc
tableAnyRowToTexinfo itemtype aligns cols =
  zipWithM alignedBlock aligns cols >>= 
  return . (text itemtype $$) . foldl (\row item -> row $$
  (if isEmpty row then empty else text " @tab ") <> item) empty

alignedBlock :: Alignment
             -> [Block]
             -> State WriterState Doc
-- XXX @flushleft and @flushright text won't get word wrapped.  Since word
-- wrapping is more important than alignment, we ignore the alignment.
alignedBlock _ = blockListToTexinfo
{-
alignedBlock AlignLeft col = do
  b <- blockListToTexinfo col
  return $ text "@flushleft" $$ b $$ text "@end flushleft"
alignedBlock AlignRight col = do
  b <- blockListToTexinfo col
  return $ text "@flushright" $$ b $$ text "@end flushright"
alignedBlock _ col = blockListToTexinfo col
-}

-- | Convert Pandoc block elements to Texinfo.
blockListToTexinfo :: [Block]
                   -> State WriterState Doc
blockListToTexinfo [] = return $ empty
blockListToTexinfo (x:xs) = do
  x' <- blockToTexinfo x
  case x of
    (Header level _) -> do
      -- We need need to insert a menu for this node.
      let (before, after) = break isHeader xs
      before' <- blockListToTexinfo before
      let menu = if level < 4
                    then collectNodes (level + 1) after
                    else []
      lines <- mapM makeMenuLine menu
      let menu' = if null lines
                    then empty
                    else text "@menu" $$
                         vcat lines $$
                         text "@end menu"
      after' <- blockListToTexinfo after
      return $ x' $$ before' $$ menu' $$ after'
    _ -> do
      xs' <- blockListToTexinfo xs
      return $ x' $$ xs'

isHeader (Header _ _) = True
isHeader _            = False

collectNodes level [] = []
collectNodes level (x:xs) =
  case x of
    (Header hl _) ->
      if hl < level
         then []
         else if hl == level
                 then x : collectNodes level xs
                 else collectNodes level xs
    _ ->
      collectNodes level xs

makeMenuLine :: Block
             -> State WriterState Doc
makeMenuLine (Header _ lst) = do
  txt <- inlineListForNode (deVerb lst)
  return $ text "* " <> txt <> text "::"

listItemToTexinfo :: [Block]
                  -> State WriterState Doc
listItemToTexinfo lst = blockListToTexinfo lst >>=
                        return . (text "@item" $$) 

defListItemToTexinfo :: ([Inline], [Block])
                     -> State WriterState Doc
defListItemToTexinfo (term, def) = do
    term' <- inlineListToTexinfo $ deVerb term
    def'  <- blockListToTexinfo def
    return $ text "@item " <> term' <> text "\n" $$ def'

-- | Convert list of inline elements to Texinfo.
inlineListToTexinfo :: [Inline]  -- ^ Inlines to convert
                  -> State WriterState Doc
inlineListToTexinfo lst = mapM inlineToTexinfo lst >>= return . hcat

-- | Convert list of inline elements to Texinfo acceptable for a node name.
inlineListForNode :: [Inline]  -- ^ Inlines to convert
                  -> State WriterState Doc
inlineListForNode lst = mapM inlineForNode lst >>= return . hcat

inlineForNode (Str str) = return $ text $ filter (not.disallowedInNode) str
inlineForNode (Emph lst) = inlineListForNode (deVerb lst)
inlineForNode (Strong lst) = inlineListForNode (deVerb lst)
inlineForNode (Strikeout lst) = inlineListForNode (deVerb lst)
inlineForNode (Superscript lst) = inlineListForNode (deVerb lst)
inlineForNode (Subscript lst) = inlineListForNode (deVerb lst)
inlineForNode (Quoted _ lst) = inlineListForNode (deVerb lst)
inlineForNode (Code str) = inlineForNode (Str str)
inlineForNode Space = return $ char ' '
inlineForNode EmDash = return $ text "---"
inlineForNode EnDash = return $ text "--"
inlineForNode Apostrophe = return $ char '\''
inlineForNode Ellipses = return $ text "..."
inlineForNode LineBreak = return empty
inlineForNode (Math _) = return empty
inlineForNode (TeX _) = return empty
inlineForNode (HtmlInline _) = return empty
inlineForNode (Link lst _) = inlineListForNode (deVerb lst)
inlineForNode (Image lst _) = inlineListForNode (deVerb lst)
inlineForNode (Note _) = return empty

-- XXX not sure what the complete set of illegal characters is.
disallowedInNode '.' = True
disallowedInNode ',' = True
disallowedInNode _ = False

-- | Convert inline element to Texinfo
inlineToTexinfo :: Inline    -- ^ Inline to convert
                -> State WriterState Doc

inlineToTexinfo (Emph lst) =
  inlineListToTexinfo (deVerb lst) >>= return . inCmd "emph"

inlineToTexinfo (Strong lst) = 
  inlineListToTexinfo (deVerb lst) >>= return . inCmd "strong" 

inlineToTexinfo (Strikeout lst) = do
  addToHeader $ "@macro textstrikeout{text}\n" ++
                "~~\\text\\~~\n" ++
                "@end macro\n"
  contents <- inlineListToTexinfo $ deVerb lst
  return $ text "@textstrikeout{" <> contents <> text "}"

inlineToTexinfo (Superscript lst) = do
  addToHeader $ "@macro textsuperscript{text}\n" ++
                "@iftex\n" ++
                "@textsuperscript{\\text\\}\n" ++
                "@end iftex\n" ++
                "@ifnottex\n" ++
                "^@{\\text\\@}\n" ++
                "@end ifnottex\n" ++
                "@end macro\n"
  contents <- inlineListToTexinfo $ deVerb lst
  return $ text "@textsuperscript{" <> contents <> char '}'

inlineToTexinfo (Subscript lst) = do
  addToHeader $ "@macro textsubscript{text}\n" ++
                "@iftex\n" ++
                "@textsubscript{\\text\\}\n" ++
                "@end iftex\n" ++
                "@ifnottex\n" ++
                "_@{\\text\\@}\n" ++
                "@end ifnottex\n" ++
                "@end macro\n"
  contents <- inlineListToTexinfo $ deVerb lst
  return $ text "@textsubscript{" <> contents <> char '}'

inlineToTexinfo (Code str) = do
  let chr = ((enumFromTo '!' '~') \\ str) !! 0
  return $ text $ "@verb{" ++ [chr] ++ str ++ [chr] ++ "}"

inlineToTexinfo (Quoted SingleQuote lst) = do
  contents <- inlineListToTexinfo lst
  return $ char '`' <> contents <> char '\''

inlineToTexinfo (Quoted DoubleQuote lst) = do
  contents <- inlineListToTexinfo lst
  return $ text "``" <> contents <> text "''"

inlineToTexinfo Apostrophe = return $ char '\''
inlineToTexinfo EmDash = return $ text "---"
inlineToTexinfo EnDash = return $ text "--"
inlineToTexinfo Ellipses = return $ text "@dots{}"
inlineToTexinfo (Str str) = return $ text (stringToTexinfo str)
inlineToTexinfo (Math str) = return $ inCmd "math" $ text str
inlineToTexinfo (TeX str) = return $ text "@tex" $$ text str $$ text "@end tex"
inlineToTexinfo (HtmlInline str) = return empty
inlineToTexinfo (LineBreak) = return $ text "@*"
inlineToTexinfo Space = return $ char ' '

inlineToTexinfo (Link txt (src, _)) = do
  case txt of
        [Code x] | x == src ->  -- autolink
             do return $ text $ "@url{" ++ x ++ "}"
        _ -> do contents <- inlineListToTexinfo $ deVerb txt
                let src1 = stringToTexinfo src
                return $ text ("@uref{" ++ src1 ++ ",") <> contents <>
                         char '}'

inlineToTexinfo (Image alternate (source, tit)) = do
  content <- inlineListToTexinfo $ deVerb alternate
  return $ text ("@image{" ++ base ++ ",,,") <> content <> text "," <>
           text (ext ++ "}")
  where
    (revext, revbase) = break (=='.') (reverse source)
    ext  = reverse revext
    base = case revbase of
            ('.' : rest) -> reverse rest
            _            -> reverse revbase

inlineToTexinfo (Note contents) = do
  contents' <- blockListToTexinfo contents
  let rawnote = stripTrailingNewlines $ render contents'
  let optNewline = "@end verbatim" `isSuffixOf` rawnote
  return $ text "@footnote{" <>
           text rawnote <>
           (if optNewline then char '\n' else empty) <>
           char '}'
