{-
Copyright (C) 2007 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.ConTeXt
   Copyright   : Copyright (C) 2006-7 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha 
   Portability : portable

Conversion of 'Pandoc' format into ConTeXt.
-}
module Text.Pandoc.Writers.ConTeXt ( 
                                  writeConTeXt 
                                 ) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Printf ( printf )
import Data.List ( (\\), intersperse )
import Control.Monad.State

type WriterState = Int -- number of next URL reference 

-- | Convert Pandoc to ConTeXt.
writeConTeXt :: WriterOptions -> Pandoc -> String
writeConTeXt options document = 
  evalState (pandocToConTeXt options document) 1 

pandocToConTeXt :: WriterOptions -> Pandoc -> State WriterState String
pandocToConTeXt options (Pandoc meta blocks) = do
  main    <- blockListToConTeXt blocks 
  let body = writerIncludeBefore options ++ main ++ writerIncludeAfter options
  head    <- if writerStandalone options
                then contextHeader options meta
                else return ""
  let toc  = if writerTableOfContents options
                then "\\placecontent\n\n"
                else "" 
  let foot = if writerStandalone options
                then "\n\\stoptext\n"
                else ""
  return $ head ++ toc ++ body ++ foot

-- | Insert bibliographic information into ConTeXt header.
contextHeader :: WriterOptions -- ^ Options, including ConTeXt header
              -> Meta          -- ^ Meta with bibliographic information
              -> State WriterState String
contextHeader options (Meta title authors date) = do
  titletext    <- if null title
                     then return "" 
                     else inlineListToConTeXt title
  let authorstext = if null authors
                       then ""
                       else if length authors == 1
                            then stringToConTeXt $ head authors
                            else stringToConTeXt $ (joinWithSep ", " $
                                 init authors) ++ " & " ++ last authors
  let datetext   = if date == ""
                       then "" 
                       else stringToConTeXt date
  let titleblock = "\\doctitle{" ++ titletext ++ "}\n\
                   \ \\author{" ++ authorstext ++ "}\n\
                   \ \\date{" ++ datetext ++ "}\n\n"
  let setupheads = if (writerNumberSections options)
                      then "\\setupheads[sectionnumber=yes, style=\\bf]\n" 
                      else "\\setupheads[sectionnumber=no, style=\\bf]\n"
  let header     = writerHeader options
  return $ header ++ setupheads ++ titleblock ++ "\\starttext\n\\maketitle\n\n"

-- escape things as needed for ConTeXt

escapeCharForConTeXt :: Char -> String
escapeCharForConTeXt ch =
 case ch of
    '{'  -> "\\letteropenbrace{}"
    '}'  -> "\\letterclosebrace{}"
    '\\' -> "\\letterbackslash{}"
    '$'  -> "\\$"
    '|'  -> "\\letterbar{}"
    '^'  -> "\\letterhat{}"
    '%'  -> "\\%"
    '~'  -> "\\lettertilde{}"
    '&'  -> "\\&"
    '#'  -> "\\#"
    '<'  -> "\\letterless{}"
    '>'  -> "\\lettermore{}"
    '_'  -> "\\letterunderscore{}"
    x    -> [x]

-- | Escape string for ConTeXt
stringToConTeXt :: String -> String
stringToConTeXt = concatMap escapeCharForConTeXt

-- | Convert Pandoc block element to ConTeXt.
blockToConTeXt :: Block -> State WriterState String 
blockToConTeXt Null = return ""
blockToConTeXt (Plain lst) = inlineListToConTeXt lst >>= (return . (++ "\n"))
blockToConTeXt (Para lst) = inlineListToConTeXt lst >>= (return . (++ "\n\n"))
blockToConTeXt (BlockQuote lst) = do
  contents <- blockListToConTeXt lst
  return $ "\\startnarrower\n" ++ contents ++ "\\stopnarrower\n\n"
blockToConTeXt (CodeBlock str) = 
  return $ "\\starttyping\n" ++ str ++ "\n\\stoptyping\n"
blockToConTeXt (RawHtml str) = return ""
blockToConTeXt (BulletList lst) = do 
  contents <- mapM listItemToConTeXt lst
  return $ "\\startltxitem\n" ++ concat contents ++ "\\stopltxitem\n"
blockToConTeXt (OrderedList lst) = do
  contents <- mapM listItemToConTeXt lst
  return $  "\\startltxenum\n" ++ concat contents ++ "\\stopltxenum\n"
blockToConTeXt (DefinitionList lst) =
  mapM defListItemToConTeXt lst >>= (return . (++ "\n") . concat)
blockToConTeXt HorizontalRule = return "\\thinrule\n\n"
blockToConTeXt (Header level lst) = do
  contents <- inlineListToConTeXt lst
  return $ if (level > 0) && (level <= 3)
              then "\\" ++ (concat (replicate (level - 1) "sub")) ++ 
                   "section{" ++ contents ++ "}\n\n"
              else contents ++ "\n\n"
blockToConTeXt (Table caption aligns widths heads rows) = do
    let colWidths = map printDecimal widths
    let colDescriptor colWidth alignment = (case alignment of
                                               AlignLeft    -> 'l' 
                                               AlignRight   -> 'r'
                                               AlignCenter  -> 'c'
                                               AlignDefault -> 'l'):
                                           "p(" ++ colWidth ++ "\\textwidth)|"
    let colDescriptors = "|" ++ (concat $ 
                                 zipWith colDescriptor colWidths aligns)
    headers <- tableRowToConTeXt heads 
    captionText <- inlineListToConTeXt caption 
    let captionText' = if null caption then "none" else captionText
    rows' <- mapM tableRowToConTeXt rows 
    return $ "\\placetable[here]{" ++ captionText' ++ "}\n\\starttable[" ++ 
             colDescriptors ++ "]\n" ++ "\\HL\n" ++ headers ++ "\\HL\n" ++ 
             concat rows' ++ "\\HL\n\\stoptable\n\n" 

printDecimal :: Float -> String
printDecimal = printf "%.2f" 

tableRowToConTeXt cols = do
  cols' <- mapM blockListToConTeXt cols
  return $ "\\NC " ++ (concat $ intersperse "\\NC " cols') ++ "\\NC\\AR\n"

listItemToConTeXt list = do
  contents <- blockListToConTeXt list
  return $ "\\item " ++ contents

defListItemToConTeXt (term, def) = do
  term' <- inlineListToConTeXt term
  def'  <- blockListToConTeXt def
  return $ "\\startdescr{" ++ term' ++ "}\n" ++
           def' ++ "\n\\stopdescr\n"

-- | Convert list of block elements to ConTeXt.
blockListToConTeXt :: [Block] -> State WriterState String
blockListToConTeXt lst = mapM blockToConTeXt lst >>= (return . concat)

-- | Convert list of inline elements to ConTeXt.
inlineListToConTeXt :: [Inline]  -- ^ Inlines to convert
                    -> State WriterState String
inlineListToConTeXt lst = mapM inlineToConTeXt lst >>= (return . concat)

isQuoted :: Inline -> Bool
isQuoted (Quoted _ _) = True
isQuoted Apostrophe = True
isQuoted _ = False

-- | Convert inline element to ConTeXt
inlineToConTeXt :: Inline    -- ^ Inline to convert
                -> State WriterState String
inlineToConTeXt (Emph lst) = do
  contents <- inlineListToConTeXt lst
  return $ "{\\em " ++ contents ++ "}" 
inlineToConTeXt (Strong lst) = do
  contents <- inlineListToConTeXt lst
  return $ "{\\bf " ++ contents ++ "}" 
inlineToConTeXt (Code str) = return $ "\\type{" ++ str ++ "}"
inlineToConTeXt (Quoted SingleQuote lst) = do
  contents <- inlineListToConTeXt lst
  return $ "\\quote{" ++ contents ++ "}"
inlineToConTeXt (Quoted DoubleQuote lst) = do
  contents <- inlineListToConTeXt lst
  return $ "\\quotation{" ++ contents ++ "}"
inlineToConTeXt Apostrophe = return "'"
inlineToConTeXt EmDash = return "---"
inlineToConTeXt EnDash = return "--"
inlineToConTeXt Ellipses = return "\\ldots{}"
inlineToConTeXt (Str str) = return $ stringToConTeXt str
inlineToConTeXt (TeX str) = return str
inlineToConTeXt (HtmlInline str) = return ""
inlineToConTeXt (LineBreak) = return "\\hfil\\break\n"
inlineToConTeXt Space = return " "
inlineToConTeXt (Link text (src, _)) = do
  next <- get
  put (next + 1)
  let ref = show next
  label <- inlineListToConTeXt text
  return $ "\\useurl[" ++ ref ++ "][" ++ src ++ "][][" ++ label ++ 
           "]\\from[" ++ ref ++ "]" 
inlineToConTeXt (Image alternate (src, tit)) = do
  alt <- inlineListToConTeXt alternate
  return $ "\\placefigure\n[]\n[fig:" ++ alt ++ "]\n{" ++
           tit ++ "}\n{\\externalfigure[" ++ src ++ "]}" 
inlineToConTeXt (Note contents) = do
  contents' <- blockListToConTeXt contents
  return $ "\\footnote{" ++ contents' ++ "}"

