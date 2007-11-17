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
   Copyright   : Copyright (C) 2007 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha 
   Portability : portable

Conversion of 'Pandoc' format into ConTeXt.
-}
module Text.Pandoc.Writers.ConTeXt ( writeConTeXt ) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Printf ( printf )
import Data.List ( (\\), intersperse )
import Control.Monad.State
import Text.PrettyPrint.HughesPJ hiding ( Str )

type WriterState = Int -- number of next URL reference 

-- | Convert Pandoc to ConTeXt.
writeConTeXt :: WriterOptions -> Pandoc -> String
writeConTeXt options document = render $ 
  evalState (pandocToConTeXt options document) 1 

pandocToConTeXt :: WriterOptions -> Pandoc -> State WriterState Doc
pandocToConTeXt options (Pandoc meta blocks) = do
  main    <- blockListToConTeXt options blocks 
  let before = if null (writerIncludeBefore options)
                  then empty
                  else text $ writerIncludeBefore options
  let after  = if null (writerIncludeAfter options)
                  then empty
                  else text $ writerIncludeAfter options
  let body = before $$ main $$ after
  head    <- if writerStandalone options
                then contextHeader options meta
                else return empty
  let toc  = if writerTableOfContents options
                then text "\\placecontent\n"
                else empty 
  let foot = if writerStandalone options
                then text "\\stoptext\n"
                else empty 
  return $ head $$ toc $$ body $$ foot

-- | Insert bibliographic information into ConTeXt header.
contextHeader :: WriterOptions -- ^ Options, including ConTeXt header
              -> Meta          -- ^ Meta with bibliographic information
              -> State WriterState Doc
contextHeader options (Meta title authors date) = do
  titletext    <- if null title
                     then return empty 
                     else inlineListToConTeXt options title
  let authorstext = if null authors
                       then ""
                       else if length authors == 1
                            then stringToConTeXt $ head authors
                            else stringToConTeXt $ (joinWithSep ", " $
                                 init authors) ++ " & " ++ last authors
  let datetext   = if date == ""
                       then "" 
                       else stringToConTeXt date
  let titleblock = text "\\doctitle{" <> titletext <> char '}' $$
                   text ("\\author{" ++ authorstext ++ "}") $$
                   text ("\\date{" ++ datetext ++ "}")
  let header     = text $ writerHeader options
  return $ header $$ titleblock $$ text "\\starttext\n\\maketitle\n"

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
blockToConTeXt :: WriterOptions
               -> Block 
               -> State WriterState Doc 
blockToConTeXt opts Null = return empty
blockToConTeXt opts (Plain lst) = 
  wrapTeXIfNeeded opts False (inlineListToConTeXt opts) lst >>= return
blockToConTeXt opts (Para lst) = 
  wrapTeXIfNeeded opts False (inlineListToConTeXt opts) lst >>= return . (<> char '\n')
blockToConTeXt opts (BlockQuote lst) = do
  contents <- blockListToConTeXt opts lst
  return $ text "\\startblockquote\n" $$ contents $$ text "\\stopblockquote"
blockToConTeXt opts (CodeBlock str) = 
  return $ text $ "\\starttyping\n" ++ str ++ "\n\\stoptyping\n" -- \n needed
  -- because \stoptyping can't have anything after it
blockToConTeXt opts (RawHtml str) = return empty
blockToConTeXt opts (BulletList lst) = do 
  contents <- mapM (listItemToConTeXt opts) lst
  return $ text "\\startltxitem" $$ vcat contents $$ text "\\stopltxitem"
blockToConTeXt opts (OrderedList (start, style, delim) lst) = do
    contents <- mapM (listItemToConTeXt opts) lst 
    let start' = if start == 1 then "" else "start=" ++ show start
    let delim' = case delim of
                        DefaultDelim -> ""
                        Period       -> "stopper=." 
                        OneParen     -> "stopper=)" 
                        TwoParens    -> "left=(,stopper=)"
    let width = maximum $ map length $ take (length contents) 
                          (orderedListMarkers (start, style, delim))
    let width' = (toEnum width + 1) / 2
    let width'' = if width' > 1.5 
                     then "width=" ++ show width' ++ "em" 
                     else ""
    let specs2Items = filter (not . null) [start', delim', width'']
    let specs2 = if null specs2Items
                    then ""
                    else "[" ++ joinWithSep "," specs2Items ++ "]"
    let style' = case style of
                        DefaultStyle -> if null specs2 then "" else "[]"
                        Decimal      -> "[n]" 
                        LowerRoman   -> "[r]"
                        UpperRoman   -> "[R]"
                        LowerAlpha   -> "[a]"
                        UpperAlpha   -> "[A]"
    let specs = style' ++ specs2
    return $ text ("\\startitemize" ++ specs) $$ vcat contents $$ 
             text "\\stopitemize\n"
blockToConTeXt opts (DefinitionList lst) =
  mapM (defListItemToConTeXt opts) lst >>= return . (<> char '\n') . vcat
blockToConTeXt opts HorizontalRule = return $ text "\\thinrule\n"
blockToConTeXt opts (Header level lst) = do
  contents <- inlineListToConTeXt opts lst
  let base = if writerNumberSections opts then "section" else "subject"
  return $ if level > 0 && level <= 3
              then char '\\' <> text (concat (replicate (level - 1) "sub")) <> 
                   text base <> char '{' <> contents <> char '}' <> char '\n'
              else contents <> char '\n'
blockToConTeXt opts (Table caption aligns widths heads rows) = do
    let colWidths = map printDecimal widths
    let colDescriptor colWidth alignment = (case alignment of
                                               AlignLeft    -> 'l' 
                                               AlignRight   -> 'r'
                                               AlignCenter  -> 'c'
                                               AlignDefault -> 'l'):
                                           "p(" ++ colWidth ++ "\\textwidth)|"
    let colDescriptors = "|" ++ (concat $ 
                                 zipWith colDescriptor colWidths aligns)
    headers <- tableRowToConTeXt opts heads 
    captionText <- inlineListToConTeXt opts caption 
    let captionText' = if null caption then text "none" else captionText
    rows' <- mapM (tableRowToConTeXt opts) rows 
    return $ text "\\placetable[here]{" <> captionText' <> char '}' $$
             text "\\starttable[" <> text colDescriptors <> char ']' $$
             text "\\HL" $$ headers $$ text "\\HL" $$
             vcat rows' $$ text "\\HL\n\\stoptable\n" 

printDecimal :: Float -> String
printDecimal = printf "%.2f" 

tableRowToConTeXt opts cols = do
  cols' <- mapM (blockListToConTeXt opts) cols
  return $ (vcat (map (text "\\NC " <>) cols')) $$ 
           text "\\NC\\AR"

listItemToConTeXt opts list = blockListToConTeXt opts list >>=
  return . (text "\\item " $$) . (nest 2)

orderedListItemToConTeXt opts marker list = blockListToConTeXt opts list >>=
  return . (text ("\\sym{" ++ marker ++ "} ") $$) . (nest 2)

defListItemToConTeXt opts (term, def) = do
  term' <- inlineListToConTeXt opts term
  def'  <- blockListToConTeXt opts def
  return $ text "\\startdescr{" <> term' <> char '}' $$ def' $$ text "\\stopdescr"

-- | Convert list of block elements to ConTeXt.
blockListToConTeXt :: WriterOptions -> [Block] -> State WriterState Doc
blockListToConTeXt opts lst = mapM (blockToConTeXt opts) lst >>= return . vcat

-- | Convert list of inline elements to ConTeXt.
inlineListToConTeXt :: WriterOptions
                    -> [Inline]  -- ^ Inlines to convert
                    -> State WriterState Doc
inlineListToConTeXt opts lst = mapM (inlineToConTeXt opts) lst >>= return . hcat

isQuoted :: Inline -> Bool
isQuoted (Quoted _ _) = True
isQuoted Apostrophe = True
isQuoted _ = False

-- | Convert inline element to ConTeXt
inlineToConTeXt :: WriterOptions
                -> Inline    -- ^ Inline to convert
                -> State WriterState Doc
inlineToConTeXt opts (Emph lst) = do 
  contents <- inlineListToConTeXt opts lst
  return $ text "{\\em " <> contents <> char '}'
inlineToConTeXt opts (Strong lst) = do
  contents <- inlineListToConTeXt opts lst
  return $ text "{\\bf " <> contents <> char '}'
inlineToConTeXt opts (Strikeout lst) = do
  contents <- inlineListToConTeXt opts lst
  return $ text "\\overstrikes{" <> contents <> char '}'
inlineToConTeXt opts (Superscript lst) = do
  contents <- inlineListToConTeXt opts lst
  return $ text "\\high{" <> contents <> char '}'
inlineToConTeXt opts (Subscript lst) = do
  contents <- inlineListToConTeXt opts lst
  return $ text "\\low{" <> contents <> char '}'
inlineToConTeXt opts (Code str) = return $ text $ "\\type{" ++ str ++ "}"
inlineToConTeXt opts (Quoted SingleQuote lst) = do
  contents <- inlineListToConTeXt opts lst
  return $ text "\\quote{" <> contents <> char '}'
inlineToConTeXt opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToConTeXt opts lst
  return $ text "\\quotation{" <> contents <> char '}'
inlineToConTeXt opts Apostrophe = return $ char '\''
inlineToConTeXt opts EmDash = return $ text "---"
inlineToConTeXt opts EnDash = return $ text "--"
inlineToConTeXt opts Ellipses = return $ text "\\ldots{}"
inlineToConTeXt opts (Str str) = return $ text $ stringToConTeXt str
inlineToConTeXt opts (TeX str) = return $ text str
inlineToConTeXt opts (HtmlInline str) = return empty
inlineToConTeXt opts (LineBreak) = return $ text "\\crlf\n"
inlineToConTeXt opts Space = return $ char ' '
inlineToConTeXt opts (Link [Code str] (src, tit)) = -- since ConTeXt has its own 
  inlineToConTeXt opts (Link [Str str] (src, tit))  -- way of printing links... 
inlineToConTeXt opts (Link txt (src, _)) = do
  next <- get
  put (next + 1)
  let ref = show next
  label <- inlineListToConTeXt opts txt
  return $ text "\\useurl[" <> text ref <> text "][" <> text src <>
           text "][][" <> label <> text "]\\from[" <> text ref <> char ']'
inlineToConTeXt opts (Image alternate (src, tit)) = do
  alt <- inlineListToConTeXt opts alternate
  return $ text "\\placefigure\n[]\n[fig:" <> alt <> text "]\n{" <> 
           text tit <> text "}\n{\\externalfigure[" <> text src <> text "]}" 
inlineToConTeXt opts (Note contents) = do
  contents' <- blockListToConTeXt opts contents
  return $ text "    \\footnote{" <> 
           text (stripTrailingNewlines $ render contents') <> 
           char '}'

