{-
Copyright (C) 2007-8 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2007-8 John MacFarlane
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
import Data.List ( isSuffixOf )
import Control.Monad.State
import Text.PrettyPrint.HughesPJ hiding ( Str )

data WriterState = 
  WriterState { stNextRef          :: Int  -- number of next URL reference
              , stOrderedListLevel :: Int  -- level of ordered list
              , stOptions          :: WriterOptions -- writer options
              }

data BlockWrapper = Pad Doc | Reg Doc 

orderedListStyles = cycle ["[n]","[a]", "[r]", "[g]"] 

-- | Convert Pandoc to ConTeXt.
writeConTeXt :: WriterOptions -> Pandoc -> String
writeConTeXt options document = 
  let defaultWriterState = WriterState { stNextRef = 1
                                       , stOrderedListLevel = 0
                                       , stOptions = options
                                       } 
  in  render $ 
  evalState (pandocToConTeXt options document) defaultWriterState 

pandocToConTeXt :: WriterOptions -> Pandoc -> State WriterState Doc
pandocToConTeXt options (Pandoc meta blocks) = do
  main    <- blockListToConTeXt blocks 
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
blockToConTeXt :: Block 
               -> State WriterState BlockWrapper
blockToConTeXt Null = return $ Reg empty
blockToConTeXt (Plain lst) = do
  st <- get
  let options = stOptions st
  contents <- wrapTeXIfNeeded options False inlineListToConTeXt lst 
  return $ Reg contents
blockToConTeXt (Para lst) = do 
  st <- get
  let options = stOptions st
  contents <- wrapTeXIfNeeded options False inlineListToConTeXt lst
  return $ Pad contents
blockToConTeXt (BlockQuote lst) = do
  contents <- blockListToConTeXt lst
  return $ Pad $ text "\\startblockquote" $$ contents $$ text "\\stopblockquote"
blockToConTeXt (CodeBlock str) = 
  return $ Reg $ text $ "\\starttyping\n" ++ str ++ "\n\\stoptyping\n" 
  -- \n because \stoptyping can't have anything after it, inc. }
blockToConTeXt (RawHtml str) = return $ Reg empty
blockToConTeXt (BulletList lst) = do 
  contents <- mapM listItemToConTeXt lst
  return $ Pad $ text "\\startitemize" $$ vcat contents $$ text "\\stopitemize"
blockToConTeXt (OrderedList (start, style, delim) lst) = do
    st <- get
    let level = stOrderedListLevel st
    put $ st {stOrderedListLevel = level + 1}
    contents <- mapM listItemToConTeXt lst
    put $ st {stOrderedListLevel = level} 
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
                        DefaultStyle -> orderedListStyles !! level
                        Decimal      -> "[n]" 
                        LowerRoman   -> "[r]"
                        UpperRoman   -> "[R]"
                        LowerAlpha   -> "[a]"
                        UpperAlpha   -> "[A]"
    let specs = style' ++ specs2
    return $ Pad $ text ("\\startitemize" ++ specs) $$ vcat contents $$ 
             text "\\stopitemize"
blockToConTeXt (DefinitionList lst) =
  mapM defListItemToConTeXt lst >>= return . Pad . wrappedBlocksToDoc
blockToConTeXt HorizontalRule = return $ Pad $ text "\\thinrule"
blockToConTeXt (Header level lst) = do
  contents <- inlineListToConTeXt lst
  st <- get
  let opts = stOptions st
  let base = if writerNumberSections opts then "section" else "subject"
  return $ Pad $ if level >= 1 && level <= 5
                    then char '\\' <> text (concat (replicate (level - 1) "sub")) <> 
                         text base <> char '{' <> contents <> char '}'
                    else contents
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
    let captionText' = if null caption then text "none" else captionText
    rows' <- mapM tableRowToConTeXt rows 
    return $ Pad $ text "\\placetable[here]{" <> captionText' <> char '}' $$
             text "\\starttable[" <> text colDescriptors <> char ']' $$
             text "\\HL" $$ headers $$ text "\\HL" $$
             vcat rows' $$ text "\\HL\n\\stoptable"

printDecimal :: Float -> String
printDecimal = printf "%.2f" 

tableRowToConTeXt cols = do
  cols' <- mapM blockListToConTeXt cols
  return $ (vcat (map (text "\\NC " <>) cols')) $$ 
           text "\\NC\\AR"

listItemToConTeXt list = blockListToConTeXt list >>=
  return . (text "\\item " $$) . (nest 2)

orderedListItemToConTeXt marker list = blockListToConTeXt list >>=
  return . (text ("\\sym{" ++ marker ++ "} ") $$) . (nest 2)

defListItemToConTeXt (term, def) = do
  term' <- inlineListToConTeXt term
  def'  <- blockListToConTeXt def
  return $ Pad $ text "\\startdescr{" <> term' <> char '}' $$ def' $$ text "\\stopdescr"

wrappedBlocksToDoc :: [BlockWrapper] -> Doc
wrappedBlocksToDoc = foldr addBlock empty 
     where addBlock (Pad d) accum | isEmpty accum = d
           addBlock (Pad d) accum = d $$ text "" $$ accum
           addBlock (Reg d) accum = d $$ accum

-- | Convert list of block elements to ConTeXt.
blockListToConTeXt :: [Block] -> State WriterState Doc
blockListToConTeXt lst = mapM blockToConTeXt lst >>= return . wrappedBlocksToDoc

-- | Convert list of inline elements to ConTeXt.
inlineListToConTeXt :: [Inline]  -- ^ Inlines to convert
                    -> State WriterState Doc
inlineListToConTeXt lst = mapM inlineToConTeXt lst >>= return . hcat

isQuoted :: Inline -> Bool
isQuoted (Quoted _ _) = True
isQuoted Apostrophe = True
isQuoted _ = False

-- | Convert inline element to ConTeXt
inlineToConTeXt :: Inline    -- ^ Inline to convert
                -> State WriterState Doc
inlineToConTeXt (Emph lst) = do 
  contents <- inlineListToConTeXt lst
  return $ text "{\\em " <> contents <> char '}'
inlineToConTeXt (Strong lst) = do
  contents <- inlineListToConTeXt lst
  return $ text "{\\bf " <> contents <> char '}'
inlineToConTeXt (Strikeout lst) = do
  contents <- inlineListToConTeXt lst
  return $ text "\\overstrikes{" <> contents <> char '}'
inlineToConTeXt (Superscript lst) = do
  contents <- inlineListToConTeXt lst
  return $ text "\\high{" <> contents <> char '}'
inlineToConTeXt (Subscript lst) = do
  contents <- inlineListToConTeXt lst
  return $ text "\\low{" <> contents <> char '}'
inlineToConTeXt (Code str) = return $ text $ "\\type{" ++ str ++ "}"
inlineToConTeXt (Quoted SingleQuote lst) = do
  contents <- inlineListToConTeXt lst
  return $ text "\\quote{" <> contents <> char '}'
inlineToConTeXt (Quoted DoubleQuote lst) = do
  contents <- inlineListToConTeXt lst
  return $ text "\\quotation{" <> contents <> char '}'
inlineToConTeXt Apostrophe = return $ char '\''
inlineToConTeXt EmDash = return $ text "---"
inlineToConTeXt EnDash = return $ text "--"
inlineToConTeXt Ellipses = return $ text "\\ldots{}"
inlineToConTeXt (Str str) = return $ text $ stringToConTeXt str
inlineToConTeXt (Math str) = return $ char '$' <> text str <> char '$'
inlineToConTeXt (TeX str) = return $ text str
inlineToConTeXt (HtmlInline str) = return empty
inlineToConTeXt (LineBreak) = return $ text "\\crlf\n"
inlineToConTeXt Space = return $ char ' '
inlineToConTeXt (Link [Code str] (src, tit)) = -- since ConTeXt has its own 
  inlineToConTeXt (Link [Str str] (src, tit))  -- way of printing links... 
inlineToConTeXt (Link txt (src, _)) = do
  st <- get
  let next = stNextRef st
  put $ st {stNextRef = next + 1}
  let ref = show next
  label <- inlineListToConTeXt txt
  return $ text "\\useURL[" <> text ref <> text "][" <> text src <>
           text "][][" <> label <> text "]\\from[" <> text ref <> char ']'
inlineToConTeXt (Image alternate (src, tit)) = do
  alt <- inlineListToConTeXt alternate
  return $ text "\\placefigure\n[]\n[fig:" <> alt <> text "]\n{" <> 
           text tit <> text "}\n{\\externalfigure[" <> text src <> text "]}" 
inlineToConTeXt (Note contents) = do
  contents' <- blockListToConTeXt contents
  let rawnote = stripTrailingNewlines $ render contents'
  -- note: a \n before } is needed when note ends with a \stoptyping
  let optNewline = "\\stoptyping" `isSuffixOf` rawnote
  return $ text "\\footnote{" <> 
           text rawnote <> (if optNewline then char '\n' else empty) <> char '}'

