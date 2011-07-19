{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2007-2010 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2007-2010 John MacFarlane
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
import Data.List ( intercalate )
import Control.Monad.State
import Text.Pandoc.Pretty
import Text.Pandoc.Templates ( renderTemplate )
import Network.URI ( isAbsoluteURI, unEscapeString )

data WriterState = 
  WriterState { stNextRef          :: Int  -- number of next URL reference
              , stOrderedListLevel :: Int  -- level of ordered list
              , stOptions          :: WriterOptions -- writer options
              }

orderedListStyles :: [[Char]]
orderedListStyles = cycle ["[n]","[a]", "[r]", "[g]"] 

-- | Convert Pandoc to ConTeXt.
writeConTeXt :: WriterOptions -> Pandoc -> String
writeConTeXt options document = 
  let defaultWriterState = WriterState { stNextRef = 1
                                       , stOrderedListLevel = 0
                                       , stOptions = options
                                       } 
  in evalState (pandocToConTeXt options document) defaultWriterState 

pandocToConTeXt :: WriterOptions -> Pandoc -> State WriterState String
pandocToConTeXt options (Pandoc (Meta title authors date) blocks) = do
  let colwidth = if writerWrapText options
                    then Just $ writerColumns options
                    else Nothing
  titletext <- if null title
                  then return ""
                  else liftM (render colwidth) $ inlineListToConTeXt title
  authorstext <- mapM (liftM (render colwidth) . inlineListToConTeXt) authors
  datetext <-  if null date
                  then return ""
                  else liftM (render colwidth) $ inlineListToConTeXt date
  body <- blockListToConTeXt blocks
  let main = render colwidth $ body
  let context  = writerVariables options ++
                 [ ("toc", if writerTableOfContents options then "yes" else "")
                 , ("body", main)
                 , ("title", titletext)
                 , ("date", datetext) ] ++
                 [ ("author", a) | a <- authorstext ]
  return $ if writerStandalone options
              then renderTemplate context $ writerTemplate options
              else main

-- escape things as needed for ConTeXt

escapeCharForConTeXt :: Char -> String
escapeCharForConTeXt ch =
 case ch of
    '{'    -> "\\letteropenbrace{}"
    '}'    -> "\\letterclosebrace{}"
    '\\'   -> "\\letterbackslash{}"
    '$'    -> "\\$"
    '|'    -> "\\letterbar{}"
    '^'    -> "\\letterhat{}"
    '%'    -> "\\%"
    '~'    -> "\\lettertilde{}"
    '&'    -> "\\&"
    '#'    -> "\\#"
    '<'    -> "\\letterless{}"
    '>'    -> "\\lettermore{}"
    '['    -> "{[}"
    ']'    -> "{]}"
    '_'    -> "\\letterunderscore{}"
    '\160' -> "~"
    x      -> [x]

-- | Escape string for ConTeXt
stringToConTeXt :: String -> String
stringToConTeXt = concatMap escapeCharForConTeXt

-- | Convert Pandoc block element to ConTeXt.
blockToConTeXt :: Block 
               -> State WriterState Doc
blockToConTeXt Null = return empty
blockToConTeXt (Plain lst) = inlineListToConTeXt lst
blockToConTeXt (Para [Image txt (src,_)]) = do
  capt <- inlineListToConTeXt txt
  return $ blankline $$ "\\placefigure[here,nonumber]" <> braces capt <>
           braces ("\\externalfigure" <> brackets (text src)) <> blankline
blockToConTeXt (Para lst) = do 
  contents <- inlineListToConTeXt lst
  return $ contents <> blankline
blockToConTeXt (BlockQuote lst) = do
  contents <- blockListToConTeXt lst
  return $ "\\startblockquote" $$ nest 0 contents $$ "\\stopblockquote" <> blankline
blockToConTeXt (CodeBlock _ str) =
  return $ flush ("\\starttyping" <> cr <> text str <> cr <> "\\stoptyping") $$ blankline
  -- blankline because \stoptyping can't have anything after it, inc. '}'
blockToConTeXt (RawBlock "context" str) = return $ text str <> blankline
blockToConTeXt (RawBlock _ _ ) = return empty
blockToConTeXt (BulletList lst) = do
  contents <- mapM listItemToConTeXt lst
  return $ "\\startitemize" $$ vcat contents $$ text "\\stopitemize" <> blankline
blockToConTeXt (OrderedList (start, style', delim) lst) = do
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
                          (orderedListMarkers (start, style', delim))
    let width' = (toEnum width + 1) / 2
    let width'' = if width' > (1.5 :: Double) 
                     then "width=" ++ show width' ++ "em" 
                     else ""
    let specs2Items = filter (not . null) [start', delim', width'']
    let specs2 = if null specs2Items
                    then ""
                    else "[" ++ intercalate "," specs2Items ++ "]"
    let style'' = case style' of
                        DefaultStyle -> orderedListStyles !! level
                        Decimal      -> "[n]" 
                        Example      -> "[n]" 
                        LowerRoman   -> "[r]"
                        UpperRoman   -> "[R]"
                        LowerAlpha   -> "[a]"
                        UpperAlpha   -> "[A]"
    let specs = style'' ++ specs2
    return $ "\\startitemize" <> text specs $$ vcat contents $$
             "\\stopitemize" <> blankline
blockToConTeXt (DefinitionList lst) =
  liftM vcat $ mapM defListItemToConTeXt lst
blockToConTeXt HorizontalRule = return $ "\\thinrule" <> blankline
blockToConTeXt (Header level lst) = do
  contents <- inlineListToConTeXt lst
  st <- get
  let opts = stOptions st
  let base = if writerNumberSections opts then "section" else "subject"
  let level' = if writerChapters opts then level - 1 else level
  return $ if level' >= 1 && level' <= 5
               then char '\\' <> text (concat (replicate (level' - 1) "sub")) <>
                         text base <> char '{' <> contents <> char '}' <> blankline
               else if level' == 0
                       then "\\chapter{" <> contents <> "}"
                       else contents <> blankline
blockToConTeXt (Table caption aligns widths heads rows) = do
    let colDescriptor colWidth alignment = (case alignment of
                                               AlignLeft    -> 'l' 
                                               AlignRight   -> 'r'
                                               AlignCenter  -> 'c'
                                               AlignDefault -> 'l'):
           if colWidth == 0
              then "|"
              else ("p(" ++ printf "%.2f" colWidth ++ "\\textwidth)|")
    let colDescriptors = "|" ++ (concat $ 
                                 zipWith colDescriptor widths aligns)
    headers <- if all null heads
                  then return empty
                  else liftM ($$ "\\HL") $ tableRowToConTeXt heads 
    captionText <- inlineListToConTeXt caption 
    let captionText' = if null caption then text "none" else captionText
    rows' <- mapM tableRowToConTeXt rows 
    return $ "\\placetable[here]" <> braces captionText' $$
             "\\starttable" <> brackets (text colDescriptors) $$
             "\\HL" $$ headers $$
             vcat rows' $$ "\\HL" $$ "\\stoptable" <> blankline

tableRowToConTeXt :: [[Block]] -> State WriterState Doc
tableRowToConTeXt cols = do
  cols' <- mapM blockListToConTeXt cols
  return $ (vcat (map ("\\NC " <>) cols')) $$ "\\NC\\AR"

listItemToConTeXt :: [Block] -> State WriterState Doc
listItemToConTeXt list = blockListToConTeXt list >>=
  return . ("\\item" $$) . (nest 2)

defListItemToConTeXt :: ([Inline], [[Block]]) -> State WriterState Doc
defListItemToConTeXt (term, defs) = do
  term' <- inlineListToConTeXt term
  def'  <- liftM vsep $ mapM blockListToConTeXt defs
  return $ "\\startdescr" <> braces term' $$ nest 2 def' $$
           "\\stopdescr" <> blankline

-- | Convert list of block elements to ConTeXt.
blockListToConTeXt :: [Block] -> State WriterState Doc
blockListToConTeXt lst = liftM vcat $ mapM blockToConTeXt lst

-- | Convert list of inline elements to ConTeXt.
inlineListToConTeXt :: [Inline]  -- ^ Inlines to convert
                    -> State WriterState Doc
inlineListToConTeXt lst = liftM hcat $ mapM inlineToConTeXt lst

-- | Convert inline element to ConTeXt
inlineToConTeXt :: Inline    -- ^ Inline to convert
                -> State WriterState Doc
inlineToConTeXt (Emph lst) = do 
  contents <- inlineListToConTeXt lst
  return $ braces $ "\\em " <> contents
inlineToConTeXt (Strong lst) = do
  contents <- inlineListToConTeXt lst
  return $ braces $ "\\bf " <> contents
inlineToConTeXt (Strikeout lst) = do
  contents <- inlineListToConTeXt lst
  return $ "\\overstrikes" <> braces contents
inlineToConTeXt (Superscript lst) = do
  contents <- inlineListToConTeXt lst
  return $ "\\high" <> braces contents
inlineToConTeXt (Subscript lst) = do
  contents <- inlineListToConTeXt lst
  return $ "\\low" <> braces contents
inlineToConTeXt (SmallCaps lst) = do
  contents <- inlineListToConTeXt lst
  return $ braces $ "\\sc " <> contents
inlineToConTeXt (Code _ str) | not ('{' `elem` str || '}' `elem` str) =
  return $ "\\type" <> braces (text str)
inlineToConTeXt (Code _ str) =
  return $ "\\mono" <> braces (text $ stringToConTeXt str)
inlineToConTeXt (Quoted SingleQuote lst) = do
  contents <- inlineListToConTeXt lst
  return $ "\\quote" <> braces contents
inlineToConTeXt (Quoted DoubleQuote lst) = do
  contents <- inlineListToConTeXt lst
  return $ "\\quotation" <> braces contents
inlineToConTeXt (Cite _ lst) = inlineListToConTeXt lst
inlineToConTeXt Apostrophe = return $ char '\''
inlineToConTeXt EmDash = return "---"
inlineToConTeXt EnDash = return "--"
inlineToConTeXt Ellipses = return "\\ldots{}"
inlineToConTeXt (Str str) = return $ text $ stringToConTeXt str
inlineToConTeXt (Math InlineMath str) =
  return $ char '$' <> text str <> char '$'
inlineToConTeXt (Math DisplayMath str) =
  return $ text "\\startformula "  <> text str <> text " \\stopformula"
inlineToConTeXt (RawInline "context" str) = return $ text str
inlineToConTeXt (RawInline "tex" str) = return $ text str
inlineToConTeXt (RawInline _ _) = return empty
inlineToConTeXt (LineBreak) = return $ text "\\crlf" <> cr
inlineToConTeXt Space = return space
inlineToConTeXt (Link [Code _ str] (src, tit)) = -- since ConTeXt has its own 
  inlineToConTeXt (Link [Str str] (src, tit))  -- way of printing links... 
inlineToConTeXt (Link txt (src, _)) = do
  st <- get
  let next = stNextRef st
  put $ st {stNextRef = next + 1}
  let ref = show next
  label <- inlineListToConTeXt txt
  return $ "\\useURL" <> brackets (text ref) <> brackets (text src) <>
           brackets empty <> brackets label <>
           "\\from" <> brackets (text ref)
inlineToConTeXt (Image _ (src, _)) = do
  let src' = if isAbsoluteURI src
                then src
                else unEscapeString src
  return $ braces $ "\\externalfigure" <> brackets (text src')
inlineToConTeXt (Note contents) = do
  contents' <- blockListToConTeXt contents
  return $ text "\\footnote{" <>
           nest 2 contents' <> char '}'
