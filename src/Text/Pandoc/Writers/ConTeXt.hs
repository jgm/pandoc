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
import Text.Pandoc.Generic (queryWith)
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
                  else liftM (render colwidth) $ inlineListToConTeXt stringToConTeXt title
  authorstext <- mapM (liftM (render colwidth) . (inlineListToConTeXt stringToConTeXt)) authors
  datetext <-  if null date
                  then return ""
                  else liftM (render colwidth) $ inlineListToConTeXt stringToConTeXt date
  body <- blockListToConTeXt stringToConTeXt blocks
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

-- | Hyphenate URLs, if the string is an absolute URL, otherwise, escape the string for ConTeXt
urlHyphenate :: String -> String
urlHyphenate s = if isAbsoluteURI s then "\\hyphenatedurl{" ++ s ++ "}" else stringToConTeXt s

-- | Convert Pandoc block element to ConTeXt.
blockToConTeXt :: (String -> String)
               ->Block
               -> State WriterState Doc
blockToConTeXt _ Null = return empty
blockToConTeXt strXform (Plain lst) = inlineListToConTeXt strXform lst
blockToConTeXt strXform (Para [Image txt (src,_)]) = do
  capt <- inlineListToConTeXt strXform txt
  return $ blankline $$ "\\placefigure[here,nonumber]" <> braces capt <>
           braces ("\\externalfigure" <> brackets (text src)) <> blankline
blockToConTeXt strXform (Para lst) = do 
  contents <- inlineListToConTeXt strXform lst
  return $ contents <> blankline
blockToConTeXt strXform (BlockQuote lst) = do
  contents <- blockListToConTeXt strXform lst
  return $ "\\startblockquote" $$ nest 0 contents $$ "\\stopblockquote" <> blankline
blockToConTeXt _ (CodeBlock _ str) =
  return $ flush ("\\starttyping" <> cr <> text str <> cr <> "\\stoptyping") $$ blankline
  -- blankline because \stoptyping can't have anything after it, inc. '}'
blockToConTeXt _ (RawBlock "context" str) = return $ text str <> blankline
blockToConTeXt _ (RawBlock _ _ ) = return empty
blockToConTeXt strXform (BulletList lst) = do
  contents <- mapM (listItemToConTeXt strXform) lst
  return $ "\\startitemize" $$ vcat contents $$ text "\\stopitemize" <> blankline
blockToConTeXt strXform (OrderedList (start, style', delim) lst) = do
    st <- get
    let level = stOrderedListLevel st
    put $ st {stOrderedListLevel = level + 1}
    contents <- mapM (listItemToConTeXt strXform) lst
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
blockToConTeXt strXform (DefinitionList lst) =
  liftM vcat $ mapM (defListItemToConTeXt strXform) lst
blockToConTeXt _ HorizontalRule = return $ "\\thinrule" <> blankline
blockToConTeXt strXform (Header level lst) = do
  contents <- inlineListToConTeXt strXform lst
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
blockToConTeXt strXform (Table caption aligns widths heads rows) = do
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
                  else liftM ($$ "\\HL") $ tableRowToConTeXt strXform heads 
    captionText <- inlineListToConTeXt strXform caption 
    let captionText' = if null caption then text "none" else captionText
    rows' <- mapM (tableRowToConTeXt strXform) rows 
    return $ "\\placetable[here]" <> braces captionText' $$
             "\\starttable" <> brackets (text colDescriptors) $$
             "\\HL" $$ headers $$
             vcat rows' $$ "\\HL" $$ "\\stoptable" <> blankline

tableRowToConTeXt :: (String -> String) -> [[Block]] -> State WriterState Doc
tableRowToConTeXt strXform cols = do
  cols' <- mapM (blockListToConTeXt strXform) cols
  return $ (vcat (map ("\\NC " <>) cols')) $$ "\\NC\\AR"

listItemToConTeXt :: (String -> String) -> [Block] -> State WriterState Doc
listItemToConTeXt strXform list = blockListToConTeXt strXform list >>=
  return . ("\\item" $$) . (nest 2)

defListItemToConTeXt :: (String -> String) -> ([Inline], [[Block]]) -> State WriterState Doc
defListItemToConTeXt strXform (term, defs) = do
  term' <- inlineListToConTeXt strXform term
  def'  <- liftM vsep $ mapM (blockListToConTeXt strXform) defs
  return $ "\\startdescription" <> braces term' $$ nest 2 def' $$
           "\\stopdescription" <> blankline

-- | Convert list of block elements to ConTeXt.
blockListToConTeXt :: (String -> String) -> [Block] -> State WriterState Doc
blockListToConTeXt strXform lst = liftM vcat $ mapM (blockToConTeXt strXform) lst

-- | Convert list of inline elements to ConTeXt.
inlineListToConTeXt :: (String -> String)       -- ^ String transformer
                    -> [Inline]                 -- ^ Inlines to convert
                    -> State WriterState Doc
inlineListToConTeXt strXform lst = liftM hcat $ mapM (inlineToConTeXt strXform) lst

-- | Convert inline element to ConTeXt
inlineToConTeXt :: (String -> String)           -- ^ String transformer
                -> Inline                       -- ^ Inline to convert
                -> State WriterState Doc
inlineToConTeXt strXform (Emph lst) = do
  contents <- inlineListToConTeXt strXform lst
  return $ braces $ "\\em " <> contents
inlineToConTeXt strXform (Strong lst) = do
  contents <- inlineListToConTeXt strXform lst
  return $ braces $ "\\bf " <> contents
inlineToConTeXt strXform (Strikeout lst) = do
  contents <- inlineListToConTeXt strXform lst
  return $ "\\overstrikes" <> braces contents
inlineToConTeXt strXform (Superscript lst) = do
  contents <- inlineListToConTeXt strXform lst
  return $ "\\high" <> braces contents
inlineToConTeXt strXform (Subscript lst) = do
  contents <- inlineListToConTeXt  strXform lst
  return $ "\\low" <> braces contents
inlineToConTeXt strXform (SmallCaps lst) = do
  contents <- inlineListToConTeXt strXform lst
  return $ braces $ "\\sc " <> contents
inlineToConTeXt _ (Code _ str) | not ('{' `elem` str || '}' `elem` str) =
  return $ "\\type" <> braces (text str)
inlineToConTeXt strXform (Code _ str) =
  return $ "\\mono" <> braces (text $ strXform str)
inlineToConTeXt strXform (Quoted SingleQuote lst) = do
  contents <- inlineListToConTeXt strXform lst
  return $ "\\quote" <> braces contents
inlineToConTeXt strXform (Quoted DoubleQuote lst) = do
  contents <- inlineListToConTeXt strXform lst
  return $ "\\quotation" <> braces contents
inlineToConTeXt strXform (Cite _ lst) = inlineListToConTeXt strXform lst
inlineToConTeXt _ Apostrophe = return $ char '\''
inlineToConTeXt _ EmDash = return "---"
inlineToConTeXt _ EnDash = return "--"
inlineToConTeXt _ Ellipses = return "\\ldots{}"
inlineToConTeXt strXform (Str str) = return $ text $ strXform str
inlineToConTeXt _ (Math InlineMath str) =
  return $ char '$' <> text str <> char '$'
inlineToConTeXt _ (Math DisplayMath str) =
  return $ text "\\startformula "  <> text str <> text " \\stopformula"
inlineToConTeXt _ (RawInline "context" str) = return $ text str
inlineToConTeXt _ (RawInline "tex" str) = return $ text str
inlineToConTeXt _ (RawInline _ _) = return empty
inlineToConTeXt _ (LineBreak) = return $ text "\\crlf" <> cr
inlineToConTeXt _ Space = return space
-- ConTeXT has its own way of printing links
inlineToConTeXt strXform (Link [Code _ str] (src, tit))    = inlineToConTeXt strXform (Link [Str str] (src, tit))
-- Convert link's text, hyphenating URLs when they're seen (does deep list inspection)
inlineToConTeXt _ (Link txt          (src, _))      = do
  st <- get
  let next = stNextRef st
  put $ st {stNextRef = next + 1}
  let ref ="urlref" ++ (show next)
  label <-  inlineListToConTeXt urlHyphenate txt
  return $ "\\useURL" <> brackets (text ref) <>
           brackets (text $ escapeStringUsing [('#',"\\#")] src) <>
           brackets empty <> brackets label <>
           "\\from" <> brackets (text ref)
inlineToConTeXt _ (Image _ (src, _)) = do
  let src' = if isAbsoluteURI src
                then src
                else unEscapeString src
  return $ braces $ "\\externalfigure" <> brackets (text src')
inlineToConTeXt strXform (Note contents) = do
  contents' <- blockListToConTeXt strXform contents
  let codeBlock x@(CodeBlock _ _) = [x]
      codeBlock _ = []
  let codeBlocks = queryWith codeBlock contents
  return $ if null codeBlocks
              then text "\\footnote{" <> nest 2 contents' <> char '}'
              else text "\\startbuffer " <> nest 2 contents' <>
                   text "\\stopbuffer\\footnote{\\getbuffer}"
