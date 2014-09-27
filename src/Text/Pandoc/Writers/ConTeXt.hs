{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2007-2014 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2007-2014 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' format into ConTeXt.
-}
module Text.Pandoc.Writers.ConTeXt ( writeConTeXt ) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Options
import Text.Pandoc.Walk (query)
import Text.Printf ( printf )
import Data.List ( intercalate )
import Data.Char ( ord )
import Control.Monad.State
import Text.Pandoc.Pretty
import Text.Pandoc.Templates ( renderTemplate' )
import Network.URI ( isURI, unEscapeString )

data WriterState =
  WriterState { stNextRef          :: Int  -- number of next URL reference
              , stOrderedListLevel :: Int  -- level of ordered list
              , stOptions          :: WriterOptions -- writer options
              }

orderedListStyles :: [Char]
orderedListStyles = cycle "narg"

-- | Convert Pandoc to ConTeXt.
writeConTeXt :: WriterOptions -> Pandoc -> String
writeConTeXt options document =
  let defaultWriterState = WriterState { stNextRef = 1
                                       , stOrderedListLevel = 0
                                       , stOptions = options
                                       }
  in evalState (pandocToConTeXt options document) defaultWriterState

pandocToConTeXt :: WriterOptions -> Pandoc -> State WriterState String
pandocToConTeXt options (Pandoc meta blocks) = do
  let colwidth = if writerWrapText options
                    then Just $ writerColumns options
                    else Nothing
  metadata <- metaToJSON options
              (fmap (render colwidth) . blockListToConTeXt)
              (fmap (render colwidth) . inlineListToConTeXt)
              meta
  body <- mapM (elementToConTeXt options) $ hierarchicalize blocks
  let main = (render colwidth . vcat) body
  let context =   defField "toc" (writerTableOfContents options)
                $ defField "placelist" (intercalate ("," :: String) $
                     take (writerTOCDepth options + if writerChapters options
                                                       then 0
                                                       else 1)
                       ["chapter","section","subsection","subsubsection",
                        "subsubsubsection","subsubsubsubsection"])
                $ defField "body" main
                $ defField "number-sections" (writerNumberSections options)
                $ defField "mainlang" (maybe ""
                    (reverse . takeWhile (/=',') . reverse)
                    (lookup "lang" $ writerVariables options))
                $ metadata
  return $ if writerStandalone options
              then renderTemplate' (writerTemplate options) context
              else main

-- escape things as needed for ConTeXt

escapeCharForConTeXt :: WriterOptions -> Char -> String
escapeCharForConTeXt opts ch =
 let ligatures = writerTeXLigatures opts in
 case ch of
    '{'    -> "\\{"
    '}'    -> "\\}"
    '\\'   -> "\\letterbackslash{}"
    '$'    -> "\\$"
    '|'    -> "\\letterbar{}"
    '%'    -> "\\letterpercent{}"
    '~'    -> "\\lettertilde{}"
    '#'    -> "\\#"
    '['    -> "{[}"
    ']'    -> "{]}"
    '\160' -> "~"
    '\x2014' | ligatures -> "---"
    '\x2013' | ligatures -> "--"
    '\x2019' | ligatures -> "'"
    '\x2026' -> "\\ldots{}"
    x      -> [x]

-- | Escape string for ConTeXt
stringToConTeXt :: WriterOptions -> String -> String
stringToConTeXt opts = concatMap (escapeCharForConTeXt opts)

-- | Sanitize labels
toLabel :: String -> String
toLabel z = concatMap go z
 where go x
         | elem x "\\#[]\",{}%()|=" = "ux" ++ printf "%x" (ord x)
         | otherwise = [x]

-- | Convert Elements to ConTeXt
elementToConTeXt :: WriterOptions -> Element -> State WriterState Doc
elementToConTeXt _ (Blk block) = blockToConTeXt block
elementToConTeXt opts (Sec level _ attr title' elements) = do
  header' <- sectionHeader attr level title'
  innerContents <- mapM (elementToConTeXt opts) elements
  return $ vcat (header' : innerContents)

-- | Convert Pandoc block element to ConTeXt.
blockToConTeXt :: Block
               -> State WriterState Doc
blockToConTeXt Null = return empty
blockToConTeXt (Plain lst) = inlineListToConTeXt lst
-- title beginning with fig: indicates that the image is a figure
blockToConTeXt (Para [Image txt (src,'f':'i':'g':':':_)]) = do
  capt <- inlineListToConTeXt txt
  return $ blankline $$ "\\placefigure" <> braces capt <>
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
blockToConTeXt (Div _ bs) = blockListToConTeXt bs
blockToConTeXt (BulletList lst) = do
  contents <- mapM listItemToConTeXt lst
  return $ ("\\startitemize" <> if isTightList lst
                                   then brackets "packed"
                                   else empty) $$
    vcat contents $$ text "\\stopitemize" <> blankline
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
    let style'' = '[': (case style' of
                          DefaultStyle -> orderedListStyles !! level
                          Decimal      -> 'n'
                          Example      -> 'n'
                          LowerRoman   -> 'r'
                          UpperRoman   -> 'R'
                          LowerAlpha   -> 'a'
                          UpperAlpha   -> 'A') :
                       if isTightList lst then ",packed]" else "]"
    let specs = style'' ++ specs2
    return $ "\\startitemize" <> text specs $$ vcat contents $$
             "\\stopitemize" <> blankline
blockToConTeXt (DefinitionList lst) =
  liftM vcat $ mapM defListItemToConTeXt lst
blockToConTeXt HorizontalRule = return $ "\\thinrule" <> blankline
-- If this is ever executed, provide a default for the reference identifier.
blockToConTeXt (Header level attr lst) = sectionHeader attr level lst
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
    rows' <- mapM tableRowToConTeXt rows
    return $ "\\placetable" <> (if null caption
                                   then brackets "none"
                                   else empty)
                            <> braces captionText $$
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
  return $ "\\startdescription" <> braces term' $$ nest 2 def' $$
           "\\stopdescription" <> blankline

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
inlineToConTeXt (Code _ str) = do
  opts <- gets stOptions
  return $ "\\mono" <> braces (text $ stringToConTeXt opts str)
inlineToConTeXt (Quoted SingleQuote lst) = do
  contents <- inlineListToConTeXt lst
  return $ "\\quote" <> braces contents
inlineToConTeXt (Quoted DoubleQuote lst) = do
  contents <- inlineListToConTeXt lst
  return $ "\\quotation" <> braces contents
inlineToConTeXt (Cite _ lst) = inlineListToConTeXt lst
inlineToConTeXt (Str str) = do
  opts <- gets stOptions
  return $ text $ stringToConTeXt opts str
inlineToConTeXt (Math InlineMath str) =
  return $ char '$' <> text str <> char '$'
inlineToConTeXt (Math DisplayMath str) =
  return $ text "\\startformula "  <> text str <> text " \\stopformula" <> space
inlineToConTeXt (RawInline "context" str) = return $ text str
inlineToConTeXt (RawInline "tex" str) = return $ text str
inlineToConTeXt (RawInline _ _) = return empty
inlineToConTeXt (LineBreak) = return $ text "\\crlf" <> cr
inlineToConTeXt Space = return space
-- Handle HTML-like internal document references to sections
inlineToConTeXt (Link txt          (('#' : ref), _)) = do
  opts <- gets stOptions
  contents <-  inlineListToConTeXt txt
  let ref' = toLabel $ stringToConTeXt opts ref
  return $ text "\\in"
           <> braces (if writerNumberSections opts
                         then contents <+> text "(\\S"
                         else contents)  -- prefix
           <> braces (if writerNumberSections opts
                         then text ")"
                         else empty)  -- suffix
           <> brackets (text ref')

inlineToConTeXt (Link txt          (src, _))      = do
  let isAutolink = txt == [Str (unEscapeString src)]
  st <- get
  let next = stNextRef st
  put $ st {stNextRef = next + 1}
  let ref = "url" ++ show next
  contents <-  inlineListToConTeXt txt
  return $ "\\useURL"
           <> brackets (text ref)
           <> brackets (text $ escapeStringUsing [('#',"\\#"),('%',"\\%")] src)
           <> (if isAutolink
                  then empty
                  else brackets empty <> brackets contents)
           <> "\\from"
           <> brackets (text ref)
inlineToConTeXt (Image _ (src, _)) = do
  let src' = if isURI src
                then src
                else unEscapeString src
  return $ braces $ "\\externalfigure" <> brackets (text src')
inlineToConTeXt (Note contents) = do
  contents' <- blockListToConTeXt contents
  let codeBlock x@(CodeBlock _ _) = [x]
      codeBlock _ = []
  let codeBlocks = query codeBlock contents
  return $ if null codeBlocks
              then text "\\footnote{" <> nest 2 contents' <> char '}'
              else text "\\startbuffer " <> nest 2 contents' <>
                   text "\\stopbuffer\\footnote{\\getbuffer}"
inlineToConTeXt (Span _ ils) = inlineListToConTeXt ils

-- | Craft the section header, inserting the secton reference, if supplied.
sectionHeader :: Attr
              -> Int
              -> [Inline]
              -> State WriterState Doc
sectionHeader (ident,classes,_) hdrLevel lst = do
  contents <- inlineListToConTeXt lst
  st <- get
  let opts = stOptions st
  let level' = if writerChapters opts then hdrLevel - 1 else hdrLevel
  let ident' = toLabel ident
  let (section, chapter) = if "unnumbered" `elem` classes
                              then (text "subject", text "title")
                              else (text "section", text "chapter")
  return $ if level' >= 1 && level' <= 5
               then char '\\'
                    <> text (concat (replicate (level' - 1) "sub"))
                    <> section
                    <> (if (not . null) ident' then brackets (text ident') else empty)
                    <> braces contents
                    <> blankline
               else if level' == 0
                       then char '\\' <> chapter <> braces contents
                       else contents <> blankline

