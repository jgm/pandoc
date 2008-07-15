{-
Copyright (C) 2006-7 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.Docbook
   Copyright   : Copyright (C) 2006-7 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to Docbook XML.
-}
module Text.Pandoc.Writers.Docbook ( writeDocbook) where
import Text.Pandoc.Definition
import Text.Pandoc.XML
import Text.Pandoc.Shared
import Text.Pandoc.Readers.TeXMath
import Data.List ( isPrefixOf, drop )
import Text.PrettyPrint.HughesPJ hiding ( Str )

-- | Convert list of authors to a docbook <author> section
authorToDocbook :: [Char] -> Doc
authorToDocbook name = inTagsIndented "author" $ 
  if ',' `elem` name
    then -- last name first
         let (lastname, rest) = break (==',') name 
             firstname = removeLeadingSpace rest in
         inTagsSimple "firstname" (text $ escapeStringForXML firstname) <> 
         inTagsSimple "surname" (text $ escapeStringForXML lastname) 
    else -- last name last
         let namewords = words name
             lengthname = length namewords 
             (firstname, lastname) = case lengthname of
               0  -> ("","") 
               1  -> ("", name)
               n  -> (joinWithSep " " (take (n-1) namewords), last namewords)
          in inTagsSimple "firstname" (text $ escapeStringForXML firstname) $$ 
             inTagsSimple "surname" (text $ escapeStringForXML lastname) 

-- | Convert Pandoc document to string in Docbook format.
writeDocbook :: WriterOptions -> Pandoc -> String
writeDocbook opts (Pandoc (Meta title authors date) blocks) = 
  let head'    = if writerStandalone opts
                    then text (writerHeader opts)
                    else empty
      meta     = if writerStandalone opts
                    then inTagsIndented "articleinfo" $
                         (inTagsSimple "title" (wrap opts title)) $$ 
                         (vcat (map authorToDocbook authors)) $$ 
                         (inTagsSimple "date" (text $ escapeStringForXML date)) 
                    else empty
      elements = hierarchicalize blocks
      before   = writerIncludeBefore opts
      after    = writerIncludeAfter opts
      body     = (if null before then empty else text before) $$
                 vcat (map (elementToDocbook opts) elements) $$
                 (if null after then empty else text after)
      body'    = if writerStandalone opts
                   then inTagsIndented "article" (meta $$ body)
                   else body 
  in  render $ head' $$ body' $$ text ""

-- | Convert an Element to Docbook.
elementToDocbook :: WriterOptions -> Element -> Doc
elementToDocbook opts (Blk block) = blockToDocbook opts block 
elementToDocbook opts (Sec title elements) =
  -- Docbook doesn't allow sections with no content, so insert some if needed
  let elements' = if null elements
                    then [Blk (Para [])]
                    else elements
  in  inTagsIndented "section" $
      inTagsSimple "title" (wrap opts title) $$
      vcat (map (elementToDocbook opts) elements') 

-- | Convert a list of Pandoc blocks to Docbook.
blocksToDocbook :: WriterOptions -> [Block] -> Doc
blocksToDocbook opts = vcat . map (blockToDocbook opts)

-- | Auxiliary function to convert Plain block to Para.
plainToPara :: Block -> Block
plainToPara (Plain x) = Para x
plainToPara x         = x

-- | Convert a list of pairs of terms and definitions into a list of 
-- Docbook varlistentrys.
deflistItemsToDocbook :: WriterOptions -> [([Inline],[Block])] -> Doc
deflistItemsToDocbook opts items = 
  vcat $ map (\(term, def) -> deflistItemToDocbook opts term def) items

-- | Convert a term and a list of blocks into a Docbook varlistentry.
deflistItemToDocbook :: WriterOptions -> [Inline] -> [Block] -> Doc
deflistItemToDocbook opts term def =
  let def' = map plainToPara def
  in  inTagsIndented "varlistentry" $
      inTagsIndented "term" (inlinesToDocbook opts term) $$
      inTagsIndented "listitem" (blocksToDocbook opts def')

-- | Convert a list of lists of blocks to a list of Docbook list items.
listItemsToDocbook :: WriterOptions -> [[Block]] -> Doc
listItemsToDocbook opts items = vcat $ map (listItemToDocbook opts) items

-- | Convert a list of blocks into a Docbook list item.
listItemToDocbook :: WriterOptions -> [Block] -> Doc
listItemToDocbook opts item =
  inTagsIndented "listitem" $ blocksToDocbook opts $ map plainToPara item

-- | Convert a Pandoc block element to Docbook.
blockToDocbook :: WriterOptions -> Block -> Doc
blockToDocbook _ Null = empty
blockToDocbook _ (Header _ _) = empty -- should not occur after hierarchicalize
blockToDocbook opts (Plain lst) = wrap opts lst
blockToDocbook opts (Para lst) = inTagsIndented "para" $ wrap opts lst
blockToDocbook opts (BlockQuote blocks) =
  inTagsIndented "blockquote" $ blocksToDocbook opts blocks
blockToDocbook _ (CodeBlock _ str) = 
  text "<screen>\n" <> text (escapeStringForXML str) <> text "\n</screen>"
blockToDocbook opts (BulletList lst) = 
  inTagsIndented "itemizedlist" $ listItemsToDocbook opts lst 
blockToDocbook _ (OrderedList _ []) = empty 
blockToDocbook opts (OrderedList (start, numstyle, _) (first:rest)) =
  let attribs  = case numstyle of
                       DefaultStyle -> []
                       Decimal      -> [("numeration", "arabic")]
                       UpperAlpha   -> [("numeration", "upperalpha")]
                       LowerAlpha   -> [("numeration", "loweralpha")]
                       UpperRoman   -> [("numeration", "upperroman")]
                       LowerRoman   -> [("numeration", "lowerroman")]
      items    = if start == 1
                    then listItemsToDocbook opts (first:rest)
                    else (inTags True "listitem" [("override",show start)]
                         (blocksToDocbook opts $ map plainToPara first)) $$ 
                         listItemsToDocbook opts rest 
  in  inTags True "orderedlist" attribs items
blockToDocbook opts (DefinitionList lst) = 
  inTagsIndented "variablelist" $ deflistItemsToDocbook opts lst 
blockToDocbook _ (RawHtml str) = text str -- raw XML block 
blockToDocbook _ HorizontalRule = empty -- not semantic
blockToDocbook opts (Table caption aligns widths headers rows) =
  let alignStrings = map alignmentToString aligns
      captionDoc   = if null caption
                      then empty
                      else inTagsIndented "caption" 
                           (inlinesToDocbook opts caption)
      tableType    = if isEmpty captionDoc then "informaltable" else "table"
  in  inTagsIndented tableType $ captionDoc $$
     (colHeadsToDocbook opts alignStrings widths headers) $$ 
     (vcat $ map (tableRowToDocbook opts alignStrings) rows)

colHeadsToDocbook :: WriterOptions 
                  -> [[Char]]
                  -> [Float] 
                  -> [[Block]] 
                  -> Doc
colHeadsToDocbook opts alignStrings widths headers =
  let heads = zipWith3 (\align width item -> 
              tableItemToDocbook opts "th" align width item) 
              alignStrings widths headers
  in  inTagsIndented "tr" $ vcat heads

alignmentToString :: Alignment -> [Char]
alignmentToString alignment = case alignment of
                                 AlignLeft -> "left"
                                 AlignRight -> "right"
                                 AlignCenter -> "center"
                                 AlignDefault -> "left"

tableRowToDocbook :: WriterOptions -> [[Char]] -> [[Block]] -> Doc
tableRowToDocbook opts aligns cols = inTagsIndented "tr" $ 
  vcat $ zipWith3 (tableItemToDocbook opts "td") aligns (repeat 0) cols

tableItemToDocbook :: WriterOptions
                   -> [Char]
                   -> [Char]
                   -> Float
                   -> [Block]
                   -> Doc
tableItemToDocbook opts tag align width item =
  let attrib = [("align", align)] ++ 
               if width /= 0
                  then [("style", "{width: " ++ 
                        show (truncate (100*width) :: Integer) ++ "%;}")]
                  else [] 
  in  inTags True tag attrib $ vcat $ map (blockToDocbook opts) item

-- | Take list of inline elements and return wrapped doc.
wrap :: WriterOptions -> [Inline] -> Doc
wrap opts lst = if writerWrapText opts
                   then fsep $ map (inlinesToDocbook opts) (splitBy Space lst)
                   else inlinesToDocbook opts lst

-- | Convert a list of inline elements to Docbook.
inlinesToDocbook :: WriterOptions -> [Inline] -> Doc
inlinesToDocbook opts lst = hcat $ map (inlineToDocbook opts) lst

-- | Convert an inline element to Docbook.
inlineToDocbook :: WriterOptions -> Inline -> Doc
inlineToDocbook _ (Str str) = text $ escapeStringForXML str 
inlineToDocbook opts (Emph lst) = 
  inTagsSimple "emphasis" $ inlinesToDocbook opts lst
inlineToDocbook opts (Strong lst) = 
  inTags False "emphasis" [("role", "strong")] $ inlinesToDocbook opts lst
inlineToDocbook opts (Strikeout lst) = 
  inTags False "emphasis" [("role", "strikethrough")] $
  inlinesToDocbook opts lst
inlineToDocbook opts (Superscript lst) = 
  inTagsSimple "superscript" $ inlinesToDocbook opts lst
inlineToDocbook opts (Subscript lst) = 
  inTagsSimple "subscript" $ inlinesToDocbook opts lst
inlineToDocbook opts (SmallCaps lst) = 
  inTags False "emphasis" [("role", "smallcaps")] $
  inlinesToDocbook opts lst
inlineToDocbook opts (Quoted _ lst) = 
  inTagsSimple "quote" $ inlinesToDocbook opts lst
inlineToDocbook _ Apostrophe = char '\''
inlineToDocbook _ Ellipses = text "&#8230;"
inlineToDocbook _ EmDash = text "&#8212;" 
inlineToDocbook _ EnDash = text "&#8211;" 
inlineToDocbook _ (Code str) = 
  inTagsSimple "literal" $ text (escapeStringForXML str)
inlineToDocbook opts (Math str) = inlinesToDocbook opts $ readTeXMath str
inlineToDocbook _ (TeX _) = empty
inlineToDocbook _ (HtmlInline _) = empty
inlineToDocbook _ LineBreak = text $ "<literallayout></literallayout>" 
inlineToDocbook _ Space = char ' '
inlineToDocbook opts (Link txt (src, _)) =
  if isPrefixOf "mailto:" src
     then let src' = drop 7 src
              emailLink = inTagsSimple "email" $ text $ 
                          escapeStringForXML $ src'
          in  if txt == [Code src']
                 then emailLink
                 else inlinesToDocbook opts txt <+> char '(' <> emailLink <> 
                      char ')'
     else inTags False "ulink" [("url", src)] $ inlinesToDocbook opts txt
inlineToDocbook _ (Image _ (src, tit)) = 
  let titleDoc = if null tit
                   then empty
                   else inTagsIndented "objectinfo" $
                        inTagsIndented "title" (text $ escapeStringForXML tit)
  in  inTagsIndented "inlinemediaobject" $ inTagsIndented "imageobject" $
      titleDoc $$ selfClosingTag "imagedata" [("fileref", src)] 
inlineToDocbook opts (Note contents) = 
  inTagsIndented "footnote" $ blocksToDocbook opts contents
