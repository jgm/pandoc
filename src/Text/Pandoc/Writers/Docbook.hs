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
import Text.Pandoc.Shared
import Data.List ( isPrefixOf, drop )
import Text.PrettyPrint.HughesPJ hiding ( Str )

--
-- code to format XML
--

-- | Escape one character as needed for XML.
escapeCharForXML :: Char -> String
escapeCharForXML x = case x of
                       '&'  -> "&amp;"
                       '<'  -> "&lt;"
                       '>'  -> "&gt;"
                       '"'  -> "&quot;"
                       '\160' -> "&nbsp;"
                       c    -> [c] 

-- | True if the character needs to be escaped.
needsEscaping :: Char -> Bool
needsEscaping c = c `elem` "&<>\"\160"

-- | Escape string as needed for XML.  Entity references are not preserved.
escapeStringForXML :: String -> String
escapeStringForXML ""  = ""
escapeStringForXML str = 
  case break needsEscaping str of
    (okay, "")     -> okay
    (okay, (c:cs)) -> okay ++ escapeCharForXML c ++ escapeStringForXML cs 

-- | Return a text object with a string of formatted XML attributes. 
attributeList :: [(String, String)] -> Doc
attributeList = text .  concatMap 
  (\(a, b) -> " " ++ escapeStringForXML a ++ "=\"" ++ 
  escapeStringForXML b ++ "\"") 

-- | Put the supplied contents between start and end tags of tagType,
--   with specified attributes and (if specified) indentation.
inTags:: Bool -> String -> [(String, String)] -> Doc -> Doc
inTags isIndented tagType attribs contents = 
  let openTag = char '<' <> text tagType <> attributeList attribs <> 
                char '>'
      closeTag  = text "</" <> text tagType <> char '>'
  in  if isIndented
         then openTag $$ nest 2 contents $$ closeTag
         else openTag <> contents <> closeTag

-- | Return a self-closing tag of tagType with specified attributes
selfClosingTag :: String -> [(String, String)] -> Doc
selfClosingTag tagType attribs = 
  char '<' <> text tagType <> attributeList attribs <> text " />" 
 
-- | Put the supplied contents between start and end tags of tagType.
inTagsSimple :: String -> Doc -> Doc
inTagsSimple tagType = inTags False tagType []

-- | Put the supplied contents in indented block btw start and end tags.
inTagsIndented :: String -> Doc -> Doc
inTagsIndented tagType = inTags True tagType []

--
-- Docbook writer
--

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
  let head     = if writerStandalone opts
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
  in  render $ head $$ body' $$ text ""

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
plainToPara (Plain x) = Para x
plainToPara x = x

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
blockToDocbook opts Null = empty
blockToDocbook opts (Plain lst) = wrap opts lst
blockToDocbook opts (Para lst) = inTagsIndented "para" $ wrap opts lst
blockToDocbook opts (BlockQuote blocks) =
  inTagsIndented "blockquote" $ blocksToDocbook opts blocks
blockToDocbook opts (CodeBlock str) = 
  text "<screen>\n" <> text (escapeStringForXML str) <> text "\n</screen>"
blockToDocbook opts (BulletList lst) = 
  inTagsIndented "itemizedlist" $ listItemsToDocbook opts lst 
blockToDocbook opts (OrderedList _ []) = empty 
blockToDocbook opts (OrderedList (start, numstyle, numdelim) (first:rest)) =
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
blockToDocbook opts (RawHtml str) = text str -- raw XML block 
blockToDocbook opts HorizontalRule = empty -- not semantic
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

colHeadsToDocbook opts alignStrings widths headers =
  let heads = zipWith3 (\align width item -> 
              tableItemToDocbook opts "th" align width item) 
              alignStrings widths headers
  in  inTagsIndented "tr" $ vcat heads

alignmentToString alignment = case alignment of
                                 AlignLeft -> "left"
                                 AlignRight -> "right"
                                 AlignCenter -> "center"
                                 AlignDefault -> "left"

tableRowToDocbook opts aligns cols = inTagsIndented "tr" $ 
  vcat $ zipWith3 (tableItemToDocbook opts "td") aligns (repeat 0) cols

tableItemToDocbook opts tag align width item =
  let attrib = [("align", align)] ++ 
               if width /= 0
                  then [("style", "{width: " ++ 
                        show (truncate (100*width)) ++ "%;}")]
                  else [] 
  in  inTags True tag attrib $ vcat $ map (blockToDocbook opts) item

-- | Take list of inline elements and return wrapped doc.
wrap :: WriterOptions -> [Inline] -> Doc
wrap opts lst = fsep $ map (inlinesToDocbook opts) (splitBy Space lst)

-- | Convert a list of inline elements to Docbook.
inlinesToDocbook :: WriterOptions -> [Inline] -> Doc
inlinesToDocbook opts lst = hcat $ map (inlineToDocbook opts) lst

-- | Convert an inline element to Docbook.
inlineToDocbook :: WriterOptions -> Inline -> Doc
inlineToDocbook opts (Str str) = text $ escapeStringForXML str 
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
inlineToDocbook opts (Quoted _ lst) = 
  inTagsSimple "quote" $ inlinesToDocbook opts lst
inlineToDocbook opts Apostrophe = char '\''
inlineToDocbook opts Ellipses = text "&#8230;"
inlineToDocbook opts EmDash = text "&#8212;" 
inlineToDocbook opts EnDash = text "&#8211;" 
inlineToDocbook opts (Code str) = 
  inTagsSimple "literal" $ text (escapeStringForXML str)
inlineToDocbook opts (TeX str) = inlineToDocbook opts (Code str)
inlineToDocbook opts (HtmlInline str) = empty
inlineToDocbook opts LineBreak = text $ "<literallayout></literallayout>" 
inlineToDocbook opts Space = char ' '
inlineToDocbook opts (Link txt (src, tit)) =
  if isPrefixOf "mailto:" src
     then let src' = drop 7 src
              emailLink = inTagsSimple "email" $ text $ 
                          escapeStringForXML $ src'
          in  if txt == [Code src']
                 then emailLink
                 else inlinesToDocbook opts txt <+> char '(' <> emailLink <> 
                      char ')'
     else inTags False "ulink" [("url", src)] $ inlinesToDocbook opts txt
inlineToDocbook opts (Image alt (src, tit)) = 
  let titleDoc = if null tit
                   then empty
                   else inTagsIndented "objectinfo" $
                        inTagsIndented "title" (text $ escapeStringForXML tit)
  in  inTagsIndented "inlinemediaobject" $ inTagsIndented "imageobject" $
      titleDoc $$ selfClosingTag "imagedata" [("fileref", src)] 
inlineToDocbook opts (Note contents) = 
  inTagsIndented "footnote" $ blocksToDocbook opts contents
