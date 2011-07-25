{-
Copyright (C) 2006-2010 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-2010 John MacFarlane
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
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Readers.TeXMath
import Data.List ( isPrefixOf, intercalate, isSuffixOf )
import Data.Char ( toLower )
import Text.Pandoc.Highlighting (languages, languagesByExtension)
import Text.Pandoc.Pretty

-- | Convert list of authors to a docbook <author> section
authorToDocbook :: WriterOptions -> [Inline] -> Doc
authorToDocbook opts name' =
  let name = render Nothing $ inlinesToDocbook opts name'
  in  if ',' `elem` name
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
                    n  -> (intercalate " " (take (n-1) namewords), last namewords)
               in inTagsSimple "firstname" (text $ escapeStringForXML firstname) $$ 
                  inTagsSimple "surname" (text $ escapeStringForXML lastname) 

-- | Convert Pandoc document to string in Docbook format.
writeDocbook :: WriterOptions -> Pandoc -> String
writeDocbook opts (Pandoc (Meta tit auths dat) blocks) = 
  let title = inlinesToDocbook opts tit
      authors = map (authorToDocbook opts) auths
      date = inlinesToDocbook opts dat
      elements = hierarchicalize blocks
      colwidth = if writerWrapText opts
                    then Just $ writerColumns opts
                    else Nothing
      render' = render colwidth
      opts' = if "</book>" `isSuffixOf`
                      (removeTrailingSpace $ writerTemplate opts)
                 then opts{ writerChapters = True }
                 else opts
      main     = render' $ vcat (map (elementToDocbook opts') elements)
      context = writerVariables opts ++
                [ ("body", main)
                , ("title", render' title)
                , ("date", render' date) ] ++
                [ ("author", render' a) | a <- authors ]
  in  if writerStandalone opts
         then renderTemplate context $ writerTemplate opts
         else main

-- | Convert an Element to Docbook.
elementToDocbook :: WriterOptions -> Element -> Doc
elementToDocbook opts (Blk block) = blockToDocbook opts block 
elementToDocbook opts (Sec _ _num id' title elements) =
  -- Docbook doesn't allow sections with no content, so insert some if needed
  let elements' = if null elements
                    then [Blk (Para [])]
                    else elements
      tag = if writerChapters opts
               then "chapter"
               else "section"
  in  inTags True tag [("id",id')] $
      inTagsSimple "title" (inlinesToDocbook opts title) $$
      vcat (map (elementToDocbook opts{ writerChapters = False }) elements') 

-- | Convert a list of Pandoc blocks to Docbook.
blocksToDocbook :: WriterOptions -> [Block] -> Doc
blocksToDocbook opts = vcat . map (blockToDocbook opts)

-- | Auxiliary function to convert Plain block to Para.
plainToPara :: Block -> Block
plainToPara (Plain x) = Para x
plainToPara x         = x

-- | Convert a list of pairs of terms and definitions into a list of 
-- Docbook varlistentrys.
deflistItemsToDocbook :: WriterOptions -> [([Inline],[[Block]])] -> Doc
deflistItemsToDocbook opts items = 
  vcat $ map (\(term, defs) -> deflistItemToDocbook opts term defs) items

-- | Convert a term and a list of blocks into a Docbook varlistentry.
deflistItemToDocbook :: WriterOptions -> [Inline] -> [[Block]] -> Doc
deflistItemToDocbook opts term defs =
  let def' = concatMap (map plainToPara) defs
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
blockToDocbook opts (Plain lst) = inlinesToDocbook opts lst
blockToDocbook opts (Para [Image txt (src,_)]) =
  let capt = inlinesToDocbook opts txt
  in  inTagsIndented "figure" $
        inTagsSimple "title" capt $$
        (inTagsIndented "mediaobject" $
           (inTagsIndented "imageobject"
             (selfClosingTag "imagedata" [("fileref",src)])) $$
           inTagsSimple "textobject" (inTagsSimple "phrase" capt))
blockToDocbook opts (Para lst) =
  inTagsIndented "para" $ inlinesToDocbook opts lst
blockToDocbook opts (BlockQuote blocks) =
  inTagsIndented "blockquote" $ blocksToDocbook opts blocks
blockToDocbook _ (CodeBlock (_,classes,_) str) =
  text ("<programlisting" ++ lang ++ ">") <> cr <>
     flush (text (escapeStringForXML str) <> cr <> text "</programlisting>")
    where lang  = if null langs
                     then ""
                     else " language=\"" ++ escapeStringForXML (head langs) ++
                          "\""
          isLang l    = map toLower l `elem` map (map toLower) languages
          langsFrom s = if isLang s
                           then [s]
                           else languagesByExtension . map toLower $ s
          langs       = concatMap langsFrom classes
blockToDocbook opts (BulletList lst) = 
  inTagsIndented "itemizedlist" $ listItemsToDocbook opts lst 
blockToDocbook _ (OrderedList _ []) = empty 
blockToDocbook opts (OrderedList (start, numstyle, _) (first:rest)) =
  let attribs  = case numstyle of
                       DefaultStyle -> []
                       Decimal      -> [("numeration", "arabic")]
                       Example      -> [("numeration", "arabic")]
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
blockToDocbook _ (RawBlock "docbook" str) = text str -- raw XML block 
-- we allow html for compatibility with earlier versions of pandoc
blockToDocbook _ (RawBlock "html" str) = text str -- raw XML block
blockToDocbook _ (RawBlock _ _) = empty
blockToDocbook _ HorizontalRule = empty -- not semantic
blockToDocbook opts (Table caption aligns widths headers rows) =
  let captionDoc   = if null caption
                        then empty
                        else inTagsIndented "title"
                              (inlinesToDocbook opts caption)
      tableType    = if isEmpty captionDoc then "informaltable" else "table"
      percent w    = show (truncate (100*w) :: Integer) ++ "*"
      coltags = vcat $ zipWith (\w al -> selfClosingTag "colspec"
                       ([("colwidth", percent w) | w > 0] ++
                        [("align", alignmentToString al)])) widths aligns
      head' = if all null headers
                 then empty
                 else inTagsIndented "thead" $
                         tableRowToDocbook opts headers
      body' = inTagsIndented "tbody" $
              vcat $ map (tableRowToDocbook opts) rows
  in  inTagsIndented tableType $ captionDoc $$
        (inTags True "tgroup" [("cols", show (length headers))] $
         coltags $$ head' $$ body')

alignmentToString :: Alignment -> [Char]
alignmentToString alignment = case alignment of
                                 AlignLeft -> "left"
                                 AlignRight -> "right"
                                 AlignCenter -> "center"
                                 AlignDefault -> "left"

tableRowToDocbook :: WriterOptions
                  -> [[Block]]
                  -> Doc
tableRowToDocbook opts cols =
  inTagsIndented "row" $ vcat $ map (tableItemToDocbook opts) cols

tableItemToDocbook :: WriterOptions
                   -> [Block]
                   -> Doc
tableItemToDocbook opts item =
  inTags True "entry" [] $ vcat $ map (blockToDocbook opts) item

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
inlineToDocbook opts (Cite _ lst) =
  inlinesToDocbook opts lst 
inlineToDocbook _ Apostrophe = char '\''
inlineToDocbook _ Ellipses = text "…"
inlineToDocbook _ EmDash = text "—"
inlineToDocbook _ EnDash = text "–"
inlineToDocbook _ (Code _ str) = 
  inTagsSimple "literal" $ text (escapeStringForXML str)
inlineToDocbook opts (Math _ str) = inlinesToDocbook opts $ readTeXMath str
inlineToDocbook _ (RawInline _ _) = empty
inlineToDocbook _ LineBreak = inTagsSimple "literallayout" empty
inlineToDocbook _ Space = space
inlineToDocbook opts (Link txt (src, _)) =
  if isPrefixOf "mailto:" src
     then let src' = drop 7 src
              emailLink = inTagsSimple "email" $ text $ 
                          escapeStringForXML $ src'
          in  case txt of
               [Code _ s] | s == src' -> emailLink
               _             -> inlinesToDocbook opts txt <+>
                                  char '(' <> emailLink <> char ')'
     else (if isPrefixOf "#" src
              then inTags False "link" [("linkend", drop 1 src)]
              else inTags False "ulink" [("url", src)]) $
          inlinesToDocbook opts txt
inlineToDocbook _ (Image _ (src, tit)) = 
  let titleDoc = if null tit
                   then empty
                   else inTagsIndented "objectinfo" $
                        inTagsIndented "title" (text $ escapeStringForXML tit)
  in  inTagsIndented "inlinemediaobject" $ inTagsIndented "imageobject" $
      titleDoc $$ selfClosingTag "imagedata" [("fileref", src)]
inlineToDocbook opts (Note contents) = 
  inTagsIndented "footnote" $ blocksToDocbook opts contents
