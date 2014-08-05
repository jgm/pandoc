{-# LANGUAGE OverloadedStrings, PatternGuards #-}
{-
Copyright (C) 2006-2014 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-2014 John MacFarlane
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
import Text.Pandoc.Walk
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Options
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Readers.TeXMath
import Data.List ( stripPrefix, isPrefixOf, intercalate, isSuffixOf )
import Data.Char ( toLower )
import Control.Applicative ((<$>))
import Data.Monoid ( Any(..) )
import Text.Pandoc.Highlighting ( languages, languagesByExtension )
import Text.Pandoc.Pretty
import qualified Text.Pandoc.Builder as B
import Text.TeXMath
import qualified Text.XML.Light as Xml
import Data.Generics (everywhere, mkT)

-- | Convert list of authors to a docbook <author> section
authorToDocbook :: WriterOptions -> [Inline] -> B.Inlines
authorToDocbook opts name' =
  let name = render Nothing $ inlinesToDocbook opts name'
      colwidth = if writerWrapText opts
                    then Just $ writerColumns opts
                    else Nothing
  in  B.rawInline "docbook" $ render colwidth $
      if ',' `elem` name
         then -- last name first
              let (lastname, rest) = break (==',') name
                  firstname = triml rest in
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
writeDocbook opts (Pandoc meta blocks) =
  let elements = hierarchicalize blocks
      colwidth = if writerWrapText opts
                    then Just $ writerColumns opts
                    else Nothing
      render' = render colwidth
      opts' = if "/book>" `isSuffixOf`
                      (trimr $ writerTemplate opts)
                 then opts{ writerChapters = True }
                 else opts
      startLvl = if writerChapters opts' then 0 else 1
      auths'   = map (authorToDocbook opts) $ docAuthors meta
      meta'    = B.setMeta "author" auths' meta
      Just metadata = metaToJSON opts
                 (Just . render colwidth . (vcat .
                          (map (elementToDocbook opts' startLvl)) . hierarchicalize))
                 (Just . render colwidth . inlinesToDocbook opts')
                 meta'
      main     = render' $ vcat (map (elementToDocbook opts' startLvl) elements)
      context = defField "body" main
              $ defField "mathml" (case writerHTMLMathMethod opts of
                                        MathML _ -> True
                                        _        -> False)
              $ metadata
  in  if writerStandalone opts
         then renderTemplate' (writerTemplate opts) context
         else main

-- | Convert an Element to Docbook.
elementToDocbook :: WriterOptions -> Int -> Element -> Doc
elementToDocbook opts _   (Blk block) = blockToDocbook opts block
elementToDocbook opts lvl (Sec _ _num (id',_,_) title elements) =
  -- Docbook doesn't allow sections with no content, so insert some if needed
  let elements' = if null elements
                    then [Blk (Para [])]
                    else elements
      tag = case lvl of
                 n | n == 0           -> "chapter"
                   | n >= 1 && n <= 5 -> "sect" ++ show n
                   | otherwise        -> "simplesect"
  in  inTags True tag [("id", writerIdentifierPrefix opts ++ id')] $
      inTagsSimple "title" (inlinesToDocbook opts title) $$
      vcat (map (elementToDocbook opts (lvl + 1)) elements')

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
blockToDocbook opts (Div _ bs) = blocksToDocbook opts $ map plainToPara bs
blockToDocbook _ (Header _ _ _) = empty -- should not occur after hierarchicalize
blockToDocbook opts (Plain lst) = inlinesToDocbook opts lst
-- title beginning with fig: indicates that the image is a figure
blockToDocbook opts (Para [Image txt (src,'f':'i':'g':':':_)]) =
  let alt  = inlinesToDocbook opts txt
      capt = if null txt
                then empty
                else inTagsSimple "title" alt
  in  inTagsIndented "figure" $
        capt $$
        (inTagsIndented "mediaobject" $
           (inTagsIndented "imageobject"
             (selfClosingTag "imagedata" [("fileref",src)])) $$
           inTagsSimple "textobject" (inTagsSimple "phrase" alt))
blockToDocbook opts (Para lst)
  | hasLineBreaks lst = flush $ nowrap $ inTagsSimple "literallayout" $ inlinesToDocbook opts lst
  | otherwise         = inTagsIndented "para" $ inlinesToDocbook opts lst
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
  let attribs = [("spacing", "compact") | isTightList lst]
  in  inTags True "itemizedlist" attribs $ listItemsToDocbook opts lst
blockToDocbook _ (OrderedList _ []) = empty
blockToDocbook opts (OrderedList (start, numstyle, _) (first:rest)) =
  let numeration = case numstyle of
                       DefaultStyle -> []
                       Decimal      -> [("numeration", "arabic")]
                       Example      -> [("numeration", "arabic")]
                       UpperAlpha   -> [("numeration", "upperalpha")]
                       LowerAlpha   -> [("numeration", "loweralpha")]
                       UpperRoman   -> [("numeration", "upperroman")]
                       LowerRoman   -> [("numeration", "lowerroman")]
      spacing    = [("spacing", "compact") | isTightList (first:rest)]
      attribs    = numeration ++ spacing
      items      = if start == 1
                      then listItemsToDocbook opts (first:rest)
                      else (inTags True "listitem" [("override",show start)]
                           (blocksToDocbook opts $ map plainToPara first)) $$
                           listItemsToDocbook opts rest
  in  inTags True "orderedlist" attribs items
blockToDocbook opts (DefinitionList lst) =
  let attribs = [("spacing", "compact") | isTightList $ concatMap snd lst]
  in  inTags True "variablelist" attribs $ deflistItemsToDocbook opts lst
blockToDocbook _ (RawBlock f str)
  | f == "docbook" = text str -- raw XML block
  | f == "html"    = text str -- allow html for backwards compatibility
  | otherwise      = empty
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

hasLineBreaks :: [Inline] -> Bool
hasLineBreaks = getAny . query isLineBreak . walk removeNote
  where
    removeNote :: Inline -> Inline
    removeNote (Note _) = Str ""
    removeNote x = x
    isLineBreak :: Inline -> Any
    isLineBreak LineBreak = Any True
    isLineBreak _ = Any False

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
inlineToDocbook opts (Span _ ils) =
  inlinesToDocbook opts ils
inlineToDocbook _ (Code _ str) =
  inTagsSimple "literal" $ text (escapeStringForXML str)
inlineToDocbook opts (Math t str)
  | isMathML (writerHTMLMathMethod opts) =
    case writeMathML dt <$> readTeX str of
      Right r  -> inTagsSimple tagtype
                  $ text $ Xml.ppcElement conf
                  $ fixNS
                  $ removeAttr r
      Left _   -> inlinesToDocbook opts
                  $ texMathToInlines t str
  | otherwise = inlinesToDocbook opts $ texMathToInlines t str
     where (dt, tagtype) = case t of
                            InlineMath  -> (DisplayInline,"inlineequation")
                            DisplayMath -> (DisplayBlock,"informalequation")
           conf = Xml.useShortEmptyTags (const False) Xml.defaultConfigPP
           removeAttr e = e{ Xml.elAttribs = [] }
           fixNS' qname = qname{ Xml.qPrefix = Just "mml" }
           fixNS = everywhere (mkT fixNS')
inlineToDocbook _ (RawInline f x) | f == "html" || f == "docbook" = text x
                                  | otherwise                     = empty
inlineToDocbook _ LineBreak = text "\n"
inlineToDocbook _ Space = space
inlineToDocbook opts (Link txt (src, _))
  | Just email <- stripPrefix "mailto:" src =
      let emailLink = inTagsSimple "email" $ text $
                      escapeStringForXML $ email
      in  case txt of
           [Str s] | escapeURI s == email -> emailLink
           _             -> inlinesToDocbook opts txt <+>
                              char '(' <> emailLink <> char ')'
  | otherwise =
      (if isPrefixOf "#" src
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

isMathML :: HTMLMathMethod -> Bool
isMathML (MathML _) = True
isMathML _          = False
