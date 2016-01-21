{-# LANGUAGE OverloadedStrings, PatternGuards #-}
{-
Copyright (C) 2006-2015 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-2015 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to Docbook XML.
-}
module Text.Pandoc.Writers.TEI (writeTEI) where
import Text.Pandoc.Definition
import Text.Pandoc.XML
import Text.Pandoc.Shared
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Options
import Text.Pandoc.Templates (renderTemplate')
import Data.List ( stripPrefix, isPrefixOf, isSuffixOf )
import Data.Char ( toLower )
import Text.Pandoc.Highlighting ( languages, languagesByExtension )
import Text.Pandoc.Pretty
import Text.Pandoc.ImageSize
import qualified Text.Pandoc.Builder as B

-- | Convert list of authors to a docbook <author> section
authorToTEI :: WriterOptions -> [Inline] -> B.Inlines
authorToTEI opts name' =
  let name = render Nothing $ inlinesToTEI opts name'
      colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  in  B.rawInline "tei" $ render colwidth $
      inTagsSimple "author" (text $ escapeStringForXML name)

-- | Convert Pandoc document to string in Docbook format.
writeTEI :: WriterOptions -> Pandoc -> String
writeTEI opts (Pandoc meta blocks) =
  let elements = hierarchicalize blocks
      colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
      render' = render colwidth
      opts' = if "/book>" `isSuffixOf`
                      (trimr $ writerTemplate opts)
                 then opts{ writerChapters = True }
                 else opts
      startLvl = if writerChapters opts' then 0 else 1
      auths'   = map (authorToTEI opts) $ docAuthors meta
      meta'    = B.setMeta "author" auths' meta
      Just metadata = metaToJSON opts
                 (Just . render colwidth . (vcat .
                          (map (elementToTEI opts' startLvl)) . hierarchicalize))
                 (Just . render colwidth . inlinesToTEI opts')
                 meta'
      main    = render' $ vcat (map (elementToTEI opts' startLvl) elements)
      context = defField "body" main
              $ defField "mathml" (case writerHTMLMathMethod opts of
                                        MathML _ -> True
                                        _        -> False)
              $ metadata
  in  if writerStandalone opts
         then renderTemplate' (writerTemplate opts) context
         else main

-- | Convert an Element to TEI.
elementToTEI :: WriterOptions -> Int -> Element -> Doc
elementToTEI opts _   (Blk block) = blockToTEI opts block
elementToTEI opts lvl (Sec _ _num (id',_,_) title elements) =
  -- TEI doesn't allow sections with no content, so insert some if needed
  let elements' = if null elements
                    then [Blk (Para [])]
                    else elements
      divType = case lvl of
                 n | n == 0           -> "chapter"
                   | n >= 1 && n <= 5 -> "level" ++ show n
                   | otherwise        -> "section"
  in inTags True "div" [("type", divType) | not (null id')] $
--                        ("id", writerIdentifierPrefix opts ++ id') | not (null id')] $
      inTagsSimple "head" (inlinesToTEI opts title) $$
      vcat (map (elementToTEI opts (lvl + 1)) elements')

-- | Convert a list of Pandoc blocks to TEI.
blocksToTEI :: WriterOptions -> [Block] -> Doc
blocksToTEI opts = vcat . map (blockToTEI opts)

-- | Auxiliary function to convert Plain block to Para.
plainToPara :: Block -> Block
plainToPara (Plain x) = Para x
plainToPara x         = x

-- | Convert a list of pairs of terms and definitions into a TEI 
-- list with labels and items.
deflistItemsToTEI :: WriterOptions -> [([Inline],[[Block]])] -> Doc
deflistItemsToTEI opts items =
 vcat $ map (\(term, defs) -> deflistItemToTEI opts term defs) items

-- | Convert a term and a list of blocks into a TEI varlistentry.
deflistItemToTEI :: WriterOptions -> [Inline] -> [[Block]] -> Doc
deflistItemToTEI opts term defs =
  let def' = concatMap (map plainToPara) defs
  in  inTagsIndented "label" (inlinesToTEI opts term) $$
      inTagsIndented "item" (blocksToTEI opts def')

-- | Convert a list of lists of blocks to a list of TEI list items.
listItemsToTEI :: WriterOptions -> [[Block]] -> Doc
listItemsToTEI opts items = vcat $ map (listItemToTEI opts) items

-- | Convert a list of blocks into a TEI list item.
listItemToTEI :: WriterOptions -> [Block] -> Doc
listItemToTEI opts item =
  inTagsIndented "item" $ blocksToTEI opts $ map plainToPara item

imageToTEI :: WriterOptions -> Attr -> String -> Doc
imageToTEI _ attr src = selfClosingTag "graphic" $
  ("url", src) : idAndRole attr ++ dims
  where
    dims = go Width "width" ++ go Height "depth"
    go dir dstr = case (dimension dir attr) of
                    Just a  -> [(dstr, show a)]
                    Nothing -> []

-- | Convert a Pandoc block element to TEI.
blockToTEI :: WriterOptions -> Block -> Doc
blockToTEI _ Null = empty
-- Add ids to paragraphs in divs with ids - this is needed for
-- pandoc-citeproc to get link anchors in bibliographies:
blockToTEI opts (Div (ident,_,_) [Para lst]) =
  let attribs = [("id", ident) | not (null ident)] in
      inTags False "p" attribs $ inlinesToTEI opts lst
blockToTEI opts (Div _ bs) = blocksToTEI opts $ map plainToPara bs
blockToTEI _ (Header _ _ _) = empty -- should not occur after hierarchicalize
-- For TEI simple, text must be within containing block element, so
-- we use plainToPara to ensure that Plain text ends up contained by
-- something.
blockToTEI opts (Plain lst) = blockToTEI opts $ Para lst
-- title beginning with fig: indicates that the image is a figure
--blockToTEI opts (Para [Image attr txt (src,'f':'i':'g':':':_)]) =
--  let alt  = inlinesToTEI opts txt
--      capt = if null txt
--                then empty
--                else inTagsSimple "title" alt
--  in  inTagsIndented "figure" $
--        capt $$
--        (inTagsIndented "mediaobject" $
--           (inTagsIndented "imageobject"
--             (imageToTEI opts attr src)) $$
--           inTagsSimple "textobject" (inTagsSimple "phrase" alt))
blockToTEI opts (Para lst) =
  inTags False "p" [] $ inlinesToTEI opts lst
blockToTEI opts (BlockQuote blocks) =
  inTagsIndented "quote" $ blocksToTEI opts blocks
blockToTEI _ (CodeBlock (_,classes,_) str) =
  text ("<ab type='codeblock " ++ lang ++ "'>") <> cr <>
     flush (text (escapeStringForXML str) <> cr <> text "</ab>")
    where lang  = if null langs
                     then ""
                     else escapeStringForXML (head langs) 
          isLang l    = map toLower l `elem` map (map toLower) languages
          langsFrom s = if isLang s
                           then [s]
                           else languagesByExtension . map toLower $ s
          langs       = concatMap langsFrom classes
blockToTEI opts (BulletList lst) =
  let attribs = [("type", "unordered")]
  in  inTags True "list" attribs $ listItemsToTEI opts lst
blockToTEI _ (OrderedList _ []) = empty
blockToTEI opts (OrderedList (start, numstyle, _) (first:rest)) =
  let attribs = case numstyle of
                       DefaultStyle -> []
                       Decimal      -> [("type", "ordered:arabic")]
                       Example      -> [("type", "ordered:arabic")]
                       UpperAlpha   -> [("type", "ordered:upperalpha")]
                       LowerAlpha   -> [("type", "ordered:loweralpha")]
                       UpperRoman   -> [("type", "ordered:upperroman")]
                       LowerRoman   -> [("type", "ordered:lowerroman")]
      items      = if start == 1
                      then listItemsToTEI opts (first:rest)
                      else (inTags True "item" [("n",show start)]
                           (blocksToTEI opts $ map plainToPara first)) $$
                           listItemsToTEI opts rest
  in  inTags True "list" attribs items
blockToTEI opts (DefinitionList lst) =
  let attribs = [("type", "definition")]
  in  inTags True "list" attribs $ deflistItemsToTEI opts lst
blockToTEI _ (RawBlock f str)
  | f == "tei"     = text str -- raw TEI block (should such a thing exist).
--  | f == "html"    = text str -- allow html for backwards compatibility
  | otherwise      = empty
blockToTEI _ HorizontalRule =
  selfClosingTag "milestone" [("unit","undefined"), ("type","separator"),("rendition","line")]

-- | TEI Tables
-- TEI Simple's tables are composed of cells and rows; other 
-- table info in the AST is here lossily discard.
blockToTEI opts (Table _ _ _ headers rows) =
  let
    headers' = tableHeadersToTEI opts headers
--    headers' = if all null headers
--               then return empty
--               else tableRowToTEI opts headers
  in
    inTags True "table" [] $ 
    vcat $ [headers'] <> map (tableRowToTEI opts) rows 

tableRowToTEI :: WriterOptions
                  -> [[Block]]
                  -> Doc
tableRowToTEI opts cols =
  inTagsIndented "row" $ vcat $ map (tableItemToTEI opts) cols

tableHeadersToTEI :: WriterOptions
                  -> [[Block]]
                  -> Doc
tableHeadersToTEI opts cols =
  inTags True "row" [("role","label")] $ vcat $ map (tableItemToTEI opts) cols

tableItemToTEI :: WriterOptions
                  -> [Block]
                  -> Doc
tableItemToTEI opts item =
  inTags False "cell" [] $ vcat $ map (blockToTEI opts) item

-- | Convert a list of inline elements to TEI.
inlinesToTEI :: WriterOptions -> [Inline] -> Doc
inlinesToTEI opts lst = hcat $ map (inlineToTEI opts) lst

-- | Convert an inline element to TEI.
inlineToTEI :: WriterOptions -> Inline -> Doc
inlineToTEI _ (Str str) = text $ escapeStringForXML str
inlineToTEI opts (Emph lst) =
  inTags False "hi" [("rendition","simple:italic")] $ inlinesToTEI opts lst
inlineToTEI opts (Strong lst) =
  inTags False "hi" [("rendition", "simple:bold")] $ inlinesToTEI opts lst
inlineToTEI opts (Strikeout lst) =
  inTags False "hi" [("rendition", "simple:strikethrough")] $
  inlinesToTEI opts lst
inlineToTEI opts (Superscript lst) =
  inTags False "hi" [("rendition", "simple:superscript")] $ inlinesToTEI opts lst
inlineToTEI opts (Subscript lst) =
  inTags False "hi" [("rendition", "simple:subscript")] $ inlinesToTEI opts lst
inlineToTEI opts (SmallCaps lst) =
  inTags False "hi" [("rendition", "simple:smallcaps")] $
  inlinesToTEI opts lst
inlineToTEI opts (Quoted _ lst) =
  inTagsSimple "quote" $ inlinesToTEI opts lst
inlineToTEI opts (Cite _ lst) =
  inlinesToTEI opts lst
inlineToTEI opts (Span _ ils) =
  inlinesToTEI opts ils
inlineToTEI _ (Code _ str) =
  inTags False "seg" [("type","code")] $ text (escapeStringForXML str)
-- Distinguish display from inline math by wrapping the former in a "figure."
inlineToTEI _ (Math t str) =
  case t of
    InlineMath  -> inTags False "formula" [("notation","TeX")] $
                   text (str)
    DisplayMath -> inTags True "figure" [("type","math")] $
                   inTags False "formula" [("notation","TeX")] $ text (str)
      
inlineToTEI _ (RawInline f x) | f == "tei"     = text x
                              | otherwise      = empty
inlineToTEI _ LineBreak = selfClosingTag "lb" []
inlineToTEI _ Space = space
-- because we use \n for LineBreak, we can't do soft breaks:
inlineToTEI _ SoftBreak = space
inlineToTEI opts (Link attr txt (src, _))
  | Just email <- stripPrefix "mailto:" src =
      let emailLink = text $
                      escapeStringForXML $ email
      in  case txt of
           [Str s] | escapeURI s == email -> emailLink
           _             -> inlinesToTEI opts txt <+>
                              char '(' <> emailLink <> char ')'
  | otherwise =
      (if isPrefixOf "#" src
            then inTags False "ref" $ ("target", drop 1 src) : idAndRole attr
            else inTags False "ref" $ ("target", src) : idAndRole attr ) $
        inlinesToTEI opts txt
inlineToTEI opts (Image attr description (src, tit)) =
  let titleDoc = if null tit
                   then empty
                   else inTags False "figDesc" [] (text $ escapeStringForXML tit)
      imageDesc = if null description
                  then empty
                  else inTags False "head" [] (inlinesToTEI opts description)
  in  inTagsIndented "figure" $ imageDesc $$
      imageToTEI opts attr src $$ titleDoc
inlineToTEI opts (Note contents) =
  inTagsIndented "note" $ blocksToTEI opts contents

idAndRole :: Attr -> [(String, String)]
idAndRole (id',cls,_) = ident ++ role
  where
    ident = if null id'
               then []
               else [("id", id')]
    role  = if null cls
               then []
               else [("role", unwords cls)]

