{-
Copyright (C) 2006 John MacFarlane <jgm at berkeley dot edu>

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
   Copyright   : Copyright (C) 2006 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm at berkeley dot edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to Docbook XML.
-}
module Text.Pandoc.Writers.Docbook ( 
                                     writeDocbook
                                   ) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.Entities ( encodeEntities )
import Text.Regex ( mkRegex, matchRegex )
import Data.Char ( toLower, ord )
import Data.List ( isPrefixOf, partition )
import Text.PrettyPrint.HughesPJ hiding ( Str )

-- | Data structure for defining hierarchical Pandoc documents
data Element = Blk Block 
             | Sec [Inline] [Element] deriving (Eq, Read, Show)

-- | Returns true on Header block with level at least 'level'
headerAtLeast :: Int -> Block -> Bool
headerAtLeast level (Header x _) = x <= level
headerAtLeast level _ = False

-- | Convert list of Pandoc blocks into list of Elements (hierarchical) 
hierarchicalize :: [Block] -> [Element]
hierarchicalize [] = []
hierarchicalize (block:rest) = 
  case block of
    (Header level title) -> let (thisSection, rest') = break (headerAtLeast 
                                                       level) rest in
                            (Sec title (hierarchicalize thisSection)):
                            (hierarchicalize rest') 
    x                    -> (Blk x):(hierarchicalize rest)

-- | Convert list of authors to a docbook <author> section
authorToDocbook :: WriterOptions -> [Char] -> Doc
authorToDocbook opts name = inTagsIndented opts "author" $ 
  if ',' `elem` name
    then -- last name first
      let (lastname, rest) = break (==',') name 
          firstname = removeLeadingSpace rest in
      inTagsSimple opts "firstname" (text $ stringToSGML opts firstname) <> 
      inTagsSimple opts "surname" (text $ stringToSGML opts lastname) 
    else -- last name last
      let namewords = words name
          lengthname = length namewords 
          (firstname, lastname) = case lengthname of
            0  -> ("","") 
            1  -> ("", name)
            n  -> (joinWithSep " " (take (n-1) namewords), last namewords) in
       inTagsSimple opts "firstname" (text $ stringToSGML opts firstname) $$ 
       inTagsSimple opts "surname" (text $ stringToSGML opts lastname) 

-- | Convert Pandoc document to string in Docbook format.
writeDocbook :: WriterOptions -> Pandoc -> String
writeDocbook opts (Pandoc (Meta title authors date) blocks) = 
  let head = if (writerStandalone opts)
                then text (writerHeader opts)
                else empty
      meta = if (writerStandalone opts)
                then inTagsIndented opts "articleinfo" $
                     (inTagsSimple opts "title" (inlinesToDocbook opts title)) $$ 
                     (vcat (map (authorToDocbook opts) authors)) $$ 
                     (inTagsSimple opts "date" (text $ stringToSGML opts date)) 
                else empty
      blocks' = replaceReferenceLinks blocks
      (noteBlocks, blocks'') = partition isNoteBlock blocks' 
      opts' = opts {writerNotes = noteBlocks}
      elements = hierarchicalize blocks''
      body = text (writerIncludeBefore opts') <>
             vcat (map (elementToDocbook opts') elements) $$
             text (writerIncludeAfter opts')
      body' = if writerStandalone opts'
                then inTagsIndented opts "article" (meta $$ body)
                else body in  
  render $ head $$ body' <> text "\n"

-- | Convert an Element to Docbook.
elementToDocbook :: WriterOptions -> Element -> Doc
elementToDocbook opts (Blk block) = blockToDocbook opts block 
elementToDocbook opts (Sec title elements) =
  -- Docbook doesn't allow sections with no content, so insert some if needed
  let elements' = if null elements
                    then [Blk (Para [])]
                    else elements in 
  inTagsIndented opts "section" $
  inTagsSimple opts "title" (wrap opts title) $$
  vcat (map (elementToDocbook opts) elements') 

-- | Convert a list of Pandoc blocks to Docbook.
blocksToDocbook :: WriterOptions -> [Block] -> Doc
blocksToDocbook opts = vcat . map (blockToDocbook opts)

-- | Convert a list of lists of blocks to a list of Docbook list items.
listItemsToDocbook :: WriterOptions -> [[Block]] -> Doc
listItemsToDocbook opts items = 
  vcat $ map (listItemToDocbook opts) items

-- | Convert a list of blocks into a Docbook list item.
listItemToDocbook :: WriterOptions -> [Block] -> Doc
listItemToDocbook opts item =
  let plainToPara (Plain x) = Para x
      plainToPara y = y in
  let item' = map plainToPara item in
  inTagsIndented opts "listitem" (blocksToDocbook opts item')

-- | Convert a Pandoc block element to Docbook.
blockToDocbook :: WriterOptions -> Block -> Doc
blockToDocbook opts Blank = text ""
blockToDocbook opts Null = empty
blockToDocbook opts (Plain lst) = wrap opts lst
blockToDocbook opts (Para lst) = 
  inTagsIndented opts "para" (wrap opts lst)
blockToDocbook opts (BlockQuote blocks) =
  inTagsIndented opts "blockquote" (blocksToDocbook opts blocks)
blockToDocbook opts (CodeBlock str) = 
  text "<screen>\n" <> text (escapeSGML str) <> text "\n</screen>"
blockToDocbook opts (BulletList lst) = 
  inTagsIndented opts "itemizedlist" $ listItemsToDocbook opts lst 
blockToDocbook opts (OrderedList lst) = 
  inTagsIndented opts "orderedlist" $ listItemsToDocbook opts lst 
blockToDocbook opts (RawHtml str) = text str -- raw XML block 
blockToDocbook opts HorizontalRule = empty -- not semantic
blockToDocbook opts (Note _ _) = empty -- shouldn't occur
blockToDocbook opts (Key _ _) = empty  -- shouldn't occur
blockToDocbook opts _ = inTagsIndented opts "para" (text "Unknown block type")

-- | Put string in CDATA section
cdata :: String -> Doc
cdata str = text $ "<![CDATA[" ++ str ++ "]]>"

-- | Take list of inline elements and return wrapped doc.
wrap :: WriterOptions -> [Inline] -> Doc
wrap opts lst = fsep $ map (inlinesToDocbook opts) (splitBy Space lst)

-- | Convert a list of inline elements to Docbook.
inlinesToDocbook :: WriterOptions -> [Inline] -> Doc
inlinesToDocbook opts lst = hcat (map (inlineToDocbook opts) lst)

-- | Convert an inline element to Docbook.
inlineToDocbook :: WriterOptions -> Inline -> Doc
inlineToDocbook opts (Str str) = text $ stringToSGML opts str 
inlineToDocbook opts (Emph lst) = 
  inTagsSimple opts "emphasis" (inlinesToDocbook opts lst)
inlineToDocbook opts (Strong lst) = 
  inTags False opts "emphasis" [("role", "strong")] 
  (inlinesToDocbook opts lst)
inlineToDocbook opts (Code str) = 
  inTagsSimple opts "literal" $ text (escapeSGML str)
inlineToDocbook opts (TeX str) = inlineToDocbook opts (Code str)
inlineToDocbook opts (HtmlInline str) = empty
inlineToDocbook opts LineBreak = 
  text $ "<literallayout></literallayout>" 
inlineToDocbook opts Space = char ' '
inlineToDocbook opts (Link txt (Src src tit)) =
  case (matchRegex (mkRegex "mailto:(.*)") src) of
    Just [addr] -> inTagsSimple opts "email" $ text (escapeSGML addr)
    Nothing     -> inTags False opts "ulink" [("url", src)] $
                   inlinesToDocbook opts txt
inlineToDocbook opts (Link text (Ref ref)) = empty -- shouldn't occur
inlineToDocbook opts (Image alt (Src src tit)) = 
  let titleDoc = if null tit
                   then empty
                   else inTagsIndented opts "objectinfo" $
                        inTagsIndented opts "title" 
                        (text $ stringToSGML opts tit) in
  inTagsIndented opts "inlinemediaobject" $ 
  inTagsIndented opts "imageobject" $
  titleDoc $$ selfClosingTag opts "imagedata" [("fileref", src)] 
inlineToDocbook opts (Image alternate (Ref ref)) = empty --shouldn't occur
inlineToDocbook opts (NoteRef ref) = 
  let notes = writerNotes opts
      hits = filter (\(Note r _) -> r == ref) notes in
  if null hits
    then empty
    else let (Note _ contents) = head hits in
         inTagsIndented opts "footnote" $ blocksToDocbook opts contents
