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
import Text.Pandoc.Writers.HTML ( stringToSmartHtml, stringToHtml )
import Text.Pandoc.Entities ( encodeEntities )
import Text.Html ( stringToHtmlString )
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
    (Header level title) -> let (thisSection, rest') = break (headerAtLeast level) rest in
                            (Sec title (hierarchicalize thisSection)):(hierarchicalize rest') 
    x                    -> (Blk x):(hierarchicalize rest)

-- | Convert list of authors to a docbook <author> section
authorToDocbook :: WriterOptions -> [Char] -> Doc
authorToDocbook options name = indentedInTags options "author" $ 
  if ',' `elem` name
    then -- last name first
      let (lastname, rest) = break (==',') name 
          firstname = removeLeadingSpace rest in
      inTags "firstname" (text $ stringToXML options firstname) <> 
      inTags "surname" (text $ stringToXML options lastname) 
    else -- last name last
      let namewords = words name
          lengthname = length namewords 
          (firstname, lastname) = case lengthname of
            0  -> ("","") 
            1  -> ("", name)
            n  -> (joinWithSep " " (take (n-1) namewords), last namewords) in
       inTags "firstname" (text $ stringToXML options firstname) $$ 
       inTags "surname" (text $ stringToXML options lastname) 

-- | Convert Pandoc document to string in Docbook format.
writeDocbook :: WriterOptions -> Pandoc -> String
writeDocbook options (Pandoc (Meta title authors date) blocks) = 
  let head = if (writerStandalone options)
                then text (writerHeader options)
                else empty
      meta = if (writerStandalone options)
                then indentedInTags options "articleinfo" $
                     (inTags "title" (inlinesToDocbook options title)) $$ 
                     (vcat (map (authorToDocbook options) authors)) $$ 
                     (inTags "date" (text date)) 
                else empty
      blocks' = replaceReferenceLinks blocks
      (noteBlocks, blocks'') = partition isNoteBlock blocks' 
      options' = options {writerNotes = noteBlocks}
      elements = hierarchicalize blocks''
      body = text (writerIncludeBefore options') <>
             vcat (map (elementToDocbook options') elements) $$
             text (writerIncludeAfter options')
      body' = if writerStandalone options'
                then indentedInTags options' "article" (meta $$ body)
                else body in  
  render $ head $$ body' <> text "\n"

-- | Put the supplied contents between start and end tags of tagType,
--   with specified attributes.
inTagsWithAttrib :: String -> [(String, String)] -> Doc -> Doc
inTagsWithAttrib tagType attribs contents = text ("<" ++ tagType ++ 
  (concatMap (\(a, b) -> " " ++ attributeStringToXML a ++ 
  "=\"" ++ attributeStringToXML b ++ "\"") attribs)) <> 
  if isEmpty contents
    then text " />" -- self-closing tag
    else text ">" <> contents <> text ("</" ++ tagType ++ ">") 

-- | Put the supplied contents between start and end tags of tagType.
inTags :: String -> Doc -> Doc
inTags tagType contents = inTagsWithAttrib tagType [] contents

-- | Put the supplied contents in indented block btw start and end tags.
indentedInTags :: WriterOptions -> [Char] -> Doc -> Doc
indentedInTags options tagType contents = text ("<" ++ tagType ++ ">") $$
  nest 2 contents $$ text ("</" ++ tagType ++ ">") 

-- | Convert an Element to Docbook.
elementToDocbook :: WriterOptions -> Element -> Doc
elementToDocbook options (Blk block) = blockToDocbook options block 
elementToDocbook options (Sec title elements) =
  -- Docbook doesn't allow sections with no content, so insert some if needed
  let elements' = if null elements
                    then [Blk (Para [])]
                    else elements in 
  indentedInTags options "section" $
  inTags "title" (wrap options title) $$
  vcat (map (elementToDocbook options) elements') 

-- | Convert a list of Pandoc blocks to Docbook.
blocksToDocbook :: WriterOptions -> [Block] -> Doc
blocksToDocbook options = vcat . map (blockToDocbook options)

-- | Convert a list of lists of blocks to a list of Docbook list items.
listItemsToDocbook :: WriterOptions -> [[Block]] -> Doc
listItemsToDocbook options items = 
  vcat $ map (listItemToDocbook options) items

-- | Convert a list of blocks into a Docbook list item.
listItemToDocbook :: WriterOptions -> [Block] -> Doc
listItemToDocbook options item =
  let plainToPara (Plain x) = Para x
      plainToPara y = y in
  let item' = map plainToPara item in
  indentedInTags options "listitem" (blocksToDocbook options item')

-- | Convert a Pandoc block element to Docbook.
blockToDocbook :: WriterOptions -> Block -> Doc
blockToDocbook options Blank = text ""
blockToDocbook options Null = empty
blockToDocbook options (Plain lst) = wrap options lst
blockToDocbook options (Para lst) = 
  indentedInTags options "para" (wrap options lst)
blockToDocbook options (BlockQuote blocks) =
  indentedInTags options "blockquote" (blocksToDocbook options blocks)
blockToDocbook options (CodeBlock str) = 
  text "<screen>\n" <> text (codeStringToXML str) <> text "\n</screen>"
blockToDocbook options (BulletList lst) = 
  indentedInTags options "itemizedlist" $ listItemsToDocbook options lst 
blockToDocbook options (OrderedList lst) = 
  indentedInTags options "orderedlist" $ listItemsToDocbook options lst 
blockToDocbook options (RawHtml str) = text str -- raw XML block 
blockToDocbook options HorizontalRule = empty -- not semantic
blockToDocbook options (Note _ _) = empty -- shouldn't occur
blockToDocbook options (Key _ _) = empty  -- shouldn't occur
blockToDocbook options _ = indentedInTags options "para" (text "Unknown block type")

-- | Put string in CDATA section
cdata :: String -> Doc
cdata str = text $ "<![CDATA[" ++ str ++ "]]>"

-- | Take list of inline elements and return wrapped doc.
wrap :: WriterOptions -> [Inline] -> Doc
wrap options lst = fsep $ map (hcat . (map (inlineToDocbook options))) (splitBySpace lst)

-- | Escape a string for XML (with "smart" option if specified).
stringToXML :: WriterOptions -> String -> String
stringToXML options = encodeEntities .
                      (if writerSmart options
                         then stringToSmartHtml
                         else stringToHtml)

-- | Escape string to XML appropriate for attributes
attributeStringToXML :: String -> String
attributeStringToXML = gsub "\"" "&quot;" . codeStringToXML

-- | Escape a literal string for XML.
codeStringToXML :: String -> String
codeStringToXML = encodeEntities . gsub "<" "&lt;" . gsub "&" "&amp;" 

-- | Convert a list of inline elements to Docbook.
inlinesToDocbook :: WriterOptions -> [Inline] -> Doc
inlinesToDocbook options lst = hcat (map (inlineToDocbook options) lst)

-- | Convert an inline element to Docbook.
inlineToDocbook :: WriterOptions -> Inline -> Doc
inlineToDocbook options (Str str) = text $ stringToXML options str 
inlineToDocbook options (Emph lst) = 
  inTags "emphasis" (inlinesToDocbook options lst)
inlineToDocbook options (Strong lst) = 
  inTagsWithAttrib "emphasis" [("role", "strong")] 
  (inlinesToDocbook options lst)
inlineToDocbook options (Code str) = 
  inTags "literal" $ text (codeStringToXML str)
inlineToDocbook options (TeX str) = inlineToDocbook options (Code str)
inlineToDocbook options (HtmlInline str) = empty
inlineToDocbook options LineBreak = 
  text $ "<literallayout></literallayout>" 
inlineToDocbook options Space = char ' '
inlineToDocbook options (Link txt (Src src tit)) =
  case (matchRegex (mkRegex "mailto:(.*)") src) of
    Just [addr] -> inTags "email" $ text (codeStringToXML addr)
    Nothing     -> inTagsWithAttrib "ulink" [("url", src)] $
                   inlinesToDocbook options txt
inlineToDocbook options (Link text (Ref ref)) = empty -- shouldn't occur
inlineToDocbook options (Image alt (Src src tit)) = 
  let titleDoc = if null tit
                   then empty
                   else indentedInTags options "objectinfo" $
                        indentedInTags options "title" 
                        (text $ stringToXML options tit) in
  indentedInTags options "inlinemediaobject" $ 
  indentedInTags options "imageobject" $
  titleDoc $$ inTagsWithAttrib "imagedata" [("fileref", src)] empty 
inlineToDocbook options (Image alternate (Ref ref)) = empty --shouldn't occur
inlineToDocbook options (NoteRef ref) = 
  let notes = writerNotes options
      hits = filter (\(Note r _) -> r == ref) notes in
  if null hits
    then empty
    else let (Note _ contents) = head hits in
         indentedInTags options "footnote" $ blocksToDocbook options contents
