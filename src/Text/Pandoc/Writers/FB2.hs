{-
Copyright (c) 2011, Sergey Astanin
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
    * Neither the name of the Sergey Astanin nor the names of other
      contributors may be used to endorse or promote products derived from this
      software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

{- | Conversion of 'Pandoc' documents to FB2 (FictionBook2) format.

FictionBook is an XML-based e-book format. For more information see:
<http://www.fictionbook.org/index.php/Eng:XML_Schema_Fictionbook_2.1>

-}
module Text.Pandoc.Writers.FB2 (writeFB2)  where

import Data.Char (toUpper)
import Text.XML.Light

import Text.Pandoc.Definition
import Text.Pandoc.Shared (WriterOptions, orderedListMarkers)
import Text.Pandoc.Generic (bottomUp)

-- | Produce an FB2 document from a 'Pandoc' document.
writeFB2 :: WriterOptions    -- ^ conversion options
         -> Pandoc           -- ^ document to convert
         -> String           -- ^ FictionBook2 document (not encoded yet)
writeFB2 _ (Pandoc meta blocks) = (xml_head ++) . showContent $ fb2_xml
  where
  fb2_xml = el "FictionBook" (fb2_attrs, [desc, body])
  xml_head = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  fb2_attrs =
      let xmlns = "http://www.gribuser.ru/xml/fictionbook/2.0"
          xlink = "http://www.w3.org/1999/xlink"
      in  [ Attr (QName "xmlns" Nothing Nothing) xmlns
          , Attr (QName "l" Nothing (Just "xmlns")) xlink ]
  desc = description meta
  body = el "body" $ frontpage ++ renderSections 1 blocks
  frontpage =
      [ el "title" (el "p" (cMap toXml . docTitle $ meta))
      , el "annotation" ((map (el "p" . cMap plain)
                                  (docAuthors meta ++ [docDate meta])))
      ]
  description meta' =
      el "description"
         [ el "title-info"
             ((booktitle meta') ++ (authors meta') ++ (docdate meta'))
         , el "document-info"
           [ el "program-used" "pandoc" ] -- FIXME: add version info
         ]
  booktitle :: Meta -> [Content]
  booktitle meta' =
      let t = cMap toXml . docTitle $ meta'
      in  if null $ t
          then []
          else [ el "book-title" t ]
  authors :: Meta -> [Content]
  authors meta' = cMap author (docAuthors meta')
  author :: [Inline] -> [Content]
  author ss =
      let ws = words . cMap plain $ ss
          email = (el "email") `fmap` (take 1 $ filter ('@' `elem`) ws)
          ws' = filter ('@' `notElem`) ws
          names = case ws' of
                    (nickname:[]) -> [ el "nickname" nickname ]
                    (fname:lname:[]) -> [ el "first-name" fname
                                       , el "last-name" lname ]
                    (fname:rest) -> [ el "first-name" fname
                                   , el "middle-name" (concat . init $ rest)
                                   , el "last-name" (last rest) ]
                    ([]) -> []
      in  list $ el "author" (names ++ email)
  docdate :: Meta -> [Content]
  docdate meta' =
      let ss = docDate meta'
          d = cMap toXml ss
      in  if null d
          then []
          else [el "date" d]

-- Divide the stream of blocks into sections and convert to XML representation.
renderSections :: Int -> [Block] -> [Content]
renderSections level blocks =
    let secs = splitSections level blocks
    in  map (renderSection level) secs

renderSection :: Int -> ([Inline], [Block]) -> Content
renderSection level (ttl, bs) =
    let title = if null ttl
                then []
                else [el "title" (el "p" (cMap toXml ttl))]
                -- FIXME: only <p> and <empty-line> are allowed within <title>
        subsecs = splitSections (level + 1) bs
        content = if length subsecs == 1  -- if no subsections
                  then cMap blockToXml bs
                  else renderSections (level + 1) bs
    in  el "section" (title ++ content)

-- Divide the stream of block elements into sections: [(title, blocks).
splitSections :: Int -> [Block] -> [([Inline], [Block])]
splitSections level blocks = reverse $ revSplit (reverse blocks)
  where
  revSplit rblocks =
    let (lastsec, before) = break sameLevel rblocks
    in case before of
      ((Header _ inlines):prevblocks) ->
          (inlines, reverse lastsec) : revSplit prevblocks
      _ -> if null lastsec
           then []
           else [([], reverse lastsec)]
  sameLevel (Header n _) = n == level
  sameLevel _ = False

-- Convert a block-level Pandoc's element to FictionBook XML representation.
blockToXml :: Block -> [Content]
blockToXml (Plain ss) = cMap toXml ss  -- FIXME: can lead to malformed FB2
blockToXml (Para ss) = [ el "p" (cMap toXml ss) ]
blockToXml (CodeBlock _ s) = map (el "p" . el "code") . lines $ s
blockToXml (RawBlock _ s) = [ el "p" (el "code" s) ]
blockToXml (BlockQuote bs) = [ el "cite" (cMap blockToXml bs) ]
blockToXml (OrderedList a bss) =
    let markers = orderedListMarkers a
        mkitem mrk bs = el "cite" $ [ txt mrk, txt " " ] ++ cMap blockToXml bs
    in  map (uncurry mkitem) (zip markers bss)
blockToXml (BulletList bss) =
    let mkitem bs = el "cite" $ [ txt "• " ] ++ cMap blockToXml bs
    in  map mkitem bss
blockToXml (DefinitionList defs) =
    cMap mkdef defs
    where
      mkdef (term, bss) =
          let def = cMap (cMap blockToXml) bss
          in  [ el "p" (wrap "strong" term), el "cite" def ]
blockToXml (Header _ _) = undefined  -- should never happen
blockToXml HorizontalRule = [ el "empty-line" ()
                            , el "p" (txt (replicate 10 '—'))
                            , el "empty-line" () ]
blockToXml _ = []  -- FIXME: Table, Null

-- Convert a Pandoc's Inline element to FictionBook XML representation.
toXml :: Inline -> [Content]
toXml (Str s) = [txt s]
toXml (Emph ss) = list $ wrap "emphasis" ss
toXml (Strong ss) = list $ wrap "strong" ss
toXml (Strikeout ss) = list $ wrap "strikethrough" ss
toXml (Superscript ss) = list $ wrap "sup" ss
toXml (Subscript ss) = list $ wrap "sub" ss
toXml (SmallCaps ss) = cMap toXml $ bottomUp (map toUpper) ss
toXml (Quoted SingleQuote ss) = [txt "‘"] ++ cMap toXml ss ++ [txt "’"]
toXml (Quoted DoubleQuote ss) = [txt "“"] ++ cMap toXml ss ++ [txt "”"]
toXml (Cite _ ss) = cMap toXml ss  -- FIXME: support citation styles
toXml (Code _ s) = [el "code" s]
toXml Space = [txt " "]
toXml EmDash = [txt "—"]
toXml EnDash = [txt "–"]
toXml Apostrophe = [txt "’"]
toXml Ellipses = [txt "…"]
toXml LineBreak = [el "empty-line" ()]
toXml (Math _ s) = [el "code" s] -- FIXME: generate formula images
toXml (RawInline _ s) = [el "code" s] -- FIXME: attempt to convert to plaintext
toXml (Link text (_,_)) = cMap toXml text -- FIXME: url in footnotes
toXml (Image alt (_,_)) = cMap toXml alt  -- FIXME: embed images
toXml (Note _) = [txt ""] -- FIXME: implement footnotes

-- Wrap all inlines with an XML tag (given its unqualified name).
wrap :: String -> [Inline] -> Content
wrap tagname inlines = el tagname $ cMap toXml inlines

-- Create a singleton list.
list :: a -> [a]
list = (:[])

-- Convert an 'Inline' to plaintext.
plain :: Inline -> String
plain (Str s) = s
plain (Emph ss) = concat (map plain ss)
plain (Strong ss) = concat (map plain ss)
plain (Strikeout ss) = concat (map plain ss)
plain (Superscript ss) = concat (map plain ss)
plain (Subscript ss) = concat (map plain ss)
plain (SmallCaps ss) = concat (map plain ss)
plain (Quoted _ ss) = concat (map plain ss)
plain (Cite _ ss) = concat (map plain ss)  -- FIXME
plain (Code _ s) = s
plain Space = " "
plain EmDash = "—"
plain EnDash = "–"
plain Apostrophe = "’"
plain Ellipses = "…"
plain LineBreak = "\n"
plain (Math _ s) = s
plain (RawInline _ s) = s
plain (Link text (url,_)) = concat (map plain text ++ [" <", url, ">"])
plain (Image alt _) = concat (map plain alt)
plain (Note _) = ""  -- FIXME

-- | Create an XML element.
el :: (Node t)
   => String   -- ^ unqualified element name
   -> t        -- ^ node contents
   -> Content  -- ^ XML content
el name cs = Elem $ unode name cs

-- | Create a plain-text XML content.
txt :: String -> Content
txt s = Text $ CData CDataText s Nothing

-- | Abbreviation for 'concatMap'.
cMap :: (a -> [b]) -> [a] -> [b]
cMap = concatMap