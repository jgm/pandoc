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
   Module      : Text.Pandoc.Writers.RST 
   Copyright   : Copyright (C) 2006 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm at berkeley dot edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to reStructuredText.

reStructuredText:  <http://docutils.sourceforge.net/rst.html>
-}
module Text.Pandoc.Writers.RST (
                                writeRST 
                               ) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import List ( nubBy )
import Text.PrettyPrint.HughesPJ hiding ( Str )

-- | Convert Pandoc to reStructuredText.
writeRST :: WriterOptions -> Pandoc -> String
writeRST options (Pandoc meta blocks) = 
  let (main, refs) = unzip $ map (blockToRST (writerTabStop options)) 
                       (reformatBlocks $ replaceReferenceLinks blocks) 
      top = if (writerStandalone options)
              then (metaToRST meta) $$ text (writerHeader options) 
              else empty in 
  -- remove duplicate keys
  let refs' = nubBy (\x y -> (render x) == (render y)) refs in
  let body = text (writerIncludeBefore options) <> 
             vcat main $$ text (writerIncludeAfter options) in
  render $ top <> body $$ vcat refs' $$ text "\n"

-- | Escape special RST characters.
escapeString :: String -> String
escapeString = backslashEscape "`\\|*_"

-- | Convert list of inline elements into one 'Doc' of wrapped text
-- and another containing references.
wrappedRST :: [Inline] -> (Doc, Doc)
wrappedRST lst = 
  let wrap_section sec = fsep $ map (fst . inlineListToRST) $ 
                         (splitBy Space sec) in
  ((vcat $ map wrap_section $ (splitBy LineBreak lst)),
  vcat $ map (snd . inlineToRST) lst)

-- | Remove reference keys, and make sure there are blanks before each list.
reformatBlocks :: [Block] -> [Block]
reformatBlocks [] = []
reformatBlocks ((Plain x):(OrderedList y):rest) = 
    (Para x):(reformatBlocks ((OrderedList y):rest))
reformatBlocks ((Plain x):(BulletList y):rest) = 
    (Para x):(reformatBlocks ((BulletList y):rest))
reformatBlocks ((OrderedList x):rest) = 
    (OrderedList (map reformatBlocks x)):(reformatBlocks rest)
reformatBlocks ((BulletList x):rest) = 
    (BulletList (map reformatBlocks x)):(reformatBlocks rest)
reformatBlocks ((BlockQuote x):rest) = 
    (BlockQuote (reformatBlocks x)):(reformatBlocks rest)
reformatBlocks ((Note ref x):rest) = 
    (Note ref (reformatBlocks x)):(reformatBlocks rest)
reformatBlocks ((Key x1 y1):rest) = reformatBlocks rest
reformatBlocks (x:rest) = x:(reformatBlocks rest)

-- | Convert bibliographic information to 'Doc'.
metaToRST :: Meta -> Doc
metaToRST (Meta title authors date) = 
    (titleToRST title) <> (authorsToRST authors) <> (dateToRST date) 

-- | Convert title to 'Doc'.
titleToRST :: [Inline] -> Doc
titleToRST [] = empty
titleToRST lst = 
  let title = fst $ inlineListToRST lst in
  let titleLength = length $ render title in
  let border = text (replicate titleLength '=') in
  border <> char '\n' <> title <> char '\n' <> border <> text "\n\n"

-- | Convert author list to 'Doc'.
authorsToRST :: [String] -> Doc
authorsToRST [] = empty
authorsToRST (first:rest) = text ":Author: " <> text first <> 
                            char '\n' <> (authorsToRST rest)

-- | Convert date to 'Doc'.
dateToRST :: String -> Doc
dateToRST [] = empty
dateToRST str = text ":Date: " <> text (escapeString str) <> char '\n'

-- | Convert Pandoc block element to a 'Doc' containing the main text and 
-- another one containing any references.
blockToRST :: Int        -- ^ tab stop
           -> Block      -- ^ block element to convert
           -> (Doc, Doc) -- ^ first element is text, second is references for end of file
blockToRST tabStop Null = (empty, empty)
blockToRST tabStop (Plain lst) = wrappedRST lst
blockToRST tabStop (Para [TeX str]) =    -- raw latex block
  let str' = if (endsWith '\n' str) then (str ++ "\n") else (str ++ "\n\n") in
  (hang (text "\n.. raw:: latex\n") 3 (vcat $ map text (lines str')), empty)
blockToRST tabStop (Para lst) = ( (fst $ wrappedRST lst) <> (text "\n"), 
                                  snd $ wrappedRST lst )
blockToRST tabStop (BlockQuote lst) = 
  let (main, refs) = unzip $ map (blockToRST tabStop) lst in
  ((nest tabStop $ vcat $ main) <> text "\n", vcat refs)
blockToRST tabStop (Note ref blocks) = 
  let (main, refs) = unzip $ map (blockToRST tabStop) blocks in
  ((hang (text ".. [" <> text (escapeString ref) <> text "] ") 3 (vcat main)),
   vcat refs)
blockToRST tabStop (Key txt (Src src tit)) = 
  (text "ERROR - KEY FOUND", empty) -- shouldn't have a key here
blockToRST tabStop (CodeBlock str) =  (hang (text "::\n") tabStop 
  (vcat $ map text (lines ('\n':(str ++ "\n\n")))), empty)
blockToRST tabStop (RawHtml str) = 
  let str' = if (endsWith '\n' str) then (str ++ "\n") else (str ++ "\n\n") in
  (hang (text "\n.. raw:: html\n") 3 (vcat $ map text (lines str')), empty)
blockToRST tabStop (BulletList lst) =  
  let (main, refs) = unzip $ map (bulletListItemToRST tabStop) lst in
  (vcat main <> text "\n", vcat refs)
blockToRST tabStop (OrderedList lst) =  
  let (main, refs) = unzip $ zipWith (orderedListItemToRST tabStop) 
                                     (enumFromTo 1 (length lst)) lst in
  (vcat main <> text "\n", vcat refs)
blockToRST tabStop HorizontalRule = (text "--------------\n", empty)
blockToRST tabStop (Header level lst) = 
  let (headerText, refs) = inlineListToRST lst in
  let headerLength = length $ render headerText in
  let headerChar = if (level > 5) then ' ' else "=-~^'" !! (level - 1) in
  let border = text $ replicate headerLength headerChar in
  (headerText <> char '\n' <> border <> char '\n', refs)
blockToRST tabStop (Table caption _ _ headers rows) =
  blockToRST tabStop (Para [Str "pandoc: TABLE unsupported in RST writer"])


-- | Convert bullet list item (list of blocks) to reStructuredText.
-- Returns a pair of 'Doc', the first the main text, the second references
bulletListItemToRST :: Int        -- ^ tab stop
                    -> [Block]    -- ^ list item (list of blocks)
                    -> (Doc, Doc)
bulletListItemToRST tabStop list = 
  let (main, refs) = unzip $ map (blockToRST tabStop) list in
  (hang (text "-  ") tabStop (vcat main), (vcat refs))

-- | Convert an ordered list item (list of blocks) to reStructuredText.
-- Returns a pair of 'Doc', the first the main text, the second references
orderedListItemToRST :: Int         -- ^ tab stop
                     -> Int         -- ^ ordinal number of list item
                     -> [Block]     -- ^ list item (list of blocks)
                     -> (Doc, Doc)
orderedListItemToRST tabStop num list = 
  let (main, refs) = unzip $ map (blockToRST tabStop) list 
      spacer = if (length (show num) < 2) then " " else "" in
  (hang (text ((show num) ++ "." ++ spacer)) tabStop (vcat main), (vcat refs))

-- | Convert a list of inline elements to reStructuredText.
-- Returns a pair of 'Doc', the first the main text, the second references.
inlineListToRST :: [Inline] -> (Doc, Doc)
inlineListToRST lst = let (main, refs) = unzip $ map inlineToRST lst in
                      (hcat main, hcat refs)

-- | Convert an inline element to reStructuredText.
-- Returns a pair of 'Doc', the first the main text, the second references.
inlineToRST :: Inline -> (Doc, Doc) -- second Doc is list of refs for end of file
inlineToRST (Emph lst) = let (main, refs) = inlineListToRST lst in
                         (text "*" <> main <> text "*", refs)
inlineToRST (Strong lst) = let (main, refs) = inlineListToRST lst in
                         (text "**" <> main <> text "**", refs)
inlineToRST (Quoted SingleQuote lst) = let (main, refs) = inlineListToRST lst in
  (char '\'' <> main <> char '\'', refs)
inlineToRST (Quoted DoubleQuote lst) = let (main, refs) = inlineListToRST lst in
  (char '"' <> main <> char '"', refs)
inlineToRST EmDash = (text "--", empty)
inlineToRST EnDash = (char '-', empty)
inlineToRST Apostrophe = (char '\'', empty)
inlineToRST Ellipses = (text "...", empty)
inlineToRST (Code str) = (text $ "``" ++ str ++ "``", empty)
inlineToRST (Str str) = (text $ escapeString str, empty)
inlineToRST (TeX str) = (text str, empty)
inlineToRST (HtmlInline str) = (empty, empty)
inlineToRST (LineBreak) = inlineToRST Space -- RST doesn't have line breaks
inlineToRST Space = (char ' ', empty)
--
-- Note:  can assume reference links have been replaced where possible 
-- with explicit links.
--
inlineToRST (Link txt (Src src tit)) = 
  let (linktext, ref') = if (null txt) || (txt == [Str ""])
                            then (text "link", empty)
                            else inlineListToRST $ normalizeSpaces txt in
  let link = char '`' <> linktext <> text "`_" 
      linktext' = render linktext in
  let linktext'' = if (':' `elem` linktext')
                     then "`" ++ linktext' ++ "`"
                     else linktext' in
    let ref = text ".. _" <> text linktext'' <> text ": " <> text src  in
    (link, ref' $$ ref)
inlineToRST (Link txt (Ref ref)) = 
  let (linktext, refs1) = inlineListToRST txt 
      (reftext, refs2) = inlineListToRST ref in
  (char '[' <> linktext <> text "][" <> reftext <> char ']', refs1 $$ refs2)
inlineToRST (Image alternate (Src source tit)) = 
  let (alt, ref') = if (null alternate) || (alternate == [Str ""])
                       then (text "image", empty) 
                       else inlineListToRST $ normalizeSpaces alternate in
  let link = char '|' <> alt <> char '|' in
  let ref = text ".. " <> link <> text " image:: " <> text source  in
  (link, ref' $$ ref)
-- The following case won't normally occur...
inlineToRST (Image alternate (Ref ref)) = 
  let (alttext, refs1) = inlineListToRST alternate
      (reftext, refs2) = inlineListToRST ref in
  (char '|' <> alttext <> char '|', refs1 $$ refs2)
inlineToRST (NoteRef ref) = 
  (text " [" <> text (escapeString ref) <> char ']' <> char '_', empty)
