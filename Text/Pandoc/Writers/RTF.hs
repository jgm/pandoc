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
   Module      : Text.Pandoc.Writers.RTF
   Copyright   : Copyright (C) 2006-7 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha 
   Portability : portable

Conversion of 'Pandoc' documents to RTF (rich text format).
-}
module Text.Pandoc.Writers.RTF ( writeRTF ) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.Readers.TeXMath
import Data.List ( isSuffixOf )
import Data.Char ( ord, isDigit )

-- | Convert Pandoc to a string in rich text format.
writeRTF :: WriterOptions -> Pandoc -> String
writeRTF options (Pandoc meta blocks) = 
  let head = if writerStandalone options
                then rtfHeader (writerHeader options) meta 
                else ""
      toc  = if writerTableOfContents options
                then tableOfContents $ filter isHeaderBlock blocks
                else "" 
      foot = if writerStandalone options then "\n}\n" else "" 
      body = writerIncludeBefore options ++ 
             concatMap (blockToRTF 0 AlignDefault) blocks ++ 
             writerIncludeAfter options
  in  head ++ toc ++ body ++ foot

-- | Construct table of contents from list of header blocks.
tableOfContents :: [Block] -> String 
tableOfContents headers =
  let contentsTree = hierarchicalize headers
  in  concatMap (blockToRTF 0 AlignDefault) $ 
      [Header 1 [Str "Contents"], 
       BulletList (map elementToListItem contentsTree)]

elementToListItem :: Element -> [Block]
elementToListItem (Blk _) = []
elementToListItem (Sec sectext subsecs) = [Plain sectext] ++
  if null subsecs
     then []
     else [BulletList (map elementToListItem subsecs)]

-- | Convert unicode characters (> 127) into rich text format representation.
handleUnicode :: String -> String
handleUnicode [] = []
handleUnicode (c:cs) =
  if ord c > 127 
     then '\\':'u':(show (ord c)) ++ "?" ++ handleUnicode cs
     else c:(handleUnicode cs)

-- | Escape special characters.
escapeSpecial :: String -> String
escapeSpecial = escapeStringUsing (('\t',"\\tab "):(backslashEscapes "{\\}"))

-- | Escape strings as needed for rich text format.
stringToRTF :: String -> String
stringToRTF = handleUnicode . escapeSpecial

-- | Escape things as needed for code block in RTF.
codeStringToRTF :: String -> String
codeStringToRTF str = joinWithSep "\\line\n" $ lines (stringToRTF str)

-- | Deal with raw LaTeX.
latexToRTF :: String -> String
latexToRTF str = "{\\cf1 " ++ (stringToRTF str) ++ "\\cf0 }"

-- | Make a paragraph with first-line indent, block indent, and space after.
rtfParSpaced :: Int       -- ^ space after (in twips)
             -> Int       -- ^ block indent (in twips)
             -> Int       -- ^ first line indent (relative to block) (in twips)
             -> Alignment -- ^ alignment
             -> String    -- ^ string with content
             -> String 
rtfParSpaced spaceAfter indent firstLineIndent alignment content = 
  let alignString = case alignment of
                           AlignLeft -> "\\ql "
                           AlignRight -> "\\qr "
                           AlignCenter -> "\\qc "
                           AlignDefault -> "\\ql "
  in  "{\\pard " ++ alignString ++
      "\\f0 \\sa" ++ (show spaceAfter) ++ " \\li" ++ (show indent) ++ 
      " \\fi" ++ (show firstLineIndent) ++ " " ++ content ++ "\\par}\n"

-- | Default paragraph. 
rtfPar :: Int       -- ^ block indent (in twips)
       -> Int       -- ^ first line indent (relative to block) (in twips)
       -> Alignment -- ^ alignment
       -> String    -- ^ string with content
       -> String 
rtfPar = rtfParSpaced 180 

-- | Compact paragraph (e.g. for compact list items).
rtfCompact ::  Int       -- ^ block indent (in twips)
           ->  Int       -- ^ first line indent (relative to block) (in twips)
           ->  Alignment -- ^ alignment
           ->  String    -- ^ string with content
           ->  String 
rtfCompact = rtfParSpaced 0 

-- number of twips to indent
indentIncrement = 720
listIncrement = 360

-- | Returns appropriate bullet list marker for indent level.
bulletMarker :: Int -> String
bulletMarker indent = case indent `mod` 720 of
                             0         -> "\\bullet "
                             otherwise -> "\\endash "

-- | Returns appropriate (list of) ordered list markers for indent level.
orderedMarkers :: Int -> ListAttributes -> [String]
orderedMarkers indent (start, style, delim) = 
  if style == DefaultStyle && delim == DefaultDelim
     then case indent `mod` 720 of
              0         -> orderedListMarkers (start, Decimal, Period)
              otherwise -> orderedListMarkers (start, LowerAlpha, Period)
     else orderedListMarkers (start, style, delim)

-- | Returns RTF header.
rtfHeader :: String    -- ^ header text
          -> Meta      -- ^ bibliographic information
          -> String
rtfHeader headerText (Meta title authors date) =
  let titletext = if null title
                     then "" 
                     else rtfPar 0 0 AlignCenter $
                          "\\b \\fs36 " ++ inlineListToRTF title
      authorstext = if null authors
                       then "" 
                       else rtfPar 0 0 AlignCenter (" " ++ (joinWithSep "\\" $
                                                    map stringToRTF authors))
      datetext = if date == "" 
                    then ""
                    else rtfPar 0 0 AlignCenter (" " ++ stringToRTF date) in
  let spacer = if null (titletext ++ authorstext ++ datetext)
                  then ""
                  else rtfPar 0 0 AlignDefault "" in
  headerText ++ titletext ++ authorstext ++ datetext ++ spacer

-- | Convert Pandoc block element to RTF.
blockToRTF :: Int       -- ^ indent level
           -> Alignment -- ^ alignment
           -> Block     -- ^ block to convert
           -> String
blockToRTF _ _ Null = ""
blockToRTF indent alignment (Plain lst) = 
  rtfCompact indent 0 alignment $ inlineListToRTF lst
blockToRTF indent alignment (Para lst) = 
  rtfPar indent 0 alignment $ inlineListToRTF lst
blockToRTF indent alignment (BlockQuote lst) = 
  concatMap (blockToRTF (indent + indentIncrement) alignment) lst 
blockToRTF indent _ (CodeBlock _ str) =
  rtfPar indent 0 AlignLeft ("\\f1 " ++ (codeStringToRTF str))
blockToRTF _ _ (RawHtml str) = ""
blockToRTF indent alignment (BulletList lst) = spaceAtEnd $ 
  concatMap (listItemToRTF alignment indent (bulletMarker indent)) lst
blockToRTF indent alignment (OrderedList attribs lst) = spaceAtEnd $ concat $ 
  zipWith (listItemToRTF alignment indent) (orderedMarkers indent attribs) lst
blockToRTF indent alignment (DefinitionList lst) = spaceAtEnd $ 
  concatMap (definitionListItemToRTF alignment indent) lst
blockToRTF indent _ HorizontalRule = 
  rtfPar indent 0 AlignCenter "\\emdash\\emdash\\emdash\\emdash\\emdash"
blockToRTF indent alignment (Header level lst) = rtfPar indent 0 alignment $
  "\\b \\fs" ++ (show (40 - (level * 4))) ++ " " ++ inlineListToRTF lst
blockToRTF indent alignment (Table caption aligns sizes headers rows) = 
  tableRowToRTF True indent aligns sizes headers ++ 
  concatMap (tableRowToRTF False indent aligns sizes) rows ++
  rtfPar indent 0 alignment (inlineListToRTF caption)

tableRowToRTF :: Bool -> Int -> [Alignment] -> [Float] -> [[Block]] -> String 
tableRowToRTF header indent aligns sizes cols =
  let columns = concat $ zipWith (tableItemToRTF indent) aligns cols
      totalTwips = 6 * 1440 -- 6 inches
      rightEdges = tail $ scanl (\sofar new -> sofar + floor (new * totalTwips))
                                0 sizes
      cellDefs = map (\edge -> (if header
                                   then "\\clbrdrb\\brdrs"
                                   else "") ++ "\\cellx" ++ show edge)
                     rightEdges
      start = "{\n\\trowd \\trgaph120\n" ++ concat cellDefs ++ "\n" ++
              "\\trkeep\\intbl\n{\n"
      end = "}\n\\intbl\\row}\n"
  in  start ++ columns ++ end

tableItemToRTF :: Int -> Alignment -> [Block] -> String 
tableItemToRTF indent alignment item =
  let contents = concatMap (blockToRTF indent alignment) item
  in  "{\\intbl " ++ contents ++ "\\cell}\n"

-- | Ensure that there's the same amount of space after compact
-- lists as after regular lists.
spaceAtEnd :: String -> String
spaceAtEnd str = 
  if isSuffixOf "\\par}\n" str
     then (take ((length str) - 6) str) ++ "\\sa180\\par}\n"
     else str

-- | Convert list item (list of blocks) to RTF.
listItemToRTF :: Alignment  -- ^ alignment
              -> Int        -- ^ indent level
              -> String     -- ^ list start marker
              -> [Block]    -- ^ list item (list of blocks)
              -> [Char]
listItemToRTF alignment indent marker [] = 
  rtfCompact (indent + listIncrement) (0 - listIncrement) alignment 
             (marker ++ "\\tx" ++ (show listIncrement) ++ "\\tab ") 
listItemToRTF alignment indent marker list = 
  let (first:rest) = map (blockToRTF (indent + listIncrement) alignment) list
      listMarker = "\\fi" ++ show (0 - listIncrement) ++ " " ++ marker ++ "\\tx" ++
                      show listIncrement ++ "\\tab"
      insertListMarker ('\\':'f':'i':'-':d:xs) | isDigit d =
        listMarker ++ dropWhile isDigit xs
      insertListMarker ('\\':'f':'i':d:xs) | isDigit d =
        listMarker ++ dropWhile isDigit xs
      insertListMarker (x:xs) =
        x : insertListMarker xs
      insertListmarker [] = []
      -- insert the list marker into the (processed) first block
  in  insertListMarker first ++ concat rest

-- | Convert definition list item (label, list of blocks) to RTF.
definitionListItemToRTF :: Alignment          -- ^ alignment
                        -> Int                -- ^ indent level
                        -> ([Inline],[Block]) -- ^ list item (list of blocks)
                        -> [Char]
definitionListItemToRTF alignment indent (label, items) =
  let labelText = blockToRTF indent alignment (Plain label)
      itemsText = concatMap (blockToRTF (indent + listIncrement) alignment) items
  in  labelText ++ itemsText 

-- | Convert list of inline items to RTF.
inlineListToRTF :: [Inline]   -- ^ list of inlines to convert
                -> String
inlineListToRTF lst = concatMap inlineToRTF lst

-- | Convert inline item to RTF.
inlineToRTF :: Inline         -- ^ inline to convert
            -> String
inlineToRTF (Emph lst) = "{\\i " ++ (inlineListToRTF lst) ++ "}"
inlineToRTF (Strong lst) = "{\\b " ++ (inlineListToRTF lst) ++ "}"
inlineToRTF (Strikeout lst) = "{\\strike " ++ (inlineListToRTF lst) ++ "}"
inlineToRTF (Superscript lst) = "{\\super " ++ (inlineListToRTF lst) ++ "}"
inlineToRTF (Subscript lst) = "{\\sub " ++ (inlineListToRTF lst) ++ "}"
inlineToRTF (Quoted SingleQuote lst) = 
  "\\u8216'" ++ (inlineListToRTF lst) ++ "\\u8217'"
inlineToRTF (Quoted DoubleQuote lst) = 
  "\\u8220\"" ++ (inlineListToRTF lst) ++ "\\u8221\""
inlineToRTF Apostrophe = "\\u8217'"
inlineToRTF Ellipses = "\\u8230?"
inlineToRTF EmDash = "\\u8212-"
inlineToRTF EnDash = "\\u8211-"
inlineToRTF (Code str) = "{\\f1 " ++ (codeStringToRTF str) ++ "}"
inlineToRTF (Str str) = stringToRTF str
inlineToRTF (Math str) = inlineListToRTF $ readTeXMath str
inlineToRTF (TeX str) = ""
inlineToRTF (HtmlInline str) = ""
inlineToRTF (LineBreak) = "\\line "
inlineToRTF Space = " "
inlineToRTF (Link text (src, tit)) = 
  "{\\field{\\*\\fldinst{HYPERLINK \"" ++ (codeStringToRTF src) ++ 
  "\"}}{\\fldrslt{\\ul\n" ++ (inlineListToRTF text) ++ "\n}}}\n"
inlineToRTF (Image alternate (source, tit)) = 
  "{\\cf1 [image: " ++ source ++ "]\\cf0}" 
inlineToRTF (Note contents) =
  "{\\super\\chftn}{\\*\\footnote\\chftn\\~\\plain\\pard " ++ 
  (concatMap (blockToRTF 0 AlignDefault) contents) ++ "}"
