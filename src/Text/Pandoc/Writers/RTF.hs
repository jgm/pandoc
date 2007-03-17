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
   Module      :  
   Copyright   : Copyright (C) 2006 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm at berkeley dot edu>
   Stability   : alpha 
   Portability : portable

Conversion of 'Pandoc' documents to RTF (rich text format).
-}
module Text.Pandoc.Writers.RTF (
                                writeRTF
                               ) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Regex ( matchRegexAll, mkRegex )
import List ( isSuffixOf )
import Char ( ord, chr )

-- | Convert Pandoc to a string in rich text format.
writeRTF :: WriterOptions -> Pandoc -> String
writeRTF options (Pandoc meta blocks) = 
  -- assumes all notes are at outer level 
  let notes = filter isNoteBlock blocks in  
  let head = if writerStandalone options
                then rtfHeader notes (writerHeader options) meta 
                else ""  
      foot = if writerStandalone options then "\n}\n" else "" 
      body = (writerIncludeBefore options) ++ (concatMap (blockToRTF notes 0) 
             (replaceReferenceLinks blocks)) ++
             (writerIncludeAfter options) in
  head ++ body ++ foot

-- | Convert unicode characters (> 127) into rich text format representation.
handleUnicode :: String -> String
handleUnicode [] = []
handleUnicode (c:cs) = if (ord c) > 127 
                          then '\\':'u':(show (ord c)) ++ "?" ++ 
                               (handleUnicode cs)
                          else c:(handleUnicode cs)

escapeSpecial = backslashEscape "{\\}"
escapeTab = substitute "\\t" "\\tab "

-- | Escape strings as needed for rich text format.
stringToRTF :: String -> String
stringToRTF = handleUnicode . escapeSpecial . escapeTab

-- | Escape raw LaTeX strings for RTF.  Don't escape \t; it might
-- be the first letter of a command!
latexStringToRTF :: String -> String
latexStringToRTF = handleUnicode . escapeSpecial  

-- | Escape things as needed for code block in RTF.
codeStringToRTF :: String -> String
codeStringToRTF str = joinWithSep "\\line\n" (lines (stringToRTF str))

-- | Deal with raw LaTeX.
latexToRTF :: String -> String
latexToRTF str = "{\\cf1 " ++ (latexStringToRTF str) ++ "\\cf0 } "

-- | Make a paragraph with first-line indent, block indent, and space after.
rtfParSpaced :: Int     -- ^ space after (in twips)
             -> Int     -- ^ block indent (in twips)
             -> Int     -- ^ first line indent (relative to block) (in twips)
             -> String  -- ^ string with content
             -> String 
rtfParSpaced spaceAfter indent firstLineIndent content = 
  "{\\pard \\f0 \\sa" ++ (show spaceAfter) ++ " \\li" ++ (show indent) ++ 
  " \\fi" ++ (show firstLineIndent) ++ " " ++ content ++ "\\par}\n"

-- | Default paragraph. 
rtfPar :: Int     -- ^ block indent (in twips)
       -> Int     -- ^ first line indent (relative to block) (in twips)
       -> String  -- ^ string with content
       -> String 
rtfPar = rtfParSpaced 180 

-- | Compact paragraph (e.g. for compact list items).
rtfCompact ::  Int     -- ^ block indent (in twips)
           ->  Int     -- ^ first line indent (relative to block) (in twips)
           ->  String  -- ^ string with content
           ->  String 
rtfCompact = rtfParSpaced 0 

-- number of twips to indent
indentIncrement = 720
listIncrement = 360

-- | Returns appropriate bullet list marker for indent level.
bulletMarker :: Int -> String
bulletMarker indent = case (indent `mod` 720) of
                             0         -> "\\bullet "
                             otherwise -> "\\endash "

-- | Returns appropriate (list of) ordered list markers for indent level.
orderedMarkers :: Int -> [String]
orderedMarkers indent = 
  case (indent `mod` 720) of
      0         -> map (\x -> show x ++ ".") [1..]
      otherwise -> map (\x -> show x ++ ".") $ cycle ['a'..'z']

-- | Returns RTF header.
rtfHeader :: [Block]   -- ^ list of note blocks
          -> String    -- ^ header text
          -> Meta      -- ^ bibliographic information
          -> String
rtfHeader notes headerText (Meta title authors date) =
    let titletext = if null title
                       then "" 
                       else rtfPar 0 0 ("\\qc \\b \\fs36 " ++ 
                                        inlineListToRTF notes title)
        authorstext = if null authors
                         then "" 
                         else rtfPar 0 0 ("\\qc " ++ (joinWithSep "\\" 
                                         (map stringToRTF authors))) 
        datetext = if date == "" 
                      then ""
                      else rtfPar 0 0 ("\\qc " ++ stringToRTF date) in
    let spacer = if null (titletext ++ authorstext ++ datetext)
                    then ""
                    else rtfPar 0 0 "" in
    headerText ++ titletext ++ authorstext ++ datetext ++ spacer

-- | Convert Pandoc block element to RTF.
blockToRTF :: [Block]   -- ^ list of note blocks
           -> Int       -- ^ indent level
           -> Block     -- ^ block to convert
           -> String
blockToRTF notes indent Null = ""
blockToRTF notes indent (Plain lst) = 
  rtfCompact indent 0 (inlineListToRTF notes lst)
blockToRTF notes indent (Para lst) = 
  rtfPar indent 0 (inlineListToRTF notes lst)
blockToRTF notes indent (BlockQuote lst) = 
  concatMap (blockToRTF notes (indent + indentIncrement)) lst 
blockToRTF notes indent (Note ref lst) = "" -- shouldn't be any aftr filtering
blockToRTF notes indent (Key _ _) = ""
blockToRTF notes indent (CodeBlock str) =
  rtfPar indent 0 ("\\f1 " ++ (codeStringToRTF str))
blockToRTF notes indent (RawHtml str) = ""
blockToRTF notes indent (BulletList lst) = 
  spaceAtEnd $ 
  concatMap (listItemToRTF notes indent (bulletMarker indent)) lst
blockToRTF notes indent (OrderedList lst) = 
  spaceAtEnd $ concat $ 
  zipWith (listItemToRTF notes indent) (orderedMarkers indent) lst
blockToRTF notes indent HorizontalRule = 
  rtfPar indent 0 "\\qc \\emdash\\emdash\\emdash\\emdash\\emdash"
blockToRTF notes indent (Header level lst) = 
  rtfPar indent 0 ("\\b \\fs" ++ (show (40 - (level * 4))) ++ " " ++ 
  (inlineListToRTF notes lst))
blockToRTF notes indent (Table caption _ _ headers rows) = 
  blockToRTF notes indent (Para [Str "pandoc: TABLE unsupported in RST writer"])

-- | Ensure that there's the same amount of space after compact
-- lists as after regular lists.
spaceAtEnd :: String -> String
spaceAtEnd str = 
  if isSuffixOf "\\par}\n" str
     then (take ((length str) - 6) str) ++ "\\sa180\\par}\n"
     else str

-- | Convert list item (list of blocks) to RTF.
listItemToRTF :: [Block]    -- ^ list of note blocks
              -> Int        -- ^ indent level
              -> String     -- ^ list start marker
              -> [Block]    -- ^ list item (list of blocks)
              -> [Char]
listItemToRTF notes indent marker [] = 
  rtfCompact (indent + listIncrement) (0 - listIncrement) 
             (marker ++ "\\tx" ++ (show listIncrement) ++ "\\tab ") 
listItemToRTF notes indent marker list = 
  let (first:rest) = map (blockToRTF notes (indent + listIncrement)) list in
  -- insert the list marker into the (processed) first block
  let modFirst = case matchRegexAll (mkRegex "\\\\fi-?[0-9]+") first of
                    Just (before, matched, after, _) -> before ++ "\\fi" ++
                               show (0 - listIncrement) ++ " " ++ marker ++ "\\tx" ++ 
                               show listIncrement ++ "\\tab" ++ after
                    Nothing -> first in
  modFirst ++ (concat rest)

-- | Convert list of inline items to RTF.
inlineListToRTF :: [Block]    -- ^ list of note blocks
                -> [Inline]   -- ^ list of inlines to convert
                -> String
inlineListToRTF notes lst = concatMap (inlineToRTF notes) lst

-- | Convert inline item to RTF.
inlineToRTF :: [Block]        -- ^ list of note blocks
            -> Inline         -- ^ inline to convert
            -> String
inlineToRTF notes (Emph lst) = "{\\i " ++ (inlineListToRTF notes lst) ++ "} "
inlineToRTF notes (Strong lst) = 
  "{\\b " ++ (inlineListToRTF notes lst) ++ "} "
inlineToRTF notes (Quoted SingleQuote lst) = 
  "\\u8216'" ++ (inlineListToRTF notes lst) ++ "\\u8217'"
inlineToRTF notes (Quoted DoubleQuote lst) = 
  "\\u8220\"" ++ (inlineListToRTF notes lst) ++ "\\u8221\""
inlineToRTF notes Apostrophe = "\\u8217'"
inlineToRTF notes Ellipses = "\\u8230?"
inlineToRTF notes EmDash = "\\u8212-"
inlineToRTF notes EnDash = "\\u8211-"
inlineToRTF notes (Code str) = "{\\f1 " ++ (codeStringToRTF str) ++ "} "
inlineToRTF notes (Str str) = stringToRTF str
inlineToRTF notes (TeX str) = latexToRTF str
inlineToRTF notes (HtmlInline str) = ""
inlineToRTF notes (LineBreak) = "\\line "
inlineToRTF notes Space = " "
inlineToRTF notes (Link text (Src src tit)) = 
  "{\\field{\\*\\fldinst{HYPERLINK \"" ++ (codeStringToRTF src) ++ 
  "\"}}{\\fldrslt{\\ul\n" ++ (inlineListToRTF notes text) ++ "\n}}}\n"
inlineToRTF notes (Link text (Ref ref)) = 
  "[" ++ (inlineListToRTF notes text) ++ "][" ++ 
  (inlineListToRTF notes ref) ++ "]"  -- this is what markdown does
inlineToRTF notes (Image alternate (Src source tit)) = 
  "{\\cf1 [image: " ++ source ++ "]\\cf0}" 
inlineToRTF notes (Image alternate (Ref ref)) = "![" ++ 
  (inlineListToRTF notes alternate) ++ "][" ++ 
  (inlineListToRTF notes ref) ++ "]"
inlineToRTF [] (NoteRef ref) = ""
inlineToRTF ((Note firstref firstblocks):rest) (NoteRef ref) = 
  if firstref == ref
     then "{\\super\\chftn}{\\*\\footnote\\chftn\\~\\plain\\pard " ++ 
          (concatMap (blockToRTF rest 0) firstblocks)  ++ "}"
     else inlineToRTF rest (NoteRef ref) 

