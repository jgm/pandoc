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
   Module      : Text.Pandoc.Writers.Markdown 
   Copyright   : Copyright (C) 2006 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm at berkeley dot edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to markdown-formatted plain text.

Markdown:  <http://daringfireball.net/projects/markdown/>
-}
module Text.Pandoc.Writers.Markdown (
                                     writeMarkdown
                                    ) where
import Text.Regex ( matchRegex, mkRegex )
import Text.Pandoc.Definition
import Text.Pandoc.Shared 
import Data.List ( group )
import Text.PrettyPrint.HughesPJ hiding ( Str )

-- | Convert Pandoc to Markdown.
writeMarkdown :: WriterOptions -> Pandoc -> String
writeMarkdown options (Pandoc meta blocks) = 
  let body = text (writerIncludeBefore options) <> 
             vcat (map (blockToMarkdown (writerTabStop options)) 
             (formatKeys blocks)) $$ text (writerIncludeAfter options) in
  let head = if (writerStandalone options)
                then ((metaToMarkdown meta) $$ text (writerHeader options)) 
                else empty in
  render $ head <> body

-- | Escape special characters for Markdown.
escapeString :: String -> String
escapeString = backslashEscape "`<\\*_^" 

-- | Escape embedded \" in link title.
escapeLinkTitle :: String -> String
escapeLinkTitle = substitute "\"" "\\\""

-- | Take list of inline elements and return wrapped doc.
wrappedMarkdown :: [Inline] -> Doc
wrappedMarkdown lst = 
  let wrapSection sec = fsep $ map inlineListToMarkdown $ (splitBy Space sec) 
      wrappedSecs     = map wrapSection $ splitBy LineBreak lst
      wrappedSecs'    = foldr (\s rest -> if not (null rest)
                                            then (s <> text "  "):rest
                                            else s:rest) [] wrappedSecs  in
  vcat wrappedSecs'

-- | Insert Blank block between key and non-key
formatKeys :: [Block] -> [Block]
formatKeys [] = []
formatKeys [x] = [x]
formatKeys ((Key x1 y1):(Key x2 y2):rest) = 
  (Key x1 y1):(formatKeys ((Key x2 y2):rest))
formatKeys ((Key x1 y1):rest) = (Key x1 y1):Blank:(formatKeys rest)
formatKeys (x:(Key x1 y1):rest) = x:Blank:(formatKeys ((Key x1 y1):rest))
formatKeys (x:rest) = x:(formatKeys rest)

-- | Convert bibliographic information into Markdown header.
metaToMarkdown :: Meta -> Doc
metaToMarkdown (Meta [] [] "") = empty
metaToMarkdown (Meta title [] "") = (titleToMarkdown title) <> (text "\n")
metaToMarkdown (Meta title authors "") = (titleToMarkdown title) <> 
  (text "\n") <> (authorsToMarkdown authors) <> (text "\n")
metaToMarkdown (Meta title authors date) = (titleToMarkdown title) <> 
  (text "\n") <> (authorsToMarkdown authors) <> (text "\n") <> 
  (dateToMarkdown date) <> (text "\n")

titleToMarkdown :: [Inline] -> Doc
titleToMarkdown lst = text "% " <> (inlineListToMarkdown lst)

authorsToMarkdown :: [String] -> Doc
authorsToMarkdown lst = 
  text "% " <> text (joinWithSep ", " (map escapeString lst))

dateToMarkdown :: String -> Doc
dateToMarkdown str = text "% " <> text (escapeString str)

-- | Convert Pandoc block element to markdown.
blockToMarkdown :: Int    -- ^ Tab stop
                -> Block  -- ^ Block element
                -> Doc 
blockToMarkdown tabStop Blank = text ""
blockToMarkdown tabStop Null = empty
blockToMarkdown tabStop (Plain lst) = wrappedMarkdown lst
blockToMarkdown tabStop (Para lst) = (wrappedMarkdown lst) <> (text "\n")
blockToMarkdown tabStop (BlockQuote lst) = 
  (vcat $ map (\line -> (text "> ") <> (text line)) $ lines $ render $ vcat $ 
  map (blockToMarkdown tabStop) lst) <> (text "\n")
blockToMarkdown tabStop (Note ref lst) = 
  let lns = lines $ render $ vcat $ map (blockToMarkdown tabStop) lst in
  if null lns
     then empty
     else let first = head lns
              rest = tail lns in
          text ("[^" ++ (escapeString ref) ++ "]: ") <> (text first) $$ 
         (vcat $ map (\line -> (text "    ") <> (text line)) rest) <> 
         text "\n"
blockToMarkdown tabStop (Key txt (Src src tit)) = 
  text "  " <> char '[' <> inlineListToMarkdown txt <> char ']' <> 
  text ": " <> text src <> 
  if tit /= "" then text (" \"" ++ (escapeLinkTitle tit) ++ "\"") else empty
blockToMarkdown tabStop (CodeBlock str) = 
  (nest tabStop $ vcat $ map text (lines str)) <> text "\n"
blockToMarkdown tabStop (RawHtml str) = text str
blockToMarkdown tabStop (BulletList lst) = 
  vcat (map (bulletListItemToMarkdown tabStop) lst) <> text "\n"
blockToMarkdown tabStop (OrderedList lst) =  
  vcat (zipWith (orderedListItemToMarkdown tabStop) 
  (enumFromTo 1 (length lst))  lst) <> text "\n"
blockToMarkdown tabStop HorizontalRule = text "\n* * * * *\n"
blockToMarkdown tabStop (Header level lst) = text ((replicate level '#') ++ 
  " ") <> (inlineListToMarkdown lst) <> (text "\n")
blockToMarkdown tabStop (Table caption _ _ headers rows) =
  blockToMarkdown tabStop (Para [Str "pandoc: TABLE unsupported in Markdown writer"])


bulletListItemToMarkdown tabStop list = 
  hang (text "-  ") tabStop (vcat (map (blockToMarkdown tabStop) list))

-- | Convert ordered list item (a list of blocks) to markdown.
orderedListItemToMarkdown :: Int      -- ^ tab stop
                          -> Int      -- ^ ordinal number of list item
                          -> [Block]  -- ^ list item (list of blocks)
                          -> Doc
orderedListItemToMarkdown tabStop num list = 
  hang (text ((show num) ++ "." ++ spacer)) tabStop 
  (vcat (map (blockToMarkdown tabStop) list))
     where spacer = if (num < 10) then " " else ""

-- | Convert list of Pandoc inline elements to markdown.
inlineListToMarkdown :: [Inline] -> Doc
inlineListToMarkdown lst = hcat $ map inlineToMarkdown lst

-- | Convert Pandoc inline element to markdown.
inlineToMarkdown :: Inline -> Doc
inlineToMarkdown (Emph lst) = text "*" <> 
  (inlineListToMarkdown lst) <> text "*"
inlineToMarkdown (Strong lst) = text "**" <> 
  (inlineListToMarkdown lst) <> text "**"
inlineToMarkdown (Quoted SingleQuote lst) = char '\'' <> 
  (inlineListToMarkdown lst) <> char '\''
inlineToMarkdown (Quoted DoubleQuote lst) = char '"' <> 
  (inlineListToMarkdown lst) <> char '"'
inlineToMarkdown EmDash = text "--"
inlineToMarkdown EnDash = char '-'
inlineToMarkdown Apostrophe = char '\''
inlineToMarkdown Ellipses = text "..."
inlineToMarkdown (Code str) =
  let tickGroups = filter (\s -> '`' `elem` s) $ group str 
      longest    = if null tickGroups
                     then 0
                     else maximum $ map length tickGroups 
      marker     = replicate (longest + 1) '`' 
      spacer     = if (longest == 0) then "" else " " in 
  text (marker ++ spacer ++ str ++ spacer ++ marker)
inlineToMarkdown (Str str) = text $ escapeString str
inlineToMarkdown (TeX str) = text str
inlineToMarkdown (HtmlInline str) = text str 
inlineToMarkdown (LineBreak) = text "  \n" 
inlineToMarkdown Space = char ' '
inlineToMarkdown (Link txt (Src src tit)) = 
  let linktext = if (null txt) || (txt == [Str ""])
                    then text "link"
                    else inlineListToMarkdown txt in
  char '[' <> linktext <> char ']' <> char '(' <> text src <> 
  (if tit /= ""
      then text (" \"" ++ (escapeLinkTitle tit) ++ "\"") 
      else empty) <> char ')'
inlineToMarkdown (Link txt (Ref ref)) = 
  let first = char '[' <> inlineListToMarkdown txt <> char ']'
      second = if (txt == ref) 
                 then text "[]"
                 else char '[' <> inlineListToMarkdown ref <> char ']' in
      first <> second
inlineToMarkdown (Image alternate (Src source tit)) = 
  let alt = if (null alternate) || (alternate == [Str ""])
               then text "image" 
               else inlineListToMarkdown alternate in
  char '!' <> char '[' <> alt <> char ']' <> char '(' <> text source <> 
  (if tit /= ""
      then text (" \"" ++ (escapeLinkTitle tit) ++ "\"") 
      else empty) <> char ')'
inlineToMarkdown (Image alternate (Ref ref)) = 
  char '!' <> inlineToMarkdown (Link alternate (Ref ref))
inlineToMarkdown (NoteRef ref) = 
  text "[^" <> text (escapeString ref) <> char ']'
