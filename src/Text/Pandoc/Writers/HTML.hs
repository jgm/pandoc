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
   Module      : Text.Pandoc.Writers.HTML 
   Copyright   : Copyright (C) 2006 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm at berkeley dot edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to HTML.
-}
module Text.Pandoc.Writers.HTML ( 
                                 writeHtml,
                                ) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.Entities ( escapeSGMLString )
import Text.Regex ( mkRegex, matchRegex )
import Numeric ( showHex )
import Data.Char ( ord, toLower )
import Data.List ( isPrefixOf, partition )
import Text.PrettyPrint.HughesPJ hiding ( Str )

-- | Convert Pandoc document to string in HTML format.
writeHtml :: WriterOptions -> Pandoc -> String
writeHtml opts (Pandoc (Meta title authors date) blocks) = 
  let titlePrefix = writerTitlePrefix opts in
  let topTitle = if not (null titlePrefix)
                    then [Str titlePrefix] ++ (if not (null title) 
                                                  then [Str " - "] ++ title
                                                  else [])
                    else title in
  let head = if (writerStandalone opts)
                then htmlHeader opts (Meta topTitle authors date)
                else empty 
      titleBlocks = if (writerStandalone opts) && (not (null title)) && 
                    (not (writerS5 opts))
                       then [RawHtml "<h1 class=\"title\">", Plain title, 
                             RawHtml "</h1>"]
                       else []
      foot = if (writerStandalone opts) 
               then text "</body>\n</html>"
               else empty 
      blocks' = replaceReferenceLinks (titleBlocks ++ blocks)
      (noteBlocks, blocks'') = partition isNoteBlock blocks' 
      before = writerIncludeBefore opts
      after = writerIncludeAfter opts
      body = (if null before then empty else text before) $$
             vcat (map (blockToHtml opts) blocks'') $$
             footnoteSection opts noteBlocks $$
             (if null after then empty else text after) in
  render $ head $$ body $$ foot $$ text ""

-- | Convert list of Note blocks to a footnote <div>.
-- Assumes notes are sorted.
footnoteSection :: WriterOptions -> [Block] -> Doc
footnoteSection opts notes =
  if null notes 
    then empty
    else inTags True "div" [("class","footnotes")] $
         selfClosingTag "hr" [] $$ (inTagsIndented "ol" 
         (vcat $ map (blockToHtml opts) notes))

-- | Obfuscate a "mailto:" link using Javascript.
obfuscateLink :: WriterOptions -> [Inline] -> String -> Doc
obfuscateLink opts txt src =
  let emailRegex = mkRegex "mailto:*([^@]*)@(.*)"
      text' = render $ inlineListToHtml opts txt 
      src' = map toLower src in
  case (matchRegex emailRegex src') of
    (Just [name, domain]) ->
      let domain' = substitute "." " dot " domain
          at' = obfuscateChar '@' in
      let linkText = if src' == ("mailto:" ++ text')
                        then "e"
                        else "'" ++ text' ++ "'" 
          altText  = if src' == ("mailto:" ++ text')
                        then name ++ " at " ++ domain'
                        else text' ++ " (" ++ name ++ " at " ++ 
                             domain' ++ ")" in 
      if writerStrictMarkdown opts
        then inTags False "a" [("href", obfuscateString src')] $
             text $ obfuscateString text'
        else inTags False "script" [("type", "text/javascript")] 
             (text ("\n<!--\nh='" ++ 
             obfuscateString domain ++ "';a='" ++ at' ++ "';n='" ++ 
             obfuscateString name ++ "';e=n+a+h;\n" ++
             "document.write('<a h'+'ref'+'=\"ma'+'ilto'+':'+e+'\">'+" ++ 
             linkText  ++ "+'<\\/'+'a'+'>');\n// -->\n")) <> 
             inTagsSimple "noscript" (text (obfuscateString altText))
    _ -> inTags False "a" [("href", src)]  (text text') -- malformed email

-- | Obfuscate character as entity.
obfuscateChar :: Char -> String
obfuscateChar char = 
  let num = ord char in
  let numstr = if even num then (show num) else ("x" ++ (showHex num "")) in
  "&#" ++ numstr ++ ";"

-- | Obfuscate string using entities.
obfuscateString :: String -> String
obfuscateString = concatMap obfuscateChar

-- | Return an HTML header with appropriate bibliographic information.
htmlHeader :: WriterOptions -> Meta -> Doc
htmlHeader opts (Meta title authors date) = 
  let titletext = inTagsSimple "title" (wrap opts title)
      authortext = if (null authors) 
                      then empty 
                      else selfClosingTag "meta" [("name", "author"), 
                           ("content", 
                            joinWithSep ", " (map escapeSGMLString authors))]  
      datetext = if (date == "")
                    then empty 
                    else selfClosingTag "meta" [("name", "date"),
                         ("content", escapeSGMLString date)] in
  text (writerHeader opts) $$ authortext $$ datetext $$ titletext $$ 
  text "</head>\n<body>"

-- | Take list of inline elements and return wrapped doc.
wrap :: WriterOptions -> [Inline] -> Doc
wrap opts lst = fsep $ map (inlineListToHtml opts) (splitBy Space lst)

-- | Convert Pandoc block element to HTML.
blockToHtml :: WriterOptions -> Block -> Doc
blockToHtml opts Blank = text ""
blockToHtml opts Null = empty
blockToHtml opts (Plain lst) = wrap opts lst 
blockToHtml opts (Para lst) = inTagsIndented "p" $ wrap opts lst
blockToHtml opts (BlockQuote blocks) = 
  if (writerS5 opts)
     then  -- in S5, treat list in blockquote specially
           -- if default is incremental, make it nonincremental; 
           -- otherwise incremental
           let inc = not (writerIncremental opts) in
           case blocks of 
              [BulletList lst] -> blockToHtml (opts {writerIncremental = 
                                                        inc}) (BulletList lst)
              [OrderedList lst] -> blockToHtml (opts {writerIncremental =
                                                       inc}) (OrderedList lst)
              otherwise         -> inTagsIndented "blockquote" $
                                   vcat $ map (blockToHtml opts) blocks
     else inTagsIndented "blockquote" $ vcat $ map (blockToHtml opts) blocks
blockToHtml opts (Note ref lst) = 
  let contents = (vcat $ map (blockToHtml opts) lst) in
  inTags True "li" [("id", "fn" ++ ref)] $
  contents <> inTags False "a" [("href", "#fnref" ++ ref), 
                                ("class", "footnoteBacklink"), 
                                ("title", "Jump back to footnote " ++ ref)] 
                     (text "&#8617;")
blockToHtml opts (Key _ _) = empty
blockToHtml opts (CodeBlock str) = 
  text "<pre><code>" <> text (escapeSGMLString str) <> text "\n</code></pre>"
blockToHtml opts (RawHtml str) = text str 
blockToHtml opts (BulletList lst) = 
  let attribs = if (writerIncremental opts)
                   then [("class","incremental")]
                   else [] in
  inTags True "ul" attribs $ vcat $ map (listItemToHtml opts) lst 
blockToHtml opts (OrderedList lst) = 
  let attribs = if (writerIncremental opts)
                   then [("class","incremental")]
                   else [] in
  inTags True "ol" attribs $ vcat $ map (listItemToHtml opts) lst 
blockToHtml opts HorizontalRule = selfClosingTag "hr" []
blockToHtml opts (Header level lst) = 
  let contents = wrap opts lst in
  if ((level > 0) && (level <= 6))
      then inTagsSimple ("h" ++ show level) contents 
      else inTagsSimple "p" contents 
blockToHtml opts (Table caption aligns widths headers rows) =
  let alignStrings = map alignmentToString aligns
      captionDoc   = if null caption
                       then empty
                       else inTagsSimple "caption" 
                            (inlineListToHtml opts caption) in
  inTagsIndented "table" $ captionDoc $$ 
  (colHeadsToHtml opts alignStrings widths headers) $$ 
  (vcat $ map (tableRowToHtml opts alignStrings) rows)

colHeadsToHtml opts alignStrings widths headers =
  let heads = zipWith3
              (\align width item -> tableItemToHtml opts "th" align width item) 
              alignStrings widths headers in
  inTagsIndented "tr" $ vcat heads

alignmentToString alignment = case alignment of
                                 AlignLeft -> "left"
                                 AlignRight -> "right"
                                 AlignCenter -> "center"
                                 AlignDefault -> "left"

tableRowToHtml opts aligns cols =
  inTagsIndented "tr" $ vcat $ zipWith3 (tableItemToHtml opts "td") aligns (repeat 0) cols

tableItemToHtml opts tag align width item =
  let attrib = [("align", align)] ++ 
               if (width /= 0) 
                 then [("style", "{width: " ++ 
                                 show (truncate (100*width)) ++ "%;}")]
                 else [] in 
  inTags False tag attrib $ vcat $ map (blockToHtml opts) item

listItemToHtml :: WriterOptions -> [Block] -> Doc
listItemToHtml opts list = 
  inTagsSimple "li" $ vcat $ map (blockToHtml opts) list

-- | Convert list of Pandoc inline elements to HTML.
inlineListToHtml :: WriterOptions -> [Inline] -> Doc
inlineListToHtml opts lst = hcat (map (inlineToHtml opts) lst)

-- | Convert Pandoc inline element to HTML.
inlineToHtml :: WriterOptions -> Inline -> Doc
inlineToHtml opts (Emph lst) = 
  inTagsSimple "em" (inlineListToHtml opts lst)
inlineToHtml opts (Strong lst) = 
  inTagsSimple "strong" (inlineListToHtml opts lst)
inlineToHtml opts (Code str) =  
  inTagsSimple "code" $ text (escapeSGMLString str)
inlineToHtml opts (Quoted SingleQuote lst) =
  text "&lsquo;" <> (inlineListToHtml opts lst) <> text "&rsquo;"
inlineToHtml opts (Quoted DoubleQuote lst) =
  text "&ldquo;" <> (inlineListToHtml opts lst) <> text "&rdquo;"
inlineToHtml opts EmDash = text "&mdash;"
inlineToHtml opts EnDash = text "&ndash;"
inlineToHtml opts Ellipses = text "&hellip;"
inlineToHtml opts Apostrophe = text "&rsquo;"
inlineToHtml opts (Str str) = text $ escapeSGMLString str
inlineToHtml opts (TeX str) = text $ escapeSGMLString str
inlineToHtml opts (HtmlInline str) = text str
inlineToHtml opts (LineBreak) = selfClosingTag "br" []
inlineToHtml opts Space = space
inlineToHtml opts (Link txt (Src src title)) = 
  if (isPrefixOf "mailto:" src)
     then obfuscateLink opts txt src 
     else inTags False "a" ([("href", src)] ++ 
          if null title then [] else [("title", title)]) 
          (inlineListToHtml opts txt)
inlineToHtml opts (Link txt (Ref ref)) = 
  char '[' <> (inlineListToHtml opts txt) <> text "][" <> 
  (inlineListToHtml opts ref) <> char ']'
  -- this is what markdown does, for better or worse
inlineToHtml opts (Image alt (Src source title)) = 
  let alternate = render $ inlineListToHtml opts alt in 
  selfClosingTag "img" $ [("src", source)] ++
  (if null alternate then [] else [("alt", alternate)]) ++
  [("title", title)]  -- note:  null title is included, as in Markdown.pl 
inlineToHtml opts (Image alternate (Ref ref)) = 
  text "![" <> (inlineListToHtml opts alternate) <> text "][" <> 
  (inlineListToHtml opts ref) <> char ']'
inlineToHtml opts (NoteRef ref) = 
  inTags False "sup" [("class", "footnoteRef"), ("id", "fnref" ++ ref)]
  (inTags False "a" [("href", "#fn" ++ ref)] $ text ref) 
