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
module Text.Pandoc.Writers.HTML ( writeHtml, writeHtmlString ) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Regex ( mkRegex, matchRegex )
import Numeric ( showHex )
import Data.Char ( ord, toLower )
import Data.List ( isPrefixOf, partition )
import Text.XHtml.Strict

-- | Convert Pandoc document to Html string.
writeHtmlString :: WriterOptions -> Pandoc -> String
writeHtmlString opts = 
  if writerStandalone opts
     then renderHtml . (writeHtml opts)
     else renderHtmlFragment . (writeHtml opts)

-- | Convert Pandoc document to Html structure.
writeHtml :: WriterOptions -> Pandoc -> Html
writeHtml opts (Pandoc (Meta tit authors date) blocks) = 
  let titlePrefix = writerTitlePrefix opts
      topTitle    = inlineListToHtml opts tit
      topTitle'   = if not (null titlePrefix)
                       then stringToHtml titlePrefix +++
                            if not (null tit)
                               then '-' +++ topTitle
                               else noHtml
                       else topTitle
      head        = header $ thetitle topTitle' +++ 
                    meta ! [httpequiv "Content-Type", 
                            content "text/html; charset=UTF-8"] +++
                    meta ! [name "generator", content "pandoc"] +++
                    (toHtmlFromList $ 
                    map (\a -> meta ! [name "author", content a]) authors) +++
                    (if null date
                       then noHtml
                       else meta ! [name "date", content date]) +++
                    primHtml (writerHeader opts)
      titleHeader = if (writerStandalone opts) && (not (null tit)) && 
                    (not (writerS5 opts))
                        then h1 ! [theclass "title"] $ topTitle
                        else noHtml
      blocks'     = replaceReferenceLinks blocks
      (noteBlocks, blocks'') = partition isNoteBlock blocks' 
      before      = primHtml $ writerIncludeBefore opts
      after       = primHtml $ writerIncludeAfter opts
      thebody     = before +++ titleHeader +++
                    toHtmlFromList (map (blockToHtml opts) blocks'') +++
                    footnoteSection opts noteBlocks +++ after
  in  if writerStandalone opts
         then head +++ (body thebody)
         else thebody

-- | Convert list of Note blocks to a footnote <div>.
-- Assumes notes are sorted.
footnoteSection :: WriterOptions -> [Block] -> Html
footnoteSection opts notes =
  if null notes 
    then noHtml
    else thediv ! [theclass "footnotes"] $
         hr +++ (olist $ toHtmlFromList $ map (blockToHtml opts) notes)

-- | Obfuscate a "mailto:" link using Javascript.
obfuscateLink :: WriterOptions -> [Inline] -> String -> Html
obfuscateLink opts txt src =
  let emailRegex = mkRegex "mailto:*([^@]*)@(.*)"
      text' = show $ inlineListToHtml opts txt 
      src'  = map toLower src in
  case (matchRegex emailRegex src') of
    (Just [name, domain]) ->
      let domain'  = substitute "." " dot " domain
          at'      = obfuscateChar '@'
          linkText = if src' == ("mailto:" ++ text')
                        then "e"
                        else "'" ++ text' ++ "'" 
          altText  = if src' == ("mailto:" ++ text')
                        then name ++ " at " ++ domain'
                        else text' ++ " (" ++ name ++ " at " ++ 
                             domain' ++ ")" in 
      if writerStrictMarkdown opts
        then -- need to use primHtml or &'s are escaped to &amp; in URL
             primHtml $ "<a href=\"" ++ (obfuscateString src')
             ++ "\">" ++ (obfuscateString text') ++ "</a>"
        else (script ! [thetype "text/javascript"] $
             primHtml ("\n<!--\nh='" ++ 
             obfuscateString domain ++ "';a='" ++ at' ++ "';n='" ++ 
             obfuscateString name ++ "';e=n+a+h;\n" ++
             "document.write('<a h'+'ref'+'=\"ma'+'ilto'+':'+e+'\">'+" ++ 
             linkText  ++ "+'<\\/'+'a'+'>');\n// -->\n")) +++  
             noscript (primHtml $ obfuscateString altText)
    _ -> anchor ! [href src] $ inlineListToHtml opts txt -- malformed email

-- | Obfuscate character as entity.
obfuscateChar :: Char -> String
obfuscateChar char = 
  let num = ord char in
  let numstr = if even num then (show num) else ("x" ++ (showHex num "")) in
  "&#" ++ numstr ++ ";"

-- | Obfuscate string using entities.
obfuscateString :: String -> String
obfuscateString = concatMap obfuscateChar

-- | Convert Pandoc block element to HTML.
blockToHtml :: WriterOptions -> Block -> Html
blockToHtml opts Null = noHtml
blockToHtml opts (Plain lst) = inlineListToHtml opts lst
blockToHtml opts (Para lst) = paragraph $ inlineListToHtml opts lst
blockToHtml opts (BlockQuote blocks) = 
  if (writerS5 opts)
     then  -- in S5, treat list in blockquote specially
           -- if default is incremental, make it nonincremental; 
           -- otherwise incremental
           let inc = not (writerIncremental opts) in
           case blocks of 
              [BulletList lst]  -> blockToHtml (opts {writerIncremental = 
                                                        inc}) (BulletList lst)
              [OrderedList lst] -> blockToHtml (opts {writerIncremental =
                                                       inc}) (OrderedList lst)
              otherwise         -> blockquote $ toHtmlFromList $ 
                                   map (blockToHtml opts) blocks
     else blockquote $ toHtmlFromList $ map (blockToHtml opts) blocks
blockToHtml opts (Note ref lst) = 
  let contents = toHtmlFromList $ map (blockToHtml opts) lst
      backlink = anchor ! [href ("#fnref" ++ ref), theclass "footnoteBacklink",
                           title ("Jump back to footnote " ++ ref)] $
                 (primHtmlChar "#8617") in
  li ! [identifier ("fn" ++ ref)] $ contents +++ backlink
blockToHtml opts (Key _ _) = noHtml
blockToHtml opts (CodeBlock str) = 
  pre $ thecode << (str ++ "\n") -- the final \n for consistency with Markdown.pl
blockToHtml opts (RawHtml str) = primHtml str 
blockToHtml opts (BulletList lst) = 
  let attribs = if writerIncremental opts
                   then [theclass "incremental"]
                   else [] in
  unordList ! attribs $ map (blockListToHtml opts) lst 
blockToHtml opts (OrderedList lst) = 
  let attribs = if writerIncremental opts
                   then [theclass "incremental"]
                   else [] in
  ordList ! attribs $ map (blockListToHtml opts) lst 
blockToHtml opts (DefinitionList lst) = 
  let attribs = if writerIncremental opts
                   then [theclass "incremental"]
                   else [] in
  defList ! attribs $ map (\(term, def) -> (inlineListToHtml opts term, 
                          blockListToHtml opts def)) lst 
blockToHtml opts HorizontalRule = hr
blockToHtml opts (Header level lst) = 
  let contents = inlineListToHtml opts lst in
  case level of
    1 -> h1 contents
    2 -> h2 contents
    3 -> h3 contents
    4 -> h4 contents
    5 -> h5 contents
    6 -> h6 contents
    _ -> paragraph contents
blockToHtml opts (Table capt aligns widths headers rows) =
  let alignStrings = map alignmentToString aligns
      captionDoc   = if null capt
                       then noHtml
                       else caption $ inlineListToHtml opts capt in
  table $ captionDoc +++
  (colHeadsToHtml opts alignStrings widths headers) +++
  (toHtmlFromList $ map (tableRowToHtml opts alignStrings) rows)

colHeadsToHtml opts alignStrings widths headers =
  let heads = zipWith3
              (\align width item -> tableItemToHtml opts th align width item) 
              alignStrings widths headers in
  tr $ toHtmlFromList heads

alignmentToString alignment = case alignment of
                                 AlignLeft    -> "left"
                                 AlignRight   -> "right"
                                 AlignCenter  -> "center"
                                 AlignDefault -> "left"
tableRowToHtml opts aligns cols =
  tr $ toHtmlFromList $ zipWith3 (tableItemToHtml opts td) aligns (repeat 0) cols

tableItemToHtml opts tag align' width item =
  let attrib = [align align'] ++ 
               if (width /= 0) 
                 then [thestyle ("{width: " ++ show (truncate (100*width)) ++ "%;}")]
                 else [] in 
  tag ! attrib $ toHtmlFromList $ map (blockToHtml opts) item

blockListToHtml :: WriterOptions -> [Block] -> Html
blockListToHtml opts list = 
  toHtmlFromList $ map (blockToHtml opts) list

-- | Convert list of Pandoc inline elements to HTML.
inlineListToHtml :: WriterOptions -> [Inline] -> Html
inlineListToHtml opts lst = toHtmlFromList $ map (inlineToHtml opts) lst

-- | Convert Pandoc inline element to HTML.
inlineToHtml :: WriterOptions -> Inline -> Html
inlineToHtml opts (Emph lst) = 
  emphasize $ inlineListToHtml opts lst
inlineToHtml opts (Strong lst) = 
  strong $ inlineListToHtml opts lst
inlineToHtml opts (Code str) =  
  thecode << str
inlineToHtml opts (Quoted SingleQuote lst) =
  primHtmlChar "lsquo" +++ inlineListToHtml opts lst +++ primHtmlChar "rsquo"
inlineToHtml opts (Quoted DoubleQuote lst) =
  primHtmlChar "ldquo" +++ inlineListToHtml opts lst +++ primHtmlChar "rdquo"
inlineToHtml opts EmDash = primHtmlChar "mdash"
inlineToHtml opts EnDash = primHtmlChar "ndash"
inlineToHtml opts Ellipses = primHtmlChar "hellip"
inlineToHtml opts Apostrophe = primHtmlChar "rsquo"
inlineToHtml opts (Str str) = stringToHtml str
inlineToHtml opts (TeX str) = stringToHtml str
inlineToHtml opts (HtmlInline str) = primHtml str
inlineToHtml opts (LineBreak) = br
inlineToHtml opts Space = stringToHtml " "
inlineToHtml opts (Link txt (Src src tit)) = 
  if (isPrefixOf "mailto:" src)
     then obfuscateLink opts txt src 
     else anchor ! ([href src] ++ if null tit then [] else [title tit]) $
          inlineListToHtml opts txt
inlineToHtml opts (Link txt (Ref ref)) = 
  '[' +++ (inlineListToHtml opts txt) +++ 
  ']' +++ '[' +++ (inlineListToHtml opts ref) +++
  ']'
  -- this is what markdown does, for better or worse
inlineToHtml opts (Image alttext (Src source tit)) = 
  let alternate = renderHtmlFragment $ inlineListToHtml opts alttext in 
  image ! ([src source, title tit] ++ if null alttext then [] else [alt alternate])
  -- note:  null title is included, as in Markdown.pl 
inlineToHtml opts (Image alternate (Ref ref)) = 
  '!' +++ inlineToHtml opts (Link alternate (Ref ref))
inlineToHtml opts (NoteRef ref) = 
  anchor ! [href ("#fn" ++ ref), theclass "footnoteRef", identifier ("fnref" ++ ref)] <<
  sup << ref

