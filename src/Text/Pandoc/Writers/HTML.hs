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
import Text.Pandoc.Entities (decodeEntities)
import Text.Regex ( mkRegex, matchRegex )
import Numeric ( showHex )
import Data.Char ( ord, toLower )
import Data.List ( isPrefixOf, partition )
import Control.Monad.State
import Text.XHtml.Strict

type Notes = [Html]

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
      topTitle    = evalState (inlineListToHtml opts tit) []
      topTitle'   = if null titlePrefix
                        then topTitle
                        else titlePrefix +++ " - " +++ topTitle
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
      (blocks', revnotes) = runState (blockListToHtml opts blocks) []
      notes       = reverse revnotes
      before      = primHtml $ writerIncludeBefore opts
      after       = primHtml $ writerIncludeAfter opts
      thebody     = before +++ titleHeader +++ blocks' +++
                    footnoteSection opts notes +++ after
  in  if writerStandalone opts
         then head +++ (body thebody)
         else thebody

-- | Convert list of Note blocks to a footnote <div>.
-- Assumes notes are sorted.
footnoteSection :: WriterOptions -> Notes -> Html
footnoteSection opts notes =
  if null notes 
     then noHtml
     else thediv ! [theclass "footnotes"] $
                   hr +++ (olist << notes)

-- | Obfuscate a "mailto:" link using Javascript.
obfuscateLink :: WriterOptions -> Html -> String -> Html
obfuscateLink opts text src =
  let emailRegex = mkRegex "mailto:*([^@]*)@(.*)"
      text' = show $ text
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
    _ -> anchor ! [href src] $ text  -- malformed email

-- | Obfuscate character as entity.
obfuscateChar :: Char -> String
obfuscateChar char = 
  let num = ord char in
  let numstr = if even num then (show num) else ("x" ++ (showHex num "")) in
  "&#" ++ numstr ++ ";"

-- | Obfuscate string using entities.
obfuscateString :: String -> String
obfuscateString = (concatMap obfuscateChar) . decodeEntities

-- | Convert Pandoc block element to HTML.
blockToHtml :: WriterOptions -> Block -> State Notes Html
blockToHtml opts block = 
  case block of
    (Null)            -> return $ noHtml
    (Plain lst)       -> inlineListToHtml opts lst
    (Para lst)        -> inlineListToHtml opts lst >>= (return . paragraph)
    (RawHtml str)     -> return $ primHtml str
    (HorizontalRule)  -> return $ hr
    (CodeBlock str)   -> return $ pre $ thecode << (str ++ "\n") 
                       -- the final \n for consistency with Markdown.pl
    (BlockQuote blocks) ->  -- in S5, treat list in blockquote specially
                            -- if default is incremental, make it nonincremental; 
                            -- otherwise incremental
       if writerS5 opts
          then let inc = not (writerIncremental opts) in
               case blocks of 
                  [BulletList lst]  -> blockToHtml (opts {writerIncremental = inc})
                                       (BulletList lst)
                  [OrderedList lst] -> blockToHtml (opts {writerIncremental = inc})
                                       (OrderedList lst)
                  otherwise         -> blockListToHtml opts blocks >>= 
                                       (return . blockquote)
          else blockListToHtml opts blocks >>= (return . blockquote)
    (Header level lst) -> do contents <- inlineListToHtml opts lst
                             return $  case level of
                                         1 -> h1 contents
                                         2 -> h2 contents
                                         3 -> h3 contents
                                         4 -> h4 contents
                                         5 -> h5 contents
                                         6 -> h6 contents
                                         _ -> paragraph contents
    (BulletList lst)   -> do contents <- mapM (blockListToHtml opts) lst
                             let attribs = if writerIncremental opts
                                              then [theclass "incremental"]
                                              else []
                             return $ unordList ! attribs $ contents
    (OrderedList lst)  -> do contents <- mapM (blockListToHtml opts) lst
                             let attribs = if writerIncremental opts
                                              then [theclass "incremental"]
                                              else []
                             return $ ordList ! attribs $ contents
    (DefinitionList lst) -> do contents <- mapM (\(term, def) ->
                                                 do term' <- inlineListToHtml opts term
                                                    def' <- blockListToHtml opts def
                                                    return $ (term', def'))
                                                 lst
                               let attribs = if writerIncremental opts
                                                then [theclass "incremental"]
                                                else []
                               return $ defList ! attribs $ contents
    (Table capt aligns widths headers rows) ->
                            do let alignStrings = map alignmentToString aligns
                               captionDoc <- if null capt
                                                then return noHtml
                                                else inlineListToHtml opts capt >>=
                                                     (return . caption)
                               colHeads <- colHeadsToHtml opts alignStrings 
                                                          widths headers
                               rows' <- mapM (tableRowToHtml opts alignStrings) rows
                               return $ table $ captionDoc +++ colHeads +++ rows'

colHeadsToHtml opts alignStrings widths headers =
  do heads <- sequence $ zipWith3 
                         (\align width item -> tableItemToHtml opts th align width item) 
                         alignStrings widths headers
     return $ tr $ toHtmlFromList heads

alignmentToString alignment = case alignment of
                                 AlignLeft    -> "left"
                                 AlignRight   -> "right"
                                 AlignCenter  -> "center"
                                 AlignDefault -> "left"

tableRowToHtml opts aligns cols =
  do contents <- sequence $ zipWith3 (tableItemToHtml opts td) aligns (repeat 0) cols 
     return $ tr $ toHtmlFromList contents

tableItemToHtml opts tag align' width item =
  do contents <- blockListToHtml opts item
     let attrib = [align align'] ++ 
                  if (width /= 0) 
                    then [thestyle ("{width: " ++ show (truncate (100*width)) ++ "%;}")]
                    else [] 
     return $ tag ! attrib $ contents

blockListToHtml :: WriterOptions -> [Block] -> State Notes Html
blockListToHtml opts lst = mapM (blockToHtml opts) lst >>= (return . toHtmlFromList)

-- | Convert list of Pandoc inline elements to HTML.
inlineListToHtml :: WriterOptions -> [Inline] -> State Notes Html
inlineListToHtml opts lst = mapM (inlineToHtml opts) lst >>= (return . toHtmlFromList)

-- | Convert Pandoc inline element to HTML.
inlineToHtml :: WriterOptions -> Inline -> State Notes Html
inlineToHtml opts inline =
  case inline of  
    (Str str)        -> return $ stringToHtml str
    (Space)          -> return $ stringToHtml " "
    (LineBreak)      -> return $ br
    (EmDash)         -> return $ primHtmlChar "mdash"
    (EnDash)         -> return $ primHtmlChar "ndash"
    (Ellipses)       -> return $ primHtmlChar "hellip"
    (Apostrophe)     -> return $ primHtmlChar "rsquo"
    (Emph lst)       -> inlineListToHtml opts lst >>= (return . emphasize)
    (Strong lst)     -> inlineListToHtml opts lst >>= (return . strong)
    (Code str)       -> return $ thecode << str
    (Quoted quoteType lst) ->
                        let (leftQuote, rightQuote) = case quoteType of
                              SingleQuote -> (primHtmlChar "lsquo", 
                                              primHtmlChar "rsquo")
                              DoubleQuote -> (primHtmlChar "ldquo", 
                                              primHtmlChar "rdquo") in 
                        do contents <- inlineListToHtml opts lst
                           return $ leftQuote +++ contents +++ rightQuote
    (TeX str)        -> return $ stringToHtml str
    (HtmlInline str) -> return $ primHtml str 
    (Link txt (src,tit)) ->
                        do linkText <- inlineListToHtml opts txt
                           return $ if (isPrefixOf "mailto:" src)
                                      then obfuscateLink opts linkText src 
                                      else anchor ! ([href src] ++ 
                                                     if null tit 
                                                        then [] 
                                                        else [title tit]) $ 
                                           linkText
    (Image txt (source,tit)) ->
                        do alternate <- inlineListToHtml opts txt
                           let alternate' = renderHtmlFragment alternate
                           let attributes = [src source, title tit] ++ 
                                            if null txt then [] else [alt alternate']
                           return $ image ! attributes 
                           -- note:  null title included, as in Markdown.pl 
    (Note contents)  -> do notes <- get
                           let number = (length notes) + 1
                           let ref = show number
                           htmlContents <- blockListToNote opts ref contents 
                           modify (htmlContents:) -- push contents onto front of notes
                           return $ anchor ! [href ("#fn" ++ ref),
                                              theclass "footnoteRef",
                                              identifier ("fnref" ++ ref)] << sup << ref

blockListToNote :: WriterOptions -> String -> [Block] -> State Notes Html
blockListToNote opts ref blocks =
  do contents <- blockListToHtml opts blocks
     let backlink = anchor ! [href ("#fnref" ++ ref), theclass "footnoteBacklink",
                             title ("Jump back to footnote " ++ ref)] $
                             (primHtmlChar "#8617")
     return $ li ! [identifier ("fn" ++ ref)] $ contents +++ backlink

