{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{- |
   Module      : Text.Pandoc.XML
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Functions for escaping and formatting XML.
-}
module Text.Pandoc.XML ( escapeCharForXML,
                         escapeStringForXML,
                         inTags,
                         selfClosingTag,
                         inTagsSimple,
                         inTagsIndented,
                         toEntities,
                         toHtml5Entities,
                         fromEntities,
                         html4Attributes,
                         html5Attributes,
                         rdfaAttributes ) where

import Data.Char (isAscii, isSpace, ord)
import Data.Text (Text)
import qualified Data.Text as T
import Text.HTML.TagSoup.Entity (lookupEntity, htmlEntities)
import Text.DocLayout
import Text.Printf (printf)
import qualified Data.Map as M
import Data.String
import qualified Data.Set as Set

-- | Escape one character as needed for XML.
escapeCharForXML :: Char -> Text
escapeCharForXML x = case x of
                       '&' -> "&amp;"
                       '<' -> "&lt;"
                       '>' -> "&gt;"
                       '"' -> "&quot;"
                       c   -> T.singleton c

-- | Escape string as needed for XML.  Entity references are not preserved.
escapeStringForXML :: Text -> Text
escapeStringForXML = T.concatMap escapeCharForXML . T.filter isLegalXMLChar
  where isLegalXMLChar c = c == '\t' || c == '\n' || c == '\r' ||
                           (c >= '\x20' && c <= '\xD7FF') ||
                           (c >= '\xE000' && c <= '\xFFFD') ||
                           (c >= '\x10000' && c <= '\x10FFFF')
  -- see https://www.w3.org/TR/xml/#charsets

-- | Escape newline characters as &#10;
escapeNls :: Text -> Text
escapeNls = T.concatMap $ \x -> case x of
  '\n' -> "&#10;"
  c    -> T.singleton c

-- | Return a text object with a string of formatted XML attributes.
attributeList :: (HasChars a, IsString a) => [(Text, Text)] -> Doc a
attributeList = hcat . map
  (\(a, b) -> text (T.unpack $ " " <> escapeStringForXML a <> "=\"" <>
  escapeNls (escapeStringForXML b) <> "\""))

-- | Put the supplied contents between start and end tags of tagType,
--   with specified attributes and (if specified) indentation.
inTags :: (HasChars a, IsString a)
      => Bool -> Text -> [(Text, Text)] -> Doc a -> Doc a
inTags isIndented tagType attribs contents =
  let openTag = char '<' <> text (T.unpack tagType) <> attributeList attribs <>
                char '>'
      closeTag  = text "</" <> text (T.unpack tagType) <> char '>'
  in  if isIndented
         then openTag $$ nest 2 contents $$ closeTag
         else openTag <> contents <> closeTag

-- | Return a self-closing tag of tagType with specified attributes
selfClosingTag :: (HasChars a, IsString a)
               => Text -> [(Text, Text)] -> Doc a
selfClosingTag tagType attribs =
  char '<' <> text (T.unpack tagType) <> attributeList attribs <> text " />"

-- | Put the supplied contents between start and end tags of tagType.
inTagsSimple :: (HasChars a, IsString a)
             => Text -> Doc a -> Doc a
inTagsSimple tagType = inTags False tagType []

-- | Put the supplied contents in indented block btw start and end tags.
inTagsIndented :: (HasChars a, IsString a)
               => Text -> Doc a -> Doc a
inTagsIndented tagType = inTags True tagType []

-- | Escape all non-ascii characters using numerical entities.
toEntities :: Text -> Text
toEntities = T.concatMap go
  where go c | isAscii c = T.singleton c
             | otherwise = T.pack (printf "&#x%X;" (ord c))

-- | Escape all non-ascii characters using HTML5 entities, falling
-- back to numerical entities.
toHtml5Entities :: Text -> Text
toHtml5Entities = T.concatMap go
  where go c | isAscii c = T.singleton c
             | otherwise =
                 case M.lookup c html5EntityMap of
                   Just t  -> T.singleton '&' <> t <> T.singleton ';'
                   Nothing -> T.pack ("&#" ++ show (ord c) ++ ";")

html5EntityMap :: M.Map Char Text
html5EntityMap = foldr go mempty htmlEntities
  where go (ent, s) entmap =
         case s of
           [c] -> M.insertWith
                   (\new old -> if T.length new > T.length old
                                   then old
                                   else new) c ent' entmap
             where ent' = T.takeWhile (/=';') (T.pack ent)
           _   -> entmap


-- Unescapes XML entities
fromEntities :: Text -> Text
fromEntities = T.pack . fromEntities'

fromEntities' :: Text -> String
fromEntities' (T.uncons -> Just ('&', xs)) =
  case lookupEntity $ T.unpack ent' of
        Just c  -> c <> fromEntities' rest
        Nothing -> "&" <> fromEntities' xs
    where (ent, rest) = case T.break (\c -> isSpace c || c == ';') xs of
                          (zs,T.uncons -> Just (';',ys)) -> (zs,ys)
                          (zs, ys) -> (zs,ys)
          ent'
            | Just ys <- T.stripPrefix "#X" ent = "#x" <> ys  -- workaround tagsoup bug
            | Just ('#', _) <- T.uncons ent     = ent
            | otherwise                         = ent <> ";"
fromEntities' t = case T.uncons t of
  Just (x, xs) -> x : fromEntities' xs
  Nothing      -> ""

html5Attributes :: Set.Set Text
html5Attributes = Set.fromList
  [ "abbr"
  , "accept"
  , "accept-charset"
  , "accesskey"
  , "action"
  , "allow"
  , "allowfullscreen"
  , "allowpaymentrequest"
  , "allowusermedia"
  , "alt"
  , "as"
  , "async"
  , "autocapitalize"
  , "autocomplete"
  , "autofocus"
  , "autoplay"
  , "charset"
  , "checked"
  , "cite"
  , "class"
  , "color"
  , "cols"
  , "colspan"
  , "content"
  , "contenteditable"
  , "controls"
  , "coords"
  , "crossorigin"
  , "data"
  , "datetime"
  , "decoding"
  , "default"
  , "defer"
  , "dir"
  , "dirname"
  , "disabled"
  , "download"
  , "draggable"
  , "enctype"
  , "enterkeyhint"
  , "for"
  , "form"
  , "formaction"
  , "formenctype"
  , "formmethod"
  , "formnovalidate"
  , "formtarget"
  , "headers"
  , "height"
  , "hidden"
  , "high"
  , "href"
  , "hreflang"
  , "http-equiv"
  , "id"
  , "imagesizes"
  , "imagesrcset"
  , "inputmode"
  , "integrity"
  , "is"
  , "ismap"
  , "itemid"
  , "itemprop"
  , "itemref"
  , "itemscope"
  , "itemtype"
  , "kind"
  , "label"
  , "lang"
  , "list"
  , "loading"
  , "loop"
  , "low"
  , "manifest"
  , "max"
  , "maxlength"
  , "media"
  , "method"
  , "min"
  , "minlength"
  , "multiple"
  , "muted"
  , "name"
  , "nomodule"
  , "nonce"
  , "novalidate"
  , "onabort"
  , "onafterprint"
  , "onauxclick"
  , "onbeforeprint"
  , "onbeforeunload"
  , "onblur"
  , "oncancel"
  , "oncanplay"
  , "oncanplaythrough"
  , "onchange"
  , "onclick"
  , "onclose"
  , "oncontextmenu"
  , "oncopy"
  , "oncuechange"
  , "oncut"
  , "ondblclick"
  , "ondrag"
  , "ondragend"
  , "ondragenter"
  , "ondragexit"
  , "ondragleave"
  , "ondragover"
  , "ondragstart"
  , "ondrop"
  , "ondurationchange"
  , "onemptied"
  , "onended"
  , "onerror"
  , "onfocus"
  , "onhashchange"
  , "oninput"
  , "oninvalid"
  , "onkeydown"
  , "onkeypress"
  , "onkeyup"
  , "onlanguagechange"
  , "onload"
  , "onloadeddata"
  , "onloadedmetadata"
  , "onloadend"
  , "onloadstart"
  , "onmessage"
  , "onmessageerror"
  , "onmousedown"
  , "onmouseenter"
  , "onmouseleave"
  , "onmousemove"
  , "onmouseout"
  , "onmouseover"
  , "onmouseup"
  , "onoffline"
  , "ononline"
  , "onpagehide"
  , "onpageshow"
  , "onpaste"
  , "onpause"
  , "onplay"
  , "onplaying"
  , "onpopstate"
  , "onprogress"
  , "onratechange"
  , "onrejectionhandled"
  , "onreset"
  , "onresize"
  , "onscroll"
  , "onsecuritypolicyviolation"
  , "onseeked"
  , "onseeking"
  , "onselect"
  , "onstalled"
  , "onstorage"
  , "onsubmit"
  , "onsuspend"
  , "ontimeupdate"
  , "ontoggle"
  , "onunhandledrejection"
  , "onunload"
  , "onvolumechange"
  , "onwaiting"
  , "onwheel"
  , "open"
  , "optimum"
  , "pattern"
  , "ping"
  , "placeholder"
  , "playsinline"
  , "poster"
  , "preload"
  , "readonly"
  , "referrerpolicy"
  , "rel"
  , "required"
  , "reversed"
  , "role"
  , "rows"
  , "rowspan"
  , "sandbox"
  , "scope"
  , "selected"
  , "shape"
  , "size"
  , "sizes"
  , "slot"
  , "span"
  , "spellcheck"
  , "src"
  , "srcdoc"
  , "srclang"
  , "srcset"
  , "start"
  , "step"
  , "style"
  , "tabindex"
  , "target"
  , "title"
  , "translate"
  , "type"
  , "typemustmatch"
  , "updateviacache"
  , "usemap"
  , "value"
  , "width"
  , "workertype"
  , "wrap"
  ]

-- See https://en.wikipedia.org/wiki/RDFa, https://www.w3.org/TR/rdfa-primer/
rdfaAttributes :: Set.Set Text
rdfaAttributes = Set.fromList
  [ "about"
  , "rel"
  , "rev"
  , "src"
  , "href"
  , "resource"
  , "property"
  , "content"
  , "datatype"
  , "typeof"
  , "vocab"
  , "prefix"
  ]

html4Attributes :: Set.Set Text
html4Attributes = Set.fromList
  [ "abbr"
  , "accept"
  , "accept-charset"
  , "accesskey"
  , "action"
  , "align"
  , "alink"
  , "alt"
  , "archive"
  , "axis"
  , "background"
  , "bgcolor"
  , "border"
  , "cellpadding"
  , "cellspacing"
  , "char"
  , "charoff"
  , "charset"
  , "checked"
  , "cite"
  , "class"
  , "classid"
  , "clear"
  , "code"
  , "codebase"
  , "codetype"
  , "color"
  , "cols"
  , "colspan"
  , "compact"
  , "content"
  , "coords"
  , "data"
  , "datetime"
  , "declare"
  , "defer"
  , "dir"
  , "disabled"
  , "enctype"
  , "face"
  , "for"
  , "frame"
  , "frameborder"
  , "headers"
  , "height"
  , "href"
  , "hreflang"
  , "hspace"
  , "http-equiv"
  , "id"
  , "ismap"
  , "label"
  , "lang"
  , "language"
  , "link"
  , "longdesc"
  , "marginheight"
  , "marginwidth"
  , "maxlength"
  , "media"
  , "method"
  , "multiple"
  , "name"
  , "nohref"
  , "noresize"
  , "noshade"
  , "nowrap"
  , "object"
  , "onblur"
  , "onchange"
  , "onclick"
  , "ondblclick"
  , "onfocus"
  , "onkeydown"
  , "onkeypress"
  , "onkeyup"
  , "onload"
  , "onmousedown"
  , "onmousemove"
  , "onmouseout"
  , "onmouseover"
  , "onmouseup"
  , "onreset"
  , "onselect"
  , "onsubmit"
  , "onunload"
  , "profile"
  , "prompt"
  , "readonly"
  , "rel"
  , "rev"
  , "rows"
  , "rowspan"
  , "rules"
  , "scheme"
  , "scope"
  , "scrolling"
  , "selected"
  , "shape"
  , "size"
  , "span"
  , "src"
  , "standby"
  , "start"
  , "style"
  , "summary"
  , "tabindex"
  , "target"
  , "text"
  , "title"
  , "usemap"
  , "valign"
  , "value"
  , "valuetype"
  , "version"
  , "vlink"
  , "vspace"
  , "width"
  ]
