{-# LANGUAGE OverloadedStrings     #-}
{- |
   Module      : Text.Pandoc.Readers.HTML.TagCategories
   Copyright   : Copyright (C) 2006-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Categories of tags.
-}
module Text.Pandoc.Readers.HTML.TagCategories
  ( blockHtmlTags
  , blockDocBookTags
  , eitherBlockOrInline
  , epubTags
  , blockTags
  , sectioningContent
  , groupingContent
  )
where

import Data.Set (Set, fromList, unions)
import Data.Text (Text)

eitherBlockOrInline :: Set Text
eitherBlockOrInline = fromList
  ["audio", "applet", "button", "iframe", "embed",
   "del", "ins", "progress", "map", "area", "noscript", "script",
   "object", "svg", "video", "source"]

blockHtmlTags :: Set Text
blockHtmlTags = fromList
   ["?xml", "!DOCTYPE", "address", "article", "aside",
    "blockquote", "body", "canvas",
    "caption", "center", "col", "colgroup", "dd", "details",
    "dir", "div", "dl", "dt", "fieldset", "figcaption", "figure",
    "footer", "form", "h1", "h2", "h3", "h4",
    "h5", "h6", "head", "header", "hgroup", "hr", "html",
    "isindex", "main", "menu", "meta", "noframes", "nav",
    "ol", "output", "p", "pre",
    "section", "summary", "table", "tbody", "textarea",
    "thead", "tfoot", "ul", "dd",
    "dt", "frameset", "li", "tbody", "td", "tfoot",
    "th", "thead", "tr", "script", "style"]

-- We want to allow raw docbook in markdown documents, so we
-- include docbook block tags here too.
blockDocBookTags :: Set Text
blockDocBookTags = fromList
   ["calloutlist", "bibliolist", "glosslist", "itemizedlist",
    "orderedlist", "segmentedlist", "simplelist",
    "variablelist", "caution", "important", "note", "tip",
    "warning", "address", "literallayout", "programlisting",
    "programlistingco", "screen", "screenco", "screenshot",
    "synopsis", "example", "informalexample", "figure",
    "informalfigure", "table", "informaltable", "para",
    "simpara", "formalpara", "equation", "informalequation",
    "figure", "screenshot", "mediaobject", "qandaset",
    "procedure", "task", "cmdsynopsis", "funcsynopsis",
    "classsynopsis", "blockquote", "epigraph", "msgset",
    "sidebar", "title"]

epubTags :: Set Text
epubTags = fromList ["case", "switch", "default"]

blockTags :: Set Text
blockTags = unions [blockHtmlTags, blockDocBookTags, epubTags]

sectioningContent :: [Text]
sectioningContent = ["article", "aside", "nav", "section"]


groupingContent :: [Text]
groupingContent = ["p", "hr", "pre", "blockquote", "ol"
                  , "ul", "li", "dl", "dt", "dt", "dd"
                  , "figure", "figcaption", "div", "main"]
