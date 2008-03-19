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
   Module      : Text.Pandoc.XML
   Copyright   : Copyright (C) 2006-7 John MacFarlane
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
                         inTagsIndented  ) where
import Text.PrettyPrint.HughesPJ

-- | Escape one character as needed for XML.
escapeCharForXML :: Char -> String
escapeCharForXML x = case x of
                       '&'  -> "&amp;"
                       '<'  -> "&lt;"
                       '>'  -> "&gt;"
                       '"'  -> "&quot;"
                       '\160' -> "&nbsp;"
                       c    -> [c]

-- | True if the character needs to be escaped.
needsEscaping :: Char -> Bool
needsEscaping c = c `elem` "&<>\"\160"

-- | Escape string as needed for XML.  Entity references are not preserved.
escapeStringForXML :: String -> String
escapeStringForXML ""  = ""
escapeStringForXML str =
  case break needsEscaping str of
    (okay, "")     -> okay
    (okay, (c:cs)) -> okay ++ escapeCharForXML c ++ escapeStringForXML cs

-- | Return a text object with a string of formatted XML attributes.
attributeList :: [(String, String)] -> Doc
attributeList = text .  concatMap
  (\(a, b) -> " " ++ escapeStringForXML a ++ "=\"" ++
  escapeStringForXML b ++ "\"")

-- | Put the supplied contents between start and end tags of tagType,
--   with specified attributes and (if specified) indentation.
inTags:: Bool -> String -> [(String, String)] -> Doc -> Doc
inTags isIndented tagType attribs contents =
  let openTag = char '<' <> text tagType <> attributeList attribs <>
                char '>'
      closeTag  = text "</" <> text tagType <> char '>'
  in  if isIndented
         then openTag $$ nest 2 contents $$ closeTag
         else openTag <> contents <> closeTag

-- | Return a self-closing tag of tagType with specified attributes
selfClosingTag :: String -> [(String, String)] -> Doc
selfClosingTag tagType attribs =
  char '<' <> text tagType <> attributeList attribs <> text " />"

-- | Put the supplied contents between start and end tags of tagType.
inTagsSimple :: String -> Doc -> Doc
inTagsSimple tagType = inTags False tagType []

-- | Put the supplied contents in indented block btw start and end tags.
inTagsIndented :: String -> Doc -> Doc
inTagsIndented tagType = inTags True tagType []
