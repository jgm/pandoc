{-
Copyright (C) 2006-2014 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-2014 John MacFarlane
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
                         fromEntities ) where

import Text.Pandoc.Pretty
import Data.Char (ord, isAscii, isSpace)
import Text.Pandoc.Compat.TagSoupEntity (lookupEntity)

-- | Escape one character as needed for XML.
escapeCharForXML :: Char -> String
escapeCharForXML x = case x of
                       '&'    -> "&amp;"
                       '<'    -> "&lt;"
                       '>'    -> "&gt;"
                       '"'    -> "&quot;"
                       c      -> [c]

-- | Escape string as needed for XML.  Entity references are not preserved.
escapeStringForXML :: String -> String
escapeStringForXML = concatMap escapeCharForXML

-- | Escape newline characters as &#10;
escapeNls :: String -> String
escapeNls (x:xs)
  | x == '\n' = "&#10;" ++ escapeNls xs
  | otherwise = x : escapeNls xs
escapeNls []     = []

-- | Return a text object with a string of formatted XML attributes.
attributeList :: [(String, String)] -> Doc
attributeList = hcat . map
  (\(a, b) -> text (' ' : escapeStringForXML a ++ "=\"" ++
  escapeNls (escapeStringForXML b) ++ "\""))

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

-- | Escape all non-ascii characters using numerical entities.
toEntities :: String -> String
toEntities [] = ""
toEntities (c:cs)
  | isAscii c = c : toEntities cs
  | otherwise = "&#" ++ show (ord c) ++ ";" ++ toEntities cs

-- Unescapes XML entities
fromEntities :: String -> String
fromEntities ('&':xs) =
  case lookupEntity ent of
        Just c  -> c : fromEntities rest
        Nothing -> '&' : fromEntities xs
    where (ent, rest) = case break (\c -> isSpace c || c == ';') xs of
                             (zs,';':ys) -> (zs,ys)
                             _           -> ("",xs)
fromEntities (x:xs) = x : fromEntities xs
fromEntities [] = []
