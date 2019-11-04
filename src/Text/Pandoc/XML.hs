{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{- |
   Module      : Text.Pandoc.XML
   Copyright   : Copyright (C) 2006-2019 John MacFarlane
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
                         fromEntities ) where

import Prelude
import Data.Char (isAscii, isSpace, ord)
import Data.Text (Text)
import qualified Data.Text as T
import Text.HTML.TagSoup.Entity (lookupEntity, htmlEntities)
import Text.DocLayout
import Text.Printf (printf)
import qualified Data.Map as M
import Data.String

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
