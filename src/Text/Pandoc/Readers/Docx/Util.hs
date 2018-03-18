{-# LANGUAGE NoImplicitPrelude #-}
module Text.Pandoc.Readers.Docx.Util (
                                        NameSpaces
                                      , elemName
                                      , isElem
                                      , elemToNameSpaces
                                      , findChildByName
                                      , findChildrenByName
                                      , findAttrByName
                                      ) where

import Prelude
import Data.Maybe (mapMaybe)
import Text.XML.Light

type NameSpaces = [(String, String)]

elemToNameSpaces :: Element -> NameSpaces
elemToNameSpaces = mapMaybe attrToNSPair . elAttribs

attrToNSPair :: Attr -> Maybe (String, String)
attrToNSPair (Attr (QName s _ (Just "xmlns")) val) = Just (s, val)
attrToNSPair _                                     = Nothing

elemName :: NameSpaces -> String -> String -> QName
elemName ns prefix name =
  QName name (lookup prefix ns) (if null prefix then Nothing else Just prefix)

isElem :: NameSpaces -> String -> String -> Element -> Bool
isElem ns prefix name element =
  let ns' = ns ++ elemToNameSpaces element
  in qName (elName element) == name &&
     qURI (elName element) == lookup prefix ns'

findChildByName :: NameSpaces -> String -> String -> Element -> Maybe Element
findChildByName ns pref name el =
  let ns' = ns ++ elemToNameSpaces el
  in  findChild (elemName ns' pref name) el

findChildrenByName :: NameSpaces -> String -> String -> Element -> [Element]
findChildrenByName ns pref name el =
  let ns' = ns ++ elemToNameSpaces el
  in  findChildren (elemName ns' pref name) el

findAttrByName :: NameSpaces -> String -> String -> Element -> Maybe String
findAttrByName ns pref name el =
  let ns' = ns ++ elemToNameSpaces el
  in  findAttr (elemName ns' pref name) el
