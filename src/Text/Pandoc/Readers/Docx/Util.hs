module Text.Pandoc.Readers.Docx.Util (
                                        NameSpaces
                                      , elemName
                                      , isElem
                                      , elemToNameSpaces
                                      ) where

import Text.XML.Light
import Data.Maybe (mapMaybe)

type NameSpaces = [(String, String)]

elemToNameSpaces :: Element -> NameSpaces
elemToNameSpaces = mapMaybe attrToNSPair . elAttribs

attrToNSPair :: Attr -> Maybe (String, String)
attrToNSPair (Attr (QName s _ (Just "xmlns")) val) = Just (s, val)
attrToNSPair _ = Nothing

elemName :: NameSpaces -> String -> String -> QName
elemName ns prefix name = QName name (lookup prefix ns) (Just prefix)

isElem :: NameSpaces -> String -> String -> Element -> Bool
isElem ns prefix name element =
  qName (elName element) == name &&
  qURI (elName element) == lookup prefix ns
