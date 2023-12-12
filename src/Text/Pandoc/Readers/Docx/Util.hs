{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Docx.Util
   Copyright   : © 2014-2020 Jesse Rosenthal <jrosenthal@jhu.edu>,
                   2014-2023 John MacFarlane <jgm@berkeley.edu>,
                   2015 Nikolay Yakimov <root@livid.pp.ru>
   License     : GNU GPL, version 2 or above

   Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
   Stability   : alpha
   Portability : portable

Docx reader utility functions.
-}
module Text.Pandoc.Readers.Docx.Util (
                                        NameSpaces
                                      , elemName
                                      , isElem
                                      , elemToNameSpaces
                                      , findChildByName
                                      , findChildrenByName
                                      , findElementByName
                                      , findAttrByName
                                      , extractChildren
                                      ) where

import qualified Data.Text as T
import Data.Text (Text)
import Text.Pandoc.XML.Light
import qualified Data.Map as M
import Data.List (partition)

type NameSpaces = M.Map Text Text

elemToNameSpaces :: Element -> NameSpaces
elemToNameSpaces = foldr (\(Attr qn val) ->
                             case qn of
                               QName s _ (Just "xmlns") -> M.insert s val
                               _ -> id) mempty . elAttribs

elemName :: NameSpaces -> Text -> Text -> QName
elemName ns prefix name =
  QName name (M.lookup prefix ns)
             (if T.null prefix then Nothing else Just prefix)

isElem :: NameSpaces -> Text -> Text -> Element -> Bool
isElem ns prefix name element =
  let ns' = ns <> elemToNameSpaces element
  in qName (elName element) == name &&
     qURI (elName element) == M.lookup prefix ns'

findChildByName :: NameSpaces -> Text -> Text -> Element -> Maybe Element
findChildByName ns pref name el =
  let ns' = ns <> elemToNameSpaces el
  in  findChild (elemName ns' pref name) el

findChildrenByName :: NameSpaces -> Text -> Text -> Element -> [Element]
findChildrenByName ns pref name el =
  let ns' = ns <> elemToNameSpaces el
  in  findChildren (elemName ns' pref name) el

-- | Like 'findChildrenByName', but searches descendants.
findElementByName :: NameSpaces -> Text -> Text -> Element -> Maybe Element
findElementByName ns pref name el =
  let ns' = ns <> elemToNameSpaces el
  in  findElement (elemName ns' pref name) el

findAttrByName :: NameSpaces -> Text -> Text -> Element -> Maybe Text
findAttrByName ns pref name el =
  let ns' = ns <> elemToNameSpaces el
  in  findAttr (elemName ns' pref name) el


-- | Removes child elements that satisfy a given condition.
-- Returns the modified element and the list of removed children.
extractChildren :: Element -> (Element -> Bool) -> Maybe (Element, [Element])
extractChildren el condition
  | null removedChildren = Nothing  -- No children removed, return Nothing
  | otherwise = Just (modifiedElement, removedChildren)  -- Children removed, return Just
  where
    -- Separate the children based on the condition
    (removedChildren, keptChildren) = partition condition (onlyElems' $ elContent el)

    -- Helper function to filter only Element types from Content
    onlyElems' :: [Content] -> [Element]
    onlyElems' = foldr (\c acc -> case c of
                                   Elem e -> e : acc
                                   _      -> acc) []

    -- Reconstruct the element with the kept children
    modifiedElement = el { elContent = map Elem keptChildren }
