{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Docx.Util
   Copyright   : © 2014-2020 Jesse Rosenthal <jrosenthal@jhu.edu>,
                   2014-2024 John MacFarlane <jgm@berkeley.edu>,
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

import Data.List (partition)
import Text.Pandoc.XML.Light
import Text.Pandoc.Readers.OOXML.Shared
  (NameSpaces, elemName, isElem, elemToNameSpaces,
   findChildByName, findChildrenByName, findElementByName, findAttrByName)


-- | Removes child elements that satisfy a given condition.
-- Returns the modified element and the list of removed children.
extractChildren :: Element -> (Element -> Bool) -> Maybe (Element, [Element])
extractChildren el condition
  | null removedChildren = Nothing  -- No children removed, return Nothing
  | otherwise = Just (modifiedElement, removedChildren)  -- Children removed, return Just
  where
    -- Separate the children based on the condition
    (removedChildren, keptChildren) = partition condition (onlyElems $ elContent el)

    -- Reconstruct the element with the kept children
    modifiedElement = el { elContent = map Elem keptChildren }
