{-# LANGUAGE FlexibleInstances #-}
{- |
   Module      : Text.Pandoc.XML.Light.Proc
   Copyright   : Copyright (C) 2007 Galois, Inc., 2021-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

   This code is taken from xml-light, released under the BSD3 license.
-}
module Text.Pandoc.XML.Light.Proc
  (
    -- * Replacement for xml-light's Text.XML.Proc
    strContent
  , onlyElems
  , elChildren
  , onlyText
  , findChildren
  , filterChildren
  , filterChildrenName
  , findChild
  , filterChild
  , filterChildName
  , findElement
  , filterElement
  , filterElementName
  , findElements
  , filterElements
  , filterElementsName
  , findAttr
  , lookupAttr
  , lookupAttrBy
  , findAttrBy
  ) where

import Data.Text (Text)
import Data.Maybe (listToMaybe)
import Data.List(find)
import Text.Pandoc.XML.Light.Types

--
-- copied from xml-light Text.XML.Proc
--

-- | Get the text value of an XML element.  This function
-- ignores non-text elements, and concatenates all text elements.
strContent         :: Element -> Text
strContent          = mconcat . map cdData . onlyText . elContent

-- | Select only the elements from a list of XML content.
onlyElems          :: [Content] -> [Element]
onlyElems xs        = [ x | Elem x <- xs ]

-- | Select only the elements from a parent.
elChildren         :: Element -> [Element]
elChildren e        = [ x | Elem x <- elContent e ]

-- | Select only the text from a list of XML content.
onlyText           :: [Content] -> [CData]
onlyText xs         = [ x | Text x <- xs ]

-- | Find all immediate children with the given name.
findChildren       :: QName -> Element -> [Element]
findChildren q e    = filterChildren ((q ==) . elName) e

-- | Filter all immediate children wrt a given predicate.
filterChildren       :: (Element -> Bool) -> Element -> [Element]
filterChildren p e    = filter p (onlyElems (elContent e))


-- | Filter all immediate children wrt a given predicate over their names.
filterChildrenName      :: (QName -> Bool) -> Element -> [Element]
filterChildrenName p e   = filter (p.elName) (onlyElems (elContent e))


-- | Find an immediate child with the given name.
findChild          :: QName -> Element -> Maybe Element
findChild q e       = listToMaybe (findChildren q e)

-- | Find an immediate child with the given name.
filterChild          :: (Element -> Bool) -> Element -> Maybe Element
filterChild p e       = listToMaybe (filterChildren p e)

-- | Find an immediate child with name matching a predicate.
filterChildName      :: (QName -> Bool) -> Element -> Maybe Element
filterChildName p e   = listToMaybe (filterChildrenName p e)

-- | Find the left-most occurrence of an element matching given name.
findElement        :: QName -> Element -> Maybe Element
findElement q e     = listToMaybe (findElements q e)

-- | Filter the left-most occurrence of an element wrt. given predicate.
filterElement        :: (Element -> Bool) -> Element -> Maybe Element
filterElement p e     = listToMaybe (filterElements p e)

-- | Filter the left-most occurrence of an element wrt. given predicate.
filterElementName     :: (QName -> Bool) -> Element -> Maybe Element
filterElementName p e  = listToMaybe (filterElementsName p e)

-- | Find all non-nested occurrences of an element.
-- (i.e., once we have found an element, we do not search
-- for more occurrences among the element's children).
findElements       :: QName -> Element -> [Element]
findElements qn e = filterElementsName (qn==) e

-- | Find all non-nested occurrences of an element wrt. given predicate.
-- (i.e., once we have found an element, we do not search
-- for more occurrences among the element's children).
filterElements       :: (Element -> Bool) -> Element -> [Element]
filterElements p e
 | p e        = [e]
 | otherwise  = concatMap (filterElements p) $ onlyElems $ elContent e

-- | Find all non-nested occurrences of an element wrt a predicate over element names.
-- (i.e., once we have found an element, we do not search
-- for more occurrences among the element's children).
filterElementsName       :: (QName -> Bool) -> Element -> [Element]
filterElementsName p e = filterElements (p.elName) e

-- | Lookup the value of an attribute.
findAttr          :: QName -> Element -> Maybe Text
findAttr x e       = lookupAttr x (elAttribs e)

-- | Lookup attribute name from list.
lookupAttr        :: QName -> [Attr] -> Maybe Text
lookupAttr x       = lookupAttrBy (x ==)

-- | Lookup the first attribute whose name satisfies the given predicate.
lookupAttrBy       :: (QName -> Bool) -> [Attr] -> Maybe Text
lookupAttrBy p as   = attrVal `fmap` find (p . attrKey) as

-- | Lookup the value of the first attribute whose name
-- satisfies the given predicate.
findAttrBy         :: (QName -> Bool) -> Element -> Maybe Text
findAttrBy p e      = lookupAttrBy p (elAttribs e)
