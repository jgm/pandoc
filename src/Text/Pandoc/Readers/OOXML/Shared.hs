{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.OOXML.Shared
   Copyright   : © 2025 Anton Antic
   License     : GNU GPL, version 2 or above

   Maintainer  : Anton Antic <anton@everworker.ai>
   Stability   : alpha
   Portability : portable

Shared utilities for Office Open XML (OOXML) readers (DOCX, PPTX).
Provides common functions for ZIP archive handling, XML parsing,
namespace management, and DrawingML parsing.
-}
module Text.Pandoc.Readers.OOXML.Shared
  ( -- * Constants
    emusPerInch
  , emuToInches
  , inchesToEmu
    -- * Types
  , NameSpaces
  , elemName
  , elemToNameSpaces
  , isElem
  , findChildByName
  , findChildrenByName
  , findElementByName
  , findAttrByName
  ) where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Text.Pandoc.XML.Light

-- | Type alias for namespace mappings
type NameSpaces = M.Map Text Text

-- | English Metric Units per inch
-- 1 inch = 914400 EMUs (used in OOXML for dimensions)
emusPerInch :: Integer
emusPerInch = 914400

-- | Convert EMUs to inches
emuToInches :: Integer -> Double
emuToInches n = fromIntegral n / fromIntegral emusPerInch

-- | Convert inches to EMUs
inchesToEmu :: Double -> Integer
inchesToEmu n = round (n * fromIntegral emusPerInch)

-- | Extract namespace declarations from element attributes
elemToNameSpaces :: Element -> NameSpaces
elemToNameSpaces = foldr (\(Attr qn val) ->
                            case qn of
                              QName s _ (Just "xmlns") -> M.insert s val
                              _ -> id) mempty . elAttribs

-- | Create a qualified name from namespace map, prefix, and local name
elemName :: NameSpaces -> Text -> Text -> QName
elemName ns prefix name =
  QName name
        (M.lookup prefix ns)
        (if T.null prefix then Nothing else Just prefix)

-- | Check if element matches namespace prefix and local name
isElem :: NameSpaces -> Text -> Text -> Element -> Bool
isElem ns prefix name element =
  let ns' = ns <> elemToNameSpaces element
  in  qName (elName element) == name &&
      qURI (elName element) == M.lookup prefix ns'

-- | Find first child element matching namespace and name
findChildByName :: NameSpaces -> Text -> Text -> Element -> Maybe Element
findChildByName ns pref name el =
  let ns' = ns <> elemToNameSpaces el
  in  findChild (elemName ns' pref name) el

-- | Find all children matching namespace and name
findChildrenByName :: NameSpaces -> Text -> Text -> Element -> [Element]
findChildrenByName ns pref name el =
  let ns' = ns <> elemToNameSpaces el
  in  findChildren (elemName ns' pref name) el

-- | Find element anywhere in descendants matching namespace and name
findElementByName :: NameSpaces -> Text -> Text -> Element -> Maybe Element
findElementByName ns pref name el =
  let ns' = ns <> elemToNameSpaces el
  in  findElement (elemName ns' pref name) el

-- | Find attribute value by namespace prefix and name
findAttrByName :: NameSpaces -> Text -> Text -> Element -> Maybe Text
findAttrByName ns pref name el =
  let ns' = ns <> elemToNameSpaces el
  in  findAttr (elemName ns' pref name) el
