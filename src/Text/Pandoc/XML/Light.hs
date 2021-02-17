{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.XML.Light
   Copyright   : Copyright (C) 2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

xml-light, which we used in pandoc's the XML-based readers, has
some limitations: in particular, it produces nodes with String
instead of Text, and the parser falls over on processing instructions
(see #7091).

This module exports much of the API of xml-light, but using Text instead
of String. In addition, the xml-light parsers are replaced by xeno's
fast parser.  (The xeno types are mapped to types
isomorphic to xml-light's, to avoid the need for massive code modifications
elsewhere.)  Bridge functions to map xml-light types to this module's
types are also provided (since libraries like texmath still use xml-light).

In the future we may want to move to using xeno or another
xml library in the code base, but this change gives us
better performance and accuracy without much change in the
code that used xml-light.
-}
module Text.Pandoc.XML.Light
  ( module Text.Pandoc.XML.Light.Types
  , module Text.Pandoc.XML.Light.Proc
  , module Text.Pandoc.XML.Light.Output
    -- * Replacement for xml-light's Text.XML.Input
  , parseXMLElement
  , parseXMLContents
  ) where

import qualified Control.Exception as E
import qualified Xeno.DOM as X
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Pandoc.XML.Light.Types
import Text.Pandoc.XML.Light.Proc
import Text.Pandoc.XML.Light.Output
import Text.Pandoc.XML (fromEntities)
import qualified Data.ByteString as B
import qualified Text.Pandoc.UTF8 as UTF8

-- Drop in replacement for parseXMLDoc in xml-light.
parseXMLElement :: TL.Text -> Either T.Text Element
parseXMLElement t =
  elementToElement <$>
    either (Left . T.pack . E.displayException) Right
    (X.parse $ UTF8.fromText $ TL.toStrict t)

parseXMLContents :: TL.Text -> Either T.Text [Content]
parseXMLContents t =
  elContent <$> parseXMLElement ("<wrapper>" <> t <> "</wrapper>")

elementToElement :: X.Node -> Element
elementToElement nd =
  Element (nameToQName (X.name nd))
          attrs
          (map contentToContent (X.contents nd))
          Nothing
 where
  attrs = map (\(k,v) -> Attr (nameToQName k) (UTF8.toText v))
              (X.attributes nd)
  nameToQName n =
    let t = UTF8.toText n
        (x, y) = T.break (== ':') t
    in  if T.null y
           then QName x Nothing Nothing
           else QName x Nothing (Just (T.drop 1 y))

contentToContent :: X.Content -> Content
contentToContent (X.Element nd) =
  Elem (elementToElement nd)
contentToContent (X.Text bs) =
  Text (CData CDataText (resolveEntities $ UTF8.toText bs) Nothing)
   where resolveEntities
           | B.any (== 38) bs = fromEntities
           | otherwise        = id
contentToContent (X.CData bs) =
  Text (CData CDataVerbatim (UTF8.toText bs) Nothing)

