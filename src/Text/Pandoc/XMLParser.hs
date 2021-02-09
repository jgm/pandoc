{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.XMLParser
   Copyright   : Copyright (C) 2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Bridge to allow using xml-conduit's parser with xml-light's types.
-}
module Text.Pandoc.XMLParser
  ( parseXMLElement
  , parseXMLContents
  , module Text.XML.Light.Types
  ) where

import qualified Control.Exception as E
import qualified Text.XML as Conduit
import Text.XML.Unresolved (InvalidEventStream(..))
import qualified Text.XML.Light as Light
import Text.XML.Light.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

-- Drop in replacement for parseXMLDoc in xml-light.
parseXMLElement :: TL.Text -> Either T.Text Light.Element
parseXMLElement t =
  elementToElement .  Conduit.documentRoot <$>
    either (Left . T.pack . E.displayException) Right
    (Conduit.parseText Conduit.def{ Conduit.psRetainNamespaces = True } t)

parseXMLContents :: TL.Text -> Either T.Text [Light.Content]
parseXMLContents t =
  case Conduit.parseText Conduit.def{ Conduit.psRetainNamespaces = True } t of
    Left e ->
      case E.fromException e of
        Just (ContentAfterRoot _) ->
          elContent <$> parseXMLElement ("<wrapper>" <> t <> "</wrapper>")
        _ -> Left . T.pack . E.displayException $ e
    Right x -> Right [Light.Elem . elementToElement . Conduit.documentRoot $ x]

elementToElement :: Conduit.Element -> Light.Element
elementToElement (Conduit.Element name attribMap nodes) =
  Light.Element (nameToQname name) attrs (mapMaybe nodeToContent nodes) Nothing
 where
  attrs = map (\(n,v) -> Light.Attr (nameToQname n) (T.unpack v)) $
              M.toList attribMap
  nameToQname (Conduit.Name localName mbns mbpref) =
    case mbpref of
      Nothing | "xmlns:" `T.isPrefixOf` localName ->
           Light.QName (T.unpack $ T.drop 6 localName)  (T.unpack <$> mbns)
                       (Just "xmlns")
      _ -> Light.QName (T.unpack localName) (T.unpack <$> mbns)
                       (T.unpack <$> mbpref)

nodeToContent :: Conduit.Node -> Maybe Light.Content
nodeToContent (Conduit.NodeElement el) =
  Just (Light.Elem (elementToElement el))
nodeToContent (Conduit.NodeContent t) =
  Just (Light.Text (Light.CData Light.CDataText (T.unpack t) Nothing))
nodeToContent _ = Nothing

