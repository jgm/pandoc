{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.XML.Light
   Copyright   : Copyright (C) 2021-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

xml-light, which we used in pandoc's the XML-based readers, has
some limitations: in particular, it produces nodes with String
instead of Text, and the parser falls over on processing instructions
(see #7091).

This module exports much of the API of xml-light, but using Text instead
of String. In addition, the xml-light parsers are replaced by xml-conduit's
well-tested parser.  (The xml-conduit types are mapped to types
isomorphic to xml-light's, to avoid the need for massive code modifications
elsewhere.)  Bridge functions to map xml-light types to this module's
types are also provided (since libraries like texmath still use xml-light).

Another advantage of the xml-conduit parser is that it gives us
detailed information on xml parse errors.

In the future we may want to move to using xml-conduit or another
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
    --  * Versions that allow passing in a custom entity table
  , parseXMLElementWithEntities
  , parseXMLContentsWithEntities
  ) where

import qualified Control.Exception as E
import qualified Text.XML as Conduit
import Text.XML.Unresolved (InvalidEventStream(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Text.Pandoc.XML.Light.Types
import Text.Pandoc.XML.Light.Proc
import Text.Pandoc.XML.Light.Output
import qualified Data.XML.Types as XML

-- Drop in replacement for parseXMLDoc in xml-light.
parseXMLElement :: TL.Text -> Either T.Text Element
parseXMLElement = parseXMLElementWithEntities mempty

-- Drop in replacement for parseXMLDoc in xml-light.
parseXMLElementWithEntities :: M.Map T.Text T.Text
                            -> TL.Text -> Either T.Text Element
parseXMLElementWithEntities entityMap t =
  elementToElement .  Conduit.documentRoot <$>
    either (Left . T.pack . E.displayException) Right
    (Conduit.parseText Conduit.def{ Conduit.psRetainNamespaces = True
                                  , Conduit.psDecodeEntities = decodeEnts } t)
 where
   decodeEnts ref = case M.lookup ref entityMap of
                      Nothing -> XML.ContentEntity ref
                      Just t' -> XML.ContentText t'

parseXMLContents :: TL.Text -> Either T.Text [Content]
parseXMLContents = parseXMLContentsWithEntities mempty

parseXMLContentsWithEntities :: M.Map T.Text T.Text
                             -> TL.Text -> Either T.Text [Content]
parseXMLContentsWithEntities entityMap t =
  case Conduit.parseText Conduit.def{ Conduit.psRetainNamespaces = True
                                    , Conduit.psDecodeEntities = decodeEnts
                                    } t of
    Left e ->
      case E.fromException e of
        Just (ContentAfterRoot _) ->
          elContent <$> parseXMLElementWithEntities entityMap
                          ("<wrapper>" <> t <> "</wrapper>")
        _ -> Left . T.pack . E.displayException $ e
    Right x -> Right [Elem . elementToElement . Conduit.documentRoot $ x]
 where
   decodeEnts ref = case M.lookup ref entityMap of
                      Nothing -> XML.ContentEntity ref
                      Just t' -> XML.ContentText t'

elementToElement :: Conduit.Element -> Element
elementToElement (Conduit.Element name attribMap nodes) =
  Element (nameToQname name) attrs (mapMaybe nodeToContent nodes) Nothing
 where
  attrs = map (\(n,v) -> Attr (nameToQname n) v) $
              M.toList attribMap
  nameToQname (Conduit.Name localName mbns mbpref) =
    case mbpref of
      Nothing ->
        case T.stripPrefix "xmlns:" localName of
          Just rest -> QName rest mbns (Just "xmlns")
          Nothing   -> QName localName mbns mbpref
      _ -> QName localName mbns mbpref

nodeToContent :: Conduit.Node -> Maybe Content
nodeToContent (Conduit.NodeElement el) =
  Just (Elem (elementToElement el))
nodeToContent (Conduit.NodeContent t) =
  Just (Text (CData CDataText t Nothing))
nodeToContent _ = Nothing
