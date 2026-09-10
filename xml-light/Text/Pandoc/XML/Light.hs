{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.XML.Light
   Copyright   : Copyright (C) 2021-2024 John MacFarlane
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
import qualified Text.XML.Stream.Parse as P
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL
import Data.Char (isSpace)
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
                                  , Conduit.psDecodeEntities =
                                      entityResolver entityMap } t)

parseXMLContents :: TL.Text -> Either T.Text [Content]
parseXMLContents = parseXMLContentsWithEntities mempty

-- | Parse a list of XML contents.  Unlike 'parseXMLElementWithEntities',
-- this does not require a single root element: multiple sibling
-- elements and top-level text are accepted.  An XML declaration,
-- DOCTYPE, comments, and processing instructions are skipped.
parseXMLContentsWithEntities :: M.Map T.Text T.Text
                             -> TL.Text -> Either T.Text [Content]
parseXMLContentsWithEntities entityMap t =
  case runConduit (CL.sourceList (TL.toChunks t) .| parser
                    .| CL.fold step (Right ([], []))) of
    Left err -> Left . T.pack . E.displayException $ (err :: E.SomeException)
    Right st -> normalizeTop <$> finishContents st
 where
   parser = P.parseTextPos P.def{ P.psRetainNamespaces = True
                                , P.psDecodeEntities =
                                    entityResolver entityMap }

entityResolver :: M.Map T.Text T.Text -> T.Text -> XML.Content
entityResolver entityMap ref =
  case M.lookup ref entityMap of
    Nothing -> XML.ContentEntity ref
    Just t' -> XML.ContentText t'

-- An element that is being built, with its children so far in reverse order.
data Frame = Frame QName [Attr] [Content]

-- The top-level contents built so far (reversed) and the stack of
-- open elements (innermost first); or an error.
type BuildState = Either T.Text ([Content], [Frame])

-- Fold one parse event into the content forest being built, checking
-- that start and end tags are balanced.
step :: BuildState -> P.EventPos -> BuildState
step st@(Left _) _ = st
step (Right (cs, stack)) (pos, event) =
  case event of
    XML.EventBeginElement name attribs ->
      case mapM toAttr attribs of
        Left e -> Left e
        Right attrs -> Right (cs, Frame (nameToQName name) attrs [] : stack)
    XML.EventEndElement name ->
      case stack of
        Frame fname attrs children : stack'
          | nameToQName name == fname ->
              emit (Elem (Element fname attrs (mergeText (reverse children))
                           Nothing)) stack'
        _ -> Left $ atPos $ "Unexpected close tag " <>
                      showQ (nameToQName name)
    XML.EventContent c ->
      case contentToText c of
        Left e -> Left e
        Right txt -> emit (textContent txt) stack
    XML.EventCDATA txt -> emit (textContent txt) stack
    _ -> Right (cs, stack)
      -- skip begin/end document, doctype, comments, and PIs
 where
  -- add finished content to the enclosing element (or the top level)
  emit c [] = Right (c : cs, [])
  emit c (Frame name attrs children : stack') =
    Right (cs, Frame name attrs (c : children) : stack')

  textContent txt = Text (CData CDataText txt Nothing)

  toAttr (name, vals) =
    Attr (nameToQName name) . T.concat <$> mapM contentToText vals

  contentToText (XML.ContentText txt) = Right txt
  contentToText (XML.ContentEntity ref) =
    Left $ atPos $ "Unresolved entity &" <> ref <> ";"

  atPos msg = case pos of
                Just pr -> T.pack (show pr) <> ": " <> msg
                Nothing -> msg

finishContents :: BuildState -> Either T.Text [Content]
finishContents (Left e) = Left e
finishContents (Right (cs, [])) = Right (mergeText (reverse cs))
finishContents (Right (_, Frame name _ _ : _)) =
  Left $ "Missing close tag for " <> showQ name

showQ :: QName -> T.Text
showQ (QName name _ Nothing)    = name
showQ (QName name _ (Just pre)) = pre <> ":" <> name

-- Merge adjacent text nodes (the stream parser splits text at
-- entity and CDATA boundaries).
mergeText :: [Content] -> [Content]
mergeText (Text cd : cs) =
  case span isText cs of
    ([], _)    -> Text cd : mergeText cs
    (ts, rest) -> Text cd{ cdData = T.concat (cdData cd :
                                      [d | Text (CData _ d _) <- ts]) }
                  : mergeText rest
 where
  isText Text{} = True
  isText _      = False
mergeText (c : cs) = c : mergeText cs
mergeText [] = []

-- If the content is a single element surrounded only by whitespace
-- (as in a complete XML document, where there may be newlines after
-- an XML declaration or DOCTYPE), drop the whitespace.
normalizeTop :: [Content] -> [Content]
normalizeTop cs =
  case filter (not . isWhitespaceText) cs of
    cs'@[Elem _] -> cs'
    _            -> cs
 where
  isWhitespaceText (Text cd) = T.all isSpace (cdData cd)
  isWhitespaceText _         = False

nameToQName :: Conduit.Name -> QName
nameToQName (Conduit.Name localName mbns mbpref) =
  case mbpref of
    Nothing ->
      case T.stripPrefix "xmlns:" localName of
        Just rest -> QName rest mbns (Just "xmlns")
        Nothing   -> QName localName mbns mbpref
    _ -> QName localName mbns mbpref

elementToElement :: Conduit.Element -> Element
elementToElement (Conduit.Element name attribMap nodes) =
  Element (nameToQName name) attrs (mapMaybe nodeToContent nodes) Nothing
 where
  attrs = map (\(n,v) -> Attr (nameToQName n) v) $
              M.toList attribMap

nodeToContent :: Conduit.Node -> Maybe Content
nodeToContent (Conduit.NodeElement el) =
  Just (Elem (elementToElement el))
nodeToContent (Conduit.NodeContent t) =
  Just (Text (CData CDataText t Nothing))
nodeToContent _ = Nothing
