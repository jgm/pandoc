{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
  ( -- * Basic types, duplicating those from xml-light but with Text
    -- instead of String
    Line
  , Content(..)
  , Element(..)
  , Attr(..)
  , CData(..)
  , CDataKind(..)
  , QName(..)
  , Node(..)
  , unode
  , unqual
  , add_attr
  , add_attrs
    -- * Conversion functions from xml-light types
  , fromXLQName
  , fromXLCData
  , fromXLElement
  , fromXLAttr
  , fromXLContent
    -- * Replacement for xml-light's Text.XML.Proc
  , strContent
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
    -- * Replacement for xml-light's Text.XML.Output
  , ppTopElement
  , ppElement
  , ppContent
  , ppcElement
  , ppcContent
  , showTopElement
  , showElement
  , showContent
  , useShortEmptyTags
  , defaultConfigPP
  , ConfigPP(..)
    -- * Replacement for xml-light's Text.XML.Input
  , parseXMLElement
  , parseXMLContents
  ) where

import qualified Control.Exception as E
import qualified Text.XML as Conduit
import Text.XML.Unresolved (InvalidEventStream(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, singleton, fromText, toLazyText)
import qualified Data.Map as M
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Maybe (mapMaybe, listToMaybe)
import Data.List(find)
import qualified Text.XML.Light as XL

-- Drop in replacement for parseXMLDoc in xml-light.
parseXMLElement :: TL.Text -> Either T.Text Element
parseXMLElement t =
  elementToElement .  Conduit.documentRoot <$>
    either (Left . T.pack . E.displayException) Right
    (Conduit.parseText Conduit.def{ Conduit.psRetainNamespaces = True } t)

parseXMLContents :: TL.Text -> Either T.Text [Content]
parseXMLContents t =
  case Conduit.parseText Conduit.def{ Conduit.psRetainNamespaces = True } t of
    Left e ->
      case E.fromException e of
        Just (ContentAfterRoot _) ->
          elContent <$> parseXMLElement ("<wrapper>" <> t <> "</wrapper>")
        _ -> Left . T.pack . E.displayException $ e
    Right x -> Right [Elem . elementToElement . Conduit.documentRoot $ x]

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

unqual :: Text -> QName
unqual x = QName x Nothing Nothing

-- | Add an attribute to an element.
add_attr :: Attr -> Element -> Element
add_attr a e = add_attrs [a] e

-- | Add some attributes to an element.
add_attrs :: [Attr] -> Element -> Element
add_attrs as e = e { elAttribs = as ++ elAttribs e }

--
-- type definitions lightly modified from xml-light
--

-- | A line is an Integer
type Line     = Integer

-- | XML content
data Content  = Elem Element
              | Text CData
              | CRef Text
                deriving (Show, Typeable, Data)

-- | XML elements
data Element  = Element {
                  elName      :: QName,
                  elAttribs   :: [Attr],
                  elContent   :: [Content],
                  elLine      :: Maybe Line
                } deriving (Show, Typeable, Data)

-- | XML attributes
data Attr     = Attr {
                  attrKey :: QName,
                  attrVal :: Text
                } deriving (Eq, Ord, Show, Typeable, Data)

-- | XML CData
data CData    = CData {
                  cdVerbatim  :: CDataKind,
                  cdData      :: Text,
                  cdLine      :: Maybe Line
                } deriving (Show, Typeable, Data)

data CDataKind
 = CDataText      -- ^ Ordinary character data; pretty printer escapes &, < etc.
 | CDataVerbatim  -- ^ Unescaped character data; pretty printer embeds it in <![CDATA[..
 | CDataRaw       -- ^ As-is character data; pretty printer passes it along without any escaping or CDATA wrap-up.
   deriving ( Eq, Show, Typeable, Data )

-- | XML qualified names
data QName    = QName {
                  qName   :: Text,
                  qURI    :: Maybe Text,
                  qPrefix :: Maybe Text
                } deriving (Show, Typeable, Data)


instance Eq QName where
  q1 == q2  = compare q1 q2 == EQ

instance Ord QName where
  compare q1 q2 =
    case compare (qName q1) (qName q2) of
      EQ  -> case (qURI q1, qURI q2) of
               (Nothing,Nothing) -> compare (qPrefix q1) (qPrefix q2)
               (u1,u2)           -> compare u1 u2
      x   -> x

class Node t where
 node :: QName -> t -> Element

instance Node ([Attr],[Content]) where
  node n (attrs,cont) = Element { elName     = n
                                , elAttribs  = attrs
                                , elContent  = cont
                                , elLine     = Nothing
                                }

instance Node [Attr]             where node n as   = node n (as,[]::[Content])
instance Node Attr               where node n a    = node n [a]
instance Node ()                 where node n ()   = node n ([]::[Attr])

instance Node [Content]          where node n cs     = node n ([]::[Attr],cs)
instance Node Content            where node n c      = node n [c]
instance Node ([Attr],Content)   where node n (as,c) = node n (as,[c])
instance Node (Attr,Content)     where node n (a,c)  = node n ([a],[c])

instance Node ([Attr],[Element]) where
  node n (as,cs) = node n (as,map Elem cs)

instance Node ([Attr],Element)   where node n (as,c) = node n (as,[c])
instance Node (Attr,Element)     where node n (a,c)  = node n ([a],c)
instance Node [Element]          where node n es     = node n ([]::[Attr],es)
instance Node Element            where node n e      = node n [e]

instance Node ([Attr],[CData])   where
  node n (as,cs) = node n (as,map Text cs)

instance Node ([Attr],CData)     where node n (as,c) = node n (as,[c])
instance Node (Attr,CData)       where node n (a,c)  = node n ([a],c)
instance Node [CData]            where node n es     = node n ([]::[Attr],es)
instance Node CData              where node n e      = node n [e]

instance Node ([Attr],Text)      where
  node n (as,t) = node n (as, CData { cdVerbatim = CDataText
                                    , cdData = t
                                    , cdLine = Nothing })

instance Node (Attr,Text )       where node n (a,t)  = node n ([a],t)
instance Node Text               where node n t      = node n ([]::[Attr],t)

-- | Create node with unqualified name
unode :: Node t => Text -> t -> Element
unode = node . unqual

--
-- conversion from xml-light
--

fromXLQName :: XL.QName -> QName
fromXLQName qn = QName { qName = T.pack $ XL.qName qn
                       , qURI = T.pack <$> XL.qURI qn
                       , qPrefix = T.pack <$> XL.qPrefix qn }

fromXLCData :: XL.CData -> CData
fromXLCData cd = CData { cdVerbatim = case XL.cdVerbatim cd of
                                        XL.CDataText -> CDataText
                                        XL.CDataVerbatim -> CDataVerbatim
                                        XL.CDataRaw -> CDataRaw
                       , cdData = T.pack $ XL.cdData cd
                       , cdLine = XL.cdLine cd }

fromXLElement :: XL.Element -> Element
fromXLElement el = Element { elName = fromXLQName $ XL.elName el
                           , elAttribs = map fromXLAttr $ XL.elAttribs el
                           , elContent = map fromXLContent $ XL.elContent el
                           , elLine = XL.elLine el }

fromXLAttr :: XL.Attr -> Attr
fromXLAttr (XL.Attr qn s) = Attr (fromXLQName qn) (T.pack s)

fromXLContent :: XL.Content -> Content
fromXLContent (XL.Elem el) = Elem $ fromXLElement el
fromXLContent (XL.Text cd) = Text $ fromXLCData cd
fromXLContent (XL.CRef s)  = CRef (T.pack s)

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

-- | Find all non-nested occurances of an element.
-- (i.e., once we have found an element, we do not search
-- for more occurances among the element's children).
findElements       :: QName -> Element -> [Element]
findElements qn e = filterElementsName (qn==) e

-- | Find all non-nested occurrences of an element wrt. given predicate.
-- (i.e., once we have found an element, we do not search
-- for more occurances among the element's children).
filterElements       :: (Element -> Bool) -> Element -> [Element]
filterElements p e
 | p e        = [e]
 | otherwise  = concatMap (filterElements p) $ onlyElems $ elContent e

-- | Find all non-nested occurences of an element wrt a predicate over element names.
-- (i.e., once we have found an element, we do not search
-- for more occurances among the element's children).
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


--
-- duplicates functinos from Text.XML.Output
--

-- | The XML 1.0 header
xmlHeader :: Text
xmlHeader = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"


--------------------------------------------------------------------------------
data ConfigPP = ConfigPP
  { shortEmptyTag :: QName -> Bool
  , prettify      :: Bool
  }

-- | Default pretty orinting configuration.
--  * Always use abbreviate empty tags.
defaultConfigPP :: ConfigPP
defaultConfigPP = ConfigPP { shortEmptyTag = const True
                           , prettify      = False
                           }

-- | The predicate specifies for which empty tags we should use XML's
-- abbreviated notation <TAG />.  This is useful if we are working with
-- some XML-ish standards (such as certain versions of HTML) where some
-- empty tags should always be displayed in the <TAG></TAG> form.
useShortEmptyTags :: (QName -> Bool) -> ConfigPP -> ConfigPP
useShortEmptyTags p c = c { shortEmptyTag = p }


-- | Specify if we should use extra white-space to make document more readable.
-- WARNING: This adds additional white-space to text elements,
-- and so it may change the meaning of the document.
useExtraWhiteSpace :: Bool -> ConfigPP -> ConfigPP
useExtraWhiteSpace p c  = c { prettify = p }

-- | A configuration that tries to make things pretty
-- (possibly at the cost of changing the semantics a bit
-- through adding white space.)
prettyConfigPP     :: ConfigPP
prettyConfigPP      = useExtraWhiteSpace True defaultConfigPP


--------------------------------------------------------------------------------


-- | Pretty printing renders XML documents faithfully,
-- with the exception that whitespace may be added\/removed
-- in non-verbatim character data.
ppTopElement       :: Element -> Text
ppTopElement        = ppcTopElement prettyConfigPP

-- | Pretty printing elements
ppElement          :: Element -> Text
ppElement           = ppcElement prettyConfigPP

-- | Pretty printing content
ppContent          :: Content -> Text
ppContent           = ppcContent prettyConfigPP

-- | Pretty printing renders XML documents faithfully,
-- with the exception that whitespace may be added\/removed
-- in non-verbatim character data.
ppcTopElement      :: ConfigPP -> Element -> Text
ppcTopElement c e   = T.unlines [xmlHeader,ppcElement c e]

-- | Pretty printing elements
ppcElement         :: ConfigPP -> Element -> Text
ppcElement c        = TL.toStrict . toLazyText . ppElementS c mempty

-- | Pretty printing content
ppcContent         :: ConfigPP -> Content -> Text
ppcContent c        = TL.toStrict . toLazyText . ppContentS c mempty

ppcCData           :: ConfigPP -> CData -> Text
ppcCData c         = TL.toStrict . toLazyText . ppCDataS c mempty

type Indent = Builder

-- | Pretty printing content using ShowT
ppContentS         :: ConfigPP -> Indent -> Content -> Builder
ppContentS c i x = case x of
                     Elem e -> ppElementS c i e
                     Text t -> ppCDataS c i t
                     CRef r -> showCRefS r

ppElementS         :: ConfigPP -> Indent -> Element -> Builder
ppElementS c i e = i <> tagStart (elName e) (elAttribs e) <>
  (case elContent e of
    [] | "?" `T.isPrefixOf` qName name -> fromText " ?>"
       | shortEmptyTag c name  -> fromText " />"
    [Text t] -> singleton '>' <> ppCDataS c mempty t <> tagEnd name
    cs -> singleton '>' <> nl <>
          mconcat (map ((<> nl) . ppContentS c (sp <> i)) cs) <>
          i <> tagEnd name
      where (nl,sp)  = if prettify c then ("\n","  ") else ("","")
  )
  where name = elName e

ppCDataS           :: ConfigPP -> Indent -> CData -> Builder
ppCDataS c i t     = i <> if cdVerbatim t /= CDataText || not (prettify c)
                             then showCDataS t
                             else foldr cons mempty (T.unpack (showCData t))
  where cons         :: Char -> Builder -> Builder
        cons '\n' ys  = singleton '\n' <> i <> ys
        cons y ys     = singleton y <> ys



--------------------------------------------------------------------------------

-- | Adds the <?xml?> header.
showTopElement     :: Element -> Text
showTopElement c    = xmlHeader <> showElement c

showContent        :: Content -> Text
showContent         = ppcContent defaultConfigPP

showElement        :: Element -> Text
showElement         = ppcElement defaultConfigPP

showCData          :: CData -> Text
showCData           = ppcCData defaultConfigPP

-- Note: crefs should not contain '&', ';', etc.
showCRefS          :: Text -> Builder
showCRefS r         = singleton '&' <> fromText r <> singleton ';'

-- | Convert a text element to characters.
showCDataS         :: CData -> Builder
showCDataS cd =
 case cdVerbatim cd of
   CDataText     -> escStr (cdData cd)
   CDataVerbatim -> fromText "<![CDATA[" <> escCData (cdData cd) <>
                    fromText "]]>"
   CDataRaw      -> fromText (cdData cd)

--------------------------------------------------------------------------------
escCData           :: Text -> Builder
escCData t
  | "]]>" `T.isPrefixOf` t =
     fromText "]]]]><![CDATA[>" <> fromText (T.drop 3 t)
escCData t
  = case T.uncons t of
      Nothing     -> mempty
      Just (c,t') -> singleton c <> escCData t'

escChar            :: Char -> Builder
escChar c = case c of
  '<'   -> fromText "&lt;"
  '>'   -> fromText "&gt;"
  '&'   -> fromText "&amp;"
  '"'   -> fromText "&quot;"
  -- we use &#39 instead of &apos; because IE apparently has difficulties
  -- rendering &apos; in xhtml.
  -- Reported by Rohan Drape <rohan.drape@gmail.com>.
  '\''  -> fromText "&#39;"
  _     -> singleton c

  {- original xml-light version:
  -- NOTE: We escape '\r' explicitly because otherwise they get lost
  -- when parsed back in because of then end-of-line normalization rules.
  _ | isPrint c || c == '\n' -> singleton c
    | otherwise -> showText "&#" . showsT oc . singleton ';'
      where oc = ord c
  -}

escStr             :: Text -> Builder
escStr cs          = if T.any needsEscape cs
                        then mconcat (map escChar (T.unpack cs))
                        else fromText cs
 where
  needsEscape '<' = True
  needsEscape '>' = True
  needsEscape '&' = True
  needsEscape '"' = True
  needsEscape '\'' = True
  needsEscape _ = False

tagEnd             :: QName -> Builder
tagEnd qn           = fromText "</" <> showQName qn <> singleton '>'

tagStart           :: QName -> [Attr] -> Builder
tagStart qn as      = singleton '<' <> showQName qn <> as_str
 where as_str       = if null as
                         then mempty
                         else mconcat (map showAttr as)

showAttr           :: Attr -> Builder
showAttr (Attr qn v) = singleton ' ' <> showQName qn <>
                       singleton '=' <>
                       singleton '"' <> escStr v <> singleton '"'

showQName          :: QName -> Builder
showQName q         =
  case qPrefix q of
    Nothing -> fromText (qName q)
    Just p  -> fromText p <> singleton ':' <> fromText (qName q)
