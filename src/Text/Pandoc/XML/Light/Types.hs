{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{- |
   Module      : Text.Pandoc.XML.Light.Types
   Copyright   : Copyright (C) 2007 Galois, Inc., 2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

   This code is taken from xml-light, released under the BSD3 license.
   It has been modified to use Text instead of String, and the fromXL*
   functions have been added.
-}
module Text.Pandoc.XML.Light.Types
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
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Data (Data)
import Data.Typeable (Typeable)
import qualified Text.XML.Light as XL

--
-- type definitions lightly modified from xml-light
--

-- | A line is an Integer
type Line     = Integer

-- | XML content
data Content  = Elem Element
              | Text CData
              | CRef Text
                deriving (Show, Typeable, Data, Ord, Eq)

-- | XML elements
data Element  = Element {
                  elName      :: QName,
                  elAttribs   :: [Attr],
                  elContent   :: [Content],
                  elLine      :: Maybe Line
                } deriving (Show, Typeable, Data, Ord, Eq)

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
                } deriving (Show, Typeable, Data, Ord, Eq)

data CDataKind
 = CDataText      -- ^ Ordinary character data; pretty printer escapes &, < etc.
 | CDataVerbatim  -- ^ Unescaped character data; pretty printer embeds it in <![CDATA[..
 | CDataRaw       -- ^ As-is character data; pretty printer passes it along without any escaping or CDATA wrap-up.
   deriving ( Eq, Ord, Show, Typeable, Data )

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

unqual :: Text -> QName
unqual x = QName x Nothing Nothing

-- | Add an attribute to an element.
add_attr :: Attr -> Element -> Element
add_attr a e = add_attrs [a] e

-- | Add some attributes to an element.
add_attrs :: [Attr] -> Element -> Element
add_attrs as e = e { elAttribs = as ++ elAttribs e }

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


