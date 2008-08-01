{-# LANGUAGE FlexibleInstances #-}
--------------------------------------------------------------------
-- |
-- Module    : Text.XML.Light
-- Copyright : (c) Galois, Inc. 2007
-- License   : BSD3
--
-- Maintainer: Iavor S. Diatchki <diatchki@galois.com>
-- Stability : provisional
-- Portability: portability
--
-- A lightweight XML parsing, filtering and generating library.
--
-- This module reexports functions from:
--
-- * "Text.XML.Light.Types"
--
-- * "Text.XML.Light.Proc"
--
-- * "Text.XML.Light.Input"
--
-- * "Text.XML.Light.Output"
--

module Text.XML.Light (

    module Text.XML.Light,
    module Text.XML.Light.Types,
    module Text.XML.Light.Proc,
    module Text.XML.Light.Input,
    module Text.XML.Light.Output

  ) where

import Text.XML.Light.Types
import Text.XML.Light.Proc
import Text.XML.Light.Input
import Text.XML.Light.Output

-- | Add an attribute to an element.
add_attr :: Attr -> Element -> Element
add_attr a e = add_attrs [a] e

-- | Add some attributes to an element.
add_attrs :: [Attr] -> Element -> Element
add_attrs as e = e { elAttribs = as ++ elAttribs e }

-- | Create an unqualified name.
unqual :: String -> QName
unqual x = blank_name { qName = x }

-- | A smart element constructor which uses the type of its argument
-- to determine what sort of element to make.
class Node t where
  node :: QName -> t -> Element

instance Node ([Attr],[Content]) where
  node n (attrs,cont) = blank_element { elName     = n
                                      , elAttribs  = attrs
                                      , elContent  = cont
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
instance Node ([Element])        where node n es     = node n ([]::[Attr],es)
instance Node (Element)          where node n e      = node n [e]

instance Node ([Attr],[CData])   where
  node n (as,cs) = node n (as,map Text cs)

instance Node ([Attr],CData)     where node n (as,c) = node n (as,[c])
instance Node (Attr,CData)       where node n (a,c)  = node n ([a],c)
instance Node [CData]            where node n es     = node n ([]::[Attr],es)
instance Node CData              where node n e      = node n [e]

instance Node ([Attr],String)    where
  node n (as,t) = node n (as,blank_cdata { cdData = t })

instance Node (Attr,String)      where node n (a,t)  = node n ([a],t)
instance Node [Char]             where node n t      = node n ([]::[Attr],t)

-- | Create node with unqualified name
unode :: Node t => String -> t -> Element
unode = node . unqual
