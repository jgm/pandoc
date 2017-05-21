{-
Copyright © 2012-2017 John MacFarlane <jgm@berkeley.edu>
            2017 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Text.Pandoc.Lua.StackInstances
   Copyright   : © 2012-2017 John MacFarlane
                 © 2017 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

StackValue instances for pandoc types.
-}
module Text.Pandoc.Lua.StackInstances () where

import Control.Applicative ( (<|>) )
import Scripting.Lua
  ( LTYPE(..), LuaState, StackValue(..), ltype, newtable, objlen )
import Text.Pandoc.Definition
import Text.Pandoc.Lua.SharedInstances ()
import Text.Pandoc.Lua.Util ( addValue, getTable, pushViaConstructor )
import Text.Pandoc.Shared ( safeRead )

instance StackValue Pandoc where
  push lua (Pandoc meta blocks) = do
    newtable lua
    addValue lua "blocks" blocks
    addValue lua "meta"   meta
  peek lua idx = do
    blocks <- getTable lua idx "blocks"
    meta   <- getTable lua idx "meta"
    return $ Pandoc <$> meta <*> blocks
  valuetype _ = TTABLE

instance StackValue Meta where
  push lua (Meta mmap) = push lua mmap
  peek lua idx = fmap Meta <$> peek lua idx
  valuetype _ = TTABLE

instance StackValue MetaValue where
  push = pushMetaValue
  peek = peekMetaValue
  valuetype = \case
    MetaBlocks _  -> TTABLE
    MetaBool _    -> TBOOLEAN
    MetaInlines _ -> TTABLE
    MetaList _    -> TTABLE
    MetaMap _     -> TTABLE
    MetaString _  -> TSTRING

instance StackValue Block where
  push = pushBlock
  peek = peekBlock
  valuetype _ = TTABLE

instance StackValue Inline where
  push = pushInline
  peek = peekInline
  valuetype _ = TTABLE

instance StackValue Citation where
  push lua (Citation cid prefix suffix mode noteNum hash) =
    pushViaConstructor lua "Citation" cid mode prefix suffix noteNum hash
  peek lua idx = do
    id' <- getTable lua idx "citationId"
    prefix <- getTable lua idx "citationPrefix"
    suffix <- getTable lua idx "citationSuffix"
    mode <- getTable lua idx "citationMode"
    num <- getTable lua idx "citationNoteNum"
    hash <- getTable lua idx "citationHash"
    return $ Citation <$> id' <*> prefix <*> suffix <*> mode <*> num <*> hash
  valuetype _ = TTABLE

instance StackValue Alignment where
  push lua = push lua . show
  peek lua idx = (>>= safeRead) <$> peek lua idx
  valuetype _ = TSTRING

instance StackValue CitationMode where
  push lua = push lua . show
  peek lua idx = (>>= safeRead) <$> peek lua idx
  valuetype _ = TSTRING

instance StackValue Format where
  push lua (Format f) = push lua f
  peek lua idx = fmap Format <$> peek lua idx
  valuetype _ = TSTRING

instance StackValue ListNumberDelim where
  push lua = push lua . show
  peek lua idx = (>>= safeRead) <$> peek lua idx
  valuetype _ = TSTRING

instance StackValue ListNumberStyle where
  push lua = push lua . show
  peek lua idx = (>>= safeRead) <$> peek lua idx
  valuetype _ = TSTRING

instance StackValue MathType where
  push lua = push lua . show
  peek lua idx = (>>= safeRead) <$> peek lua idx
  valuetype _ = TSTRING

instance StackValue QuoteType where
  push lua = push lua . show
  peek lua idx = (>>= safeRead) <$> peek lua idx
  valuetype _ = TSTRING

-- | Push an meta value element to the top of the lua stack.
pushMetaValue :: LuaState -> MetaValue -> IO ()
pushMetaValue lua = \case
  MetaBlocks blcks  -> pushViaConstructor lua "MetaBlocks" blcks
  MetaBool bool     -> push lua bool
  MetaInlines inlns -> pushViaConstructor lua "MetaInlines" inlns
  MetaList metalist -> pushViaConstructor lua "MetaList" metalist
  MetaMap metamap   -> pushViaConstructor lua "MetaMap" metamap
  MetaString str    -> push lua str

-- | Interpret the value at the given stack index as meta value.
peekMetaValue :: LuaState -> Int -> IO (Maybe MetaValue)
peekMetaValue lua idx = do
  -- Get the contents of an AST element.
  let elementContent :: StackValue a => IO (Maybe a)
      elementContent = peek lua idx
  luatype <- ltype lua idx
  case luatype of
    TBOOLEAN -> fmap MetaBool <$> peek lua idx
    TSTRING  -> fmap MetaString <$> peek lua idx
    TTABLE   -> do
      tag <- getTable lua idx "t"
      case tag of
        Just "MetaBlocks"  -> fmap MetaBlocks  <$> elementContent
        Just "MetaBool"    -> fmap MetaBool    <$> elementContent
        Just "MetaMap"     -> fmap MetaMap     <$> elementContent
        Just "MetaInlines" -> fmap MetaInlines <$> elementContent
        Just "MetaList"    -> fmap MetaList    <$> elementContent
        Just "MetaString"  -> fmap MetaString  <$> elementContent
        Nothing -> do
          -- no meta value tag given, try to guess.
          len <- objlen lua idx
          if len <= 0
            then fmap MetaMap <$> peek lua idx
            else  (fmap MetaInlines <$> peek lua idx)
                  <|> (fmap MetaBlocks <$> peek lua idx)
                  <|> (fmap MetaList <$> peek lua idx)
        _        -> return Nothing
    _        -> return Nothing

-- | Push an block element to the top of the lua stack.
pushBlock :: LuaState -> Block -> IO ()
pushBlock lua = \case
  BlockQuote blcks         -> pushViaConstructor lua "BlockQuote" blcks
  BulletList items         -> pushViaConstructor lua "BulletList" items
  CodeBlock attr code      -> pushViaConstructor lua "CodeBlock" code (LuaAttr attr)
  DefinitionList items     -> pushViaConstructor lua "DefinitionList" items
  Div attr blcks           -> pushViaConstructor lua "Div" blcks (LuaAttr attr)
  Header lvl attr inlns    -> pushViaConstructor lua "Header" lvl inlns (LuaAttr attr)
  HorizontalRule           -> pushViaConstructor lua "HorizontalRule"
  LineBlock blcks          -> pushViaConstructor lua "LineBlock" blcks
  OrderedList lstAttr list -> pushViaConstructor lua "OrderedList" list lstAttr
  Null                     -> pushViaConstructor lua "Null"
  Para blcks               -> pushViaConstructor lua "Para" blcks
  Plain blcks              -> pushViaConstructor lua "Plain" blcks
  RawBlock f cs            -> pushViaConstructor lua "RawBlock" f cs
  Table capt aligns widths headers rows ->
    pushViaConstructor lua "Table" capt aligns widths headers rows

-- | Return the value at the given index as block if possible.
peekBlock :: LuaState -> Int -> IO (Maybe Block)
peekBlock lua idx = do
  tag <- getTable lua idx "t"
  case tag of
    Nothing -> return Nothing
    Just t -> case t of
      "BlockQuote"     -> fmap BlockQuote <$> elementContent
      "BulletList"     -> fmap BulletList <$> elementContent
      "CodeBlock"      -> fmap (withAttr CodeBlock) <$> elementContent
      "DefinitionList" -> fmap DefinitionList <$> elementContent
      "Div"            -> fmap (withAttr Div) <$> elementContent
      "Header"         -> fmap (\(lvl, LuaAttr attr, lst) -> Header lvl attr lst)
                          <$> elementContent
      "HorizontalRule" -> return (Just HorizontalRule)
      "LineBlock"      -> fmap LineBlock <$> elementContent
      "OrderedList"    -> fmap (uncurry OrderedList) <$> elementContent
      "Null"           -> return (Just Null)
      "Para"           -> fmap Para <$> elementContent
      "Plain"          -> fmap Plain <$> elementContent
      "RawBlock"       -> fmap (uncurry RawBlock) <$> elementContent
      "Table"          -> fmap (\(capt, aligns, widths, headers, body) ->
                                  Table capt aligns widths headers body)
                          <$> elementContent
      _ -> return Nothing
 where
   -- Get the contents of an AST element.
   elementContent :: StackValue a => IO (Maybe a)
   elementContent = getTable lua idx "c"

-- | Push an inline element to the top of the lua stack.
pushInline :: LuaState -> Inline -> IO ()
pushInline lua = \case
  Cite citations lst       -> pushViaConstructor lua "Cite" lst citations
  Code attr lst            -> pushViaConstructor lua "Code" lst (LuaAttr attr)
  Emph inlns               -> pushViaConstructor lua "Emph" inlns
  Image attr alt (src,tit) -> pushViaConstructor lua "Image" alt src tit (LuaAttr attr)
  LineBreak                -> pushViaConstructor lua "LineBreak"
  Link attr lst (src,tit)  -> pushViaConstructor lua "Link" lst src tit (LuaAttr attr)
  Note blcks               -> pushViaConstructor lua "Note" blcks
  Math mty str             -> pushViaConstructor lua "Math" mty str
  Quoted qt inlns          -> pushViaConstructor lua "Quoted" qt inlns
  RawInline f cs           -> pushViaConstructor lua "RawInline" f cs
  SmallCaps inlns          -> pushViaConstructor lua "SmallCaps" inlns
  SoftBreak                -> pushViaConstructor lua "SoftBreak"
  Space                    -> pushViaConstructor lua "Space"
  Span attr inlns          -> pushViaConstructor lua "Span" inlns (LuaAttr attr)
  Str str                  -> pushViaConstructor lua "Str" str
  Strikeout inlns          -> pushViaConstructor lua "Strikeout" inlns
  Strong inlns             -> pushViaConstructor lua "Strong" inlns
  Subscript inlns          -> pushViaConstructor lua "Subscript" inlns
  Superscript inlns        -> pushViaConstructor lua "Superscript" inlns

-- | Return the value at the given index as inline if possible.
peekInline :: LuaState -> Int -> IO (Maybe Inline)
peekInline lua idx = do
  tag <- getTable lua idx "t"
  case tag of
    Nothing -> return Nothing
    Just t -> case t of
      "Cite"       -> fmap (uncurry Cite) <$> elementContent
      "Code"       -> fmap (withAttr Code) <$> elementContent
      "Emph"       -> fmap Emph <$> elementContent
      "Image"      -> fmap (\(LuaAttr attr, lst, tgt) -> Image attr lst tgt)
                      <$> elementContent
      "Link"       -> fmap (\(LuaAttr attr, lst, tgt) -> Link attr lst tgt)
                      <$> elementContent
      "LineBreak"  -> return (Just LineBreak)
      "Note"       -> fmap Note <$> elementContent
      "Math"       -> fmap (uncurry Math) <$> elementContent
      "Quoted"     -> fmap (uncurry Quoted) <$> elementContent
      "RawInline"  -> fmap (uncurry RawInline) <$> elementContent
      "SmallCaps"  -> fmap SmallCaps <$> elementContent
      "SoftBreak"  -> return (Just SoftBreak)
      "Space"      -> return (Just Space)
      "Span"       -> fmap (withAttr Span) <$> elementContent
      "Str"        -> fmap Str <$> elementContent
      "Strikeout"  -> fmap Strikeout <$> elementContent
      "Strong"     -> fmap Strong <$> elementContent
      "Subscript"  -> fmap Subscript <$> elementContent
      "Superscript"-> fmap Superscript <$> elementContent
      _ -> return Nothing
 where
   -- Get the contents of an AST element.
   elementContent :: StackValue a => IO (Maybe a)
   elementContent = getTable lua idx "c"

withAttr :: (Attr -> a -> b) -> (LuaAttr, a) -> b
withAttr f (attributes, x) = f (fromLuaAttr attributes) x

-- | Wrapper for Attr
newtype LuaAttr = LuaAttr { fromLuaAttr :: Attr }

instance StackValue LuaAttr where
  push lua (LuaAttr (id', classes, kv)) =
    pushViaConstructor lua "Attr" id' classes kv
  peek lua idx = fmap LuaAttr <$> peek lua idx
  valuetype _ = TTABLE
