{-
Copyright © 2012-2015 John MacFarlane <jgm@berkeley.edu>
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
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE OverlappingInstances #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Text.Pandoc.Lua.StackInstances
   Copyright   : Copyright © 2017 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

StackValue instances for pandoc types.
-}
module Text.Pandoc.Lua.StackInstances () where

import Data.Aeson ( FromJSON(..), ToJSON(..), Result(..), Value, fromJSON )
import Scripting.Lua
  ( LTYPE(..), LuaState, StackValue(..)
  , gettable, newtable, pop, rawgeti, rawset, rawseti, settable
  )
import Scripting.Lua.Aeson ()
import Text.Pandoc.Definition
  ( Block(..), Inline(..), Meta(..), Pandoc(..)
  , Citation(..), CitationMode(..), Format(..), MathType(..), QuoteType(..) )

import qualified Text.Pandoc.UTF8 as UTF8

maybeFromJson :: (FromJSON a) => Maybe Value -> Maybe a
maybeFromJson mv = fromJSON <$> mv >>= \case
  Success x -> Just x
  _         -> Nothing

instance StackValue Pandoc where
  push lua (Pandoc meta blocks) = do
    newtable lua
    setField lua (-1) "blocks" blocks
    setField lua (-1) "meta"   meta
  peek lua idx = do
    blocks <- getField lua idx "blocks"
    meta   <- getField lua idx "meta"
    return $ Pandoc <$> meta <*> blocks
  valuetype _ = TTABLE

instance StackValue Meta where
  push lua = push lua . toJSON
  peek lua = fmap maybeFromJson . peek lua
  valuetype _ = TTABLE

instance StackValue Block where
  push lua = \case
    BlockQuote blcks  -> pushTagged  lua "BlockQuote" blcks
    BulletList items  -> pushTagged  lua "BulletList" items
    HorizontalRule    -> pushTagged' lua "HorizontalRule"
    LineBlock blcks   -> pushTagged  lua "LineBlock" blcks
    Null              -> pushTagged' lua "Null"
    Para blcks        -> pushTagged  lua "Para" blcks
    Plain blcks       -> pushTagged  lua "Plain" blcks
    -- fall back to conversion via aeson's Value
    x                 -> push lua (toJSON x)
  peek lua i = peekBlock lua i
  valuetype _ = TTABLE

instance StackValue Inline where
  push lua = \case
    Cite citations lst -> pushTagged  lua "Cite" (citations, lst)
    Code attr lst      -> pushTagged  lua "Code" (attr, lst)
    Emph inlns         -> pushTagged  lua "Emph" inlns
    Image attr lst tgt -> pushTagged  lua "Image" (attr, lst, tgt)
    LineBreak          -> pushTagged' lua "LineBreak"
    Link attr lst tgt  -> pushTagged  lua "Link" (attr, lst, tgt)
    Note blcks         -> pushTagged  lua "Note" blcks
    Math mty str       -> pushTagged  lua "Math" (mty, str)
    Quoted qt inlns    -> pushTagged  lua "Quoted" (qt, inlns)
    RawInline f cs     -> pushTagged  lua "RawInline" (f, cs)
    SmallCaps inlns    -> pushTagged  lua "SmallCaps" inlns
    SoftBreak          -> pushTagged' lua "SoftBreak"
    Space              -> pushTagged' lua "Space"
    Span attr inlns    -> pushTagged  lua "Span" (attr, inlns)
    Str str            -> pushTagged  lua "Str" str
    Strikeout inlns    -> pushTagged  lua "Strikeout" inlns
    Strong inlns       -> pushTagged  lua "Strong" inlns
    Subscript inlns    -> pushTagged  lua "Subscript" inlns
    Superscript inlns  -> pushTagged  lua "Superscript" inlns
  peek = peekInline
  valuetype _ = TTABLE

instance StackValue Citation where
  push lua c = do
    newtable lua
    setField lua (-1) "citationId" (citationId c)
    setField lua (-1) "citationPrefix" (citationPrefix c)
    setField lua (-1) "citationSuffix" (citationSuffix c)
    setField lua (-1) "citationMode" (citationMode c)
    setField lua (-1) "citationNoteNum" (citationNoteNum c)
    setField lua (-1) "citationHash" (citationHash c)
  peek lua idx = do
    id' <- getField lua idx "citationId"
    prefix <- getField lua idx "citationPrefix"
    suffix <- getField lua idx "citationSuffix"
    mode <- getField lua idx "citationMode"
    num <- getField lua idx "citationNoteNum"
    hash <- getField lua idx "citationHash"
    return $ Citation
      <$> id'
      <*> prefix
      <*> suffix
      <*> mode
      <*> num
      <*> hash
  valuetype _ = TTABLE

instance StackValue CitationMode where
  push lua = \case
    AuthorInText   -> pushTagged' lua "AuthorInText"
    NormalCitation -> pushTagged' lua "NormalCitation"
    SuppressAuthor -> pushTagged' lua "SuppressAuthor"
  peek lua idx = do
    tag <- getField lua idx "t"
    case tag of
      Just "AuthorInText"   -> return $ Just AuthorInText
      Just "NormalCitation" -> return $ Just NormalCitation
      Just "SuppressAuthor" -> return $ Just SuppressAuthor
      _ -> return Nothing
  valuetype _ = TSTRING

instance StackValue Format where
  push lua (Format f) = push lua f
  peek lua idx = fmap Format <$> peek lua idx
  valuetype _ = TSTRING

instance StackValue MathType where
  push lua = \case
    InlineMath -> pushTagged' lua "InlineMath"
    DisplayMath -> pushTagged' lua "DisplayMath"
  peek lua idx = do
    res <- getField lua idx "t"
    case res of
      Just "InlineMath" -> return $ Just InlineMath
      Just "DisplayMath" -> return $ Just DisplayMath
      _ -> return Nothing
  valuetype _ = TTABLE

instance StackValue QuoteType where
  push lua = \case
    SingleQuote -> pushTagged' lua "SingleQuote"
    DoubleQuote -> pushTagged' lua "DoubleQuote"
  peek lua idx = do
    res <- getField lua idx "t"
    case res of
      Just "SingleQuote" -> return $ Just SingleQuote
      Just "DoubleQuote" -> return $ Just DoubleQuote
      _ -> return Nothing
  valuetype _ = TTABLE

#if MIN_VERSION_base(4,8,0)
instance {-# OVERLAPS #-} StackValue [Char] where
#else
instance StackValue [Char] where
#endif
  push lua cs = push lua (UTF8.fromString cs)
  peek lua i = fmap UTF8.toString <$> peek lua i
  valuetype _ = TSTRING

instance (StackValue a, StackValue b) => StackValue (a, b) where
  push lua (a, b) = do
    newtable lua
    setIntField lua (-1) 1 a
    setIntField lua (-1) 2 b
  peek lua idx = do
    a <- getIntField lua idx 1
    b <- getIntField lua idx 2
    return $ (,) <$> a <*> b
  valuetype _ = TTABLE

instance (StackValue a, StackValue b, StackValue c) =>
         StackValue (a, b, c)
 where
  push lua (a, b, c) = do
    newtable lua
    setIntField lua (-1) 1 a
    setIntField lua (-1) 2 b
    setIntField lua (-1) 3 c
  peek lua idx = do
    a <- getIntField lua idx 1
    b <- getIntField lua idx 2
    c <- getIntField lua idx 3
    return $ (,,) <$> a <*> b <*> c
  valuetype _ = TTABLE

-- | Push a value to the lua stack, tagged with a given string. This currently
-- creates a structure equivalent to what the JSONified value would look like
-- when pushed to lua.
pushTagged :: StackValue a => LuaState -> String -> a -> IO ()
pushTagged lua tag value = do
  newtable lua
  setField lua (-1) "t" tag
  setField lua (-1) "c" value

pushTagged' :: LuaState -> String -> IO ()
pushTagged' lua tag = do
  newtable lua
  push lua "t"
  push lua tag
  rawset lua (-3)

-- | Return the value at the given index as inline if possible.
peekInline :: LuaState -> Int -> IO (Maybe Inline)
peekInline lua idx = do
  tag <- getField lua idx "t"
  case tag of
    Nothing -> return Nothing
    Just t -> case t of
      "Cite"       -> fmap (uncurry Cite) <$> elementContent
      "Code"       -> fmap (uncurry Code) <$> elementContent
      "Emph"       -> fmap Emph <$> elementContent
      "Image"      -> fmap (\(attr, lst, tgt) -> Image attr lst tgt)
                      <$> elementContent
      "Link"       -> fmap (\(attr, lst, tgt) -> Link attr lst tgt)
                      <$> elementContent
      "LineBreak"  -> return (Just LineBreak)
      "Note"       -> fmap Note <$> elementContent
      "Math"       -> fmap (uncurry Math) <$> elementContent
      "Quoted"     -> fmap (uncurry Quoted) <$> elementContent
      "RawInline"  -> fmap (uncurry RawInline) <$> elementContent
      "SmallCaps"  -> fmap SmallCaps <$> elementContent
      "SoftBreak"  -> return (Just SoftBreak)
      "Space"      -> return (Just Space)
      "Span"       -> fmap (uncurry Span) <$> elementContent
      "Str"        -> fmap Str <$> elementContent
      "Strikeout"  -> fmap Strikeout <$> elementContent
      "Strong"     -> fmap Strong <$> elementContent
      "Subscript"  -> fmap Subscript <$> elementContent
      "Superscript"-> fmap Superscript <$> elementContent
      _ -> return Nothing
 where
   -- Get the contents of an AST element.
   elementContent :: StackValue a => IO (Maybe a)
   elementContent = getField lua idx "c"

-- | Return the value at the given index as block if possible.
peekBlock :: LuaState -> Int -> IO (Maybe Block)
peekBlock lua idx = do
  tag <- getField lua idx "t"
  case tag of
    Nothing -> return Nothing
    Just t -> case t of
      "BlockQuote"     -> fmap BlockQuote <$> elementContent
      "BulletList"     -> fmap BulletList <$> elementContent
      "HorizontalRule" -> return (Just HorizontalRule)
      "LineBlock"      -> fmap LineBlock <$> elementContent
      "Null"           -> return (Just Null)
      "Para"           -> fmap Para <$> elementContent
      "Plain"          -> fmap Plain <$> elementContent
      -- fall back to construction via aeson's Value
      _ -> maybeFromJson <$> peek lua idx
 where
   -- Get the contents of an AST element.
   elementContent :: StackValue a => IO (Maybe a)
   elementContent = getField lua idx "c"

-- | Adjust the stack index, assuming that @n@ new elements have been pushed on
-- the stack.
adjustIndexBy :: Int -> Int -> Int
adjustIndexBy idx n =
  if idx < 0
  then idx - n
  else idx

-- | Get value behind key from table at given index.
getField :: (StackValue a, StackValue b) => LuaState -> Int -> a -> IO (Maybe b)
getField lua idx key = do
  push lua key
  gettable lua (idx `adjustIndexBy` 1)
  peek lua (-1) <* pop lua 1

-- | Set value for key for table at the given index
setField :: (StackValue a, StackValue b) => LuaState -> Int -> a -> b -> IO ()
setField lua idx key value = do
  push lua key
  push lua value
  settable lua (idx `adjustIndexBy` 2)

-- | Get value behind key from table at given index.
getIntField :: StackValue a => LuaState -> Int -> Int -> IO (Maybe a)
getIntField lua idx key =
  rawgeti lua idx key
  *> peek lua (-1)
  <* pop lua 1

-- | Set numeric key/value in table at the given index
setIntField :: StackValue a => LuaState -> Int -> Int -> a -> IO ()
setIntField lua idx key value = do
  push lua value
  rawseti lua (idx `adjustIndexBy` 1) key
