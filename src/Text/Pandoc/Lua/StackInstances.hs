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

import Control.Applicative ( (<|>) )
import Data.Aeson ( FromJSON(..), ToJSON(..), Result(..), Value, fromJSON )
import Scripting.Lua
  ( LTYPE(..), LuaState, StackValue(..)
  , call, getglobal2, gettable, ltype, newtable, next, objlen
  , pop, pushnil, rawgeti, rawset, rawseti, settable
  )
import Scripting.Lua.Aeson ()
import Text.Pandoc.Definition
  ( Block(..), Inline(..), Meta(..), MetaValue(..), Pandoc(..)
  , Citation(..), CitationMode(..), Format(..), MathType(..), QuoteType(..) )

import qualified Data.Map as M
import qualified Text.Pandoc.UTF8 as UTF8

maybeFromJson :: (FromJSON a) => Maybe Value -> Maybe a
maybeFromJson mv = fromJSON <$> mv >>= \case
  Success x -> Just x
  _         -> Nothing

instance StackValue Pandoc where
  push lua (Pandoc meta blocks) = do
    newtable lua
    addKeyValue lua "blocks" blocks
    addKeyValue lua "meta"   meta
  peek lua idx = do
    blocks <- getField lua idx "blocks"
    meta   <- getField lua idx "meta"
    return $ Pandoc <$> meta <*> blocks
  valuetype _ = TTABLE

instance StackValue Meta where
  push lua (Meta mmap) = push lua mmap
  peek lua idx = fmap Meta <$> peek lua idx
  valuetype _ = TTABLE

instance StackValue MetaValue where
  push lua = \case
    MetaBlocks blcks  -> pushViaConstructor lua "MetaBlocks" blcks
    MetaBool b        -> pushViaConstructor lua "MetaBool" b
    MetaInlines inlns -> pushViaConstructor lua "MetaInlines" inlns
    MetaList metalist -> pushViaConstructor lua "MetaList" metalist
    MetaMap metamap   -> pushViaConstructor lua "MetaMap" metamap
    MetaString cs     -> pushViaConstructor lua "MetaString" cs
  peek lua idx = do
    luatype <- ltype lua idx
    case luatype of
      TBOOLEAN -> fmap MetaBool <$> peek lua idx
      TSTRING  -> fmap MetaString <$> peek lua idx
      TTABLE   -> do
        tag <- push lua "t"
               *> gettable lua (idx `adjustIndexBy` 1)
               *> peek lua (-1)
               <* pop lua 1
        case tag of
          Just "MetaBlocks"  -> fmap MetaBlocks  <$> peekContent lua idx
          Just "MetaBool"    -> fmap MetaBool    <$> peekContent lua idx
          Just "MetaMap"     -> fmap MetaMap     <$> peekContent lua idx
          Just "MetaInlines" -> fmap MetaInlines <$> peekContent lua idx
          Just "MetaList"    -> fmap MetaList    <$> peekContent lua idx
          Just "MetaString"  -> fmap MetaString  <$> peekContent lua idx
          Nothing -> do
            len <- objlen lua idx
            if len <= 0
              then fmap MetaMap <$> peek lua idx
              else  (fmap MetaInlines <$> peek lua idx)
                    <|> (fmap MetaBlocks <$> peek lua idx)
                    <|> (fmap MetaList <$> peek lua idx)
          _        -> return Nothing
      _        -> return Nothing
  valuetype = \case
    MetaBlocks _  -> TTABLE
    MetaBool _    -> TBOOLEAN
    MetaInlines _ -> TTABLE
    MetaList _    -> TTABLE
    MetaMap _     -> TTABLE
    MetaString _  -> TSTRING

peekContent :: StackValue a => LuaState -> Int -> IO (Maybe a)
peekContent lua idx = do
  push lua "c"
  gettable lua (idx `adjustIndexBy` 1)
  peek lua (-1) <* pop lua 1

instance StackValue Block where
  push lua = \case
    BlockQuote blcks  -> pushTagged  lua "BlockQuote" blcks
    BulletList items  -> pushTagged  lua "BulletList" items
    HorizontalRule    -> pushTagged' lua "HorizontalRule"
    LineBlock blcks   -> pushTagged  lua "LineBlock" blcks
    Null              -> pushTagged' lua "Null"
    Para blcks        -> pushTagged  lua "Para" blcks
    Plain blcks       -> pushTagged  lua "Plain" blcks
    RawBlock f cs     -> pushTagged  lua "RawBlock" (f, cs)
    -- fall back to conversion via aeson's Value
    x                 -> push lua (toJSON x)
  peek lua i = peekBlock lua i
  valuetype _ = TTABLE

instance StackValue Inline where
  push lua = \case
    Cite citations lst       -> pushViaConstructor lua "Cite" lst citations
    Code attr lst            -> pushViaConstructor lua "Code" lst attr
    Emph inlns               -> pushViaConstructor lua "Emph" inlns
    Image attr alt (src,tit) -> pushViaConstructor lua "Image" alt src tit attr
    LineBreak                -> pushViaConstructor lua "LineBreak"
    Link attr lst (src,tit)  -> pushViaConstructor lua "Link" lst src tit attr
    Note blcks               -> pushViaConstructor lua "Note" blcks
    Math mty str             -> pushViaConstructor lua "Math" mty str
    Quoted qt inlns          -> pushViaConstructor lua "Quoted" qt inlns
    RawInline f cs           -> pushViaConstructor lua "RawInline" f cs
    SmallCaps inlns          -> pushViaConstructor lua "SmallCaps" inlns
    SoftBreak                -> pushViaConstructor lua "SoftBreak"
    Space                    -> pushViaConstructor lua "Space"
    Span attr inlns          -> pushViaConstructor lua "Span" inlns attr
    Str str                  -> pushViaConstructor lua "Str" str
    Strikeout inlns          -> pushViaConstructor lua "Strikeout" inlns
    Strong inlns             -> pushViaConstructor lua "Strong" inlns
    Subscript inlns          -> pushViaConstructor lua "Subscript" inlns
    Superscript inlns        -> pushViaConstructor lua "Superscript" inlns
  peek = peekInline
  valuetype _ = TTABLE

instance StackValue Citation where
  push lua (Citation cid prefix suffix mode noteNum hash) =
    pushViaConstructor lua "Citation" cid mode prefix suffix noteNum hash
  peek lua idx = do
    id' <- getField lua idx "citationId"
    prefix <- getField lua idx "citationPrefix"
    suffix <- getField lua idx "citationSuffix"
    mode <- getField lua idx "citationMode"
    num <- getField lua idx "citationNoteNum"
    hash <- getField lua idx "citationHash"
    return $ Citation <$> id' <*> prefix <*> suffix <*> mode <*> num <*> hash
  valuetype _ = TTABLE

instance StackValue CitationMode where
  push lua = \case
    AuthorInText   -> getglobal2 lua "pandoc.AuthorInText"
    NormalCitation -> getglobal2 lua "pandoc.NormalCitation"
    SuppressAuthor -> getglobal2 lua "pandoc.SuppressAuthor"
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
    InlineMath -> getglobal2 lua "pandoc.InlineMath"
    DisplayMath -> getglobal2 lua "pandoc.DisplayMath"
  peek lua idx = do
    res <- getField lua idx "t"
    case res of
      Just "InlineMath" -> return $ Just InlineMath
      Just "DisplayMath" -> return $ Just DisplayMath
      _ -> return Nothing
  valuetype _ = TTABLE

instance StackValue QuoteType where
  push lua = \case
    SingleQuote -> getglobal2 lua "pandoc.SingleQuote"
    DoubleQuote -> getglobal2 lua "pandoc.DoubleQuote"
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
    addIndexedValue lua 1 a
    addIndexedValue lua 2 b
  peek lua idx = do
    a <- getIndexedValue lua idx 1
    b <- getIndexedValue lua idx 2
    return $ (,) <$> a <*> b
  valuetype _ = TTABLE

instance (StackValue a, StackValue b, StackValue c) =>
         StackValue (a, b, c)
 where
  push lua (a, b, c) = do
    newtable lua
    addIndexedValue lua 1 a
    addIndexedValue lua 2 b
    addIndexedValue lua 3 c
  peek lua idx = do
    a <- getIndexedValue lua idx 1
    b <- getIndexedValue lua idx 2
    c <- getIndexedValue lua idx 3
    return $ (,,) <$> a <*> b <*> c
  valuetype _ = TTABLE

instance (Ord a, StackValue a, StackValue b) =>
         StackValue (M.Map a b) where
  push lua m = do
    newtable lua
    mapM_ (uncurry $ addKeyValue lua) $ M.toList m
  peek lua idx = fmap M.fromList <$> keyValuePairs lua idx
  valuetype _ = TTABLE

-- | Try reading the value under the given index as a list of key-value pairs.
keyValuePairs :: (StackValue a, StackValue b)
         => LuaState -> Int -> IO (Maybe [(a, b)])
keyValuePairs lua idx = do
  pushnil lua
  sequence <$> remainingPairs
 where
  remainingPairs = do
    res <- nextPair
    case res of
      Nothing -> return []
      Just a  -> (a:) <$> remainingPairs
  nextPair :: (StackValue a, StackValue b) => IO (Maybe (Maybe (a,b)))
  nextPair = do
    hasNext <- next lua (idx `adjustIndexBy` 1)
    if hasNext
      then do
        val <- peek lua (-1)
        key <- peek lua (-2)
        pop lua 1 -- removes the value, keeps the key
        return $ Just <$> ((,) <$> key <*> val)
      else do
        return Nothing


-- | Helper class for pushing a single value to the stack via a lua function.
-- See @pushViaCall@.
class PushViaCall a where
  pushViaCall' :: LuaState -> String -> IO () -> Int -> a

instance PushViaCall (IO ()) where
  pushViaCall' lua fn pushArgs num = do
    getglobal2 lua fn
    pushArgs
    call lua num 1

instance (StackValue a, PushViaCall b) => PushViaCall (a -> b) where
  pushViaCall' lua fn pushArgs num x =
    pushViaCall' lua fn (pushArgs *> push lua x) (num + 1)

-- | Push an value to the stack via a lua function. The lua function is called
-- with all arguments that are passed to this function and is expected to return
-- a single value.
pushViaCall :: PushViaCall a => LuaState -> String -> a
pushViaCall lua fn = pushViaCall' lua fn (return ()) 0

-- | Call a pandoc element constructor within lua, passing all given arguments.
pushViaConstructor :: PushViaCall a => LuaState -> String -> a
pushViaConstructor lua pandocFn = pushViaCall lua ("pandoc." ++ pandocFn)

-- | Push a value to the lua stack, tagged with a given string. This currently
-- creates a structure equivalent to what the JSONified value would look like
-- when pushed to lua.
pushTagged :: StackValue a => LuaState -> String -> a -> IO ()
pushTagged lua tag value = do
  newtable lua
  addKeyValue lua "t" tag
  addKeyValue lua "c" value

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
setKeyValue :: (StackValue a, StackValue b) => LuaState -> Int -> a -> b -> IO ()
setKeyValue lua idx key value = do
  push lua key
  push lua value
  settable lua (idx `adjustIndexBy` 2)

-- | Add a key-value pair to the table at the top of the stack
addKeyValue :: (StackValue a, StackValue b) => LuaState -> a -> b -> IO ()
addKeyValue lua = setKeyValue lua (-1)

-- | Get value behind key from table at given index.
getIndexedValue :: StackValue a => LuaState -> Int -> Int -> IO (Maybe a)
getIndexedValue lua idx key =
  rawgeti lua idx key
  *> peek lua (-1)
  <* pop lua 1

-- | Set numeric key/value in table at the given index
setIndexedValue :: StackValue a => LuaState -> Int -> Int -> a -> IO ()
setIndexedValue lua idx key value = do
  push lua value
  rawseti lua (idx `adjustIndexBy` 1) key

-- | Set numeric key/value in table at the top of the stack.
addIndexedValue :: StackValue a => LuaState -> Int -> a -> IO ()
addIndexedValue lua = setIndexedValue lua (-1)
