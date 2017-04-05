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
import Scripting.Lua ( LuaState, StackValue(..), newtable, pop, rawget, rawset )
import Scripting.Lua.Aeson ()
import Text.Pandoc.Definition ( Block(..), Inline(..), Pandoc(..) )

import qualified Scripting.Lua as Lua
import qualified Text.Pandoc.UTF8 as UTF8

maybeFromJson :: (FromJSON a) => Maybe Value -> Maybe a
maybeFromJson mv = fromJSON <$> mv >>= \case
  Success x -> Just x
  _         -> Nothing

instance StackValue Pandoc where
  push lua = Lua.push lua . toJSON
  peek lua i = maybeFromJson <$> peek lua i
  valuetype _ = Lua.TTABLE

instance StackValue Block where
  push lua = Lua.push lua . toJSON
  peek lua i = maybeFromJson <$> peek lua i
  valuetype _ = Lua.TTABLE

instance StackValue Inline where
  push lua = \case
    Emph inlns        -> pushTagged lua "Emph" inlns
    LineBreak         -> pushTagged' lua "LineBreak"
    Note blcks        -> pushTagged lua "Note" blcks
    SmallCaps inlns   -> pushTagged lua "SmallCaps" inlns
    SoftBreak         -> pushTagged' lua "SoftBreak"
    Space             -> pushTagged' lua "Space"
    Str s             -> pushTagged lua "Str" s
    Strikeout inlns   -> pushTagged lua "Strikeout" inlns
    Strong inlns      -> pushTagged lua "Strong" inlns
    Subscript inlns   -> pushTagged lua "Subscript" inlns
    Superscript inlns -> pushTagged lua "Superscript" inlns
    x                 -> push lua (toJSON x)
  peek = peekInline
  valuetype _ = Lua.TTABLE

#if MIN_VERSION_base(4,8,0)
instance {-# OVERLAPS #-} StackValue [Char] where
#else
instance StackValue [Char] where
#endif
  push lua cs = push lua (UTF8.fromString cs)
  peek lua i = fmap UTF8.toString <$> peek lua i
  valuetype _ = Lua.TSTRING

-- | Push a value to the lua stack, tagged with a given string. This currently
-- creates a structure equivalent to what the JSONified value would look like
-- when pushed to lua.
pushTagged :: StackValue a => LuaState -> String -> a -> IO ()
pushTagged lua tag value = do
  newtable lua
  push lua "t"
  push lua tag
  rawset lua (-3)
  push lua "c"
  push lua value
  rawset lua (-3)

pushTagged' :: LuaState -> String -> IO ()
pushTagged' lua tag = do
  newtable lua
  push lua "t"
  push lua tag
  rawset lua (-3)

-- | Return the value at the given index as inline if possible.
peekInline :: LuaState -> Int -> IO (Maybe Inline)
peekInline lua idx = do
  push lua "t"
  rawget lua (idx `adjustIndexBy` 1)
  tag <- peek lua (-1) <* pop lua 1
  case tag of
    Nothing -> return Nothing
    Just t -> case t of
      "Emph"       -> fmap Emph <$> elementContent
      "LineBreak"  -> return (Just LineBreak)
      "Note"       -> fmap Note <$> elementContent
      "SmallCaps"  -> fmap SmallCaps <$> elementContent
      "SoftBreak"  -> return (Just SoftBreak)
      "Space"      -> return (Just Space)
      "Str"        -> fmap Str <$> elementContent
      "Strikeout"  -> fmap Strikeout <$> elementContent
      "Strong"     -> fmap Strong <$> elementContent
      "Subscript"  -> fmap Subscript <$> elementContent
      "Superscript"-> fmap Superscript <$> elementContent
      _ -> maybeFromJson <$> peek lua idx
 where
  elementContent :: StackValue a => IO (Maybe a)
  elementContent = do
    push lua "c"
    rawget lua (idx `adjustIndexBy` 1)
    peek lua (-1) <* pop lua 1

-- | Adjust the stack index, assuming that @n@ new elements have been pushed on
-- the stack.
adjustIndexBy :: Int -> Int -> Int
adjustIndexBy idx n =
  if idx < 0
  then idx - n
  else idx
