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
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
#if !MIN_VERSION_base(4,8,0)
{-# LANGUAGE OverlappingInstances #-}
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Text.Pandoc.Lua.SharedInstances
   Copyright   : © 2012–2016 John MacFarlane,
                 © 2017 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Shared StackValue instances for pandoc and generic types.
-}
module Text.Pandoc.Lua.SharedInstances () where

import Scripting.Lua (LTYPE (..), StackValue (..), newtable)
import Text.Pandoc.Lua.Util (addRawInt, addValue, getRawInt, keyValuePairs)

import qualified Data.Map as M
import qualified Text.Pandoc.UTF8 as UTF8

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
    addRawInt lua 1 a
    addRawInt lua 2 b
  peek lua idx = do
    a <- getRawInt lua idx 1
    b <- getRawInt lua idx 2
    return $ (,) <$> a <*> b
  valuetype _ = TTABLE

instance (StackValue a, StackValue b, StackValue c) =>
         StackValue (a, b, c)
 where
  push lua (a, b, c) = do
    newtable lua
    addRawInt lua 1 a
    addRawInt lua 2 b
    addRawInt lua 3 c
  peek lua idx = do
    a <- getRawInt lua idx 1
    b <- getRawInt lua idx 2
    c <- getRawInt lua idx 3
    return $ (,,) <$> a <*> b <*> c
  valuetype _ = TTABLE

instance (StackValue a, StackValue b, StackValue c,
          StackValue d, StackValue e) =>
         StackValue (a, b, c, d, e)
 where
  push lua (a, b, c, d, e) = do
    newtable lua
    addRawInt lua 1 a
    addRawInt lua 2 b
    addRawInt lua 3 c
    addRawInt lua 4 d
    addRawInt lua 5 e
  peek lua idx = do
    a <- getRawInt lua idx 1
    b <- getRawInt lua idx 2
    c <- getRawInt lua idx 3
    d <- getRawInt lua idx 4
    e <- getRawInt lua idx 5
    return $ (,,,,) <$> a <*> b <*> c <*> d <*> e
  valuetype _ = TTABLE

instance (Ord a, StackValue a, StackValue b) =>
         StackValue (M.Map a b) where
  push lua m = do
    newtable lua
    mapM_ (uncurry $ addValue lua) $ M.toList m
  peek lua idx = fmap M.fromList <$> keyValuePairs lua idx
  valuetype _ = TTABLE

instance (StackValue a, StackValue b) => StackValue (Either a b) where
  push lua = \case
    Left x -> push lua x
    Right x -> push lua x
  peek lua idx = peek lua idx >>= \case
      Just left -> return . Just $ Left left
      Nothing   -> fmap Right <$> peek lua idx
  valuetype (Left x)  = valuetype x
  valuetype (Right x) = valuetype x
