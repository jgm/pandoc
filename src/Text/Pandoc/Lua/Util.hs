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
{- |
   Module      : Text.Pandoc.Lua.Util
   Copyright   : © 2012–2016 John MacFarlane,
                 © 2017 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Lua utility functions.
-}
module Text.Pandoc.Lua.Util
  ( adjustIndexBy
  , getTable
  , addValue
  , getRawInt
  , setRawInt
  , addRawInt
  , PushViaCall
  , pushViaCall
  , pushViaConstructor
  ) where

import Foreign.Lua (Lua, FromLuaStack (..), ToLuaStack (..), NumArgs,
                    StackIndex, getglobal')
import Foreign.Lua.Api (call, pop, rawget, rawgeti, rawset, rawseti)

-- | Adjust the stack index, assuming that @n@ new elements have been pushed on
-- the stack.
adjustIndexBy :: StackIndex -> StackIndex -> StackIndex
adjustIndexBy idx n =
  if idx < 0
  then idx - n
  else idx

-- | Get value behind key from table at given index.
getTable :: (ToLuaStack a, FromLuaStack b) => StackIndex -> a -> Lua b
getTable idx key = do
  push key
  rawget (idx `adjustIndexBy` 1)
  peek (-1) <* pop 1

-- | Add a key-value pair to the table at the top of the stack
addValue :: (ToLuaStack a, ToLuaStack b) => a -> b -> Lua ()
addValue key value = do
  push key
  push value
  rawset (-3)

-- | Get value behind key from table at given index.
getRawInt :: FromLuaStack a => StackIndex -> Int -> Lua a
getRawInt idx key =
  rawgeti idx key
  *> peek (-1)
  <* pop 1

-- | Set numeric key/value in table at the given index
setRawInt :: ToLuaStack a => StackIndex -> Int -> a -> Lua ()
setRawInt idx key value = do
  push value
  rawseti (idx `adjustIndexBy` 1) key

-- | Set numeric key/value in table at the top of the stack.
addRawInt :: ToLuaStack a => Int -> a -> Lua ()
addRawInt = setRawInt (-1)

-- | Helper class for pushing a single value to the stack via a lua function.
-- See @pushViaCall@.
class PushViaCall a where
  pushViaCall' :: String -> Lua () -> NumArgs -> a

instance PushViaCall (Lua ()) where
  pushViaCall' fn pushArgs num = do
    getglobal' fn
    pushArgs
    call num 1

instance (ToLuaStack a, PushViaCall b) => PushViaCall (a -> b) where
  pushViaCall' fn pushArgs num x =
    pushViaCall' fn (pushArgs *> push x) (num + 1)

-- | Push an value to the stack via a lua function. The lua function is called
-- with all arguments that are passed to this function and is expected to return
-- a single value.
pushViaCall :: PushViaCall a => String -> a
pushViaCall fn = pushViaCall' fn (return ()) 0

-- | Call a pandoc element constructor within lua, passing all given arguments.
pushViaConstructor :: PushViaCall a => String -> a
pushViaConstructor pandocFn = pushViaCall ("pandoc." ++ pandocFn)
