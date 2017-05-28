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
  , setTable
  , addValue
  , getRawInt
  , setRawInt
  , addRawInt
  , keyValuePairs
  , PushViaCall
  , pushViaCall
  , pushViaConstructor
  ) where

import Scripting.Lua
  ( LuaState, StackValue(..)
  , call, getglobal2, gettable, next, pop, pushnil, rawgeti, rawseti, settable
  )

-- | Adjust the stack index, assuming that @n@ new elements have been pushed on
-- the stack.
adjustIndexBy :: Int -> Int -> Int
adjustIndexBy idx n =
  if idx < 0
  then idx - n
  else idx

-- | Get value behind key from table at given index.
getTable :: (StackValue a, StackValue b) => LuaState -> Int -> a -> IO (Maybe b)
getTable lua idx key = do
  push lua key
  gettable lua (idx `adjustIndexBy` 1)
  peek lua (-1) <* pop lua 1

-- | Set value for key for table at the given index
setTable :: (StackValue a, StackValue b) => LuaState -> Int -> a -> b -> IO ()
setTable lua idx key value = do
  push lua key
  push lua value
  settable lua (idx `adjustIndexBy` 2)

-- | Add a key-value pair to the table at the top of the stack
addValue :: (StackValue a, StackValue b) => LuaState -> a -> b -> IO ()
addValue lua = setTable lua (-1)

-- | Get value behind key from table at given index.
getRawInt :: StackValue a => LuaState -> Int -> Int -> IO (Maybe a)
getRawInt lua idx key =
  rawgeti lua idx key
  *> peek lua (-1)
  <* pop lua 1

-- | Set numeric key/value in table at the given index
setRawInt :: StackValue a => LuaState -> Int -> Int -> a -> IO ()
setRawInt lua idx key value = do
  push lua value
  rawseti lua (idx `adjustIndexBy` 1) key

-- | Set numeric key/value in table at the top of the stack.
addRawInt :: StackValue a => LuaState -> Int -> a -> IO ()
addRawInt lua = setRawInt lua (-1)

-- | Try reading the table under the given index as a list of key-value pairs.
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
