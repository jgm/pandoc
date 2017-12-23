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
   Copyright   : © 2012–2017 John MacFarlane,
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
  , addFunction
  , getRawInt
  , setRawInt
  , addRawInt
  , raiseError
  , OrNil (..)
  , PushViaCall
  , pushViaCall
  , pushViaConstructor
  , loadScriptFromDataDir
  , dostring'
  ) where

import Control.Monad (when)
import Data.ByteString.Char8 (unpack)
import Foreign.Lua (FromLuaStack (..), NumResults, Lua, NumArgs, StackIndex,
                    ToLuaStack (..), ToHaskellFunction, getglobal')
import Foreign.Lua.Api (Status, call, pop, rawget, rawgeti, rawset, rawseti)
import Text.Pandoc.Class (readDataFile, runIOorExplode, setUserDataDir)

import qualified Foreign.Lua as Lua

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
  popValue

-- | Add a key-value pair to the table at the top of the stack.
addValue :: (ToLuaStack a, ToLuaStack b) => a -> b -> Lua ()
addValue key value = do
  push key
  push value
  rawset (-3)

-- | Add a function to the table at the top of the stack, using the given name.
addFunction :: ToHaskellFunction a => String -> a -> Lua ()
addFunction name fn = do
  Lua.push name
  Lua.pushHaskellFunction fn
  Lua.wrapHaskellFunction
  Lua.rawset (-3)

-- | Get value behind key from table at given index.
getRawInt :: FromLuaStack a => StackIndex -> Int -> Lua a
getRawInt idx key = do
  rawgeti idx key
  popValue

-- | Set numeric key/value in table at the given index
setRawInt :: ToLuaStack a => StackIndex -> Int -> a -> Lua ()
setRawInt idx key value = do
  push value
  rawseti (idx `adjustIndexBy` 1) key

-- | Set numeric key/value in table at the top of the stack.
addRawInt :: ToLuaStack a => Int -> a -> Lua ()
addRawInt = setRawInt (-1)

raiseError :: ToLuaStack a => a -> Lua NumResults
raiseError e = do
  Lua.push e
  fromIntegral <$> Lua.lerror

-- | Get, then pop the value at the top of the stack.
popValue :: FromLuaStack a => Lua a
popValue = do
  resOrError <- Lua.peekEither (-1)
  pop 1
  case resOrError of
    Left err -> Lua.throwLuaError err
    Right x -> return x

-- | Newtype wrapper intended to be used for optional Lua values. Nesting this
-- type is strongly discouraged and will likely lead to a wrong result.
newtype OrNil a = OrNil { toMaybe :: Maybe a }

instance FromLuaStack a => FromLuaStack (OrNil a) where
  peek idx = do
    noValue <- Lua.isnoneornil idx
    if noValue
      then return (OrNil Nothing)
      else OrNil . Just <$> Lua.peek idx

instance ToLuaStack a => ToLuaStack (OrNil a) where
  push (OrNil Nothing)  = Lua.pushnil
  push (OrNil (Just x)) = Lua.push x

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

-- | Load a file from pandoc's data directory.
loadScriptFromDataDir :: Maybe FilePath -> FilePath -> Lua ()
loadScriptFromDataDir datadir scriptFile = do
  script <- fmap unpack . Lua.liftIO . runIOorExplode $
            setUserDataDir datadir >> readDataFile scriptFile
  status <- dostring' script
  when (status /= Lua.OK) .
    Lua.throwTopMessageAsError' $ \msg ->
      "Couldn't load '" ++ scriptFile ++ "'.\n" ++ msg

-- | Load a string and immediately perform a full garbage collection. This is
-- important to keep the program from hanging: If the program contained a call
-- to @require@, the a new loader function was created which then become
-- garbage. If that function is collected at an inopportune times, i.e. when the
-- Lua API is called via a function that doesn't allow calling back into Haskell
-- (getraw, setraw, …). The function's finalizer, and the full program, hangs
-- when that happens.
dostring' :: String -> Lua Status
dostring' script = do
  loadRes <- Lua.loadstring script
  if loadRes == Lua.OK
    then Lua.pcall 0 1 Nothing <* Lua.gc Lua.GCCOLLECT 0
    else return loadRes
