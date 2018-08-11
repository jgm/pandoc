{-# LANGUAGE NoImplicitPrelude #-}
{-
Copyright © 2012-2018 John MacFarlane <jgm@berkeley.edu>
            2017-2018 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

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
   Copyright   : © 2012–2018 John MacFarlane,
                 © 2017-2018 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Lua utility functions.
-}
module Text.Pandoc.Lua.Util
  ( getTag
  , rawField
  , addField
  , addFunction
  , addValue
  , typeCheck
  , popValue
  , PushViaCall
  , pushViaCall
  , pushViaConstructor
  , loadScriptFromDataDir
  , dostring'
  ) where

import Prelude
import Control.Monad (when)
import Control.Monad.Catch (finally)
import Data.ByteString.Char8 (unpack)
import Foreign.Lua (FromLuaStack, Lua, NumArgs, StackIndex, Status,
                    ToLuaStack, ToHaskellFunction)
import Text.Pandoc.Class (readDataFile, runIOorExplode, setUserDataDir)

import qualified Foreign.Lua as Lua

-- | Get value behind key from table at given index.
rawField :: FromLuaStack a => StackIndex -> String -> Lua a
rawField idx key = do
  absidx <- Lua.absindex idx
  Lua.push key
  Lua.rawget absidx
  popValue

-- | Add a value to the table at the top of the stack at a string-index.
addField :: ToLuaStack a => String -> a -> Lua ()
addField = addValue

-- | Add a key-value pair to the table at the top of the stack.
addValue :: (ToLuaStack a, ToLuaStack b) => a -> b -> Lua ()
addValue key value = do
  Lua.push key
  Lua.push value
  Lua.rawset (Lua.nthFromTop 3)

-- | Add a function to the table at the top of the stack, using the given name.
addFunction :: ToHaskellFunction a => String -> a -> Lua ()
addFunction name fn = do
  Lua.push name
  Lua.pushHaskellFunction fn
  Lua.wrapHaskellFunction
  Lua.rawset (-3)

typeCheck :: StackIndex -> Lua.Type -> Lua ()
typeCheck idx expected = do
  actual <- Lua.ltype idx
  when (actual /= expected) $ do
    expName <- Lua.typename expected
    actName <- Lua.typename actual
    Lua.throwLuaError $ "expected " ++ expName ++ " but got " ++ actName ++ "."

-- | Get, then pop the value at the top of the stack.
popValue :: FromLuaStack a => Lua a
popValue = do
  resOrError <- Lua.peekEither (-1)
  Lua.pop 1
  case resOrError of
    Left err -> Lua.throwLuaError err
    Right x -> return x

-- | Helper class for pushing a single value to the stack via a lua function.
-- See @pushViaCall@.
class PushViaCall a where
  pushViaCall' :: String -> Lua () -> NumArgs -> a

instance PushViaCall (Lua ()) where
  pushViaCall' fn pushArgs num = do
    Lua.push fn
    Lua.rawget Lua.registryindex
    pushArgs
    Lua.call num 1

instance (ToLuaStack a, PushViaCall b) => PushViaCall (a -> b) where
  pushViaCall' fn pushArgs num x =
    pushViaCall' fn (pushArgs *> Lua.push x) (num + 1)

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
-- important to keep the program from hanging: If the program containes a call
-- to @require@, then a new loader function is created which then becomes
-- garbage. If that function is collected at an inopportune time, i.e. when the
-- Lua API is called via a function that doesn't allow calling back into Haskell
-- (getraw, setraw, …), then the function's finalizer, and the full program,
-- will hang.
dostring' :: String -> Lua Status
dostring' script = do
  loadRes <- Lua.loadstring script
  if loadRes == Lua.OK
    then Lua.pcall 0 1 Nothing <* Lua.gc Lua.GCCOLLECT 0
    else return loadRes

-- | Get the tag of a value. This is an optimized and specialized version of
-- @Lua.getfield idx "tag"@. It only checks for the field on the table at index
-- @idx@ and on its metatable, also ignoring any @__index@ value on the
-- metatable.
getTag :: StackIndex -> Lua String
getTag idx = do
  -- push metatable or just the table
  Lua.getmetatable idx >>= \hasMT -> when (not hasMT) (Lua.pushvalue idx)
  Lua.push "tag"
  Lua.rawget (Lua.nthFromTop 2)
  Lua.peek Lua.stackTop `finally` Lua.pop 2
