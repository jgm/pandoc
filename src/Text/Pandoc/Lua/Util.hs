{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{- |
   Module      : Text.Pandoc.Lua.Util
   Copyright   : © 2012-2021 John MacFarlane,
                 © 2017-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Lua utility functions.
-}
module Text.Pandoc.Lua.Util
  ( getTag
  , addField
  , addFunction
  , pushViaConstructor
  , callWithTraceback
  , dofileWithTraceback
  , pushViaConstr'
  ) where

import Control.Monad (unless, when)
import HsLua
import qualified HsLua as Lua

-- | Add a value to the table at the top of the stack at a string-index.
addField :: (LuaError e, Pushable a) => String -> a -> LuaE e ()
addField key value = do
  Lua.push key
  Lua.push value
  Lua.rawset (Lua.nth 3)

-- | Add a function to the table at the top of the stack, using the
-- given name.
addFunction :: Exposable e a => String -> a -> LuaE e ()
addFunction name fn = do
  Lua.push name
  Lua.pushHaskellFunction $ toHaskellFunction fn
  Lua.rawset (-3)

-- | Helper class for pushing a single value to the stack via a lua
-- function. See @pushViaCall@.
class LuaError e => PushViaCall e a where
  pushViaCall' :: LuaError e => Name -> LuaE e () -> NumArgs -> a

instance LuaError e => PushViaCall e (LuaE e ()) where
  pushViaCall' fn pushArgs num = do
    Lua.pushName @e fn
    Lua.rawget Lua.registryindex
    pushArgs
    Lua.call num 1

instance (LuaError e, Pushable a, PushViaCall e b) =>
         PushViaCall e (a -> b) where
  pushViaCall' fn pushArgs num x =
    pushViaCall' @e fn (pushArgs *> Lua.push x) (num + 1)

-- | Push an value to the stack via a lua function. The lua function is called
-- with all arguments that are passed to this function and is expected to return
-- a single value.
pushViaCall :: forall e a. LuaError e => PushViaCall e a => Name -> a
pushViaCall fn = pushViaCall' @e fn (return ()) 0

-- | Call a pandoc element constructor within Lua, passing all given arguments.
pushViaConstructor :: forall e a. LuaError e => PushViaCall e a => Name -> a
pushViaConstructor pandocFn = pushViaCall @e ("pandoc." <> pandocFn)

-- | Get the tag of a value. This is an optimized and specialized version of
-- @Lua.getfield idx "tag"@. It only checks for the field on the table at index
-- @idx@ and on its metatable, also ignoring any @__index@ value on the
-- metatable.
getTag :: LuaError e => Peeker e Name
getTag idx = do
  -- push metatable or just the table
  liftLua $ do
    Lua.getmetatable idx >>= \hasMT -> unless hasMT (Lua.pushvalue idx)
    Lua.pushName "tag"
    Lua.rawget (Lua.nth 2)
  Lua.peekName Lua.top `lastly` Lua.pop 2  -- table/metatable and `tag` field

pushViaConstr' :: forall e. LuaError e => Name -> [LuaE e ()] -> LuaE e ()
pushViaConstr' fnname pushArgs = do
  pushName @e ("pandoc." <> fnname)
  rawget @e registryindex
  sequence_ pushArgs
  call @e (fromIntegral (length pushArgs)) 1

-- | Like @'Lua.pcall'@, but uses a predefined error handler which adds a
-- traceback on error.
pcallWithTraceback :: LuaError e => NumArgs -> NumResults -> LuaE e Status
pcallWithTraceback nargs nresults = do
  let traceback' :: LuaError e => LuaE e NumResults
      traceback' = do
        l <- Lua.state
        msg <- Lua.tostring' (Lua.nthBottom 1)
        Lua.traceback l (Just msg) 2
        return 1
  tracebackIdx <- Lua.absindex (Lua.nth (Lua.fromNumArgs nargs + 1))
  Lua.pushHaskellFunction traceback'
  Lua.insert tracebackIdx
  result <- Lua.pcall nargs nresults (Just tracebackIdx)
  Lua.remove tracebackIdx
  return result

-- | Like @'Lua.call'@, but adds a traceback to the error message (if any).
callWithTraceback :: LuaError e => NumArgs -> NumResults -> LuaE e ()
callWithTraceback nargs nresults = do
  result <- pcallWithTraceback nargs nresults
  when (result /= Lua.OK)
    Lua.throwErrorAsException

-- | Run the given string as a Lua program, while also adding a traceback to the
-- error message if an error occurs.
dofileWithTraceback :: LuaError e => FilePath -> LuaE e Status
dofileWithTraceback fp = do
  loadRes <- Lua.loadfile fp
  case loadRes of
    Lua.OK -> pcallWithTraceback 0 Lua.multret
    _ -> return loadRes
