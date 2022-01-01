{-# LANGUAGE OverloadedStrings     #-}
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
  ( addField
  , callWithTraceback
  , pcallWithTraceback
  , dofileWithTraceback
  , peekViaJSON
  , pushViaJSON
  ) where

import Control.Monad (when)
import HsLua
import HsLua.Aeson (peekValue, pushValue)
import qualified Data.Aeson as Aeson
import qualified HsLua as Lua
import qualified Text.Pandoc.UTF8 as UTF8

-- | Add a value to the table at the top of the stack at a string-index.
addField :: (LuaError e, Pushable a) => String -> a -> LuaE e ()
addField key value = do
  Lua.push key
  Lua.push value
  Lua.rawset (Lua.nth 3)

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


-- These will become part of hslua-aeson in future versions.

-- | Retrieves a value from the Lua stack via JSON.
peekViaJSON :: (Aeson.FromJSON a, LuaError e) => Peeker e a
peekViaJSON idx = do
  value <- peekValue idx
  case Aeson.fromJSON value of
    Aeson.Success x -> pure x
    Aeson.Error msg -> failPeek $ "failed to decode: " <>
                       UTF8.fromString msg

-- | Pushes a value to the Lua stack as a JSON-like value.
pushViaJSON :: (Aeson.ToJSON a, LuaError e) => Pusher e a
pushViaJSON = pushValue . Aeson.toJSON
