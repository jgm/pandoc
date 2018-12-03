{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Lua.Util
   Copyright   : © 2012–2020 John MacFarlane,
                 © 2017-2020 Albert Krewinkel
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
  , pushViaConstructor
  , defineHowTo
  , throwTopMessageAsError'
  , callWithTraceback
  , dofileWithTraceback
  ) where

import Control.Monad (unless, when)
import Data.Text (Text)
import Foreign.Lua ( Lua, NumArgs, NumResults, Peekable, Pushable, StackIndex
                   , Status, ToHaskellFunction )
import qualified Foreign.Lua as Lua
import qualified Text.Pandoc.UTF8 as UTF8

-- | Get value behind key from table at given index.
rawField :: Peekable a => StackIndex -> String -> Lua a
rawField idx key = do
  absidx <- Lua.absindex idx
  Lua.push key
  Lua.rawget absidx
  Lua.popValue

-- | Add a value to the table at the top of the stack at a string-index.
addField :: Pushable a => String -> a -> Lua ()
addField = addValue

-- | Add a key-value pair to the table at the top of the stack.
addValue :: (Pushable a, Pushable b) => a -> b -> Lua ()
addValue key value = do
  Lua.push key
  Lua.push value
  Lua.rawset (Lua.nthFromTop 3)

-- | Add a function to the table at the top of the stack, using the given name.
addFunction :: ToHaskellFunction a => String -> a -> Lua ()
addFunction name fn = do
  Lua.push name
  Lua.pushHaskellFunction fn
  Lua.rawset (-3)

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

instance (Pushable a, PushViaCall b) => PushViaCall (a -> b) where
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

-- | Get the tag of a value. This is an optimized and specialized version of
-- @Lua.getfield idx "tag"@. It only checks for the field on the table at index
-- @idx@ and on its metatable, also ignoring any @__index@ value on the
-- metatable.
getTag :: StackIndex -> Lua String
getTag idx = do
  -- push metatable or just the table
  Lua.getmetatable idx >>= \hasMT -> unless hasMT (Lua.pushvalue idx)
  Lua.push ("tag" :: Text)
  Lua.rawget (Lua.nthFromTop 2)
  Lua.tostring Lua.stackTop <* Lua.pop 2 >>= \case
    Nothing -> Lua.throwMessage "untagged value"
    Just x -> return (UTF8.toString x)

-- | Modify the message at the top of the stack before throwing it as an
-- Exception.
throwTopMessageAsError' :: (String -> String) -> Lua a
throwTopMessageAsError' modifier = do
  msg <- Lua.tostring' Lua.stackTop
  Lua.pop 2 -- remove error and error string pushed by tostring'
  Lua.throwMessage (modifier (UTF8.toString msg))

-- | Mark the context of a Lua computation for better error reporting.
defineHowTo :: String -> Lua a -> Lua a
defineHowTo ctx op = Lua.errorConversion >>= \ec ->
  Lua.addContextToException ec ("Could not " <> ctx <> ": ") op

-- | Like @'Lua.pcall'@, but uses a predefined error handler which adds a
-- traceback on error.
pcallWithTraceback :: NumArgs -> NumResults -> Lua Status
pcallWithTraceback nargs nresults = do
  let traceback' :: Lua NumResults
      traceback' = do
        l <- Lua.state
        msg <- Lua.tostring' (Lua.nthFromBottom 1)
        Lua.traceback l (Just (UTF8.toString msg)) 2
        return 1
  tracebackIdx <- Lua.absindex (Lua.nthFromTop (Lua.fromNumArgs nargs + 1))
  Lua.pushHaskellFunction traceback'
  Lua.insert tracebackIdx
  result <- Lua.pcall nargs nresults (Just tracebackIdx)
  Lua.remove tracebackIdx
  return result

-- | Like @'Lua.call'@, but adds a traceback to the error message (if any).
callWithTraceback :: NumArgs -> NumResults -> Lua ()
callWithTraceback nargs nresults = do
  result <- pcallWithTraceback nargs nresults
  when (result /= Lua.OK)
    Lua.throwTopMessage

-- | Run the given string as a Lua program, while also adding a traceback to the
-- error message if an error occurs.
dofileWithTraceback :: FilePath -> Lua Status
dofileWithTraceback fp = do
  loadRes <- Lua.loadfile fp
  case loadRes of
    Lua.OK -> pcallWithTraceback 0 Lua.multret
    _ -> return loadRes
