{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
   Module      : Text.Pandoc.Lua.ErrorConversion
   Copyright   : © 2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Define how Lua errors are converted into @'PandocError'@ Haskell
exceptions, and /vice versa/.
-}
module Text.Pandoc.Lua.ErrorConversion
  ( errorConversion
  ) where

import Foreign.Lua (Lua (..), NumResults)
import Text.Pandoc.Error (PandocError (PandocLuaError))
import Text.Pandoc.Lua.Marshaling.PandocError (pushPandocError, peekPandocError)

import qualified Control.Monad.Catch as Catch
import qualified Data.Text as T
import qualified Foreign.Lua as Lua

-- | Conversions between Lua errors and Haskell exceptions, assuming
-- that all exceptions are of type @'PandocError'@.
errorConversion :: Lua.ErrorConversion
errorConversion = Lua.ErrorConversion
  { Lua.addContextToException = addContextToException
  , Lua.alternative           = alternative
  , Lua.errorToException      = errorToException
  , Lua.exceptionToError      = exceptionToError
  }

-- | Convert a Lua error, which must be at the top of the stack, into a
-- @'PandocError'@, popping the value from the stack.
errorToException :: forall a . Lua.State -> IO a
errorToException l = Lua.unsafeRunWith l $ do
  err <- peekPandocError Lua.stackTop
  Lua.pop 1
  Catch.throwM err

-- | Try the first op -- if it doesn't succeed, run the second.
alternative :: forall a . Lua a -> Lua a -> Lua a
alternative x y = Catch.try x >>= \case
  Left (_ :: PandocError) -> y
  Right x' -> return x'

-- | Add more context to an error
addContextToException :: forall a . String -> Lua a -> Lua a
addContextToException ctx op = op `Catch.catch` \case
  PandocLuaError msg -> Catch.throwM $ PandocLuaError (T.pack ctx <> msg)
  e -> Catch.throwM e

-- | Catch a @'PandocError'@ exception and raise it as a Lua error.
exceptionToError :: Lua NumResults -> Lua NumResults
exceptionToError op = op `Catch.catch` \e -> do
  pushPandocError e
  Lua.error
