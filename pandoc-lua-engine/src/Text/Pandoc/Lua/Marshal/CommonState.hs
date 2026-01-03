{-# LANGUAGE OverloadedStrings    #-}
{- |
   Module      : Text.Pandoc.Lua.Marshal.CommonState
   Copyright   : © 2012-2024 John MacFarlane
                 © 2017-2024 Albert Krewinkel
   License     : GNU GPL, version 2 or above
   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>
   Stability   : alpha

Instances to marshal (push) and unmarshal (peek) the common state.
-}
module Text.Pandoc.Lua.Marshal.CommonState
  ( typeCommonState
  , peekCommonState
  , pushCommonState
  ) where

import HsLua
import Text.Pandoc.Class (CommonState)

-- | Lua type used for the @CommonState@ object.
--
-- This is an opaque value that is required for the Lua interpreter
-- to become an instance of "PandocMonad".
--
typeCommonState :: LuaError e => DocumentedType e CommonState
typeCommonState = deftype "CommonState" [] []

-- | Retrieves the common state from Lua
peekCommonState :: LuaError e => Peeker e CommonState
peekCommonState = peekUD typeCommonState

-- | Pushes the common pandoc state to the Lua stack.
pushCommonState :: LuaError e => Pusher e CommonState
pushCommonState = pushUD typeCommonState
