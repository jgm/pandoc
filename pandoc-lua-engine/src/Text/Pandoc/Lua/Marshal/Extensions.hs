{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Text.Pandoc.Lua.Marshaling.Extensions
   Copyright   : © 2022 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling functions and instance for 'Extensions'.
-}
module Text.Pandoc.Lua.Marshal.Extensions
  ( peekExtensions
  , pushExtensions
  ) where

import HsLua
import Text.Pandoc.Extensions (Extensions)

-- | Retrieves an 'Extensions' set from the Lua stack.
peekExtensions :: LuaError e => Peeker e Extensions
peekExtensions = peekViaJSON
{-# INLINE peekExtensions #-}

-- | Pushes a set of 'Extensions' to the top of the Lua stack.
pushExtensions :: LuaError e => Pusher e Extensions
pushExtensions = pushViaJSON
{-# INLINE pushExtensions #-}

instance Peekable Extensions where
  safepeek = peekExtensions

instance Pushable Extensions where
  push = pushExtensions
