{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Text.Pandoc.Lua.Marshaling.Format
   Copyright   : © 2022 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshaling functions and instance for format related types, including
'Extensions' and 'ExtensionConfig'.
-}
module Text.Pandoc.Lua.Marshal.Format
  ( peekExtensions
  , pushExtensions
  , peekExtensionsConfig
  ) where

import HsLua
import Text.Pandoc.Extensions (Extension, Extensions, extensionsFromList, readExtension)
import Text.Pandoc.Format (ExtensionsConfig (..))

-- | Retrieves an 'Extensions' set from the Lua stack.
peekExtension :: LuaError e => Peeker e Extension
peekExtension idx = do
  extString <- peekString idx
  return $ readExtension extString
{-# INLINE peekExtension #-}

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

-- | Retrieves an 'ExtensionsConfig' value from the Lua stack.
peekExtensionsConfig :: LuaError e => Peeker e ExtensionsConfig
peekExtensionsConfig idx = do
  exts <- peekKeyValuePairs peekExtension peekBool idx
  return $ ExtensionsConfig
    { extsDefault   = extensionsFromList . map fst $ filter snd exts
    , extsSupported = extensionsFromList . map fst $ exts
    }
