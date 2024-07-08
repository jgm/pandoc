{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Lua.SourcePos
   Copyright   : © 2024 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>

Helper function to retrieve the 'SourcePos' in a Lua script.
-}
module Text.Pandoc.Lua.SourcePos
  ( luaSourcePos
  ) where

import HsLua
import Text.Parsec.Pos (SourcePos, newPos)
import Text.Read (readMaybe)
import qualified Data.Text as T
import qualified HsLua.Core.Utf8 as UTF8

-- | Returns the current position in a Lua script.
--
-- The reporting level is the level of the call stack, for which the
-- position should be reported. There might not always be a position
-- available, e.g., in C functions.
luaSourcePos :: LuaError e
             => Int                -- ^ reporting level
             -> LuaE e (Maybe SourcePos)
luaSourcePos lvl = do
  -- reporting levels:
  -- 0: this hook,
  -- 1: userdata wrapper function for the hook,
  -- 2: warn,
  -- 3: function calling warn.
  where' lvl
  locStr <- UTF8.toText <$> tostring' top
  return $ do
    (prfx, sfx) <- T.breakOnEnd ":" <$> T.stripSuffix ": " locStr
    (source, _) <- T.unsnoc prfx
    line <- readMaybe (T.unpack sfx)
    -- We have no column information, so always use column 1
    Just $ newPos (T.unpack source) line 1
