{-
Copyright © 2017 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

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
{- |
   Module      : Text.Pandoc.Lua.Module.Utils
   Copyright   : Copyright © 2017 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Utility module for lua, exposing internal helper functions.
-}
module Text.Pandoc.Lua.Module.Utils
  ( pushModule
  ) where

import Control.Applicative ((<|>))
import Foreign.Lua (FromLuaStack, Lua, LuaInteger, NumResults)
import Text.Pandoc.Definition (Pandoc, Meta, Block, Inline)
import Text.Pandoc.Lua.StackInstances ()
import Text.Pandoc.Lua.Util (OrNil (OrNil), addFunction)

import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.ByteString.Lazy as BSL
import qualified Foreign.Lua as Lua
import qualified Text.Pandoc.Shared as Shared

-- | Push the "pandoc.utils" module to the lua stack.
pushModule :: Lua NumResults
pushModule = do
  Lua.newtable
  addFunction "hierarchicalize" hierarchicalize
  addFunction "normalize_date" normalizeDate
  addFunction "sha1" sha1
  addFunction "stringify" stringify
  addFunction "to_roman_numeral" toRomanNumeral
  return 1

-- | Convert list of Pandoc blocks into (hierarchical) list of Elements.
hierarchicalize :: [Block] -> Lua [Shared.Element]
hierarchicalize = return . Shared.hierarchicalize

-- | Parse a date and convert (if possible) to "YYYY-MM-DD" format. We
-- limit years to the range 1601-9999 (ISO 8601 accepts greater than
-- or equal to 1583, but MS Word only accepts dates starting 1601).
-- Returns nil instead of a string if the conversion failed.
normalizeDate :: String -> Lua (OrNil String)
normalizeDate = return . OrNil . Shared.normalizeDate

-- | Calculate the hash of the given contents.
sha1 :: BSL.ByteString
     -> Lua String
sha1 = return . SHA.showDigest . SHA.sha1

-- | Convert pandoc structure to a string with formatting removed.
-- Footnotes are skipped (since we don't want their contents in link
-- labels).
stringify :: AstElement -> Lua String
stringify el = return $ case el of
  PandocElement pd -> Shared.stringify pd
  InlineElement i  -> Shared.stringify i
  BlockElement b   -> Shared.stringify b
  MetaElement m    -> Shared.stringify m

data AstElement
  = PandocElement Pandoc
  | MetaElement Meta
  | BlockElement Block
  | InlineElement Inline
  deriving (Show)

instance FromLuaStack AstElement where
  peek idx  = do
    res <- Lua.tryLua $  (PandocElement <$> Lua.peek idx)
                     <|> (InlineElement <$> Lua.peek idx)
                     <|> (BlockElement <$> Lua.peek idx)
                     <|> (MetaElement <$> Lua.peek idx)
    case res of
      Right x -> return x
      Left _ -> Lua.throwLuaError
        "Expected an AST element, but could not parse value as such."

-- | Convert a number < 4000 to uppercase roman numeral.
toRomanNumeral :: LuaInteger -> Lua String
toRomanNumeral = return . Shared.toRomanNumeral . fromIntegral
