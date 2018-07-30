{-# LANGUAGE NoImplicitPrelude #-}
{-
Copyright © 2017-2018 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

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
   Copyright   : Copyright © 2017-2018 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Utility module for lua, exposing internal helper functions.
-}
module Text.Pandoc.Lua.Module.Utils
  ( pushModule
  ) where

import Prelude
import Control.Applicative ((<|>))
import Data.Default (def)
import Foreign.Lua (FromLuaStack, Lua, LuaInteger, NumResults)
import Text.Pandoc.Class (runIO, setUserDataDir)
import Text.Pandoc.Definition (Pandoc, Meta, MetaValue, Block, Inline)
import Text.Pandoc.Lua.StackInstances ()
import Text.Pandoc.Lua.Util (addFunction, popValue)

import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.ByteString.Lazy as BSL
import qualified Foreign.Lua as Lua
import qualified Text.Pandoc.Builder as B
import qualified Text.Pandoc.Filter.JSON as JSONFilter
import qualified Text.Pandoc.Shared as Shared

-- | Push the "pandoc.utils" module to the lua stack.
pushModule :: Maybe FilePath -> Lua NumResults
pushModule mbDatadir = do
  Lua.newtable
  addFunction "blocks_to_inlines" blocksToInlines
  addFunction "hierarchicalize" hierarchicalize
  addFunction "normalize_date" normalizeDate
  addFunction "run_json_filter" (runJSONFilter mbDatadir)
  addFunction "sha1" sha1
  addFunction "stringify" stringify
  addFunction "to_roman_numeral" toRomanNumeral
  return 1

-- | Squashes a list of blocks into inlines.
blocksToInlines :: [Block] -> Lua.Optional [Inline] -> Lua [Inline]
blocksToInlines blks optSep = do
  let sep = case Lua.fromOptional optSep of
              Just x -> B.fromList x
              Nothing -> Shared.defaultBlocksSeparator
  return $ B.toList (Shared.blocksToInlinesWithSep sep blks)

-- | Convert list of Pandoc blocks into (hierarchical) list of Elements.
hierarchicalize :: [Block] -> Lua [Shared.Element]
hierarchicalize = return . Shared.hierarchicalize

-- | Parse a date and convert (if possible) to "YYYY-MM-DD" format. We
-- limit years to the range 1601-9999 (ISO 8601 accepts greater than
-- or equal to 1583, but MS Word only accepts dates starting 1601).
-- Returns nil instead of a string if the conversion failed.
normalizeDate :: String -> Lua (Lua.Optional String)
normalizeDate = return . Lua.Optional . Shared.normalizeDate

-- | Run a JSON filter on the given document.
runJSONFilter :: Maybe FilePath
              -> Pandoc
              -> FilePath
              -> Lua.Optional [String]
              -> Lua NumResults
runJSONFilter mbDatadir doc filterFile optArgs = do
  args <- case Lua.fromOptional optArgs of
            Just x -> return x
            Nothing -> do
              Lua.getglobal "FORMAT"
              (:[]) <$> popValue
  filterRes <- Lua.liftIO . runIO $ do
    setUserDataDir mbDatadir
    JSONFilter.apply def args filterFile doc
  case filterRes of
    Left err -> Lua.raiseError (show err)
    Right d -> (1 :: NumResults) <$ Lua.push d

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
  MetaValueElement m -> Shared.stringify m

data AstElement
  = PandocElement Pandoc
  | MetaElement Meta
  | BlockElement Block
  | InlineElement Inline
  | MetaValueElement MetaValue
  deriving (Show)

instance FromLuaStack AstElement where
  peek idx  = do
    res <- Lua.tryLua $  (PandocElement <$> Lua.peek idx)
                     <|> (InlineElement <$> Lua.peek idx)
                     <|> (BlockElement <$> Lua.peek idx)
                     <|> (MetaElement <$> Lua.peek idx)
                     <|> (MetaValueElement <$> Lua.peek idx)
    case res of
      Right x -> return x
      Left _ -> Lua.throwLuaError
        "Expected an AST element, but could not parse value as such."

-- | Convert a number < 4000 to uppercase roman numeral.
toRomanNumeral :: LuaInteger -> Lua String
toRomanNumeral = return . Shared.toRomanNumeral . fromIntegral
