{-# LANGUAGE DeriveDataTypeable #-}
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
   Module      : Text.Pandoc.Lua
   Copyright   : Copyright © 2017-2018 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc's Lua globals.
-}
module Text.Pandoc.Lua.Global
  ( Global (..)
  , setGlobals
  ) where

import Prelude
import Data.Data (Data)
import Data.Version (Version (versionBranch))
import Foreign.Lua (Lua, Peekable, Pushable)
import Foreign.Lua.Userdata ( ensureUserdataMetatable, pushAnyWithMetatable
                            , metatableName)
import Paths_pandoc (version)
import Text.Pandoc.Class (CommonState)
import Text.Pandoc.Definition (Pandoc (Pandoc), pandocTypesVersion)
import Text.Pandoc.Lua.StackInstances ()
import Text.Pandoc.Lua.Util (addFunction)
import Text.Pandoc.Options (ReaderOptions)

import qualified Foreign.Lua as Lua

-- | Permissible global Lua variables.
data Global =
    FORMAT String
  | PANDOC_API_VERSION
  | PANDOC_DOCUMENT Pandoc
  | PANDOC_READER_OPTIONS ReaderOptions
  | PANDOC_SCRIPT_FILE FilePath
  | PANDOC_STATE CommonState
  | PANDOC_VERSION
  -- Cannot derive instance of Data because of CommonState

-- | Set all given globals.
setGlobals :: [Global] -> Lua ()
setGlobals = mapM_ setGlobal

setGlobal :: Global -> Lua ()
setGlobal global = case global of
  -- This could be simplified if Global was an instance of Data.
  FORMAT format -> do
    Lua.push format
    Lua.setglobal "FORMAT"
  PANDOC_API_VERSION -> do
    Lua.push (versionBranch pandocTypesVersion)
    Lua.setglobal "PANDOC_API_VERSION"
  PANDOC_DOCUMENT doc -> do
    Lua.push (LazyPandoc doc)
    Lua.setglobal "PANDOC_DOCUMENT"
  PANDOC_READER_OPTIONS ropts -> do
    Lua.push ropts
    Lua.setglobal "PANDOC_READER_OPTIONS"
  PANDOC_SCRIPT_FILE filePath -> do
    Lua.push filePath
    Lua.setglobal "PANDOC_SCRIPT_FILE"
  PANDOC_STATE commonState -> do
    Lua.push commonState
    Lua.setglobal "PANDOC_STATE"
  PANDOC_VERSION              -> do
    Lua.push (versionBranch version)
    Lua.setglobal "PANDOC_VERSION"

-- | Readonly and lazy pandoc objects.
newtype LazyPandoc = LazyPandoc Pandoc
  deriving (Data)

instance Pushable LazyPandoc where
  push lazyDoc = pushAnyWithMetatable pushPandocMetatable lazyDoc
   where
    pushPandocMetatable = ensureUserdataMetatable (metatableName lazyDoc) $
                          addFunction "__index" indexLazyPandoc

instance Peekable LazyPandoc where
  peek = Lua.peekAny

indexLazyPandoc :: LazyPandoc -> String -> Lua Lua.NumResults
indexLazyPandoc (LazyPandoc (Pandoc meta blks)) field = 1 <$
  case field of
    "blocks" -> Lua.push blks
    "meta"   -> Lua.push meta
    _        -> Lua.pushnil
