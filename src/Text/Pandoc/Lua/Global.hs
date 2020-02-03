{-# LANGUAGE DeriveDataTypeable #-}
{- |
   Module      : Text.Pandoc.Lua
   Copyright   : Copyright © 2017-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc's Lua globals.
-}
module Text.Pandoc.Lua.Global
  ( Global (..)
  , setGlobals
  ) where

import Data.Data (Data)
import Foreign.Lua (Lua, Peekable, Pushable)
import Foreign.Lua.Userdata ( ensureUserdataMetatable, pushAnyWithMetatable
                            , metatableName)
import Paths_pandoc (version)
import Text.Pandoc.Class.CommonState (CommonState)
import Text.Pandoc.Definition (Pandoc (Pandoc), pandocTypesVersion)
import Text.Pandoc.Lua.Marshaling ()
import Text.Pandoc.Lua.Util (addFunction)
import Text.Pandoc.Options (ReaderOptions)

import qualified Data.Text as Text
import qualified Foreign.Lua as Lua

-- | Permissible global Lua variables.
data Global =
    FORMAT Text.Text
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
    Lua.push pandocTypesVersion
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
    Lua.push version
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
