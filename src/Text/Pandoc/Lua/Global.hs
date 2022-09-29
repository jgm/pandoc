{-# LANGUAGE OverloadedStrings  #-}
{- |
   Module      : Text.Pandoc.Lua
   Copyright   : Copyright © 2017-2022 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc's Lua globals.
-}
module Text.Pandoc.Lua.Global
  ( Global (..)
  , setGlobals
  ) where

import HsLua as Lua
import HsLua.Module.Version (pushVersion)
import Text.Pandoc.Class (CommonState)
import Text.Pandoc.Definition (Pandoc, pandocTypesVersion)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.Marshal.CommonState (pushCommonState)
import Text.Pandoc.Lua.Marshal.Pandoc (pushPandoc)
import Text.Pandoc.Lua.Marshal.ReaderOptions (pushReaderOptionsReadonly)
import Text.Pandoc.Lua.Marshal.WriterOptions (pushWriterOptions)
import Text.Pandoc.Lua.Orphans ()
import Text.Pandoc.Options (ReaderOptions, WriterOptions)
import Text.Pandoc.Shared (pandocVersion)

import qualified Data.Text as Text

-- | Permissible global Lua variables.
data Global =
    FORMAT Text.Text
  | PANDOC_API_VERSION
  | PANDOC_DOCUMENT Pandoc
  | PANDOC_READER_OPTIONS ReaderOptions
  | PANDOC_WRITER_OPTIONS WriterOptions
  | PANDOC_SCRIPT_FILE FilePath
  | PANDOC_STATE CommonState
  | PANDOC_VERSION
  -- Cannot derive instance of Data because of CommonState

-- | Set all given globals.
setGlobals :: [Global] -> LuaE PandocError ()
setGlobals = mapM_ setGlobal

setGlobal :: Global -> LuaE PandocError ()
setGlobal global = case global of
  -- This could be simplified if Global was an instance of Data.
  FORMAT format -> do
    Lua.pushText format
    Lua.setglobal "FORMAT"
  PANDOC_API_VERSION -> do
    pushVersion pandocTypesVersion
    Lua.setglobal "PANDOC_API_VERSION"
  PANDOC_DOCUMENT doc -> do
    pushPandoc doc
    Lua.setglobal "PANDOC_DOCUMENT"
  PANDOC_READER_OPTIONS ropts -> do
    pushReaderOptionsReadonly ropts
    Lua.setglobal "PANDOC_READER_OPTIONS"
  PANDOC_WRITER_OPTIONS wopts -> do
    pushWriterOptions wopts
    Lua.setglobal "PANDOC_WRITER_OPTIONS"
  PANDOC_SCRIPT_FILE filePath -> do
    Lua.pushString filePath
    Lua.setglobal "PANDOC_SCRIPT_FILE"
  PANDOC_STATE commonState -> do
    pushCommonState commonState
    Lua.setglobal "PANDOC_STATE"
  PANDOC_VERSION              -> do
    pushVersion pandocVersion
    Lua.setglobal "PANDOC_VERSION"
