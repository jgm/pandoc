{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{- |
   Module      : Text.Pandoc.Lua
   Copyright   : Copyright © 2017-2024 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>
   Stability   : alpha

Pandoc's Lua globals.
-}
module Text.Pandoc.Lua.Global
  ( Global (..)
  , setGlobals
  ) where

import HsLua as Lua
import HsLua.Module.Version (pushVersion)
import Text.Pandoc.Class ( getInputFiles, getOutputFile, getLog
                         , getRequestHeaders, getResourcePath, getSourceURL
                         , getUserDataDir, getTrace, getVerbosity
                         )
import Text.Pandoc.Definition (Pandoc, pandocTypesVersion)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.Marshal.List (pushPandocList)
import Text.Pandoc.Lua.Marshal.LogMessage (pushLogMessage)
import Text.Pandoc.Lua.Marshal.Pandoc (pushPandoc)
import Text.Pandoc.Lua.Marshal.ReaderOptions (pushReaderOptionsReadonly)
import Text.Pandoc.Lua.Marshal.WriterOptions (pushWriterOptions)
import Text.Pandoc.Lua.PandocLua (unPandocLua)
import Text.Pandoc.Options (ReaderOptions, WriterOptions)
import Text.Pandoc.Version (pandocVersion)

import qualified Data.Text as Text

-- | Permissible global Lua variables.
data Global =
    FORMAT Text.Text
  | PANDOC_API_VERSION
  | PANDOC_DOCUMENT Pandoc
  | PANDOC_READER_OPTIONS ReaderOptions
  | PANDOC_WRITER_OPTIONS WriterOptions
  | PANDOC_SCRIPT_FILE FilePath
  | PANDOC_STATE
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
  PANDOC_STATE -> do
    -- The common state is an opaque value. We provide a table that
    -- contains the values accessible through the PandocMonad API. This
    -- is for backwards compatibility, as the state used to be exposed
    -- as a read-only object.
    Lua.newtable
    Lua.newmetatable "CommonStateInterface"
    Lua.pushHaskellFunction $ do
      Lua.forcePeek (peekText (Lua.nthBottom 2)) >>= \case
        "input_files" -> do
          pushPandocList pushString =<< unPandocLua getInputFiles
          return 1
        "output_file" -> do
          maybe pushnil pushString =<< unPandocLua getOutputFile
          return 1
        "log" -> do
          pushPandocList pushLogMessage =<< unPandocLua getLog
          return 1
        "request_headers" -> do
          pushPandocList (pushPair pushText pushText)
                   =<< unPandocLua getRequestHeaders
          return 1
        "resource_path" -> do
          pushPandocList pushString =<< unPandocLua getResourcePath
          return 1
        "source_url" -> do
          maybe pushnil pushText =<< unPandocLua getSourceURL
          return 1
        "user_data_dir" -> do
          maybe pushnil pushString =<< unPandocLua getUserDataDir
          return 1
        "trace" -> do
          pushBool =<< unPandocLua getTrace
          return 1
        "verbosity" -> do
          pushString . show =<< unPandocLua getVerbosity
          return 1
        _ ->
          failLua "Unknown key"
    Lua.setfield (Lua.nth 2) "__index"
    Lua.setmetatable (Lua.nth 2)
    Lua.setglobal "PANDOC_STATE"
  PANDOC_VERSION              -> do
    pushVersion pandocVersion
    Lua.setglobal "PANDOC_VERSION"
