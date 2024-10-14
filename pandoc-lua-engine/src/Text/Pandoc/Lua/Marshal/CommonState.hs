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
import Text.Pandoc.Class (CommonState (..))
import Text.Pandoc.Lua.Marshal.List (pushPandocList)
import Text.Pandoc.Lua.Marshal.LogMessage (pushLogMessage)

-- | Lua type used for the @CommonState@ object.
typeCommonState :: LuaError e => DocumentedType e CommonState
typeCommonState = deftype "CommonState" []
  [ readonly "input_files" "input files passed to pandoc"
      (pushPandocList pushString, stInputFiles)

  , readonly "output_file" "the file to which pandoc will write"
      (maybe pushnil pushString, stOutputFile)

  , readonly "log" "list of log messages"
      (pushPandocList pushLogMessage, stLog)

  , readonly "request_headers" "headers to add for HTTP requests"
      (pushPandocList (pushPair pushText pushText), stRequestHeaders)

  , readonly "resource_path"
      "path to search for resources like included images"
      (pushPandocList pushString, stResourcePath)

  , readonly "source_url" "absolute URL + dir of 1st source file"
      (maybe pushnil pushText, stSourceURL)

  , readonly "user_data_dir" "directory to search for data files"
      (maybe pushnil pushString, stUserDataDir)

  , readonly "trace" "controls whether tracing messages are issued"
      (pushBool, stTrace)

  , readonly "verbosity" "verbosity level"
      (pushString . show, stVerbosity)
  ]

peekCommonState :: LuaError e => Peeker e CommonState
peekCommonState = peekUD typeCommonState

pushCommonState :: LuaError e => Pusher e CommonState
pushCommonState = pushUD typeCommonState
