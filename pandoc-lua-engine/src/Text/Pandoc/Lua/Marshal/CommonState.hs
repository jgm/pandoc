{-# LANGUAGE LambdaCase           #-}
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
  , peekCommonStateFromTable
  ) where

import Data.Default (def)
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

peekCommonStateFromTable :: LuaError e => Peeker e CommonState
peekCommonStateFromTable idx = do
  absidx <- liftLua $ absindex idx
  let setnext st = do
        liftLua (next absidx) >>= \case
          False -> pure st
          True -> do
            prop <- peekName (nth 2)
            case lookup prop setters of
              Just setter -> setnext =<< setter top st `lastly` pop 1
              Nothing -> failPeek ("Unknown field " <> fromName prop)
                         `lastly` pop 1
  liftLua pushnil
  setnext def

setters :: LuaError e
        => [ (Name, StackIndex -> CommonState -> Peek e CommonState)]
setters =
  [ ("input_files", mkS (peekList peekString) (\st x -> st{stInputFiles = x}))
  , ("output_file", mkS (peekNilOr peekString) (\st x -> st{stOutputFile = x}))
  , ("request_headers", mkS (peekList (peekPair peekText peekText))
                            (\st x -> st{ stRequestHeaders = x }))
  , ("user_data_dir", mkS (peekNilOr peekString) (\st x -> st{stUserDataDir = x}))
  , ("trace", mkS peekBool (\st x -> st{stTrace = x}))
  ]
  where
    mkS peekX setValue idx' st = setValue st <$> peekX idx'
