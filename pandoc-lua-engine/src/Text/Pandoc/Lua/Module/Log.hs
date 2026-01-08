{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{- |
   Module      : Text.Pandoc.Lua.Module.Log
   Copyright   : © 2024 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>

Logging module.
-}
module Text.Pandoc.Lua.Module.Log
  ( documentedModule
  ) where

import Data.Version (makeVersion)
import HsLua
import Text.Pandoc.Class (report, runSilently)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Logging (LogMessage (ScriptingInfo, ScriptingWarning))
import Text.Pandoc.Lua.Marshal.List (pushPandocList)
import Text.Pandoc.Lua.Marshal.LogMessage (pushLogMessage)
import Text.Pandoc.Lua.PandocLua (liftPandocLua, unPandocLua)
import Text.Pandoc.Lua.SourcePos (luaSourcePos)
import qualified Data.Text as T
import qualified HsLua.Core.Utf8 as UTF8

-- | Push the pandoc.log module on the Lua stack.
documentedModule :: Module PandocError
documentedModule = defmodule "pandoc.log"
  `withDescription`
      "Access to pandoc's logging system."
  `withFields` []
  `withFunctions`
      [ defun "info"
        ### (\msg -> do
                -- reporting levels:
                -- 0: this function,
                -- 1: userdata wrapper function for the function,
                -- 2: function calling warn.
                pos <- luaSourcePos 2
                unPandocLua $ report $ ScriptingInfo (UTF8.toText msg) pos)
        <#> parameter peekByteString "string" "message" "the info message"
        =#> []
        #? "Reports a ScriptingInfo message to pandoc's logging system."
        `since` makeVersion [3, 2]

      , defun "silence"
        ### const silence
        <#> parameter pure "function" "fn"
              "function to be silenced"
        =?> ("List of log messages triggered during the function call, " <>
             "and any value returned by the function.")
        #? T.unlines
           [ "Applies the function to the given arguments while"
           , "preventing log messages from being added to the log."
           , "The warnings and info messages reported during the function"
           , "call are returned as the first return value, with the"
           , "results of the function call following thereafter."
           ]
        `since` makeVersion [3, 2]

      , defun "warn"
        ### (\msg -> do
                -- reporting levels:
                -- 0: this function,
                -- 1: userdata wrapper function for the function,
                -- 2: function calling warn.
                pos <- luaSourcePos 2
                unPandocLua $ report $ ScriptingWarning (UTF8.toText msg) pos)
        <#> parameter peekByteString "string" "message"
              "the warning message"
        =#> []
        #? T.unlines
           [ "Reports a ScriptingWarning to pandoc's logging system."
           , "The warning will be printed to stderr unless logging"
           , "verbosity has been set to *ERROR*."
           ]
        `since` makeVersion [3, 2]
      ]

-- | Calls the function given as the first argument, but suppresses logging.
-- Returns the list of generated log messages as the first result, and the other
-- results of the function call after that.
silence :: LuaE PandocError NumResults
silence = unPandocLua $ do
  -- call function given as the first argument
  ((), messages) <- runSilently . liftPandocLua $ do
    nargs <- (NumArgs . subtract 1 . fromStackIndex) <$> gettop
    call @PandocError nargs multret

  liftPandocLua $ do
    pushPandocList pushLogMessage messages
    insert 1
    (NumResults . fromStackIndex) <$> gettop
