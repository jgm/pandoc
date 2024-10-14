{-# LANGUAGE OverloadedStrings    #-}
{- |
   Module      : Text.Pandoc.Lua.Marshal.LogMessage
   Copyright   : © 2017-2024 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>

Pushing and retrieving of pandoc log messages.
-}
module Text.Pandoc.Lua.Marshal.LogMessage
  ( peekLogMessage
  , pushLogMessage
  , typeLogMessage
  ) where

import HsLua
import Text.Pandoc.Logging (LogMessage, showLogMessage)
import qualified Data.Aeson as Aeson

-- | Type definition for pandoc log messages.
typeLogMessage :: LuaError e => DocumentedType e LogMessage
typeLogMessage = deftype "LogMessage"
  [ operation Index $ defun "__tostring"
      ### liftPure showLogMessage
      <#> udparam typeLogMessage "msg" "object"
      =#> functionResult pushText "string" "stringified log message"
  , operation (CustomOperation "__tojson") $ lambda
      ### liftPure Aeson.encode
      <#> udparam typeLogMessage "msg" "object"
      =#> functionResult pushLazyByteString "string" "JSON encoded object"
  ]
  mempty -- no members

-- | Pushes a LogMessage to the stack.
pushLogMessage :: LuaError e => Pusher e LogMessage
pushLogMessage = pushUD typeLogMessage

peekLogMessage :: LuaError e => Peeker e LogMessage
peekLogMessage  = peekUD typeLogMessage
