{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{- |
   Module      : Text.Pandoc.Lua.Marshal.PandocError
   Copyright   : © 2020-2024 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>
   Stability   : alpha

Marshal of @'PandocError'@ values.
-}
module Text.Pandoc.Lua.Marshal.PandocError
  ( peekPandocError
  , pushPandocError
  , typePandocError
  )
  where

import HsLua (LuaError, Peeker, Pusher, liftLua, pushText)
import HsLua.Packaging
import Text.Pandoc.Error (PandocError (PandocLuaError), renderError)

import qualified HsLua as Lua
import qualified HsLua.Core.Utf8 as UTF8

-- | Lua userdata type definition for PandocError.
typePandocError :: LuaError e => DocumentedType e PandocError
typePandocError = deftype "PandocError"
  [ operation Tostring $ defun "__tostring"
    ### liftPure (\case
                     PandocLuaError e -> e
                     err              -> renderError err)
    <#> udparam typePandocError "obj" "PandocError object"
    =#> functionResult pushText "string" "string representation of error."
  ]
  mempty -- no members

-- | Peek a @'PandocError'@ element to the Lua stack.
pushPandocError :: LuaError e => Pusher e PandocError
pushPandocError = pushUD typePandocError

-- | Retrieve a @'PandocError'@ from the Lua stack.
peekPandocError :: LuaError e => Peeker e PandocError
peekPandocError idx = Lua.retrieving "PandocError" $
  liftLua (Lua.ltype idx) >>= \case
    Lua.TypeUserdata -> peekUD typePandocError idx
    _ -> do
      msg <- liftLua $ do
        Lua.pushvalue idx
        Lua.state >>= \l -> Lua.liftIO (Lua.popErrorMessage l)
      return $ PandocLuaError (UTF8.toText msg)
