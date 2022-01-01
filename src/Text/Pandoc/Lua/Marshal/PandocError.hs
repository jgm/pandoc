{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{- |
   Module      : Text.Pandoc.Lua.Marshal.PandocError
   Copyright   : © 2020-2022 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Marshal of @'PandocError'@ values.
-}
module Text.Pandoc.Lua.Marshal.PandocError
  ( peekPandocError
  , pushPandocError
  , typePandocError
  )
  where

import HsLua.Core (LuaError)
import HsLua.Marshalling (Peeker, Pusher, pushString, liftLua)
import HsLua.Packaging
import Text.Pandoc.Error (PandocError (PandocLuaError))

import qualified HsLua as Lua
import qualified Text.Pandoc.UTF8 as UTF8

-- | Lua userdata type definition for PandocError.
typePandocError :: LuaError e => DocumentedType e PandocError
typePandocError = deftype "PandocError"
  [ operation Tostring $ defun "__tostring"
    ### liftPure (show @PandocError)
    <#> udparam typePandocError "obj" "PandocError object"
    =#> functionResult pushString "string" "string representation of error."
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
      msg <- liftLua $ Lua.state >>= \l -> Lua.liftIO (Lua.popErrorMessage l)
      return $ PandocLuaError (UTF8.toText msg)
