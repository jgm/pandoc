{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
Module      : Text.Pandoc.Lua.Marshal.Template
Copyright   : © 2021-2022 Albert Krewinkel
License     : GNU GPL, version 2 or above
Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshal 'Template' 'Text'.
-}
module Text.Pandoc.Lua.Marshal.Template
  ( pushTemplate
  , peekTemplate
  , typeTemplate
  ) where

import Data.Text (Text)
import HsLua as Lua
import Text.DocTemplates (Template)

-- | Pushes a 'Template' as a an opaque userdata value.
pushTemplate :: LuaError e => Pusher e (Template Text)
pushTemplate = pushUD typeTemplate

-- | Retrieves a 'Template' 'Text' value from the stack.
peekTemplate :: LuaError e => Peeker e (Template Text)
peekTemplate = peekUD typeTemplate

-- | Template object type.
typeTemplate :: LuaError e => DocumentedType e (Template Text)
typeTemplate = deftype "pandoc Template" [] []
