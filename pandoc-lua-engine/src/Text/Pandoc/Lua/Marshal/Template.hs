{-# LANGUAGE LambdaCase           #-}
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
import HsLua.Core.Utf8 as Lua
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.PandocLua (unPandocLua)
import Text.Pandoc.Templates (Template, compileTemplate, runWithDefaultPartials)

-- | Pushes a 'Template' as a an opaque userdata value.
pushTemplate :: LuaError e => Pusher e (Template Text)
pushTemplate = pushUD typeTemplate

-- | Retrieves a 'Template' 'Text' value from the stack.
peekTemplate :: Peeker PandocError (Template Text)
peekTemplate idx = liftLua (ltype idx) >>= \case
  TypeString -> do
    let path = "templates/default.custom"
    let liftPM = liftLua . unPandocLua
    tmpl <- peekText idx
    (liftPM $ runWithDefaultPartials (compileTemplate path tmpl)) >>= \case
      Left e  -> failPeek (Lua.fromString e)
      Right t -> pure t
  _ -> peekUD typeTemplate idx

-- | Template object type.
typeTemplate :: LuaError e => DocumentedType e (Template Text)
typeTemplate = deftype "pandoc Template" [] []
