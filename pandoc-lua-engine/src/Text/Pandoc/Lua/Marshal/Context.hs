{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Text.Pandoc.Lua.Marshaling.Context
   Copyright   : © 2012-2022 John MacFarlane
                 © 2017-2022 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Marshaling instance for doctemplates Context and its components.
-}
module Text.Pandoc.Lua.Marshal.Context () where

import qualified HsLua as Lua
import HsLua (Pushable)
import Text.DocTemplates (Context(..), Val(..), TemplateTarget)
import Text.DocLayout (render)

instance (TemplateTarget a, Pushable a) => Pushable (Context a) where
  push (Context m) = Lua.push m

instance (TemplateTarget a, Pushable a) => Pushable (Val a) where
  push NullVal = Lua.push ()
  push (BoolVal b) = Lua.push b
  push (MapVal ctx) = Lua.push ctx
  push (ListVal xs) = Lua.push xs
  push (SimpleVal d) = Lua.push $ render Nothing d
