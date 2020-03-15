{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Text.Pandoc.Lua.Marshaling.Context
   Copyright   : © 2012-2020 John MacFarlane
                 © 2017-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Marshaling instance for doctemplates Context and its components.
-}
module Text.Pandoc.Lua.Marshaling.Context () where

import qualified Foreign.Lua as Lua
import Foreign.Lua (Pushable)
import Text.DocTemplates (Context(..), Val(..), TemplateTarget)
import Text.DocLayout (render)

instance (TemplateTarget a, Pushable a) => Pushable (Context a) where
  push (Context m) = Lua.push m

instance (TemplateTarget a, Pushable a) => Pushable (Val a) where
  push NullVal = Lua.push ()
  push (MapVal ctx) = Lua.push ctx
  push (ListVal xs) = Lua.push xs
  push (SimpleVal d) = Lua.push $ render Nothing d
