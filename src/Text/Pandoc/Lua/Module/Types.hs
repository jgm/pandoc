{- |
   Module      : Text.Pandoc.Lua.Module.Types
   Copyright   : © 2019 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc data type constructors.
-}
module Text.Pandoc.Lua.Module.Types
  ( pushModule
  ) where

import Prelude
import Data.Version (Version)
import Foreign.Lua (Lua, NumResults)
import Text.Pandoc.Lua.Marshaling.Version ()
import Text.Pandoc.Lua.Util (addFunction)

import qualified Foreign.Lua as Lua

-- | Push the pandoc.system module on the Lua stack.
pushModule :: Lua NumResults
pushModule = do
  Lua.newtable
  addFunction "Version" (return :: Version -> Lua Version)
  return 1
