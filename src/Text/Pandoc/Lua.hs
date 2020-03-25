{- |
   Module      : Text.Pandoc.Lua
   Copyright   : Copyright © 2017–2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Running pandoc Lua filters.
-}
module Text.Pandoc.Lua
  ( runLua
  -- * Lua globals
  , Global (..)
  , setGlobals
  -- * Filters
  , runFilterFile
  ) where

import Text.Pandoc.Lua.Filter (runFilterFile)
import Text.Pandoc.Lua.Global (Global (..), setGlobals)
import Text.Pandoc.Lua.Init (runLua)
import Text.Pandoc.Lua.Marshaling ()
