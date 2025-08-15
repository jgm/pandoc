{- |
   Module      : Text.Pandoc.Lua
   Copyright   : Copyright © 2017-2024 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>
   Stability   : alpha

Running pandoc Lua filters.
-}
module Text.Pandoc.Lua
  ( -- * High-level functions
    applyFilter
  , loadCustom
  -- * Low-level functions
  , Global(..)
  , setGlobals
  , runLua
  , runLuaNoEnv
  -- * Engine
  , getEngine
  ) where

import Text.Pandoc.Lua.Custom (loadCustom)
import Text.Pandoc.Lua.Engine (getEngine, applyFilter)
import Text.Pandoc.Lua.Global (Global (..), setGlobals)
import Text.Pandoc.Lua.Run (runLua, runLuaNoEnv)
import Text.Pandoc.Lua.Orphans ()
