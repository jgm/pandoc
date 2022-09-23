{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{- |
   Module      : Text.Pandoc.Lua
   Copyright   : Copyright © 2017-2022 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Running pandoc Lua filters.
-}
module Text.Pandoc.Lua
  ( -- * High-level functions
    applyFilter
  , readCustom
  , writeCustom
  -- * Low-level functions
  , Global(..)
  , setGlobals
  , runLua
  ) where

import Text.Pandoc.Lua.Filter (applyFilter)
import Text.Pandoc.Lua.Global (Global (..), setGlobals)
import Text.Pandoc.Lua.Init (runLua)
import Text.Pandoc.Lua.Reader (readCustom)
import Text.Pandoc.Lua.Writer (writeCustom)
import Text.Pandoc.Lua.Orphans ()
