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
  , runLuaNoEnv
  -- * Engine
  , getEngine
  ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import HsLua.Core (getglobal, openlibs, run, top, tostring)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.Filter (applyFilter)
import Text.Pandoc.Lua.Global (Global (..), setGlobals)
import Text.Pandoc.Lua.Init (runLua, runLuaNoEnv)
import Text.Pandoc.Lua.Reader (readCustom)
import Text.Pandoc.Lua.Writer (writeCustom)
import Text.Pandoc.Lua.Orphans ()
import Text.Pandoc.Scripting (ScriptingEngine (..))
import qualified Text.Pandoc.UTF8 as UTF8

-- | Constructs the Lua scripting engine.
getEngine :: MonadIO m => m ScriptingEngine
getEngine = do
  versionName <- liftIO . run @PandocError $ do
    openlibs
    getglobal "_VERSION"
    tostring top
  pure $ ScriptingEngine
    { engineName = maybe "Lua (unknown version)" UTF8.toText versionName
    , engineApplyFilter = applyFilter
    , engineReadCustom = readCustom
    , engineWriteCustom = writeCustom
    }
