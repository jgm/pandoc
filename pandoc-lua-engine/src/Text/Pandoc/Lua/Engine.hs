{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{- |
   Module      : Text.Pandoc.Lua.Engine
   Copyright   : Copyright © 2017-2024 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>

Running pandoc Lua filters.
-}
module Text.Pandoc.Lua.Engine
  ( getEngine
  , applyFilter
  ) where

import Control.Exception (throw)
import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import HsLua.Core (getglobal, openlibs, run, top, tostring)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Filter (Environment (..))
import Text.Pandoc.Error (PandocError (PandocFilterError, PandocLuaError))
import Text.Pandoc.Lua.Custom (loadCustom)
import Text.Pandoc.Lua.Filter (runFilterFile)
import Text.Pandoc.Lua.Global (Global (..), setGlobals)
import Text.Pandoc.Lua.Run (runLua)
import Text.Pandoc.Lua.Orphans ()
import Text.Pandoc.Scripting (ScriptingEngine (..))
import qualified Text.Pandoc.UTF8 as UTF8
import qualified Data.Text as T

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
    , engineLoadCustom = loadCustom
    }

-- | Run the Lua filter in @filterPath@ for a transformation to the
-- target format (first element in args). Pandoc uses Lua init files to
-- setup the Lua interpreter.
applyFilter :: (PandocMonad m, MonadIO m)
            => Environment
            -> [String]
            -> FilePath
            -> Pandoc
            -> m Pandoc
applyFilter fenv args fp doc = do
  let globals = [ FORMAT $ case args of
                    x:_ -> T.pack x
                    _   -> ""
                , PANDOC_READER_OPTIONS (envReaderOptions fenv)
                , PANDOC_WRITER_OPTIONS (envWriterOptions fenv)
                , PANDOC_SCRIPT_FILE fp
                ]
  runLua >=> forceResult fp $ do
    setGlobals globals
    runFilterFile fp doc

forceResult :: (PandocMonad m, MonadIO m)
            => FilePath -> Either PandocError Pandoc -> m Pandoc
forceResult fp eitherResult = case eitherResult of
  Right x  -> return x
  Left err -> throw . PandocFilterError (T.pack fp) $ case err of
    PandocLuaError msg -> msg
    _                  -> T.pack $ show err
