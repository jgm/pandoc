{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{- |
   Module      : Text.Pandoc.Lua.Writer
   Copyright   : Copyright (C) 2012-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of Pandoc documents using a custom Lua writer.
-}
module Text.Pandoc.Lua.Writer
  ( writeCustom
  ) where

import Control.Exception
import Control.Monad ((<=<))
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import HsLua
import HsLua.Core.Run (newGCManagedState, withGCManagedState)
import Control.Monad.IO.Class (MonadIO)
import Text.Pandoc.Class (PandocMonad, findFileWithDataFallback)
import Text.Pandoc.Error (PandocError (..))
import Text.Pandoc.Format (ExtensionsConfig (..))
import Text.Pandoc.Lua.Global (Global (..), setGlobals)
import Text.Pandoc.Lua.Init (runLuaWith)
import Text.Pandoc.Lua.Marshal.Format (peekExtensionsConfig)
import Text.Pandoc.Lua.Marshal.Template (peekTemplate)
import Text.Pandoc.Templates (Template)
import Text.Pandoc.Writers (Writer (..))
import qualified Text.Pandoc.Lua.Writer.Classic as Classic

-- | Convert Pandoc to custom markup.
writeCustom :: (PandocMonad m, MonadIO m)
            => FilePath -> m (Writer m, ExtensionsConfig, m (Template Text))
writeCustom luaFile = do
  luaState <- liftIO newGCManagedState
  luaFile' <- fromMaybe luaFile <$> findFileWithDataFallback "writers" luaFile
  either throw pure <=< runLuaWith luaState $ do
    setGlobals [ PANDOC_DOCUMENT mempty
               , PANDOC_SCRIPT_FILE luaFile'
               , PANDOC_WRITER_OPTIONS def
               ]
    dofileTrace luaFile' >>= \case
      OK -> pure ()
      _  -> throwErrorAsException
    -- Most classic writers contain code that throws an error if a global
    -- is not present. This would break our check for the existence of a
    -- "Writer" function. We resort to raw access for that reason, but
    -- could also catch the error instead.
    let rawgetglobal x = do
          pushglobaltable
          pushName x
          rawget (nth 2) <* remove (nth 2) -- remove global table

    let writerField = "Pandoc Writer function"

    extsConf <- rawgetglobal "Extensions" >>= \case
      TypeNil   -> ExtensionsConfig mempty mempty <$ pop 1
      _         -> forcePeek $ peekExtensionsConfig top `lastly` pop 1

    -- Store template function in registry
    let templateField = "Pandoc Writer Template"
    rawgetglobal "Template" *> setfield registryindex templateField

    let getTemplate = liftIO $ withGCManagedState @PandocError luaState $ do
          getfield registryindex templateField >>= \case
            TypeNil   -> failLua $ "No default template for writer; " <>
                         "the global variable Template is undefined."
            _ -> do
              callTrace 0 1
              forcePeek $ peekTemplate top `lastly` pop 1

    let addProperties = (, extsConf, getTemplate)

    rawgetglobal "Writer" >>= \case
      TypeNil -> rawgetglobal "ByteStringWriter" >>= \case
        TypeNil -> do
          -- Neither `Writer` nor `BinaryWriter` are defined. Try to
          -- use the file as a classic writer.
          pop 1  -- remove nil
          pure $ addProperties . TextWriter $ \opts doc ->
            liftIO $ withGCManagedState luaState $ do
              Classic.runCustom @PandocError opts doc
        _ -> do
          -- Binary writer. Writer function is on top of the stack.
          setfield registryindex writerField
          pure $ addProperties . ByteStringWriter $ \opts doc ->
            -- Call writer with document and writer options as arguments.
            liftIO $ withGCManagedState luaState $ do
              getfield registryindex writerField
              push doc
              push opts
              callTrace 2 1
              forcePeek @PandocError $ peekLazyByteString top
      _ -> do
        -- New-type text writer. Writer function is on top of the stack.
        setfield registryindex writerField
        pure $ addProperties . TextWriter $ \opts doc ->
          liftIO $ withGCManagedState luaState $ do
            getfield registryindex writerField
            push doc
            push opts
            callTrace 2 1
            forcePeek @PandocError $ peekText top
