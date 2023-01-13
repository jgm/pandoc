{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TupleSections       #-}
{- |
   Module      : Text.Pandoc.Lua.Custom
   Copyright   : © 2021-2023 Albert Krewinkel, John MacFarlane
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>

Supports custom parsers written in Lua which produce a Pandoc AST.
-}
module Text.Pandoc.Lua.Custom ( loadCustom ) where
import Control.Exception
import Control.Monad ((<=<), (<$!>))
import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (fromMaybe)
import HsLua as Lua hiding (Operation (Div))
import HsLua.Core.Run (GCManagedState, newGCManagedState, withGCManagedState)
import Text.Pandoc.Class (PandocMonad, findFileWithDataFallback)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.Global (Global (..), setGlobals)
import Text.Pandoc.Lua.Init (runLuaWith)
import Text.Pandoc.Lua.Marshal.Format (peekExtensionsConfig)
import Text.Pandoc.Lua.Marshal.Pandoc (peekPandoc)
import Text.Pandoc.Lua.Marshal.WriterOptions (pushWriterOptions)
import Text.Pandoc.Readers (Reader (..))
import Text.Pandoc.Sources (ToSources(..))
import Text.Pandoc.Scripting (CustomComponents (..))
import Text.Pandoc.Writers (Writer (..))
import qualified Text.Pandoc.Lua.Writer.Classic as Classic

-- | Convert custom markup to Pandoc.
loadCustom :: (PandocMonad m, MonadIO m)
           => FilePath -> m (CustomComponents m)
loadCustom luaFile = do
  luaState <- liftIO newGCManagedState
  luaFile' <- fromMaybe luaFile <$>
              findFileWithDataFallback "custom"  luaFile
  either throw pure <=< runLuaWith luaState $ do
    let globals = [ PANDOC_SCRIPT_FILE luaFile ]
    setGlobals globals
    dofileTrace luaFile' >>= \case
      OK -> pure ()
      _  -> throwErrorAsException

    mextsConf <- rawgetglobal "Extensions" >>= \case
      TypeNil      -> pure Nothing
      TypeFunction -> Just <$!> do
        callTrace 0 1
        forcePeek $ peekExtensionsConfig top `lastly` pop 1
      _            -> Just <$!> do
        forcePeek $ peekExtensionsConfig top `lastly` pop 1

    mtemplate <- rawgetglobal "Template" >>= \case
      TypeNil   -> pure Nothing
      TypeFunction -> Just <$!> do
        callTrace 0 1
        forcePeek $ peekText top `lastly` pop 1
      _ -> Just <$!> do
        forcePeek $ peekText top `lastly` pop 1

    mreader <- rawgetglobal "Reader" >>= \case
      TypeNil -> do
        pop 1
        rawgetglobal "ByteStringReader" >>= \case
          TypeNil -> pure Nothing
          _ -> do
            setfield registryindex readerField
            pure . Just $ byteStringReader luaState
      _ -> do
        setfield registryindex readerField
        pure . Just $ textReader luaState

    mwriter <- rawgetglobal "Writer" >>= \case
      TypeNil -> rawgetglobal "ByteStringWriter" >>= \case
        TypeNil -> do
          -- Neither `Writer` nor `BinaryWriter` are defined. Check for
          -- "Doc"; if present, use the file as a classic writer.
          docType <- rawgetglobal "Doc"
          pop 3  -- remove nils/value of "Writer", "ByteStringWriter", "Doc"
          pure $
            if docType /= TypeFunction
            then Nothing
            else Just . TextWriter $ \opts doc ->
              liftIO $ withGCManagedState luaState $
              Classic.runCustom @PandocError opts doc
        _ -> Just <$!> do
          -- Binary writer. Writer function is on top of the stack.
          setfield registryindex writerField
          pure $ ByteStringWriter $ \opts doc ->
            -- Call writer with document and writer options as arguments.
            liftIO $ withGCManagedState luaState $ do
              getfield registryindex writerField
              push doc
              pushWriterOptions opts
              callTrace 2 1
              forcePeek @PandocError $ peekLazyByteString top
      _ -> Just <$!> do
        -- New-type text writer. Writer function is on top of the stack.
        setfield registryindex writerField
        pure $ TextWriter $ \opts doc ->
          liftIO $ withGCManagedState luaState $ do
            getfield registryindex writerField
            push doc
            pushWriterOptions opts
            callTrace 2 1
            forcePeek @PandocError $ peekText top

    pure $ CustomComponents
      { customReader = mreader
      , customWriter = mwriter
      , customTemplate = mtemplate
      , customExtensions = mextsConf
      }

-- | "Raw", non-metatable lookup of a key in the global table.
--
-- Most classic writers contain code that throws an error if a global
-- is not present. This would break our check for the existence of a
-- "Writer" function. We resort to raw access for that reason, but
-- could also catch the error instead.
--
-- TODO: This function ensures the proper behavior of legacy custom
-- writers. It should be replaced with 'getglobal' in the future.
rawgetglobal :: LuaError e => Name -> LuaE e Lua.Type
rawgetglobal x = do
  pushglobaltable
  pushName x
  rawget (nth 2) <* remove (nth 2) -- remove global table

-- | Name under which the reader function is stored in the registry.
readerField :: Name
readerField = "Pandoc Reader function"

-- | Name under which the writer function is stored in the registry.
writerField :: Name
writerField = "Pandoc Writer function"

-- | Runs a Lua action in a continueable environment.
inLua :: MonadIO m => GCManagedState -> LuaE PandocError a -> m a
inLua st = liftIO . withGCManagedState @PandocError st

-- | Returns the ByteStringReader function
byteStringReader :: MonadIO m => GCManagedState -> Reader m
byteStringReader st = ByteStringReader $ \ropts input -> inLua st $ do
  getfield registryindex readerField
  push input
  push ropts
  pcallTrace 2 1 >>= \case
    OK -> forcePeek $ peekPandoc top
    _ -> throwErrorAsException

-- | Returns the TextReader function
textReader :: MonadIO m => GCManagedState -> Reader m
textReader st = TextReader $ \ropts srcs -> inLua st $ do
  let input = toSources srcs
  getfield registryindex readerField
  push input
  push ropts
  pcallTrace 2 1 >>= \case
    OK -> forcePeek $ peekPandoc top
    _ -> throwErrorAsException
