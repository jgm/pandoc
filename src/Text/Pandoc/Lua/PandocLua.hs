{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Text.Pandoc.Lua.PandocLua
   Copyright   : Copyright © 2020-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

PandocMonad instance which allows execution of Lua operations and which
uses Lua to handle state.
-}
module Text.Pandoc.Lua.PandocLua
  ( PandocLua (..)
  , runPandocLua
  , liftPandocLua
  , loadDefaultModule
  ) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.IO.Class (MonadIO)
import HsLua as Lua
import Text.Pandoc.Class.PandocMonad (PandocMonad (..), readDefaultDataFile)
import Text.Pandoc.Error (PandocError (PandocLuaError))
import Text.Pandoc.Lua.Global (Global (..), setGlobals)
import Text.Pandoc.Lua.Marshaling.CommonState (peekCommonState)

import qualified Control.Monad.Catch as Catch
import qualified Data.Text as T
import qualified Text.Pandoc.Class.IO as IO

-- | Type providing access to both, pandoc and Lua operations.
newtype PandocLua a = PandocLua { unPandocLua :: LuaE PandocError a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadCatch
    , MonadIO
    , MonadMask
    , MonadThrow
    )

-- | Lift a @'Lua'@ operation into the @'PandocLua'@ type.
liftPandocLua :: LuaE PandocError a -> PandocLua a
liftPandocLua = PandocLua

-- | Evaluate a @'PandocLua'@ computation, running all contained Lua
-- operations..
runPandocLua :: (PandocMonad m, MonadIO m) => PandocLua a -> m a
runPandocLua pLua = do
  origState <- getCommonState
  globals <- defaultGlobals
  (result, newState) <- liftIO . Lua.run . unPandocLua $ do
    putCommonState origState
    liftPandocLua $ setGlobals globals
    r <- pLua
    c <- getCommonState
    return (r, c)
  putCommonState newState
  return result

instance {-# OVERLAPPING #-} Exposable PandocError (PandocLua NumResults) where
  partialApply _narg = unPandocLua

instance Pushable a => Exposable PandocError (PandocLua a) where
  partialApply _narg x = 1 <$ (unPandocLua x >>= Lua.push)

-- | Load a pure Lua module included with pandoc. Leaves the result on
-- the stack and returns @NumResults 1@.
--
-- The script is loaded from the default data directory. We do not load
-- from data directories supplied via command line, as this could cause
-- scripts to be executed even though they had not been passed explicitly.
loadDefaultModule :: String -> PandocLua NumResults
loadDefaultModule name = do
  script <- readDefaultDataFile (name <> ".lua")
  result <- liftPandocLua $ Lua.dostring script
  if result == Lua.OK
    then return (1 :: NumResults)
    else do
      msg <- liftPandocLua Lua.popValue
      let err = "Error while loading `" <> name <> "`.\n" <> msg
      throwError $ PandocLuaError (T.pack err)

-- | Global variables which should always be set.
defaultGlobals :: PandocMonad m => m [Global]
defaultGlobals = do
  commonState <- getCommonState
  return
    [ PANDOC_API_VERSION
    , PANDOC_STATE commonState
    , PANDOC_VERSION
    ]

instance MonadError PandocError PandocLua where
  catchError = Catch.catch
  throwError = Catch.throwM

instance PandocMonad PandocLua where
  lookupEnv = IO.lookupEnv
  getCurrentTime = IO.getCurrentTime
  getCurrentTimeZone = IO.getCurrentTimeZone
  newStdGen = IO.newStdGen
  newUniqueHash = IO.newUniqueHash

  openURL = IO.openURL

  readFileLazy = IO.readFileLazy
  readFileStrict = IO.readFileStrict
  readStdinStrict = IO.readStdinStrict

  glob = IO.glob
  fileExists = IO.fileExists
  getDataFileName = IO.getDataFileName
  getModificationTime = IO.getModificationTime

  getCommonState = PandocLua $ do
    Lua.getglobal "PANDOC_STATE"
    forcePeek $ peekCommonState Lua.top
  putCommonState = PandocLua . setGlobals . (:[]) . PANDOC_STATE

  logOutput = IO.logOutput
