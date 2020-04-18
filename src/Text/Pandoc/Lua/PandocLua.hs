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
   Copyright   : Copyright © 2020 Albert Krewinkel
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
  , addFunction
  , loadScriptFromDataDir
  ) where

import Control.Monad (when)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (MonadError (catchError, throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Foreign.Lua (Lua (..), NumResults, Pushable, ToHaskellFunction)
import Text.Pandoc.Class.PandocIO (PandocIO)
import Text.Pandoc.Class.PandocMonad (PandocMonad (..), readDataFile)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.Global (Global (..), setGlobals)
import Text.Pandoc.Lua.ErrorConversion (errorConversion)

import qualified Control.Monad.Catch as Catch
import qualified Foreign.Lua as Lua
import qualified Text.Pandoc.Class.IO as IO
import qualified Text.Pandoc.Lua.Util as LuaUtil

-- | Type providing access to both, pandoc and Lua operations.
newtype PandocLua a = PandocLua { unPandocLua :: Lua a }
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
liftPandocLua :: Lua a -> PandocLua a
liftPandocLua = PandocLua

-- | Evaluate a @'PandocLua'@ computation, running all contained Lua
-- operations..
runPandocLua :: PandocLua a -> PandocIO a
runPandocLua pLua = do
  origState <- getCommonState
  globals <- defaultGlobals
  (result, newState) <- liftIO . Lua.run' errorConversion . unPandocLua $ do
    putCommonState origState
    liftPandocLua $ setGlobals globals
    r <- pLua
    c <- getCommonState
    return (r, c)
  putCommonState newState
  return result

instance {-# OVERLAPPING #-} ToHaskellFunction (PandocLua NumResults) where
  toHsFun _narg = unPandocLua

instance Pushable a => ToHaskellFunction (PandocLua a) where
  toHsFun _narg x = 1 <$ (unPandocLua x >>= Lua.push)

-- | Add a function to the table at the top of the stack, using the given name.
addFunction :: ToHaskellFunction a => String -> a -> PandocLua ()
addFunction name fn = liftPandocLua $ do
  Lua.push name
  Lua.pushHaskellFunction fn
  Lua.rawset (-3)

-- | Load a file from pandoc's data directory.
loadScriptFromDataDir :: FilePath -> PandocLua ()
loadScriptFromDataDir scriptFile = do
  script <- readDataFile scriptFile
  status <- liftPandocLua $ Lua.dostring script
  when (status /= Lua.OK) . liftPandocLua $
    LuaUtil.throwTopMessageAsError'
      (("Couldn't load '" ++ scriptFile ++ "'.\n") ++)

-- | Global variables which should always be set.
defaultGlobals :: PandocIO [Global]
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

  glob = IO.glob
  fileExists = IO.fileExists
  getDataFileName = IO.getDataFileName
  getModificationTime = IO.getModificationTime

  getCommonState = PandocLua $ do
    Lua.getglobal "PANDOC_STATE"
    Lua.peek Lua.stackTop
  putCommonState = PandocLua . setGlobals . (:[]) . PANDOC_STATE

  logOutput = IO.logOutput
