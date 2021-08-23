{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Module      : Text.Pandoc.Class.PandocSandboxed
Copyright   : Copyright (C) 2021 John MacFarlane
License     : GNU GPL, version 2 or above

Maintainer  : John MacFarlane <jgm@berkeley.edu>
Stability   : alpha
Portability : portable

This module defines @'PandocSandboxed'@, an IO-based instance of the
@'PandocMonad'@ type class. File, data, and network access all are run
using IO operators, but only a whitelisted set of resources can be
accessed.
-}
module Text.Pandoc.Class.PandocSandboxed
  ( PandocSandboxed(..)
  , runSandboxed
 ) where

import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (StateT, evalStateT, lift, get, put)
import Data.Default (Default (def))
import Text.Pandoc.Class.CommonState (CommonState (..))
import Text.Pandoc.Class.PandocMonad
import Text.Pandoc.Definition
import Text.Pandoc.Error
import qualified Text.Pandoc.Class.IO as IO
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)

-- | Evaluate a 'PandocSandboxed' operation.
runSandboxed :: PandocSandboxed a -> IO (Either PandocError a)
runSandboxed ma = flip evalStateT def $ runExceptT $ unPandocSandboxed ma

newtype PandocSandboxed a = PandocSandboxed {
  unPandocSandboxed :: ExceptT PandocError (StateT CommonState IO) a
  } deriving ( MonadIO
             , Functor
             , Applicative
             , Monad
             , MonadCatch
             , MonadMask
             , MonadThrow
             , MonadError PandocError
             )

instance PandocMonad PandocSandboxed where
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

  getCommonState = PandocSandboxed $ lift get
  putCommonState = PandocSandboxed . lift . put

  logOutput = IO.logOutput

