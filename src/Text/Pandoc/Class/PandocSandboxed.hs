{-# LANGUAGE OverloadedStrings #-}
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

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State (StateT, evalStateT, lift, get, put)
import Data.Default (Default (def))
import Text.Pandoc.Class.CommonState (CommonState (..))
import Text.Pandoc.Class.PandocMonad
import Text.Pandoc.Definition
import Text.Pandoc.Error
import qualified Text.Pandoc.Class.IO as IO
import Text.Pandoc.Class.PandocIO (PandocIO, runIO)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)

-- | Evaluate a 'PandocSandboxed' operation.
runSandboxed :: PandocSandboxed a -> IO (Either PandocError a)
runSandboxed = runIO . unPandocSandboxed

newtype PandocSandboxed a = PandocSandboxed {
  unPandocSandboxed :: PandocIO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadCatch
             , MonadMask
             , MonadThrow
             , MonadError PandocError
             )

instance MonadIO PandocSandboxed where
  liftIO act = PandocSandboxed $ throwError $
                 PandocSandboxError "IO action"

instance PandocMonad PandocSandboxed where
  lookupEnv = PandocSandboxed . lookupEnv
  getCurrentTime = PandocSandboxed getCurrentTime
  getCurrentTimeZone = PandocSandboxed getCurrentTimeZone
  newStdGen = PandocSandboxed newStdGen
  newUniqueHash = PandocSandboxed newUniqueHash

  openURL = PandocSandboxed . openURL
  readFileLazy = PandocSandboxed . readFileLazy
  readFileStrict = PandocSandboxed . readFileStrict
  readStdinStrict = PandocSandboxed readStdinStrict

  glob = PandocSandboxed . glob
  fileExists = PandocSandboxed . fileExists
  getDataFileName = PandocSandboxed . getDataFileName
  getModificationTime = PandocSandboxed . getModificationTime

  getCommonState = PandocSandboxed getCommonState
  putCommonState = PandocSandboxed . putCommonState

  logOutput = PandocSandboxed . logOutput

