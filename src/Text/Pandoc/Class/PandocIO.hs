{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Module      : Text.Pandoc.Class.PandocIO
Copyright   : Copyright (C) 2016-2020 Jesse Rosenthal, John MacFarlane
License     : GNU GPL, version 2 or above

Maintainer  : Jesse Rosenthal <jrosenthal@jhu.edu>
Stability   : alpha
Portability : portable

This module defines @'PandocIO'@, an IO-based instance of the
@'PandocMonad'@ type class. File, data, and network access all are run
using IO operators.
-}
module Text.Pandoc.Class.PandocIO
  ( PandocIO(..)
  , runIO
  , runIOorExplode
  , extractMedia
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

-- | Evaluate a 'PandocIO' operation.
runIO :: PandocIO a -> IO (Either PandocError a)
runIO ma = flip evalStateT def $ runExceptT $ unPandocIO ma

-- | Evaluate a 'PandocIO' operation, handling any errors
-- by exiting with an appropriate message and error status.
runIOorExplode :: PandocIO a -> IO a
runIOorExplode ma = runIO ma >>= handleError

newtype PandocIO a = PandocIO {
  unPandocIO :: ExceptT PandocError (StateT CommonState IO) a
  } deriving ( MonadIO
             , Functor
             , Applicative
             , Monad
             , MonadError PandocError
             )

instance PandocMonad PandocIO where
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

  getCommonState = PandocIO $ lift get
  putCommonState = PandocIO . lift . put

  logOutput = IO.logOutput

-- | Extract media from the mediabag into a directory.
extractMedia :: FilePath -> Pandoc -> PandocIO Pandoc
extractMedia = IO.extractMedia
