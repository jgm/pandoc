{- |
Module      : Text.Pandoc.Class.Sandbox
Copyright   : Copyright (C) 2021-2022 John MacFarlane
License     : GNU GPL, version 2 or above

Maintainer  : John MacFarlane (<jgm@berkeley.edu>)
Stability   : alpha
Portability : portable

This module provides a way to run PandocMonad actions in a sandbox
(pure context, with no IO allowed and access only to designated files).
-}

module Text.Pandoc.Class.Sandbox
  ( sandbox )
where

import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Text.Pandoc.Class.PandocMonad
import Text.Pandoc.Class.PandocPure
import Text.Pandoc.Class.CommonState (CommonState(..))
import Text.Pandoc.Logging (messageVerbosity)

-- | Lift a PandocPure action into any instance of PandocMonad.
-- The main computation is done purely, but CommonState is preserved
-- continuously, and warnings are emitted after the action completes.
-- The parameter is a list of FilePaths which will be added to the
-- ersatz file system and be available for reading.
sandbox :: (PandocMonad m, MonadIO m) => [FilePath] -> PandocPure a -> m a
sandbox files action = do
  oldState <- getCommonState
  tree <- liftIO $ foldM addToFileTree mempty files
  case runPure (do putCommonState oldState
                   modifyPureState $ \ps -> ps{ stFiles = tree }
                   result <- action
                   st <- getCommonState
                   return (st, result)) of
          Left e -> throwError e
          Right (st, result) -> do
            putCommonState st
            let verbosity = stVerbosity st
            -- emit warnings, since these are not printed in runPure
            let newMessages = reverse $ take
                  (length (stLog st) - length (stLog oldState)) (stLog st)
            mapM_ logOutput
              (filter ((<= verbosity) . messageVerbosity) newMessages)
            return result
