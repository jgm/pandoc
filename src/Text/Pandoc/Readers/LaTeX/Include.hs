{-# LANGUAGE OverloadedStrings     #-}
{- |
   Module      : Text.Pandoc.Readers.LaTeX.Include
   Copyright   : Copyright (C) 2006-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable
-}
module Text.Pandoc.Readers.LaTeX.Include
  ( readFileFromTexinputs
  , insertIncluded
  )
where

import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Shared (splitTextBy)
import System.FilePath (takeExtension, addExtension)
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Text.Pandoc.Error (PandocError(PandocParseError))
import Text.Pandoc.Logging (LogMessage(CouldNotLoadIncludeFile))
import Text.Pandoc.Class (PandocMonad (..), readFileFromDirs, report)
import Text.Pandoc.Readers.LaTeX.Parsing
import Text.Pandoc.Parsing (updateState, getState, getInput, setInput,
                            getPosition, addIncludeFile, getIncludeFiles,
                            dropLatestIncludeFile)
import Data.Maybe (fromMaybe)

readFileFromTexinputs :: PandocMonad m => FilePath -> LP m (Maybe Text)
readFileFromTexinputs fp = do
  fileContentsMap <- sFileContents <$> getState
  case M.lookup (T.pack fp) fileContentsMap of
    Just t -> return (Just t)
    Nothing -> do
      dirs <- map T.unpack . splitTextBy (==':') . fromMaybe "."
               <$> lookupEnv "TEXINPUTS"
      readFileFromDirs dirs fp

insertIncluded :: PandocMonad m
               => FilePath
               -> FilePath
               -> LP m ()
insertIncluded defaultExtension f' = do
  let f = case takeExtension f' of
                ".tex" -> f'
                ".sty" -> f'
                _      -> addExtension f' defaultExtension
  pos <- getPosition
  containers <- getIncludeFiles <$> getState
  when (T.pack f `elem` containers) $
    throwError $ PandocParseError $ T.pack $ "Include file loop at " ++ show pos
  updateState $ addIncludeFile $ T.pack f
  mbcontents <- readFileFromTexinputs f
  contents <- case mbcontents of
                   Just s -> return s
                   Nothing -> do
                     report $ CouldNotLoadIncludeFile (T.pack f) pos
                     return ""
  getInput >>= setInput . (tokenize f contents ++)
  updateState dropLatestIncludeFile


