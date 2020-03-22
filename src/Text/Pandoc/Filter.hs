{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Filter
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Programmatically modifications of pandoc documents.
-}
module Text.Pandoc.Filter
  ( Filter (..)
  , applyFilters
  ) where

import System.CPUTime (getCPUTime)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import GHC.Generics (Generic)
import Text.Pandoc.Class.PandocIO (PandocIO)
import Text.Pandoc.Class.PandocMonad (report, getVerbosity)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Logging
import qualified Text.Pandoc.Filter.JSON as JSONFilter
import qualified Text.Pandoc.Filter.Lua as LuaFilter
import qualified Text.Pandoc.Filter.Path as Path
import Data.YAML
import qualified Data.Text as T
import System.FilePath (takeExtension)
import Control.Applicative ((<|>))
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Monad (foldM, when)

-- | Type of filter and path to filter file.
data Filter = LuaFilter FilePath
            | JSONFilter FilePath
            deriving (Show, Generic)

instance FromYAML Filter where
 parseYAML node =
  (withMap "Filter" $ \m -> do
    ty <- m .: "type"
    fp <- m .: "path"
    case ty of
      "lua"  -> return $ LuaFilter $ T.unpack fp
      "json" -> return $ JSONFilter $ T.unpack fp
      _      -> fail $ "Unknown filter type " ++ show (ty :: T.Text)) node
  <|>
  (withStr "Filter" $ \t -> do
    let fp = T.unpack t
    case takeExtension fp of
      ".lua"  -> return $ LuaFilter fp
      _       -> return $ JSONFilter fp) node

-- | Modify the given document using a filter.
applyFilters :: ReaderOptions
             -> [Filter]
             -> [String]
             -> Pandoc
             -> PandocIO Pandoc
applyFilters ropts filters args d = do
  expandedFilters <- mapM expandFilterPath filters
  foldM applyFilter d expandedFilters
 where
  applyFilter doc (JSONFilter f) =
    withMessages f $ JSONFilter.apply ropts args f doc
  applyFilter doc (LuaFilter f)  =
    withMessages f $ LuaFilter.apply ropts args f doc
  withMessages f action = do
    verbosity <- getVerbosity
    when (verbosity == INFO) $ report $ RunningFilter f
    starttime <- liftIO getCPUTime
    res <- action
    endtime <- liftIO getCPUTime
    when (verbosity == INFO) $ report $ FilterCompleted f $ toMilliseconds $ endtime - starttime
    return res
  toMilliseconds picoseconds = picoseconds `div` 1000000000

-- | Expand paths of filters, searching the data directory.
expandFilterPath :: Filter -> PandocIO Filter
expandFilterPath (LuaFilter fp) = LuaFilter <$> Path.expandFilterPath fp
expandFilterPath (JSONFilter fp) = JSONFilter <$> Path.expandFilterPath fp

$(deriveJSON defaultOptions ''Filter)
