{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Filter
   Copyright   : Copyright (C) 2006-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Programmatically modifications of pandoc documents.
-}
module Text.Pandoc.Filter
  ( Filter (..)
  , Environment (..)
  , applyFilters
  ) where

import System.CPUTime (getCPUTime)
import Data.Aeson
import GHC.Generics (Generic)
import Text.Pandoc.Class (report, getVerbosity, PandocMonad)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Filter.Environment (Environment (..))
import Text.Pandoc.Logging
import Text.Pandoc.Citeproc (processCitations)
import qualified Text.Pandoc.Filter.JSON as JSONFilter
import qualified Text.Pandoc.Filter.Lua as LuaFilter
import qualified Text.Pandoc.Filter.Path as Path
import qualified Data.Text as T
import System.FilePath (takeExtension)
import Control.Applicative ((<|>))
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Monad (foldM, when)

-- | Type of filter and path to filter file.
data Filter = LuaFilter FilePath
            | JSONFilter FilePath
            | CiteprocFilter -- built-in citeproc
            deriving (Show, Generic)

instance FromJSON Filter where
 parseJSON node =
  (withObject "Filter" $ \m -> do
    ty <- m .: "type"
    fp <- m .:? "path"
    let missingPath = fail $ "Expected 'path' for filter of type " ++ show ty
    let filterWithPath constr = maybe missingPath (return . constr . T.unpack)
    case ty of
      "citeproc" -> return CiteprocFilter
      "lua"  -> filterWithPath LuaFilter fp
      "json" -> filterWithPath JSONFilter fp
      _      -> fail $ "Unknown filter type " ++ show (ty :: T.Text)) node
  <|>
  (withText "Filter" $ \t -> do
    let fp = T.unpack t
    if fp == "citeproc"
       then return CiteprocFilter
       else return $
         case takeExtension fp of
           ".lua"  -> LuaFilter fp
           _       -> JSONFilter fp) node

instance ToJSON Filter where
 toJSON CiteprocFilter = object [ "type" .= String "citeproc" ]
 toJSON (LuaFilter fp) = object [ "type" .= String "lua",
                                  "path" .= String (T.pack fp) ]
 toJSON (JSONFilter fp) = object [ "type" .= String "json",
                                   "path" .= String (T.pack fp) ]

-- | Modify the given document using a filter.
applyFilters :: (PandocMonad m, MonadIO m)
             => Environment
             -> [Filter]
             -> [String]
             -> Pandoc
             -> m Pandoc
applyFilters fenv filters args d = do
  expandedFilters <- mapM expandFilterPath filters
  foldM applyFilter d expandedFilters
 where
  applyFilter doc (JSONFilter f) =
    withMessages f $ JSONFilter.apply fenv args f doc
  applyFilter doc (LuaFilter f)  =
    withMessages f $ LuaFilter.apply fenv args f doc
  applyFilter doc CiteprocFilter =
    processCitations doc
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
expandFilterPath :: (PandocMonad m, MonadIO m) => Filter -> m Filter
expandFilterPath (LuaFilter fp) = LuaFilter <$> Path.expandFilterPath fp
expandFilterPath (JSONFilter fp) = JSONFilter <$> Path.expandFilterPath fp
expandFilterPath CiteprocFilter = return CiteprocFilter
