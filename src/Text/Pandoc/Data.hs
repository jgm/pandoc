{-# LANGUAGE CPP #-}
#ifdef EMBED_DATA_FILES
{-# LANGUAGE TemplateHaskell #-}
#endif
module Text.Pandoc.Data (dataFiles) where

import System.FilePath (FilePath, splitDirectories)
import qualified Data.ByteString as B
import qualified System.FilePath.Posix as Posix
#ifdef EMBED_DATA_FILES
import Data.FileEmbed
#endif

-- We ensure that the data files are stored using Posix
-- path separators (/), even on Windows.
dataFiles :: [(FilePath, B.ByteString)]
dataFiles = map (\(fp, contents) ->
  (Posix.joinPath (splitDirectories fp), contents)) dataFiles'

dataFiles' :: [(FilePath, B.ByteString)]
#ifdef EMBED_DATA_FILES
dataFiles' = ("MANUAL.txt", $(embedFile "MANUAL.txt")) : $(embedDir "data")
#else
dataFiles' = error "dataFiles is only defined when embed_data_files flag set"
#endif
