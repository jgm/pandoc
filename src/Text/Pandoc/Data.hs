{-# LANGUAGE TemplateHaskell #-}

module Text.Pandoc.Data (dataFiles) where

import Data.FileEmbed
import qualified Data.ByteString as B
import System.FilePath (splitDirectories)
import qualified System.FilePath.Posix as Posix

-- We ensure that the data files are stored using Posix
-- path separators (/), even on Windows.
dataFiles :: [(FilePath, B.ByteString)]
dataFiles = map (\(fp, contents) ->
  (Posix.joinPath (splitDirectories fp), contents)) dataFiles'

dataFiles' :: [(FilePath, B.ByteString)]
dataFiles' = ("MANUAL.txt", $(embedFile "MANUAL.txt")) : $(embedDir "data")
