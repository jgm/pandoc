{-# LANGUAGE TemplateHaskell #-}
{- |
Module      : Text.Pandoc.Data
Copyright   : Copyright (C) 2013-2020 John MacFarlane
License     : GNU GPL, version 2 or above

Maintainer  : John MacFarlane <jgm@berkeley@edu>
Stability   : alpha
Portability : portable

Provide contents data files as Haskell values.
-}
module Text.Pandoc.Data (dataFiles) where

import qualified Data.ByteString as B
import Data.FileEmbed
import System.FilePath (splitDirectories)
import qualified System.FilePath.Posix as Posix

-- We ensure that the data files are stored using Posix
-- path separators (/), even on Windows.
dataFiles :: [(FilePath, B.ByteString)]
dataFiles = map (\(fp, contents) ->
  (Posix.joinPath (splitDirectories fp), contents)) dataFiles'

dataFiles' :: [(FilePath, B.ByteString)]
dataFiles' = ("MANUAL.txt", $(embedFile "MANUAL.txt")) :
             -- handle the hidden file separately, since embedDir doesn't
             -- include it:
             ("docx/_rels/.rels", $(embedFile "data/docx/_rels/.rels")) :
             ("pptx/_rels/.rels", $(embedFile "data/pptx/_rels/.rels")) :
             $(embedDir "data")
