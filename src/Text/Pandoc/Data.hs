{-# LANGUAGE CPP #-}
#ifdef EMBED_DATA_FILES
{-# LANGUAGE TemplateHaskell #-}
#endif
{- |
Module      : Text.Pandoc.Data
Copyright   : Copyright (C) 2013-2020 John MacFarlane
License     : GNU GPL, version 2 or above

Maintainer  : John MacFarlane <jgm@berkeley@edu>
Stability   : alpha
Portability : portable

Provide contents data files as Haskell values.
-}
module Text.Pandoc.Data (initializeDataFiles) where

import Text.Pandoc.Class (modifyCommonState, CommonState(..), PandocMonad)
#ifdef EMBED_DATA_FILES
import qualified Data.ByteString as B
import Data.FileEmbed
import System.FilePath (splitDirectories)
import qualified System.FilePath.Posix as Posix
#endif

-- | Inject data from data files into CommonState, so that
-- we don't need to read data files from file system. This
-- only has an effect if pandoc was compiled with
-- the @embed_data_files@ flag.  Generally you'll want to
-- put this at the beginning of any PandocMonad action
-- that will require reading pandoc's data files (e.g. templates).
initializeDataFiles :: PandocMonad m => m ()
initializeDataFiles =
#ifdef EMBED_DATA_FILES
  modifyCommonState $ \st ->st{ stDataFiles = dataFiles }
#else
  return ()
#endif

#ifdef EMBED_DATA_FILES
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
#endif
