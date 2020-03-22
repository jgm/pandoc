{- |
   Module      : Text.Pandoc.Filter.Path
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Expand paths of filters, searching the data directory.
-}
module Text.Pandoc.Filter.Path
  ( expandFilterPath
  ) where

import Text.Pandoc.Class.PandocMonad (PandocMonad, fileExists, getUserDataDir)
import System.FilePath ((</>), isRelative)

  -- First we check to see if a filter is found.  If not, and if it's
  -- not an absolute path, we check to see whether it's in `userdir/filters`.
  -- If not, we leave it unchanged.
expandFilterPath :: PandocMonad m => FilePath -> m FilePath
expandFilterPath fp = do
  mbDatadir <- getUserDataDir
  fpExists <- fileExists fp
  if fpExists
     then return fp
     else case mbDatadir of
               Just datadir | isRelative fp -> do
                 let filterPath = datadir </> "filters" </> fp
                 filterPathExists <- fileExists filterPath
                 if filterPathExists
                    then return filterPath
                    else return fp
               _ -> return fp
