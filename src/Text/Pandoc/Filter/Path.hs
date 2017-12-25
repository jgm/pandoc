{-
Copyright (C) 2006-2017 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Filter.Path
   Copyright   : Copyright (C) 2006-2017 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Expand paths of filters, searching the data directory.
-}
module Text.Pandoc.Filter.Path
  ( expandFilterPath
  ) where

import Control.Monad.Trans (MonadIO (liftIO))
import System.FilePath ((</>), isRelative)
import System.Directory (doesFileExist)

  -- First we check to see if a filter is found.  If not, and if it's
  -- not an absolute path, we check to see whether it's in `userdir/filters`.
  -- If not, we leave it unchanged.
expandFilterPath :: MonadIO m => Maybe FilePath -> FilePath -> m FilePath
expandFilterPath mbDatadir fp = liftIO $ do
  fpExists <- doesFileExist fp
  if fpExists
     then return fp
     else case mbDatadir of
               Just datadir | isRelative fp -> do
                 let filterPath = datadir </> "filters" </> fp
                 filterPathExists <- doesFileExist filterPath
                 if filterPathExists
                    then return filterPath
                    else return fp
               _ -> return fp
