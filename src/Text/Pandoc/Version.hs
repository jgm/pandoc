{-# LANGUAGE CPP                   #-}
{- |
   Module      : Text.Pandoc.Version
   Copyright   : Copyright (C) 2022-2023 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Version information.
-}
module Text.Pandoc.Version (
                     pandocVersion,
                     pandocVersionText
                    ) where

import Data.Version (Version, showVersion)
import Paths_pandoc (version)
import qualified Data.Text as T

-- | Version number of pandoc library.
pandocVersion :: Version
pandocVersion = version

-- | Text representation of the library's version number.
pandocVersionText :: T.Text
pandocVersionText = T.pack $ showVersion version
