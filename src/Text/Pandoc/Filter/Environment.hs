{- |
   Module      : Text.Pandoc.Filter.Environment
   Copyright   : ©2020-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
   Stability   : alpha
   Portability : portable

Environment for pandoc filters.
-}
module Text.Pandoc.Filter.Environment
  ( Environment (..)
  ) where

import Data.Default (Default (def))
import Text.Pandoc.Options (ReaderOptions, WriterOptions)

-- | Environment in which a filter is run. This includes reader and
-- writer options.
data Environment = Environment
  { envReaderOptions :: ReaderOptions
  , envWriterOptions :: WriterOptions
  }

instance Default Environment where
  def = Environment def def
