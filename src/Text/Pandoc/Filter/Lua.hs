{- |
   Module      : Text.Pandoc.Filter.Lua
   Copyright   : Copyright (C) 2006-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Apply Lua filters to modify a pandoc documents programmatically.
-}
module Text.Pandoc.Filter.Lua (apply) where

import qualified Data.Text as T
import Text.Pandoc.Class (PandocMonad)
import Control.Monad.Trans (MonadIO)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Filter.Environment (Environment (..))
import Text.Pandoc.Lua (Global (..), applyFilter)

-- | Run the Lua filter in @filterPath@ for a transformation to the
-- target format (first element in args). Pandoc uses Lua init files to
-- setup the Lua interpreter.
apply :: (PandocMonad m, MonadIO m)
      => Environment
      -> [String]
      -> FilePath
      -> Pandoc
      -> m Pandoc
apply fenv args fp doc = do
  let format = case args of
                 (x:_) -> x
                 _     -> error "Format not supplied for Lua filter"
  applyFilter [ FORMAT $ T.pack format
              , PANDOC_READER_OPTIONS (envReaderOptions fenv)
              , PANDOC_WRITER_OPTIONS (envWriterOptions fenv)
              , PANDOC_SCRIPT_FILE fp
              ] fp doc
