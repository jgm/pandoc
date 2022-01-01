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

import Control.Exception (throw)
import Control.Monad ((>=>))
import qualified Data.Text as T
import Text.Pandoc.Class (PandocMonad)
import Control.Monad.Trans (MonadIO)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Error (PandocError (PandocFilterError, PandocLuaError))
import Text.Pandoc.Filter.Environment (Environment (..))
import Text.Pandoc.Lua (Global (..), runLua, runFilterFile, setGlobals)

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
  runLua >=> forceResult fp $ do
    setGlobals [ FORMAT $ T.pack format
               , PANDOC_READER_OPTIONS (envReaderOptions fenv)
               , PANDOC_WRITER_OPTIONS (envWriterOptions fenv)
               , PANDOC_SCRIPT_FILE fp
               ]
    runFilterFile fp doc

forceResult :: (PandocMonad m, MonadIO m)
            => FilePath -> Either PandocError Pandoc -> m Pandoc
forceResult fp eitherResult = case eitherResult of
  Right x  -> return x
  Left err -> throw . PandocFilterError (T.pack fp) $ case err of
    PandocLuaError msg -> msg
    _                  -> T.pack $ show err
