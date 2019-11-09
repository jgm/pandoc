{-# LANGUAGE NoImplicitPrelude #-}
{- |
   Module      : Text.Pandoc.Filter.Lua
   Copyright   : Copyright (C) 2006-2019 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley@edu>
   Stability   : alpha
   Portability : portable

Apply Lua filters to modify a pandoc documents programmatically.
-}
module Text.Pandoc.Filter.Lua (apply) where

import Prelude
import Control.Exception (throw)
import Control.Monad ((>=>))
import qualified Data.Text as T
import Text.Pandoc.Class (PandocIO)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Error (PandocError (PandocFilterError))
import Text.Pandoc.Lua (Global (..), LuaException (..),
                        runLua, runFilterFile, setGlobals)
import Text.Pandoc.Options (ReaderOptions)

-- | Run the Lua filter in @filterPath@ for a transformation to the
-- target format (first element in args). Pandoc uses Lua init files to
-- setup the Lua interpreter.
apply :: ReaderOptions
      -> [String]
      -> FilePath
      -> Pandoc
      -> PandocIO Pandoc
apply ropts args fp doc = do
  let format = case args of
                 (x:_) -> x
                 _     -> error "Format not supplied for Lua filter"
  runLua >=> forceResult fp $ do
    setGlobals [ FORMAT $ T.pack format
               , PANDOC_READER_OPTIONS ropts
               , PANDOC_SCRIPT_FILE fp
               ]
    runFilterFile fp doc

forceResult :: FilePath -> Either LuaException Pandoc -> PandocIO Pandoc
forceResult fp eitherResult = case eitherResult of
  Right x               -> return x
  Left (LuaException s) -> throw (PandocFilterError (T.pack fp) s)
