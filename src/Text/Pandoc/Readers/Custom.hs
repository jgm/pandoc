{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{- |
   Module      : Text.Pandoc.Readers.Custom
   Copyright   : Copyright (C) 2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Supports custom parsers written in Lua which produce a Pandoc AST.
-}
module Text.Pandoc.Readers.Custom ( readCustom ) where
import Control.Exception
import Control.Monad (when)
import Data.Text (Text)
import HsLua as Lua hiding (Operation (Div), render)
import HsLua.Class.Peekable (PeekError)
import Control.Monad.IO.Class (MonadIO)
import Text.Pandoc.Definition
import Text.Pandoc.Lua (Global (..), runLua, setGlobals)
import Text.Pandoc.Lua.Util (dofileWithTraceback)
import Text.Pandoc.Options
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Sources (ToSources(..), sourcesToText)

-- | Convert custom markup to Pandoc.
readCustom :: (PandocMonad m, MonadIO m, ToSources s)
            => FilePath -> ReaderOptions -> s -> m Pandoc
readCustom luaFile opts sources = do
  let input = sourcesToText $ toSources sources
  let globals = [ PANDOC_SCRIPT_FILE luaFile ]
  res <- runLua $ do
    setGlobals globals
    stat <- dofileWithTraceback luaFile
    -- check for error in lua script (later we'll change the return type
    -- to handle this more gracefully):
    when (stat /= Lua.OK)
      Lua.throwErrorAsException
    parseCustom input opts
  case res of
    Left msg -> throw msg
    Right doc -> return doc

parseCustom :: forall e. PeekError e
            => Text
            -> ReaderOptions
            -> LuaE e Pandoc
parseCustom = invoke @e "Reader"

