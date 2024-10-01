{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Module      : Text.Pandoc.Lua.Filter
Copyright   : © 2012-2024 John MacFarlane,
              © 2017-2024 Albert Krewinkel
License     : GPL-2.0-or-later
Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>

Types and functions for running Lua filters.
-}
module Text.Pandoc.Lua.Filter
  ( runFilterFile
  , runFilterFile'
  ) where
import Control.Monad ((>=>), (<$!>))
import HsLua as Lua
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.Marshal.AST
import Text.Pandoc.Lua.Marshal.Filter
import Text.Pandoc.Lua.PandocLua ()

-- | Transform document using the filter defined in the given file.
-- Runs the filter in the global environment.
runFilterFile :: FilePath -> Pandoc -> LuaE PandocError Pandoc
runFilterFile filterPath doc = do
  Lua.pushglobaltable
  runFilterFile' Lua.top filterPath doc <* Lua.pop 1

-- | Like 'runFilterFile', but uses the table at the given index as the
-- environment in which the filter is run.
runFilterFile' :: StackIndex -> FilePath -> Pandoc
               -> LuaE PandocError Pandoc
runFilterFile' envIdx filterPath doc = do
  oldtop <- gettop
  stat <- dofileTrace' envIdx (Just filterPath)
  if stat /= OK
    then throwErrorAsException
    else do
      newtop <- gettop
      -- Use the returned filters, or the implicitly defined global
      -- filter if nothing was returned.
      luaFilters <- forcePeek $
        if newtop - oldtop >= 1
        then liftLua (rawlen top) >>= \case
          -- explicitly returned filter, either a single one or a list
          0 -> (:[]) <$!> peekFilter top  -- single filter
          _ -> peekList peekFilter top    -- list of explicit filters
        else (:[]) <$!> peekFilter envIdx -- get the implicit filter in _ENV
      settop oldtop
      runAll luaFilters doc

-- | Apply Lua filters to a document
runAll :: [Filter] -> Pandoc -> LuaE PandocError Pandoc
runAll = foldr ((>=>) . applyFully) return

-- | Like 'HsLua.Core.Trace.dofileTrace', but uses a local environment.
dofileTrace' :: LuaError e
             => StackIndex     -- ^ stack index of the environment table
             -> Maybe FilePath -- ^ file to load (or @Nothing@ for stdin)
             -> LuaE e Status
dofileTrace' envIdx fp = do
  absEnv <- Lua.absindex envIdx
  loadfile fp >>= \case
    OK -> do
      Lua.pushvalue absEnv
      Just (Name "_ENV") <- Lua.setupvalue (Lua.nth 2) 1
      pcallTrace 0 multret
    s  -> pure s
