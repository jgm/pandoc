{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{- |
Module      : Text.Pandoc.Lua.Filter
Copyright   : © 2012-2022 John MacFarlane,
              © 2017-2022 Albert Krewinkel
License     : GNU GPL, version 2 or above
Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
Stability   : alpha

Types and functions for running Lua filters.
-}
module Text.Pandoc.Lua.Filter
  ( runFilterFile
  ) where
import Control.Monad ((>=>), (<$!>))
import HsLua as Lua
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.ErrorConversion ()
import Text.Pandoc.Lua.Marshal.AST
import Text.Pandoc.Lua.Marshal.Filter


-- | Transform document using the filter defined in the given file.
runFilterFile :: FilePath -> Pandoc -> LuaE PandocError Pandoc
runFilterFile filterPath doc = do
  oldtop <- gettop
  stat <- dofileTrace filterPath
  if stat /= Lua.OK
    then throwErrorAsException
    else do
      newtop <- gettop
      -- Use the returned filters, or the implicitly defined global
      -- filter if nothing was returned.
      luaFilters <- forcePeek $
        if newtop - oldtop >= 1
        then peekList peekFilter top
        else (:[]) <$!> (liftLua pushglobaltable *> peekFilter top)
      settop oldtop
      runAll luaFilters doc

runAll :: [Filter] -> Pandoc -> LuaE PandocError Pandoc
runAll = foldr ((>=>) . applyFully) return
