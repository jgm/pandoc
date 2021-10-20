{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{- |
   Module      : Text.Pandoc.Lua.Marshaling.SimpleTable
   Copyright   : © 2020-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Definition and marshaling of the 'SimpleTable' data type used as a
convenience type when dealing with tables.
-}
module Text.Pandoc.Lua.Marshaling.SimpleTable
  ( SimpleTable (..)
  , peekSimpleTable
  , pushSimpleTable
  )
  where

import Control.Monad ((<$!>))
import HsLua as Lua
import Text.Pandoc.Definition
import Text.Pandoc.Lua.Util (pushViaConstructor)
import Text.Pandoc.Lua.Marshaling.AST

-- | A simple (legacy-style) table.
data SimpleTable = SimpleTable
  { simpleTableCaption :: [Inline]
  , simpleTableAlignments :: [Alignment]
  , simpleTableColumnWidths :: [Double]
  , simpleTableHeader :: [[Block]]
  , simpleTableBody :: [[[Block]]]
  }

-- | Push a simple table to the stack by calling the
-- @pandoc.SimpleTable@ constructor.
pushSimpleTable :: forall e. LuaError e => SimpleTable -> LuaE e ()
pushSimpleTable tbl = pushViaConstructor @e "SimpleTable"
  (simpleTableCaption tbl)
  (simpleTableAlignments tbl)
  (simpleTableColumnWidths tbl)
  (simpleTableHeader tbl)
  (simpleTableBody tbl)

-- | Retrieve a simple table from the stack.
peekSimpleTable :: forall e. LuaError e => Peeker e SimpleTable
peekSimpleTable idx = retrieving "SimpleTable" $ SimpleTable
  <$!> peekFieldRaw peekInlines "caption" idx
  <*>  peekFieldRaw (peekList peekRead) "aligns" idx
  <*>  peekFieldRaw (peekList peekRealFloat) "widths" idx
  <*>  peekFieldRaw (peekList peekBlocks) "headers" idx
  <*>  peekFieldRaw (peekList (peekList peekBlocks)) "rows" idx
