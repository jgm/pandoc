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

import Foreign.Lua (Lua, Peekable, Pushable, StackIndex)
import Text.Pandoc.Definition
import Text.Pandoc.Lua.Util (defineHowTo, pushViaConstructor, rawField)
import Text.Pandoc.Lua.Marshaling.AST ()

import qualified Foreign.Lua as Lua

-- | A simple (legacy-style) table.
data SimpleTable = SimpleTable
  { simpleTableCaption :: [Inline]
  , simpleTableAlignments :: [Alignment]
  , simpleTableColumnWidths :: [Double]
  , simpleTableHeader :: [[Block]]
  , simpleTableBody :: [[[Block]]]
  }

instance Pushable SimpleTable where
  push = pushSimpleTable

instance Peekable SimpleTable where
  peek = peekSimpleTable

-- | Push a simple table to the stack by calling the
-- @pandoc.SimpleTable@ constructor.
pushSimpleTable :: SimpleTable -> Lua ()
pushSimpleTable tbl = pushViaConstructor "SimpleTable"
  (simpleTableCaption tbl)
  (simpleTableAlignments tbl)
  (simpleTableColumnWidths tbl)
  (simpleTableHeader tbl)
  (simpleTableBody tbl)

-- | Retrieve a simple table from the stack.
peekSimpleTable :: StackIndex -> Lua SimpleTable
peekSimpleTable idx = defineHowTo "get SimpleTable" $
  SimpleTable
    <$> rawField idx "caption"
    <*> rawField idx "aligns"
    <*> rawField idx "widths"
    <*> rawField idx "headers"
    <*> rawField idx "rows"
