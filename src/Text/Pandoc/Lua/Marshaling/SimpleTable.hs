{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{- |
   Module      : Text.Pandoc.Lua.Marshaling.SimpleTable
   Copyright   : © 2020-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above
   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Definition and marshaling of the 'SimpleTable' data type used as a
convenience type when dealing with tables.
-}
module Text.Pandoc.Lua.Marshaling.SimpleTable
  ( SimpleTable (..)
  , peekSimpleTable
  , pushSimpleTable
  , mkSimpleTable
  )
  where

import HsLua as Lua
import Text.Pandoc.Definition
import Text.Pandoc.Lua.Marshaling.AST
import Text.Pandoc.Lua.Marshaling.List

-- | A simple (legacy-style) table.
data SimpleTable = SimpleTable
  { simpleTableCaption :: [Inline]
  , simpleTableAlignments :: [Alignment]
  , simpleTableColumnWidths :: [Double]
  , simpleTableHeader :: [[Block]]
  , simpleTableBody :: [[[Block]]]
  } deriving (Eq, Show)

typeSimpleTable :: LuaError e => DocumentedType e SimpleTable
typeSimpleTable = deftype "SimpleTable"
  [ operation Eq $ lambda
    ### liftPure2 (==)
    <#> udparam typeSimpleTable "a" ""
    <#> udparam typeSimpleTable "b" ""
    =#> functionResult pushBool "boolean" "whether the two objects are equal"
  , operation Tostring $ lambda
    ### liftPure show
    <#> udparam typeSimpleTable "self" ""
    =#> functionResult pushString "string" "Haskell string representation"
  ]
  [ property "caption" "table caption"
      (pushPandocList pushInline, simpleTableCaption)
      (peekInlinesFuzzy, \t capt -> t {simpleTableCaption = capt})
  , property "aligns" "column alignments"
      (pushPandocList (pushString . show), simpleTableAlignments)
      (peekList peekRead, \t aligns -> t{simpleTableAlignments = aligns})
  , property "widths" "relative column widths"
      (pushPandocList pushRealFloat, simpleTableColumnWidths)
      (peekList peekRealFloat, \t ws -> t{simpleTableColumnWidths = ws})
  , property "headers" "table header"
      (pushRow, simpleTableHeader)
      (peekRow, \t h -> t{simpleTableHeader = h})
  , property "rows" "table body rows"
      (pushPandocList pushRow, simpleTableBody)
      (peekList peekRow, \t bs -> t{simpleTableBody = bs})

  , readonly "t" "type tag (always 'SimpleTable')"
      (pushText, const "SimpleTable")

  , alias "header" "alias for `headers`" ["headers"]
  ]
 where
  pushRow = pushPandocList (pushPandocList pushBlock)

peekRow :: LuaError e => Peeker e [[Block]]
peekRow = peekList peekBlocksFuzzy

-- | Push a simple table to the stack by calling the
-- @pandoc.SimpleTable@ constructor.
pushSimpleTable :: forall e. LuaError e => SimpleTable -> LuaE e ()
pushSimpleTable = pushUD typeSimpleTable

-- | Retrieve a simple table from the stack.
peekSimpleTable :: forall e. LuaError e => Peeker e SimpleTable
peekSimpleTable = retrieving "SimpleTable" . peekUD typeSimpleTable

-- | Constructor for the 'SimpleTable' type.
mkSimpleTable :: LuaError e => DocumentedFunction e
mkSimpleTable = defun "SimpleTable"
  ### liftPure5 SimpleTable
  <#> parameter peekInlinesFuzzy "Inlines" "caption" "table caption"
  <#> parameter (peekList peekRead) "{Alignment,...}" "align" "column alignments"
  <#> parameter (peekList peekRealFloat) "{number,...}" "widths"
        "relative column widths"
  <#> parameter peekRow "{Blocks,...}" "header" "table header row"
  <#> parameter (peekList peekRow) "{{Blocks,...},...}" "body" "table body rows"
  =#> functionResult pushSimpleTable "SimpleTable" "new SimpleTable object"
