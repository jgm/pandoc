{- |
   Module      : Text.Pandoc.Lua.Module.Types
   Copyright   : © 2019-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc data type constructors.
-}
module Text.Pandoc.Lua.Module.Types
  ( pushModule
  ) where

import Data.Version (Version)
import Foreign.Lua (Lua, NumResults)
import Text.Pandoc.Definition
import Text.Pandoc.Lua.Marshaling.AST (LuaAttr, LuaListAttributes)
import Text.Pandoc.Lua.Marshaling.Version ()
import Text.Pandoc.Lua.Util (addFunction)

import qualified Foreign.Lua as Lua

-- | Push the pandoc.system module on the Lua stack.
pushModule :: Lua NumResults
pushModule = do
  Lua.newtable
  addFunction "Version" (return :: Version -> Lua Version)
  pushCloneTable
  Lua.setfield (Lua.nthFromTop 2) "clone"
  return 1

pushCloneTable :: Lua NumResults
pushCloneTable = do
  Lua.newtable
  addFunction "Attr" cloneAttr
  addFunction "Block" cloneBlock
  addFunction "Citation" cloneCitation
  addFunction "Inline" cloneInline
  addFunction "Meta" cloneMeta
  addFunction "MetaValue" cloneMetaValue
  addFunction "ListAttributes" cloneListAttributes
  addFunction "Pandoc" clonePandoc
  return 1

cloneAttr :: LuaAttr -> Lua LuaAttr
cloneAttr = return

cloneBlock :: Block -> Lua Block
cloneBlock = return

cloneCitation :: Citation -> Lua Citation
cloneCitation = return

cloneInline :: Inline -> Lua Inline
cloneInline = return

cloneListAttributes :: LuaListAttributes -> Lua LuaListAttributes
cloneListAttributes = return

cloneMeta :: Meta -> Lua Meta
cloneMeta = return

cloneMetaValue :: MetaValue -> Lua MetaValue
cloneMetaValue = return

clonePandoc :: Pandoc -> Lua Pandoc
clonePandoc = return
