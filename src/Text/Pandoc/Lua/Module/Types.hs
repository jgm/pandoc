{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Lua.Module.Types
   Copyright   : © 2019-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc data type constructors.
-}
module Text.Pandoc.Lua.Module.Types
  ( pushModule
  ) where

import HsLua (LuaE, NumResults, Peeker, Pusher)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.ErrorConversion ()
import Text.Pandoc.Lua.Marshaling.AST
import Text.Pandoc.Lua.Util (addFunction)

import qualified HsLua as Lua
import qualified HsLua.Module.Version as Version

-- | Push the pandoc.types module on the Lua stack.
pushModule :: LuaE PandocError NumResults
pushModule = do
  Lua.newtable
  Lua.pushName "Version" *> Lua.pushModule Version.documentedModule
    *> Lua.rawset (Lua.nth 3)
  pushCloneTable
  Lua.setfield (Lua.nth 2) "clone"
  return 1

pushCloneTable :: LuaE PandocError NumResults
pushCloneTable = do
  Lua.newtable
  addFunction "Attr"      $ cloneWith peekAttr pushAttr
  addFunction "Block"     $ cloneWith peekBlock pushBlock
  addFunction "Citation"  $ cloneWith peekCitation Lua.push
  addFunction "Inline"    $ cloneWith peekInline pushInline
  addFunction "Meta"      $ cloneWith peekMeta Lua.push
  addFunction "MetaValue" $ cloneWith peekMetaValue pushMetaValue
  addFunction "ListAttributes" $ cloneWith peekListAttributes pushListAttributes
  addFunction "Pandoc"    $ cloneWith peekPandoc pushPandoc
  return 1

cloneWith :: Peeker PandocError a
          -> Pusher PandocError a
          -> LuaE PandocError NumResults
cloneWith peeker pusher = do
  x <- Lua.forcePeek $ peeker (Lua.nthBottom 1)
  pusher x
  return (Lua.NumResults 1)
