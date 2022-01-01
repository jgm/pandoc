{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
Module      : Text.Pandoc.Lua.Marshaling.Sources
Copyright   : © 2021-2022 Albert Krewinkel
License     : GNU GPL, version 2 or above
Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

Marshal 'Sources'.
-}
module Text.Pandoc.Lua.Marshal.Sources
  ( pushSources
  ) where

import Data.Text (Text)
import HsLua as Lua
import Text.Pandoc.Lua.Marshal.List (newListMetatable)
import Text.Pandoc.Sources (Sources (..))
import Text.Parsec (SourcePos, sourceName)

-- | Pushes the 'Sources' as a list of lazy Lua objects.
pushSources :: LuaError e => Pusher e Sources
pushSources (Sources srcs) = do
  pushList (pushUD typeSource) srcs
  newListMetatable "pandoc Sources" $ do
    pushName "__tostring"
    pushHaskellFunction $ do
      sources <- forcePeek $ peekList (peekUD typeSource) (nthBottom 1)
      pushText . mconcat $ map snd sources
      return 1
    rawset (nth 3)
  setmetatable (nth 2)

-- | Source object type.
typeSource :: LuaError e => DocumentedType e (SourcePos, Text)
typeSource = deftype "pandoc input source"
  [ operation Tostring $ lambda
    ### liftPure snd
    <#> udparam typeSource "srcs" "Source to print in native format"
    =#> functionResult pushText "string" "Haskell representation"
  ]
  [ readonly "name" "source name"
      (pushString, sourceName . fst)
  , readonly "text" "source text"
      (pushText, snd)
  ]
