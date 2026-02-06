{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{- |
Module      : Text.Pandoc.Lua.Marshaling.Sources
Copyright   : © 2021-2026 Albert Krewinkel <albert+pandoc@tarleb.com>
License     : GPL-2.0-or-later

Marshal 'Sources'.
-}
module Text.Pandoc.Lua.Marshal.Sources
  ( peekSources
  , pushSources
  , typeSource
  ) where

import Control.Monad ((<$!>))
import Data.Text (Text)
import HsLua as Lua
import Text.Pandoc.Lua.Marshal.List (newListMetatable)
import Text.Pandoc.Sources (Sources (..), toSources)
import Text.Parsec (SourcePos, sourceName)

-- | Pushes the 'Sources' as a list of lazy Lua objects.
pushSources :: LuaError e => Pusher e Sources
pushSources (Sources srcs) = do
  pushList (pushUD typeSource) srcs
  newListMetatable "Sources" $ do
    pushName "__tostring"
    pushHaskellFunction $ do
      sources <- forcePeek $ peekList (peekUD typeSource) (nthBottom 1)
      pushText . mconcat $ map snd sources
      return 1
    rawset (nth 3)
  setmetatable (nth 2)

-- | Retrieves sources from the stack.
peekSources :: LuaError e => Peeker e Sources
peekSources idx = liftLua (ltype idx) >>= \case
  TypeTable  -> mconcat <$!> peekList peekSourcesSingleton idx
  _          -> peekSourcesSingleton idx

-- | Retrieves a Sources singleton, i.e., a list with exactly one item.
peekSourcesSingleton :: LuaError e => Peeker e Sources
peekSourcesSingleton = choice
  [ fmap toSources . peekText
  , fmap (Sources . (:[])) . peekUD typeSource
  , fmap (toSources . (:[])) . peekPair peekString peekText
  , fmap (toSources . (:[])) .
    (\idx -> (,)
      <$> peekFieldRaw peekString "name" idx
      <*> peekFieldRaw peekText "text" idx)
  ]

-- | A @Sources@ item.
type Source = (SourcePos, Text)

-- | Source object type.
typeSource :: LuaError e => DocumentedType e Source
typeSource = deftype "Source"
  [ operation Tostring $ lambda
    ### liftPure snd
    <#> udparam typeSource "srcs" "Source to print in native format"
    =#> functionResult pushText "string" "source contents"
  ]
  [ readonly "name" "source name"
      (pushString, sourceName . fst)
  , readonly "text" "source text"
      (pushText, snd)
  ]
