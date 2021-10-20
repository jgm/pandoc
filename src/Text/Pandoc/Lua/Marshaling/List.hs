{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}
{- |
Module      : Text.Pandoc.Lua.Marshaling.List
Copyright   : © 2012-2021 John MacFarlane
              © 2017-2021 Albert Krewinkel
License     : GNU GPL, version 2 or above
Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
Stability   : alpha

Marshaling/unmarshaling instances for @pandoc.List@s.
-}
module Text.Pandoc.Lua.Marshaling.List
  ( List (..)
  , peekList'
  , pushPandocList
  ) where

import Control.Monad ((<$!>))
import Data.Data (Data)
import HsLua (LuaError, Peeker, Pusher, Pushable (push), peekList, pushList)
import Text.Pandoc.Walk (Walkable (..))
import Text.Pandoc.Lua.Util (pushViaConstr')

-- | List wrapper which is marshalled as @pandoc.List@.
newtype List a = List { fromList :: [a] }
  deriving (Data, Eq, Show)

instance Pushable a => Pushable (List a) where
  push (List xs) = pushPandocList push xs

-- | Pushes a list as a numerical Lua table, setting a metatable that offers a
-- number of convenience functions.
pushPandocList :: LuaError e => Pusher e a -> Pusher e [a]
pushPandocList pushItem xs = pushViaConstr' "List" [pushList pushItem xs]

peekList' :: LuaError e => Peeker e a -> Peeker e (List a)
peekList' p = (List <$!>) . peekList p

-- List is just a wrapper, so we can reuse the walk instance for
-- unwrapped Hasekll lists.
instance Walkable [a] b => Walkable (List a) b where
  walkM f = walkM (fmap fromList . f . List)
  query f = query (f . List)
