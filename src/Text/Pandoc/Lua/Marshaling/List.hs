{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{- |
Module      : Text.Pandoc.Lua.Marshaling.List
Copyright   : © 2012-2020 John MacFarlane
              © 2017-2020 Albert Krewinkel
License     : GNU GPL, version 2 or above
Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
Stability   : alpha

Marshaling/unmarshaling instances for @pandoc.List@s.
-}
module Text.Pandoc.Lua.Marshaling.List
  ( List (..)
  ) where

import Data.Data (Data)
import Foreign.Lua (Peekable, Pushable)
import Text.Pandoc.Walk (Walkable (..))
import Text.Pandoc.Lua.Util (defineHowTo, pushViaConstructor)

import qualified Foreign.Lua as Lua

-- | List wrapper which is marshalled as @pandoc.List@.
newtype List a = List { fromList :: [a] }
  deriving (Data, Eq, Show)

instance Pushable a => Pushable (List a) where
  push (List xs) =
    pushViaConstructor "List" xs

instance Peekable a => Peekable (List a) where
  peek idx = defineHowTo "get List" $ do
    xs <- Lua.peek idx
    return $ List xs

-- List is just a wrapper, so we can reuse the walk instance for
-- unwrapped Hasekll lists.
instance Walkable [a] b => Walkable (List a) b where
  walkM f = walkM (fmap fromList . f . List)
  query f = query (f . List)
