{-
Copyright © 2017 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Text.Pandoc.Lua.StackInstances
   Copyright   : Copyright © 2017 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

StackValue instances for pandoc types.
-}
module Text.Pandoc.Lua.StackInstances () where

import Data.Aeson ( FromJSON(..), ToJSON(..), Result(..), Value, fromJSON )
import Scripting.Lua ( StackValue(..) )
import Scripting.Lua.Aeson ()
import Text.Pandoc.Definition ( Block(..), Inline(..), Pandoc(..) )

import qualified Scripting.Lua as Lua
import qualified Text.Pandoc.UTF8 as UTF8


maybeFromJson :: (FromJSON a) => Maybe Value -> Maybe a
maybeFromJson mv = fromJSON <$> mv >>= \case
  Success x -> Just x
  _         -> Nothing

instance StackValue Pandoc where
  push lua = Lua.push lua . toJSON
  peek lua i = maybeFromJson <$> peek lua i
  valuetype _ = Lua.TTABLE

instance StackValue Block where
  push lua = Lua.push lua . toJSON
  peek lua i = maybeFromJson <$> peek lua i
  valuetype _ = Lua.TTABLE

instance StackValue Inline where
  push lua = Lua.push lua . toJSON
  peek lua i = maybeFromJson <$> peek lua i
  valuetype _ = Lua.TTABLE

#if MIN_VERSION_base(4,8,0)
instance {-# OVERLAPS #-} StackValue [Char] where
#else
instance StackValue [Char] where
#endif
  push lua cs = Lua.push lua (UTF8.fromString cs)
  peek lua i = do
                 res <- Lua.peek lua i
                 return $ UTF8.toString `fmap` res
  valuetype _ = Lua.TSTRING
