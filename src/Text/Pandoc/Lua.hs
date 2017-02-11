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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Text.Pandoc.Lua
   Copyright   : Copyright © 2017 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc lua utils.
-}
module Text.Pandoc.Lua ( runLuaFilter ) where

import Control.Monad ( (>=>) )
import Control.Monad.Trans ( MonadIO(..) )
import Data.Aeson ( FromJSON(..), ToJSON(..), Result(..), Value, fromJSON )
import Data.Text ( pack, unpack )
import Data.Text.Encoding ( decodeUtf8 )
import Scripting.Lua ( StackValue(..) )
import Scripting.Lua.Aeson ()
import Text.Pandoc.Definition ( Block(..), Inline(..), Pandoc(..) )
import Text.Pandoc.Walk

import qualified Scripting.Lua as Lua
import qualified Scripting.Lua as LuaAeson

runLuaFilter :: (MonadIO m)
             => FilePath -> [String] -> Pandoc -> m Pandoc
runLuaFilter filterPath args pd = liftIO $ do
  lua <- LuaAeson.newstate
  Lua.openlibs lua
  status <- Lua.loadfile lua filterPath
  if (status /= 0)
    then do
      luaErrMsg <- unpack . decodeUtf8 <$> Lua.tostring lua 1
      error luaErrMsg
    else do
      Lua.call lua 0 0
      Lua.push lua (map pack args)
      Lua.setglobal lua "PandocParameters"
      doc <- luaFilter (undefined::Pandoc) lua "filter_doc"   >=>
             luaFilter (undefined::Block)  lua "filter_block" >=>
             luaFilter (undefined::Inline) lua "filter_inline" $
             pd
      Lua.close lua
      return doc

luaFilter :: forall a. (StackValue a, Walkable a Pandoc)
          => a -> Lua.LuaState -> String -> Pandoc -> IO Pandoc
luaFilter _ lua luaFn x = do
  fnExists <- isLuaFunction lua luaFn
  if fnExists
    then walkM (Lua.callfunc lua luaFn :: a -> IO a) x
    else return x

isLuaFunction :: Lua.LuaState -> String -> IO Bool
isLuaFunction lua fnName = do
  Lua.getglobal lua fnName
  ltype <- Lua.ltype lua (-1)
  Lua.pop lua (-1)
  return $ ltype == Lua.TFUNCTION

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

