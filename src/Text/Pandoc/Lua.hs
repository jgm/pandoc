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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Control.Monad.Trans ( MonadIO(..) )
import Data.Aeson ( ToJSON(..), fromJSON, Value, Result(..) )
import Data.Text ( pack, unpack )
import Data.Text.Encoding ( decodeUtf8 )
import Scripting.Lua ( StackValue(..) )
import Scripting.Lua.Aeson ()
import Text.Pandoc.Definition ( Pandoc )

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
      doc <- Lua.callfunc lua "run_filter" pd (map pack args)
      Lua.close lua
      return doc

instance StackValue Pandoc where
  push lua = Lua.push lua . toJSON
  peek lua i = maybeFromJson <$> peek lua i
  valuetype _ = Lua.TTABLE

maybeFromJson :: Maybe Value -> Maybe Pandoc
maybeFromJson = \case
  Nothing -> Nothing
  Just v  -> case fromJSON v of
    Success pd -> Just pd
    _          -> Nothing
