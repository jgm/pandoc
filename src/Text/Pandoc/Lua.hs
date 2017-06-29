{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable    #-}
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
{- |
   Module      : Text.Pandoc.Lua
   Copyright   : Copyright © 2017 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc lua utils.
-}
module Text.Pandoc.Lua ( LuaException(..),
                         runLuaFilter,
                         pushPandocModule ) where

import Control.Exception
import Control.Monad (unless, when, (>=>))
import Control.Monad.Trans (MonadIO (..))
import Data.Data (toConstr)
import Data.Map (Map)
import Data.Typeable (Typeable)
import Scripting.Lua (LuaState, StackValue (..))
import Text.Pandoc.Definition
import Text.Pandoc.Lua.PandocModule (pushPandocModule)
import Text.Pandoc.Lua.StackInstances ()
import Text.Pandoc.Walk

import qualified Data.Map as Map
import qualified Scripting.Lua as Lua

newtype LuaException = LuaException String
  deriving (Show, Typeable)

instance Exception LuaException

runLuaFilter :: (MonadIO m)
             => FilePath -> [String] -> Pandoc -> m Pandoc
runLuaFilter filterPath args pd = liftIO $ do
  lua <- Lua.newstate
  Lua.openlibs lua
  -- store module in global "pandoc"
  pushPandocModule lua
  Lua.setglobal lua "pandoc"
  top <- Lua.gettop lua
  status <- Lua.loadfile lua filterPath
  if status /= 0
    then do
      Just luaErrMsg <- Lua.peek lua 1
      throwIO (LuaException luaErrMsg)
    else do
      Lua.call lua 0 Lua.multret
      newtop <- Lua.gettop lua
      -- Use the implicitly defined global filter if nothing was returned
      when (newtop - top < 1) $ pushGlobalFilter lua
      Just luaFilters <- Lua.peek lua (-1)
      Lua.push lua args
      Lua.setglobal lua "PandocParameters"
      doc <- runAll luaFilters pd
      Lua.close lua
      return doc

pushGlobalFilter :: LuaState -> IO ()
pushGlobalFilter lua =
  Lua.newtable lua
  *> Lua.getglobal2 lua "pandoc.global_filter"
  *> Lua.call lua 0 1
  *> Lua.rawseti lua (-2) 1

runAll :: [LuaFilter] -> Pandoc -> IO Pandoc
runAll = foldr ((>=>) . walkMWithLuaFilter) return

walkMWithLuaFilter :: LuaFilter -> Pandoc -> IO Pandoc
walkMWithLuaFilter (LuaFilter lua fnMap) =
  walkM (execInlineLuaFilter lua fnMap) >=>
  walkM (execBlockLuaFilter  lua fnMap) >=>
  walkM (execMetaLuaFilter   lua fnMap) >=>
  walkM (execDocLuaFilter    lua fnMap)

type FunctionMap = Map String LuaFilterFunction
data LuaFilter = LuaFilter LuaState FunctionMap

newtype LuaFilterFunction = LuaFilterFunction { functionIndex :: Int }

execDocLuaFilter :: LuaState
                 -> FunctionMap
                 -> Pandoc -> IO Pandoc
execDocLuaFilter lua fnMap = tryFilter lua fnMap "Doc"

execMetaLuaFilter :: LuaState
                  -> FunctionMap
                  -> Pandoc -> IO Pandoc
execMetaLuaFilter lua fnMap (Pandoc meta blks) = do
  meta' <- tryFilter lua fnMap "Meta" meta
  return $ Pandoc meta' blks

execBlockLuaFilter :: LuaState
                   -> FunctionMap
                   -> Block -> IO Block
execBlockLuaFilter lua fnMap x = do
  tryFilter lua fnMap (show (toConstr x)) x

tryFilter :: StackValue a => LuaState -> FunctionMap -> String -> a -> IO a
tryFilter lua fnMap filterFnName x =
  case Map.lookup filterFnName fnMap of
    Nothing -> return x
    Just fn -> runFilterFunction lua fn x

tryFilterAlternatives :: StackValue a
                      => LuaState -> FunctionMap -> [String] -> a -> IO a
tryFilterAlternatives _ _ [] x = return x
tryFilterAlternatives lua fnMap (fnName : alternatives) x =
  case Map.lookup fnName fnMap of
    Nothing -> tryFilterAlternatives lua fnMap alternatives x
    Just fn -> runFilterFunction lua fn x

execInlineLuaFilter :: LuaState
                    -> FunctionMap
                    -> Inline -> IO Inline
execInlineLuaFilter lua fnMap x = do
  let tryAlt = tryFilterAlternatives lua fnMap
  case x of
    Math DisplayMath _   -> tryAlt ["DisplayMath", "Math"] x
    Math InlineMath _    -> tryAlt ["InlineMath", "Math"] x
    Quoted DoubleQuote _ -> tryAlt ["DoubleQuoted", "Quoted"] x
    Quoted SingleQuote _ -> tryAlt ["SingleQuoted", "Quoted"] x
    _                    -> tryFilter lua fnMap (show (toConstr x)) x

instance StackValue LuaFilter where
  valuetype _ = Lua.TTABLE
  push = undefined
  peek lua idx = fmap (LuaFilter lua) <$> Lua.peek lua idx

-- | Push a value to the stack via a lua filter function. The filter function is
-- called with given element as argument and is expected to return an element.
-- Alternatively, the function can return nothing or nil, in which case the
-- element is left unchanged.
runFilterFunction :: StackValue a => LuaState -> LuaFilterFunction -> a -> IO a
runFilterFunction lua lf x = do
  pushFilterFunction lua lf
  Lua.push lua x
  z <- Lua.pcall lua 1 1 0
  if (z /= 0)
    then do
      msg <- Lua.peek lua (-1)
      let prefix = "Error while running filter function: "
      throwIO . LuaException $
        case msg of
          Nothing   -> prefix ++ "could not read error message"
          Just msg' -> prefix ++ msg'
    else do
      resType <- Lua.ltype lua (-1)
      case resType of
        Lua.TNIL -> Lua.pop lua 1 *> return x
        _        -> do
          mbres <- Lua.peek lua (-1)
          case mbres of
            Nothing -> throwIO $ LuaException
                       ("Error while trying to get a filter's return "
                        ++ "value from lua stack.")
            Just res -> res <$ Lua.pop lua 1

-- | Push the filter function to the top of the stack.
pushFilterFunction :: Lua.LuaState -> LuaFilterFunction -> IO ()
pushFilterFunction lua lf =
  -- The function is stored in a lua registry table, retrieve it from there.
  Lua.rawgeti lua Lua.registryindex (functionIndex lf)

registerFilterFunction :: LuaState -> Int -> IO LuaFilterFunction
registerFilterFunction lua idx = do
  isFn <- Lua.isfunction lua idx
  unless isFn . throwIO . LuaException $ "Not a function at index " ++ show idx
  Lua.pushvalue lua idx
  refIdx <- Lua.ref lua Lua.registryindex
  return $ LuaFilterFunction refIdx

instance StackValue LuaFilterFunction where
  valuetype _ = Lua.TFUNCTION
  push = pushFilterFunction
  peek = fmap (fmap Just) . registerFilterFunction
