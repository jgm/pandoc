{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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
module Text.Pandoc.Lua (LuaException (..), pushPandocModule, runLuaFilter) where

import Control.Monad (unless, when, (>=>), mplus)
import Control.Monad.Trans (MonadIO (..))
import Data.Data (toConstr, showConstr, dataTypeOf, dataTypeConstrs, Data)
import Data.Map (Map)
import Data.Maybe (isJust)
import Foreign.Lua (Lua, FromLuaStack (peek), LuaException (..), StackIndex,
                    Status(OK), ToLuaStack (push), call, isnil, dofile,
                    getglobal', gettop, isfunction, newtable, openlibs, pcall,
                    peekEither, pop, pushvalue, rawgeti, rawseti, ref,
                    registryindex, runLua, setglobal, throwLuaError)
import Text.Pandoc.Definition
import Text.Pandoc.Lua.PandocModule (pushPandocModule)
import Text.Pandoc.Lua.StackInstances ()
import Text.Pandoc.Walk (Walkable (walkM))

import qualified Data.Map as Map

runLuaFilter :: (MonadIO m)
             => Maybe FilePath -> FilePath -> [String] -> Pandoc -> m Pandoc
runLuaFilter datadir filterPath args pd = liftIO . runLua $ do
  openlibs
  -- store module in global "pandoc"
  pushPandocModule datadir
  setglobal "pandoc"
  top <- gettop
  stat<- dofile filterPath
  if stat /= OK
    then do
      luaErrMsg <- peek (-1) <* pop 1
      throwLuaError luaErrMsg
    else do
      newtop <- gettop
      -- Use the implicitly defined global filter if nothing was returned
      when (newtop - top < 1) $ pushGlobalFilter
      luaFilters <- peek (-1)
      push args
      setglobal "PandocParameters"
      runAll luaFilters pd

pushGlobalFilter :: Lua ()
pushGlobalFilter = do
  newtable
  getglobal' "pandoc.global_filter"
  call 0 1
  rawseti (-2) 1

runAll :: [LuaFilter] -> Pandoc -> Lua Pandoc
runAll = foldr ((>=>) . walkMWithLuaFilter) return

walkMWithLuaFilter :: LuaFilter -> Pandoc -> Lua Pandoc
walkMWithLuaFilter (LuaFilter fnMap) = walkLua
  where
    walkLua :: Pandoc -> Lua Pandoc
    walkLua =
          (if hasOneOf (constructorsFor (dataTypeOf (Str [])))
           then walkM (tryFilter fnMap :: Inline -> Lua Inline)
           else return)
          >=>
          (if hasOneOf (constructorsFor (dataTypeOf (Para [])))
           then walkM (tryFilter fnMap :: Block -> Lua Block)
           else return)
          >=>
          (case Map.lookup "Meta" fnMap of
             Just fn -> walkM (\(Pandoc meta blocks) -> do
                                  meta' <- runFilterFunction fn meta
                                  return $ Pandoc meta' blocks)
             Nothing -> return)
          >=>
          (case Map.lookup "Pandoc" fnMap `mplus` Map.lookup "Doc" fnMap of
             Just fn -> runFilterFunction fn :: Pandoc -> Lua Pandoc
             Nothing -> return)
    hasOneOf = any (\k -> isJust (Map.lookup k fnMap))
    constructorsFor x = map show (dataTypeConstrs x)

type FunctionMap = Map String LuaFilterFunction
data LuaFilter = LuaFilter FunctionMap

newtype LuaFilterFunction = LuaFilterFunction { functionIndex :: Int }

-- | Try running a filter for the given element
tryFilter :: (Data a, FromLuaStack a, ToLuaStack a) => FunctionMap -> a -> Lua a
tryFilter fnMap x =
  let filterFnName = showConstr (toConstr x) in
  case Map.lookup filterFnName fnMap of
    Nothing -> return x
    Just fn -> runFilterFunction fn x

instance FromLuaStack LuaFilter where
  peek idx = LuaFilter <$> peek idx

-- | Push a value to the stack via a lua filter function. The filter function is
-- called with given element as argument and is expected to return an element.
-- Alternatively, the function can return nothing or nil, in which case the
-- element is left unchanged.
runFilterFunction :: (FromLuaStack a, ToLuaStack a)
                  => LuaFilterFunction -> a -> Lua a
runFilterFunction lf x = do
  pushFilterFunction lf
  push x
  z <- pcall 1 1 Nothing
  if z /= OK
    then do
      msg <- peek (-1)
      let prefix = "Error while running filter function: "
      throwLuaError $ prefix ++ msg
    else do
      noExplicitFilter <- isnil (-1)
      if noExplicitFilter
        then  pop 1 *> return x
        else do
          mbres <- peekEither (-1)
          case mbres of
            Left err -> throwLuaError
                        ("Error while trying to get a filter's return "
                         ++ "value from lua stack.\n" ++ err)
            Right res -> res <$ pop 1

-- | Push the filter function to the top of the stack.
pushFilterFunction :: LuaFilterFunction -> Lua ()
pushFilterFunction lf =
  -- The function is stored in a lua registry table, retrieve it from there.
  rawgeti registryindex (functionIndex lf)

registerFilterFunction :: StackIndex -> Lua LuaFilterFunction
registerFilterFunction idx = do
  isFn <- isfunction idx
  unless isFn . throwLuaError $ "Not a function at index " ++ show idx
  pushvalue idx
  refIdx <- ref registryindex
  return $ LuaFilterFunction refIdx

instance ToLuaStack LuaFilterFunction where
  push = pushFilterFunction

instance FromLuaStack LuaFilterFunction where
  peek = registerFilterFunction
