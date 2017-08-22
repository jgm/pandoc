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

import Control.Monad (mplus, unless, when, (>=>))
import Control.Monad.Trans (MonadIO (..))
import Data.Data (DataType, Data, toConstr, showConstr, dataTypeOf,
                  dataTypeConstrs)
import Data.Foldable (foldrM)
import Data.Map (Map)
import Data.Maybe (isJust)
import Foreign.Lua (Lua, FromLuaStack (peek), LuaException (..), StackIndex,
                    Status(OK), ToLuaStack (push))
import Text.Pandoc.Definition
import Text.Pandoc.Lua.PandocModule (pushPandocModule)
import Text.Pandoc.Walk (Walkable (walkM))

import qualified Data.Map as Map
import qualified Foreign.Lua as Lua

runLuaFilter :: (MonadIO m)
             => Maybe FilePath -> FilePath -> [String] -> Pandoc -> m Pandoc
runLuaFilter datadir filterPath args pd = liftIO . Lua.runLua $ do
  Lua.openlibs
  -- store module in global "pandoc"
  pushPandocModule datadir
  Lua.setglobal "pandoc"
  top <- Lua.gettop
  stat<- Lua.dofile filterPath
  if stat /= OK
    then do
      luaErrMsg <- peek (-1) <* Lua.pop 1
      Lua.throwLuaError luaErrMsg
    else do
      newtop <- Lua.gettop
      -- Use the implicitly defined global filter if nothing was returned
      when (newtop - top < 1) $ pushGlobalFilter
      luaFilters <- peek (-1)
      push args
      Lua.setglobal "PandocParameters"
      runAll luaFilters pd

pushGlobalFilter :: Lua ()
pushGlobalFilter = do
  Lua.newtable
  Lua.getglobal' "pandoc.global_filter"
  Lua.call 0 1
  Lua.rawseti (-2) 1

runAll :: [LuaFilter] -> Pandoc -> Lua Pandoc
runAll = foldr ((>=>) . walkMWithLuaFilter) return

walkMWithLuaFilter :: LuaFilter -> Pandoc -> Lua Pandoc
walkMWithLuaFilter (LuaFilter fnMap) = walkLua
  where
    walkLua :: Pandoc -> Lua Pandoc
    walkLua =
      (if hasOneOf inlineFilterNames
        then walkM (tryFilter fnMap :: Inline -> Lua Inline)
        else return)
      >=>
      (if hasOneOf blockFilterNames
        then walkM (tryFilter fnMap :: Block -> Lua Block)
        else return)
      >=>
      (case Map.lookup "Meta" fnMap of
          Just fn -> walkM (\(Pandoc meta blocks) -> do
                               meta' <- runFilterFunction fn meta
                               return $ Pandoc meta' blocks)
          Nothing -> return)
      >=>
      (case foldl mplus Nothing (map (`Map.lookup` fnMap) pandocFilterNames) of
          Just fn -> runFilterFunction fn :: Pandoc -> Lua Pandoc
          Nothing -> return)
    hasOneOf = any (\k -> isJust (Map.lookup k fnMap))

constructorsFor :: DataType -> [String]
constructorsFor x = map show (dataTypeConstrs x)

inlineFilterNames :: [String]
inlineFilterNames = constructorsFor (dataTypeOf (Str []))

blockFilterNames :: [String]
blockFilterNames = constructorsFor (dataTypeOf (Para []))

metaFilterName :: String
metaFilterName = "Meta"

pandocFilterNames :: [String]
pandocFilterNames = ["Pandoc", "Doc"]

type FunctionMap = Map String LuaFilterFunction
newtype LuaFilter = LuaFilter FunctionMap
newtype LuaFilterFunction = LuaFilterFunction { functionIndex :: Int }

-- | Try running a filter for the given element
tryFilter :: (Data a, FromLuaStack a, ToLuaStack a) => FunctionMap -> a -> Lua a
tryFilter fnMap x =
  let filterFnName = showConstr (toConstr x) in
  case Map.lookup filterFnName fnMap of
    Nothing -> return x
    Just fn -> runFilterFunction fn x

instance FromLuaStack LuaFilter where
  peek idx =
    let constrs = metaFilterName : pandocFilterNames
                  ++ blockFilterNames
                  ++ inlineFilterNames
        fn c acc = do
          Lua.getfield idx c
          filterFn <- Lua.tryLua (peek (-1))
          Lua.pop 1
          return $ case filterFn of
            Left _ -> acc
            Right f -> (c, f) : acc
    in LuaFilter . Map.fromList <$> foldrM fn [] constrs

-- | Push a value to the stack via a lua filter function. The filter function is
-- called with given element as argument and is expected to return an element.
-- Alternatively, the function can return nothing or nil, in which case the
-- element is left unchanged.
runFilterFunction :: (FromLuaStack a, ToLuaStack a)
                  => LuaFilterFunction -> a -> Lua a
runFilterFunction lf x = do
  pushFilterFunction lf
  push x
  z <- Lua.pcall 1 1 Nothing
  if z /= OK
    then do
      msg <- peek (-1)
      let prefix = "Error while running filter function: "
      Lua.throwLuaError $ prefix ++ msg
    else do
      noExplicitFilter <- Lua.isnil (-1)
      if noExplicitFilter
        then  Lua.pop 1 *> return x
        else do
          mbres <- Lua.peekEither (-1)
          case mbres of
            Left err -> Lua.throwLuaError
                        ("Error while trying to get a filter's return "
                         ++ "value from lua stack.\n" ++ err)
            Right res -> res <$ Lua.pop 1

-- | Push the filter function to the top of the stack.
pushFilterFunction :: LuaFilterFunction -> Lua ()
pushFilterFunction lf =
  -- The function is stored in a lua registry table, retrieve it from there.
  Lua.rawgeti Lua.registryindex (functionIndex lf)

registerFilterFunction :: StackIndex -> Lua LuaFilterFunction
registerFilterFunction idx = do
  isFn <- Lua.isfunction idx
  unless isFn . Lua.throwLuaError $ "Not a function at index " ++ show idx
  Lua.pushvalue idx
  refIdx <- Lua.ref Lua.registryindex
  return $ LuaFilterFunction refIdx

instance ToLuaStack LuaFilterFunction where
  push = pushFilterFunction

instance FromLuaStack LuaFilterFunction where
  peek = registerFilterFunction
