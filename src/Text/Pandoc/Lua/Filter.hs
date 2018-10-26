{-
Copyright © 2012-2018 John MacFarlane <jgm@berkeley.edu>
            2017-2018 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

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
{-# LANGUAGE NoImplicitPrelude #-}
{- |
Module      : Text.Pandoc.Lua.Filter
Copyright   : © 2012–2018 John MacFarlane,
              © 2017-2018 Albert Krewinkel
License     : GNU GPL, version 2 or above
Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
Stability   : alpha

Types and functions for running Lua filters.
-}
module Text.Pandoc.Lua.Filter ( LuaFilterFunction
                              , LuaFilter
                              , runFilterFile
                              , tryFilter
                              , runFilterFunction
                              , walkMWithLuaFilter
                              , walkInlines
                              , walkBlocks
                              , blockElementNames
                              , inlineElementNames
                              ) where
import Prelude
import Control.Monad (mplus, (>=>))
import Control.Monad.Catch (finally)
import Data.Data (Data, DataType, dataTypeConstrs, dataTypeName, dataTypeOf,
                  showConstr, toConstr, tyconUQname)
import Data.Foldable (foldrM)
import Data.Map (Map)
import Foreign.Lua (Lua, Peekable, Pushable)
import Text.Pandoc.Definition
import Text.Pandoc.Lua.StackInstances ()
import Text.Pandoc.Walk (walkM, Walkable)

import qualified Data.Map.Strict as Map
import qualified Foreign.Lua as Lua
import qualified Text.Pandoc.Lua.Util as LuaUtil

-- | Transform document using the filter defined in the given file.
runFilterFile :: FilePath -> Pandoc -> Lua Pandoc
runFilterFile filterPath doc = do
  top <- Lua.gettop
  stat <- LuaUtil.dofileWithTraceback filterPath
  if stat /= Lua.OK
    then Lua.throwTopMessage
    else do
      newtop <- Lua.gettop
      -- Use the returned filters, or the implicitly defined global
      -- filter if nothing was returned.
      luaFilters <- if newtop - top >= 1
                    then Lua.peek Lua.stackTop
                    else Lua.pushglobaltable *> fmap (:[]) Lua.popValue
      runAll luaFilters doc

runAll :: [LuaFilter] -> Pandoc -> Lua Pandoc
runAll = foldr ((>=>) . walkMWithLuaFilter) return

-- | Filter function stored in the registry
newtype LuaFilterFunction = LuaFilterFunction Lua.Reference

-- | Collection of filter functions (at most one function per element
-- constructor)
newtype LuaFilter = LuaFilter (Map String LuaFilterFunction)

instance Peekable LuaFilter where
  peek idx = do
    let constrs = metaFilterName
                : pandocFilterNames
                ++ blockElementNames
                ++ inlineElementNames
    let go constr acc = do
          Lua.getfield idx constr
          filterFn <- registerFilterFunction
          return $ case filterFn of
            Nothing -> acc
            Just fn -> Map.insert constr fn acc
    LuaFilter <$> foldrM go Map.empty constrs

-- | Register the function at the top of the stack as a filter function in the
-- registry.
registerFilterFunction :: Lua (Maybe LuaFilterFunction)
registerFilterFunction = do
  isFn <- Lua.isfunction Lua.stackTop
  if isFn
    then Just . LuaFilterFunction <$> Lua.ref Lua.registryindex
    else Nothing <$ Lua.pop 1

-- | Retrieve filter function from registry and push it to the top of the stack.
pushFilterFunction :: LuaFilterFunction -> Lua ()
pushFilterFunction (LuaFilterFunction fnRef) =
  Lua.getref Lua.registryindex fnRef


elementOrList :: Peekable a => a -> Lua [a]
elementOrList x = do
  let topOfStack = Lua.stackTop
  elementUnchanged <- Lua.isnil topOfStack
  if elementUnchanged
    then [x] <$ Lua.pop 1
    else do
       mbres <- Lua.peekEither topOfStack
       case mbres of
         Right res -> [res] <$ Lua.pop 1
         Left _    -> Lua.peekList topOfStack `finally` Lua.pop 1

-- | Try running a filter for the given element
tryFilter :: (Data a, Peekable a, Pushable a)
          => LuaFilter -> a -> Lua [a]
tryFilter (LuaFilter fnMap) x =
  let filterFnName = showConstr (toConstr x)
      catchAllName = tyconUQname $ dataTypeName (dataTypeOf x)
  in
  case Map.lookup filterFnName fnMap `mplus` Map.lookup catchAllName fnMap of
    Just fn -> runFilterFunction fn x *> elementOrList x
    Nothing -> return [x]

-- | Push a value to the stack via a lua filter function. The filter function is
-- called with given element as argument and is expected to return an element.
-- Alternatively, the function can return nothing or nil, in which case the
-- element is left unchanged.
runFilterFunction :: Pushable a => LuaFilterFunction -> a -> Lua ()
runFilterFunction lf x = do
  pushFilterFunction lf
  Lua.push x
  LuaUtil.callWithTraceback 1 1

walkMWithLuaFilter :: LuaFilter -> Pandoc -> Lua Pandoc
walkMWithLuaFilter f =
  walkInlines f >=> walkBlocks f >=> walkMeta f >=> walkPandoc f

mconcatMapM :: (Monad m, Functor m) => (a -> m [a]) -> [a] -> m [a]
mconcatMapM f = fmap mconcat . mapM f

hasOneOf :: LuaFilter -> [String] -> Bool
hasOneOf (LuaFilter fnMap) = any (\k -> Map.member k fnMap)

walkInlines :: Walkable [Inline] a => LuaFilter -> a -> Lua a
walkInlines f =
  if f `hasOneOf` inlineElementNames
     then walkM (mconcatMapM (tryFilter f :: Inline -> Lua [Inline]))
     else return

walkBlocks :: Walkable [Block] a => LuaFilter -> a -> Lua a
walkBlocks f =
  if f `hasOneOf` blockElementNames
     then walkM (mconcatMapM (tryFilter f :: Block -> Lua [Block]))
     else return

walkMeta :: LuaFilter -> Pandoc -> Lua Pandoc
walkMeta (LuaFilter fnMap) =
  case Map.lookup "Meta" fnMap of
    Just fn -> walkM (\(Pandoc meta blocks) -> do
                         meta' <- runFilterFunction fn meta *> singleElement meta
                         return $ Pandoc meta' blocks)
    Nothing -> return

walkPandoc :: LuaFilter -> Pandoc -> Lua Pandoc
walkPandoc (LuaFilter fnMap) =
  case foldl mplus Nothing (map (`Map.lookup` fnMap) pandocFilterNames) of
    Just fn -> \x -> runFilterFunction fn x *> singleElement x
    Nothing -> return

constructorsFor :: DataType -> [String]
constructorsFor x = map show (dataTypeConstrs x)

inlineElementNames :: [String]
inlineElementNames = "Inline" : constructorsFor (dataTypeOf (Str []))

blockElementNames :: [String]
blockElementNames = "Block" : constructorsFor (dataTypeOf (Para []))

metaFilterName :: String
metaFilterName = "Meta"

pandocFilterNames :: [String]
pandocFilterNames = ["Pandoc", "Doc"]

singleElement :: Peekable a => a -> Lua a
singleElement x = do
  elementUnchanged <- Lua.isnil (-1)
  if elementUnchanged
    then x <$ Lua.pop 1
    else do
    mbres <- Lua.peekEither (-1)
    case mbres of
      Right res -> res <$ Lua.pop 1
      Left err  -> do
        Lua.pop 1
        Lua.throwException $
          "Error while trying to get a filter's return " ++
          "value from lua stack.\n" ++ err
