{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{- |
Module      : Text.Pandoc.Lua.Filter
Copyright   : © 2012-2021 John MacFarlane,
              © 2017-2021 Albert Krewinkel
License     : GNU GPL, version 2 or above
Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
Stability   : alpha

Types and functions for running Lua filters.
-}
module Text.Pandoc.Lua.Filter ( LuaFilterFunction
                              , LuaFilter
                              , runFilterFile
                              , walkInlines
                              , walkInlineLists
                              , walkBlocks
                              , walkBlockLists
                              , module Text.Pandoc.Lua.Walk
                              ) where
import Control.Applicative ((<|>))
import Control.Monad (mplus, (>=>), (<$!>))
import Data.Data (Data, DataType, dataTypeConstrs, dataTypeName, dataTypeOf,
                  showConstr, toConstr, tyconUQname)
import Data.Foldable (foldrM)
import Data.List (foldl')
import Data.Map (Map)
import Data.String (IsString (fromString))
import HsLua as Lua
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.Marshaling ()
import Text.Pandoc.Lua.Marshaling.AST
import Text.Pandoc.Lua.Marshaling.List (List (..), peekList')
import Text.Pandoc.Lua.Walk (SingletonsList (..))
import Text.Pandoc.Walk (Walkable (walkM))

import qualified Data.Map.Strict as Map
import qualified Text.Pandoc.Lua.Util as LuaUtil

-- | Transform document using the filter defined in the given file.
runFilterFile :: FilePath -> Pandoc -> LuaE PandocError Pandoc
runFilterFile filterPath doc = do
  oldtop <- Lua.gettop
  stat <- LuaUtil.dofileWithTraceback filterPath
  if stat /= Lua.OK
    then Lua.throwErrorAsException
    else do
      newtop <- Lua.gettop
      -- Use the returned filters, or the implicitly defined global
      -- filter if nothing was returned.
      luaFilters <- if newtop - oldtop >= 1
                    then Lua.peek Lua.top
                    else Lua.pushglobaltable *> fmap (:[]) Lua.popValue
      runAll luaFilters doc

runAll :: [LuaFilter] -> Pandoc -> LuaE PandocError Pandoc
runAll = foldr ((>=>) . walkMWithLuaFilter) return

-- | Filter function stored in the registry
newtype LuaFilterFunction = LuaFilterFunction Lua.Reference

-- | Collection of filter functions (at most one function per element
-- constructor)
newtype LuaFilter = LuaFilter (Map Name LuaFilterFunction)

instance Peekable LuaFilter where
  peek idx = do
    let constrs = listOfInlinesFilterName
                : listOfBlocksFilterName
                : metaFilterName
                : pandocFilterNames
                ++ blockElementNames
                ++ inlineElementNames
    let go constr acc = do
          Lua.getfield idx constr
          filterFn <- registerFilterFunction
          return $ case filterFn of
            Nothing -> acc
            Just fn -> Map.insert constr fn acc
    LuaFilter <$!> foldrM go Map.empty constrs

-- | Register the function at the top of the stack as a filter function in the
-- registry.
registerFilterFunction :: LuaError e => LuaE e (Maybe LuaFilterFunction)
registerFilterFunction = do
  isFn <- Lua.isfunction Lua.top
  if isFn
    then Just . LuaFilterFunction <$> Lua.ref Lua.registryindex
    else Nothing <$ Lua.pop 1

-- | Retrieve filter function from registry and push it to the top of the stack.
pushFilterFunction :: LuaFilterFunction -> LuaE PandocError ()
pushFilterFunction (LuaFilterFunction fnRef) =
  Lua.getref Lua.registryindex fnRef

-- | Fetch either a list of elements from the stack. If there is a single
-- element instead of a list, fetch that element as a singleton list. If the top
-- of the stack is nil, return the default element that was passed to this
-- function. If none of these apply, raise an error.
elementOrList :: Peeker PandocError a -> a -> LuaE PandocError [a]
elementOrList p x = do
  elementUnchanged <- Lua.isnil top
  if elementUnchanged
    then [x] <$ pop 1
    else forcePeek . (`lastly` pop 1) $ (((:[]) <$!> p top) <|> peekList p top)

-- | Fetches a single element; returns the fallback if the value is @nil@.
singleElement :: forall a e. (LuaError e) => Peeker e a -> a -> LuaE e a
singleElement p x = do
  elementUnchanged <- Lua.isnil top
  if elementUnchanged
    then x <$ Lua.pop 1
    else forcePeek $ p top `lastly` pop 1

-- | Pop and return a value from the stack; if the value at the top of
-- the stack is @nil@, return the fallback element.
popOption :: Peeker PandocError a -> a -> LuaE PandocError a
popOption peeker fallback = forcePeek . (`lastly` pop 1) $
  (fallback <$ peekNil top) <|> peeker top

-- | Apply filter on a sequence of AST elements. Both lists and single
-- value are accepted as filter function return values.
runOnSequence :: forall a. (Data a, Pushable a)
              => Peeker PandocError a -> LuaFilter -> SingletonsList a
              -> LuaE PandocError (SingletonsList a)
runOnSequence peeker (LuaFilter fnMap) (SingletonsList xs) =
  SingletonsList <$> mconcatMapM tryFilter xs
 where
  tryFilter :: a -> LuaE PandocError [a]
  tryFilter x =
    let filterFnName = fromString $ showConstr (toConstr x)
        catchAllName = fromString . tyconUQname $ dataTypeName (dataTypeOf x)
    in case Map.lookup filterFnName fnMap <|> Map.lookup catchAllName fnMap of
         Just fn -> runFilterFunction fn x *> elementOrList peeker x
         Nothing -> return [x]

-- | Try filtering the given value without type error corrections on
-- the return value.
runOnValue :: (Data a, Pushable a)
           => Name -> Peeker PandocError a
           -> LuaFilter -> a
           -> LuaE PandocError a
runOnValue filterFnName peeker (LuaFilter fnMap) x =
  case Map.lookup filterFnName fnMap of
    Just fn -> runFilterFunction fn x *> popOption peeker x
    Nothing -> return x

-- | Push a value to the stack via a Lua filter function. The filter
-- function is called with the given element as argument and is expected
-- to return an element. Alternatively, the function can return nothing
-- or nil, in which case the element is left unchanged.
runFilterFunction :: Pushable a
                  => LuaFilterFunction -> a -> LuaE PandocError ()
runFilterFunction lf x = do
  pushFilterFunction lf
  Lua.push x
  LuaUtil.callWithTraceback 1 1

walkMWithLuaFilter :: LuaFilter -> Pandoc -> LuaE PandocError Pandoc
walkMWithLuaFilter f =
      walkInlines f
  >=> walkInlineLists f
  >=> walkBlocks f
  >=> walkBlockLists f
  >=> walkMeta f
  >=> walkPandoc f

mconcatMapM :: (Monad m) => (a -> m [a]) -> [a] -> m [a]
mconcatMapM f = fmap mconcat . mapM f

hasOneOf :: LuaFilter -> [Name] -> Bool
hasOneOf (LuaFilter fnMap) = any (`Map.member` fnMap)

contains :: LuaFilter -> Name -> Bool
contains (LuaFilter fnMap) = (`Map.member` fnMap)

walkInlines :: Walkable (SingletonsList Inline) a
            => LuaFilter -> a -> LuaE PandocError a
walkInlines lf =
  let f :: SingletonsList Inline -> LuaE PandocError (SingletonsList Inline)
      f = runOnSequence peekInline lf
  in if lf `hasOneOf` inlineElementNames
     then walkM f
     else return

walkInlineLists :: Walkable (List Inline) a
                => LuaFilter -> a -> LuaE PandocError a
walkInlineLists lf =
  let f :: List Inline -> LuaE PandocError (List Inline)
      f = runOnValue listOfInlinesFilterName (peekList' peekInline) lf
  in if lf `contains` listOfInlinesFilterName
     then walkM f
     else return

walkBlocks :: Walkable (SingletonsList Block) a
           => LuaFilter -> a -> LuaE PandocError a
walkBlocks lf =
  let f :: SingletonsList Block -> LuaE PandocError (SingletonsList Block)
      f = runOnSequence peekBlock lf
  in if lf `hasOneOf` blockElementNames
     then walkM f
     else return

walkBlockLists :: Walkable (List Block) a
               => LuaFilter -> a -> LuaE PandocError a
walkBlockLists lf =
  let f :: List Block -> LuaE PandocError (List Block)
      f = runOnValue listOfBlocksFilterName (peekList' peekBlock) lf
  in if lf `contains` listOfBlocksFilterName
     then walkM f
     else return

walkMeta :: LuaFilter -> Pandoc -> LuaE PandocError Pandoc
walkMeta lf (Pandoc m bs) = do
  m' <- runOnValue "Meta" peekMeta lf m
  return $ Pandoc m' bs

walkPandoc :: LuaFilter -> Pandoc -> LuaE PandocError Pandoc
walkPandoc (LuaFilter fnMap) =
  case foldl' mplus Nothing (map (`Map.lookup` fnMap) pandocFilterNames) of
    Just fn -> \x -> runFilterFunction fn x *> singleElement peekPandoc x
    Nothing -> return

constructorsFor :: DataType -> [Name]
constructorsFor x = map (fromString . show) (dataTypeConstrs x)

inlineElementNames :: [Name]
inlineElementNames = "Inline" : constructorsFor (dataTypeOf (Str mempty))

blockElementNames :: [Name]
blockElementNames = "Block" : constructorsFor (dataTypeOf (Para []))

listOfInlinesFilterName :: Name
listOfInlinesFilterName = "Inlines"

listOfBlocksFilterName :: Name
listOfBlocksFilterName = "Blocks"

metaFilterName :: Name
metaFilterName = "Meta"

pandocFilterNames :: [Name]
pandocFilterNames = ["Pandoc", "Doc"]
