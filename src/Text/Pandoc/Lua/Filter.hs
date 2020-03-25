{-# LANGUAGE FlexibleContexts  #-}
{- |
Module      : Text.Pandoc.Lua.Filter
Copyright   : © 2012–2020 John MacFarlane,
              © 2017-2020 Albert Krewinkel
License     : GNU GPL, version 2 or above
Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
Stability   : alpha

Types and functions for running Lua filters.
-}
module Text.Pandoc.Lua.Filter ( LuaFilterFunction
                              , LuaFilter
                              , runFilterFile
                              , walkInlines
                              , walkBlocks
                              , module Text.Pandoc.Lua.Walk
                              ) where
import Control.Applicative ((<|>))
import Control.Monad (mplus, (>=>))
import Control.Monad.Catch (finally, try)
import Data.Data (Data, DataType, dataTypeConstrs, dataTypeName, dataTypeOf,
                  showConstr, toConstr, tyconUQname)
import Data.Foldable (foldrM)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Foreign.Lua (Lua, Peekable, Pushable, StackIndex)
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.Marshaling ()
import Text.Pandoc.Lua.Marshaling.List (List (..))
import Text.Pandoc.Lua.Walk (SingletonsList (..))
import Text.Pandoc.Walk (Walkable (walkM))

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

-- | Fetch either a list of elements from the stack. If there is a single
-- element instead of a list, fetch that element as a singleton list. If the top
-- of the stack is nil, return the default element that was passed to this
-- function. If none of these apply, raise an error.
elementOrList :: Peekable a => a -> Lua [a]
elementOrList x = do
  let topOfStack = Lua.stackTop
  elementUnchanged <- Lua.isnil topOfStack
  if elementUnchanged
    then [x] <$ Lua.pop 1
    else do
       mbres <- peekEither topOfStack
       case mbres of
         Right res -> [res] <$ Lua.pop 1
         Left _    -> Lua.peekList topOfStack `finally` Lua.pop 1

-- | Pop and return a value from the stack; if the value at the top of
-- the stack is @nil@, return the fallback element.
popOption :: Peekable a => a -> Lua a
popOption fallback = fromMaybe fallback . Lua.fromOptional <$> Lua.popValue

-- | Apply filter on a sequence of AST elements. Both lists and single
-- value are accepted as filter function return values.
runOnSequence :: (Data a, Peekable a, Pushable a)
              => LuaFilter -> SingletonsList a -> Lua (SingletonsList a)
runOnSequence (LuaFilter fnMap) (SingletonsList xs) =
  SingletonsList <$> mconcatMapM tryFilter xs
 where
  tryFilter :: (Data a, Peekable a, Pushable a) => a -> Lua [a]
  tryFilter x =
    let filterFnName = showConstr (toConstr x)
        catchAllName = tyconUQname $ dataTypeName (dataTypeOf x)
    in case Map.lookup filterFnName fnMap <|> Map.lookup catchAllName fnMap of
         Just fn -> runFilterFunction fn x *> elementOrList x
         Nothing -> return [x]

-- | Try filtering the given value without type error corrections on
-- the return value.
runOnValue :: (Data a, Peekable a, Pushable a)
           => String -> LuaFilter -> a -> Lua a
runOnValue filterFnName (LuaFilter fnMap) x =
  case Map.lookup filterFnName fnMap of
    Just fn -> runFilterFunction fn x *> popOption x
    Nothing -> return x

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
      walkInlines f
  >=> walkInlineLists f
  >=> walkBlocks f
  >=> walkBlockLists f
  >=> walkMeta f
  >=> walkPandoc f

mconcatMapM :: (Monad m) => (a -> m [a]) -> [a] -> m [a]
mconcatMapM f = fmap mconcat . mapM f

hasOneOf :: LuaFilter -> [String] -> Bool
hasOneOf (LuaFilter fnMap) = any (\k -> Map.member k fnMap)

contains :: LuaFilter -> String -> Bool
contains (LuaFilter fnMap) = (`Map.member` fnMap)

walkInlines :: Walkable (SingletonsList Inline) a => LuaFilter -> a -> Lua a
walkInlines lf =
  let f :: SingletonsList Inline -> Lua (SingletonsList Inline)
      f = runOnSequence lf
  in if lf `hasOneOf` inlineElementNames
     then walkM f
     else return

walkInlineLists :: Walkable (List Inline) a => LuaFilter -> a -> Lua a
walkInlineLists lf =
  let f :: List Inline -> Lua (List Inline)
      f = runOnValue listOfInlinesFilterName lf
  in if lf `contains` listOfInlinesFilterName
     then walkM f
     else return

walkBlocks :: Walkable (SingletonsList Block) a => LuaFilter -> a -> Lua a
walkBlocks lf =
  let f :: SingletonsList Block -> Lua (SingletonsList Block)
      f = runOnSequence lf
  in if lf `hasOneOf` blockElementNames
     then walkM f
     else return

walkBlockLists :: Walkable (List Block) a => LuaFilter -> a -> Lua a
walkBlockLists lf =
  let f :: List Block -> Lua (List Block)
      f = runOnValue listOfBlocksFilterName lf
  in if lf `contains` listOfBlocksFilterName
     then walkM f
     else return

walkMeta :: LuaFilter -> Pandoc -> Lua Pandoc
walkMeta lf (Pandoc m bs) = do
  m' <- runOnValue "Meta" lf m
  return $ Pandoc m' bs

walkPandoc :: LuaFilter -> Pandoc -> Lua Pandoc
walkPandoc (LuaFilter fnMap) =
  case foldl mplus Nothing (map (`Map.lookup` fnMap) pandocFilterNames) of
    Just fn -> \x -> runFilterFunction fn x *> singleElement x
    Nothing -> return

constructorsFor :: DataType -> [String]
constructorsFor x = map show (dataTypeConstrs x)

inlineElementNames :: [String]
inlineElementNames = "Inline" : constructorsFor (dataTypeOf (Str mempty))

blockElementNames :: [String]
blockElementNames = "Block" : constructorsFor (dataTypeOf (Para []))

listOfInlinesFilterName :: String
listOfInlinesFilterName = "Inlines"

listOfBlocksFilterName :: String
listOfBlocksFilterName = "Blocks"

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
    mbres <- peekEither (-1)
    case mbres of
      Right res -> res <$ Lua.pop 1
      Left err  -> do
        Lua.pop 1
        Lua.throwMessage
          ("Error while trying to get a filter's return " <>
           "value from Lua stack.\n" <> show err)

-- | Try to convert the value at the given stack index to a Haskell value.
-- Returns @Left@ with an error message on failure.
peekEither :: Peekable a => StackIndex -> Lua (Either PandocError a)
peekEither = try . Lua.peek
