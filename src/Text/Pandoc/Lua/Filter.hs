{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Pandoc.Lua.Filter ( LuaFilterFunction
                              , LuaFilter
                              , tryFilter
                              , runFilterFunction
                              , walkMWithLuaFilter
                              , walkInlines
                              , walkBlocks
                              , blockElementNames
                              , inlineElementNames
                              ) where
import Prelude
import Control.Monad (mplus, unless, when, (>=>))
import Control.Monad.Catch (finally)
import Text.Pandoc.Definition
import Data.Foldable (foldrM)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Foreign.Lua as Lua
import Foreign.Lua (FromLuaStack (peek), Lua, StackIndex,
                    Status (OK), ToLuaStack (push))
import Text.Pandoc.Walk (walkM, Walkable)
import Data.Data (Data, DataType, dataTypeConstrs, dataTypeName, dataTypeOf,
                  showConstr, toConstr, tyconUQname)
import Text.Pandoc.Lua.StackInstances()
import Text.Pandoc.Lua.Util (typeCheck)

type FunctionMap = Map String LuaFilterFunction

newtype LuaFilterFunction = LuaFilterFunction { functionIndex :: Int }

instance ToLuaStack LuaFilterFunction where
  push = pushFilterFunction

instance FromLuaStack LuaFilterFunction where
  peek = registerFilterFunction

newtype LuaFilter = LuaFilter FunctionMap

instance FromLuaStack LuaFilter where
  peek idx =
    let constrs = metaFilterName : pandocFilterNames
                  ++ blockElementNames
                  ++ inlineElementNames
        fn c acc = do
          Lua.getfield idx c
          filterFn <- Lua.tryLua (peek (-1))
          Lua.pop 1
          return $ case filterFn of
            Left _  -> acc
            Right f -> (c, f) : acc
    in LuaFilter . Map.fromList <$> foldrM fn [] constrs

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

elementOrList :: FromLuaStack a => a -> Lua [a]
elementOrList x = do
  let topOfStack = Lua.stackTop
  elementUnchanged <- Lua.isnil topOfStack
  if elementUnchanged
    then [x] <$ Lua.pop 1
    else do
       mbres <- Lua.peekEither topOfStack
       case mbres of
         Right res -> [res] <$ Lua.pop 1
         Left _    -> do
           typeCheck Lua.stackTop Lua.TypeTable
           Lua.toList topOfStack `finally` Lua.pop 1

-- | Try running a filter for the given element
tryFilter :: (Data a, FromLuaStack a, ToLuaStack a)
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
runFilterFunction :: ToLuaStack a => LuaFilterFunction -> a -> Lua ()
runFilterFunction lf x = do
  pushFilterFunction lf
  push x
  z <- Lua.pcall 1 1 Nothing
  when (z /= OK) $ do
    let addPrefix = ("Error while running filter function: " ++)
    Lua.throwTopMessageAsError' addPrefix

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

singleElement :: FromLuaStack a => a -> Lua a
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
        Lua.throwLuaError $
          "Error while trying to get a filter's return " ++
          "value from lua stack.\n" ++ err
