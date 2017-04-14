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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- |
   Module      : Text.Pandoc.Lua
   Copyright   : Copyright © 2017 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc lua utils.
-}
module Text.Pandoc.Lua ( runLuaFilter, pushPandocModule ) where

import Control.Monad ( (>=>), when )
import Control.Monad.Trans ( MonadIO(..) )
import Data.Map ( Map )
import Scripting.Lua ( LuaState, StackValue(..) )
import Text.Pandoc.Definition
import Text.Pandoc.Lua.PandocModule ( pushPandocModule )
import Text.Pandoc.Lua.StackInstances ()
import Text.Pandoc.Walk

import qualified Data.Map as Map
import qualified Scripting.Lua as Lua

runLuaFilter :: (MonadIO m)
             => FilePath -> [String] -> Pandoc -> m Pandoc
runLuaFilter filterPath args pd = liftIO $ do
  lua <- Lua.newstate
  Lua.openlibs lua
  -- create table in registry to store filter functions
  Lua.push lua "PANDOC_FILTER_FUNCTIONS"
  Lua.newtable lua
  Lua.rawset lua Lua.registryindex
  -- store module in global "pandoc"
  pushPandocModule lua
  Lua.setglobal lua "pandoc"
  status <- Lua.loadfile lua filterPath
  if (status /= 0)
    then do
      Just luaErrMsg <- Lua.peek lua 1
      error luaErrMsg
    else do
      Lua.call lua 0 1
      Just luaFilters <- Lua.peek lua (-1)
      Lua.push lua args
      Lua.setglobal lua "PandocParameters"
      doc <- runAll luaFilters pd
      Lua.close lua
      return doc

runAll :: [LuaFilter] -> Pandoc -> IO Pandoc
runAll [] = return
runAll (x:xs) = walkMWithLuaFilter x >=> runAll xs

walkMWithLuaFilter :: LuaFilter -> Pandoc -> IO Pandoc
walkMWithLuaFilter (LuaFilter lua inlineFnMap blockFnMap docFnMap) =
  walkM (execInlineLuaFilter lua inlineFnMap) >=>
  walkM (execBlockLuaFilter  lua blockFnMap)  >=>
  walkM (execDocLuaFilter    lua docFnMap)

type InlineFunctionMap = Map String (LuaFilterFunction Inline)
type BlockFunctionMap  = Map String (LuaFilterFunction Block)
type DocFunctionMap    = Map String (LuaFilterFunction Pandoc)
data LuaFilter =
  LuaFilter LuaState InlineFunctionMap BlockFunctionMap DocFunctionMap

newtype LuaFilterFunction a = LuaFilterFunction { functionIndex :: Int }

execDocLuaFilter :: LuaState
                 -> Map String (LuaFilterFunction Pandoc)
                 -> Pandoc -> IO Pandoc
execDocLuaFilter lua fnMap x = do
  let docFnName = "Doc"
  case Map.lookup docFnName fnMap of
    Nothing -> return x
    Just fn -> runLuaFilterFunction lua fn x

execBlockLuaFilter :: LuaState
                   -> Map String (LuaFilterFunction Block)
                   -> Block -> IO Block
execBlockLuaFilter lua fnMap x = do
  let runFn :: PushViaFilterFunction Block a => LuaFilterFunction Block -> a
      runFn fn = runLuaFilterFunction lua fn
  let tryFilter :: String -> (LuaFilterFunction Block -> IO Block) -> IO Block
      tryFilter fnName callFilterFn =
        case Map.lookup fnName fnMap of
          Nothing -> return x
          Just fn -> callFilterFn fn
  case x of
    HorizontalRule             -> tryFilter "HorizontalRule" runFn
    Null                       -> tryFilter "Null" runFn
    BlockQuote blcks           -> tryFilter "BlockQuote" $ \fn -> runFn fn blcks
    BulletList items           -> tryFilter "BulletList" $ \fn -> runFn fn items
    CodeBlock attr code        -> tryFilter "CodeBlock" $ \fn -> runFn fn attr code
    DefinitionList lst         -> tryFilter "DefinitionList" $ \fn -> runFn fn lst
    Div attr content           -> tryFilter "Div" $ \fn -> runFn fn content attr
    Header lvl attr inlns      -> tryFilter "Header" $ \fn -> runFn fn lvl inlns attr
    LineBlock inlns            -> tryFilter "LineBlock" $ \fn -> runFn fn inlns
    Para inlns                 -> tryFilter "Para" $ \fn -> runFn fn inlns
    Plain inlns                -> tryFilter "Plain" $ \fn -> runFn fn inlns
    RawBlock format str        -> tryFilter "RawBlock" $ \fn -> runFn fn format str
    OrderedList (num,sty,delim) items ->
      tryFilter "OrderedList" $ \fn -> runFn fn items (num,sty,delim)
    Table capt aligns widths headers rows ->
      tryFilter "Table" $ \fn -> runFn fn capt aligns widths headers rows

execInlineLuaFilter :: LuaState
                    -> Map String (LuaFilterFunction Inline)
                    -> Inline -> IO Inline
execInlineLuaFilter lua fnMap x = do
  let runFn :: PushViaFilterFunction Inline a => LuaFilterFunction Inline -> a
      runFn fn = runLuaFilterFunction lua fn
  let tryFilter :: String -> (LuaFilterFunction Inline -> IO Inline) -> IO Inline
      tryFilter fnName callFilterFn =
        case Map.lookup fnName fnMap of
          Nothing -> return x
          Just fn -> callFilterFn fn
  let tryFilterAlternatives :: [(String, LuaFilterFunction Inline -> IO Inline)] -> IO Inline
      tryFilterAlternatives [] = return x
      tryFilterAlternatives ((fnName, callFilterFn) : alternatives) =
        case Map.lookup fnName fnMap of
          Nothing -> tryFilterAlternatives alternatives
          Just fn -> callFilterFn fn
  case x of
    LineBreak                 -> tryFilter "LineBreak" runFn
    SoftBreak                 -> tryFilter "SoftBreak" runFn
    Space                     -> tryFilter "Space"     runFn
    Cite cs lst               -> tryFilter "Cite"      $ \fn -> runFn fn lst cs
    Code attr str             -> tryFilter "Code"      $ \fn -> runFn fn str attr
    Emph lst                  -> tryFilter "Emph"      $ \fn -> runFn fn lst
    Note blks                 -> tryFilter "Note"      $ \fn -> runFn fn blks
    RawInline f str           -> tryFilter "RawInline" $ \fn -> runFn fn f str
    SmallCaps lst             -> tryFilter "SmallCaps" $ \fn -> runFn fn lst
    Span attr lst             -> tryFilter "Span"      $ \fn -> runFn fn lst attr
    Str str                   -> tryFilter "Str"       $ \fn -> runFn fn str
    Strikeout lst             -> tryFilter "Strikeout" $ \fn -> runFn fn lst
    Strong lst                -> tryFilter "Strong"    $ \fn -> runFn fn lst
    Subscript lst             -> tryFilter "Subscript" $ \fn -> runFn fn lst
    Superscript lst           -> tryFilter "Superscript" $ \fn -> runFn fn lst
    Math DisplayMath lst      -> tryFilterAlternatives
                                 [ ("DisplayMath", \fn -> runFn fn lst)
                                 , ("Math", \fn -> runFn fn DisplayMath lst)
                                 ]
    Math InlineMath lst       -> tryFilterAlternatives
                                 [ ("InlineMath", \fn -> runFn fn lst)
                                 , ("Math", \fn -> runFn fn InlineMath lst)
                                 ]
    Quoted SingleQuote lst    -> tryFilterAlternatives
                                 [ ("SingleQuoted", \fn -> runFn fn lst)
                                 , ("Quoted", \fn -> runFn fn SingleQuote lst)
                                 ]
    Quoted DoubleQuote lst    -> tryFilterAlternatives
                                 [ ("DoubleQuoted", \fn -> runFn fn lst)
                                 , ("Quoted", \fn -> runFn fn DoubleQuote lst)
                                 ]
    Link attr txt (src, tit)  -> tryFilter "Link" $
                                 \fn -> runFn fn txt src tit attr
    Image attr alt (src, tit) -> tryFilter "Image" $
                                 \fn -> runFn fn alt src tit attr

instance StackValue LuaFilter where
  valuetype _ = Lua.TTABLE
  push = undefined
  peek lua i = do
    -- TODO: find a more efficient way of doing this in a typesafe manner.
    inlineFnMap <- Lua.peek lua i
    blockFnMap  <- Lua.peek lua i
    docFnMap    <- Lua.peek lua i
    return $ LuaFilter lua <$> inlineFnMap <*> blockFnMap <*> docFnMap

-- | Helper class for pushing a single value to the stack via a lua function.
-- See @pushViaCall@.
class PushViaFilterFunction a b where
  pushViaFilterFunction' :: LuaState -> LuaFilterFunction a -> IO () -> Int -> b

instance (StackValue a) => PushViaFilterFunction a (IO a) where
  pushViaFilterFunction' lua lf pushArgs num = do
    pushFilterFunction lua lf
    pushArgs
    Lua.call lua num 1
    mbres <- Lua.peek lua (-1)
    case mbres of
      Nothing -> error $ "Error while trying to get a filter's return "
                 ++ "value from lua stack."
      Just res -> res <$ Lua.pop lua 1

instance (PushViaFilterFunction a c, StackValue b) =>
         PushViaFilterFunction a (b -> c) where
  pushViaFilterFunction' lua lf pushArgs num x =
    pushViaFilterFunction' lua lf (pushArgs *> push lua x) (num + 1)

-- | Push an value to the stack via a lua filter function. The function is
-- called with all arguments that are passed to this function and is expected to
-- return a single value.
runLuaFilterFunction :: (StackValue a, PushViaFilterFunction a b)
                     => LuaState -> LuaFilterFunction a -> b
runLuaFilterFunction lua lf = pushViaFilterFunction' lua lf (return ()) 0

-- | Push the filter function to the top of the stack.
pushFilterFunction :: Lua.LuaState -> LuaFilterFunction a -> IO ()
pushFilterFunction lua lf = do
  -- The function is stored in a lua registry table, retrieve it from there.
  push lua ("PANDOC_FILTER_FUNCTIONS"::String)
  Lua.rawget lua Lua.registryindex
  Lua.rawgeti lua (-1) (functionIndex lf)
  Lua.remove lua (-2) -- remove registry table from stack

instance StackValue (LuaFilterFunction a) where
  valuetype _ = Lua.TFUNCTION
  push lua v = pushFilterFunction lua v
  peek lua i = do
    isFn <- Lua.isfunction lua i
    when (not isFn) (error $ "Not a function at index " ++ (show i))
    Lua.pushvalue lua i
    push lua ("PANDOC_FILTER_FUNCTIONS"::String)
    Lua.rawget lua Lua.registryindex
    len <- Lua.objlen lua (-1)
    Lua.insert lua (-2)
    Lua.rawseti lua (-2) (len + 1)
    Lua.pop lua 1
    return . Just $ LuaFilterFunction (len + 1)
