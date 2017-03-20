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

import Control.Monad ( (>=>), when )
import Control.Monad.Trans ( MonadIO(..) )
import Data.Aeson ( FromJSON(..), ToJSON(..), Result(..), Value, fromJSON )
import Data.HashMap.Lazy ( HashMap )
import Data.Text ( Text, pack, unpack )
import Data.Text.Encoding ( decodeUtf8 )
import Scripting.Lua ( LuaState, StackValue(..) )
import Scripting.Lua.Aeson ()
import Text.Pandoc.Definition ( Block(..), Inline(..), Pandoc(..) )
import Text.Pandoc.Lua.PandocModule
import Text.Pandoc.Walk

import qualified Data.HashMap.Lazy as HashMap
import qualified Scripting.Lua as Lua
import qualified Scripting.Lua as LuaAeson

runLuaFilter :: (MonadIO m)
             => FilePath -> [String] -> Pandoc -> m Pandoc
runLuaFilter filterPath args pd = liftIO $ do
  lua <- LuaAeson.newstate
  Lua.openlibs lua
  Lua.newtable lua
  Lua.setglobal lua "PANDOC_FILTER_FUNCTIONS"  -- hack, store functions here
  pushPandocModule lua
  Lua.setglobal lua "pandoc"
  status <- Lua.loadfile lua filterPath
  if (status /= 0)
    then do
      luaErrMsg <- unpack . decodeUtf8 <$> Lua.tostring lua 1
      error luaErrMsg
    else do
      Lua.call lua 0 1
      Just luaFilters <- Lua.peek lua (-1)
      Lua.push lua (map pack args)
      Lua.setglobal lua "PandocParameters"
      doc <- runAll luaFilters >=> luaFilter lua "filter_doc" $ pd
      Lua.close lua
      return doc

runAll :: [LuaFilter] -> Pandoc -> IO Pandoc
runAll [] = return
runAll (x:xs) = walkMWithLuaFilter x >=> runAll xs

luaFilter :: Lua.LuaState -> String -> Pandoc -> IO Pandoc
luaFilter lua luaFn x = do
  fnExists <- isLuaFunction lua luaFn
  if fnExists
    then walkM (Lua.callfunc lua luaFn :: Pandoc -> IO Pandoc) x
    else return x

walkMWithLuaFilter :: LuaFilter -> Pandoc -> IO Pandoc
walkMWithLuaFilter (LuaFilter lua inlineFnMap blockFnMap docFnMap) =
  walkM (execInlineLuaFilter lua inlineFnMap) >=>
  walkM (execBlockLuaFilter  lua blockFnMap)  >=>
  walkM (execDocLuaFilter    lua docFnMap)

type InlineFunctionMap = HashMap Text (LuaFilterFunction Inline)
type BlockFunctionMap  = HashMap Text (LuaFilterFunction Block)
type DocFunctionMap    = HashMap Text (LuaFilterFunction Pandoc)
data LuaFilter =
  LuaFilter LuaState InlineFunctionMap BlockFunctionMap DocFunctionMap

newtype LuaFilterFunction a = LuaFilterFunction { functionIndex :: Int }

execDocLuaFilter :: LuaState
                 -> HashMap Text (LuaFilterFunction Pandoc)
                 -> Pandoc -> IO Pandoc
execDocLuaFilter lua fnMap x = do
  let docFnName = "Doc"
  case HashMap.lookup docFnName fnMap of
    Nothing -> return x
    Just fn -> runLuaFilterFunction lua fn x

execBlockLuaFilter :: LuaState
                   -> HashMap Text (LuaFilterFunction Block)
                   -> Block -> IO Block
execBlockLuaFilter lua fnMap x = do
  let filterOrId constr = case HashMap.lookup constr fnMap of
                            Nothing -> return x
                            Just fn -> runLuaFilterFunction lua fn x
  case x of
    Plain _          -> filterOrId "Plain"
    Para _           -> filterOrId "Para"
    LineBlock _      -> filterOrId "LineBlock"
    CodeBlock _ _    -> filterOrId "CodeBlock"
    RawBlock _ _     -> filterOrId "RawBlock"
    BlockQuote _     -> filterOrId "BlockQuote"
    OrderedList _ _  -> filterOrId "OrderedList"
    BulletList _     -> filterOrId "BulletList"
    DefinitionList _ -> filterOrId "DefinitionList"
    Header _ _ _     -> filterOrId "Header"
    HorizontalRule   -> filterOrId "HorizontalRule"
    Table _ _ _ _ _  -> filterOrId "Table"
    Div _ _          -> filterOrId "Div"
    Null             -> filterOrId "Null"

execInlineLuaFilter :: LuaState
                    -> HashMap Text (LuaFilterFunction Inline)
                    -> Inline -> IO Inline
execInlineLuaFilter lua fnMap x = do
  let filterOrId constr = case HashMap.lookup constr fnMap of
                            Nothing -> return x
                            Just fn -> runLuaFilterFunction lua fn x
  case x of
    Cite _ _      -> filterOrId "Cite"
    Code _ _      -> filterOrId "Code"
    Emph _        -> filterOrId "Emph"
    Image _ _ _   -> filterOrId "Image"
    LineBreak     -> filterOrId "LineBreak"
    Link _ _ _    -> filterOrId "Link"
    Math _ _      -> filterOrId "Math"
    Note _        -> filterOrId "Note"
    Quoted _ _    -> filterOrId "Quoted"
    RawInline _ _ -> filterOrId "RawInline"
    SmallCaps _   -> filterOrId "SmallCaps"
    SoftBreak     -> filterOrId "SoftBreak"
    Space         -> filterOrId "Space"
    Span _ _      -> filterOrId "Span"
    Str _         -> filterOrId "Str"
    Strikeout _   -> filterOrId "Strikeout"
    Strong _      -> filterOrId "Strong"
    Subscript _   -> filterOrId "Subscript"
    Superscript _ -> filterOrId "Superscript"

instance StackValue LuaFilter where
  valuetype _ = Lua.TTABLE
  push = undefined
  peek lua i = do
    -- TODO: find a more efficient way of doing this in a typesafe manner.
    inlineFnMap <- Lua.peek lua i
    blockFnMap  <- Lua.peek lua i
    docFnMap    <- Lua.peek lua i
    return $ LuaFilter lua <$> inlineFnMap <*> blockFnMap <*> docFnMap

runLuaFilterFunction :: (StackValue a)
                     => LuaState -> LuaFilterFunction a -> a -> IO a
runLuaFilterFunction lua lf inline = do
  pushFilterFunction lua lf
  Lua.push lua inline
  Lua.call lua 1 1
  Just res <- Lua.peek lua (-1)
  Lua.pop lua 1
  return res

-- FIXME: use registry
pushFilterFunction :: Lua.LuaState -> LuaFilterFunction a -> IO ()
pushFilterFunction lua lf = do
  Lua.getglobal lua "PANDOC_FILTER_FUNCTIONS"
  Lua.rawgeti lua (-1) (functionIndex lf)
  Lua.remove lua (-2) -- remove global from stack

instance StackValue (LuaFilterFunction a) where
  valuetype _ = Lua.TFUNCTION
  push lua v = pushFilterFunction lua v
  peek lua i = do
    isFn <- Lua.isfunction lua i
    when (not isFn) (error $ "Not a function at index " ++ (show i))
    Lua.pushvalue lua i
    Lua.getglobal lua "PANDOC_FILTER_FUNCTIONS"
    len <- Lua.objlen lua (-1)
    Lua.insert lua (-2)
    Lua.rawseti lua (-2) (len + 1)
    Lua.pop lua 1
    return . Just $ LuaFilterFunction (len + 1)


isLuaFunction :: Lua.LuaState -> String -> IO Bool
isLuaFunction lua fnName = do
  Lua.getglobal lua fnName
  res <- Lua.isfunction lua (-1)
  Lua.pop lua (-1)
  return res

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
