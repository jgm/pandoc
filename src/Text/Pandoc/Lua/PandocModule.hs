{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
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
{-# LANGUAGE CPP #-}
{- |
   Module      : Text.Pandoc.Lua.PandocModule
   Copyright   : Copyright © 2017 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc module for lua.
-}
module Text.Pandoc.Lua.PandocModule
  ( pushPandocModule
  , pushMediaBagModule
  ) where

import Control.Monad (zipWithM_)
import Data.Default (Default (..))
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.IORef (IORef, modifyIORef', readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Foreign.Lua (ToLuaStack, FromLuaStack, Lua, NumResults, liftIO)
import Foreign.Lua.FunctionCalling (ToHaskellFunction)
import System.Exit (ExitCode (..))
import Text.Pandoc.Class (CommonState (..), fetchItem, putCommonState,
                          runIO, runIOorExplode, setMediaBag)
import Text.Pandoc.Definition (Block, Inline)
import Text.Pandoc.Lua.Filter (walkInlines, walkBlocks, LuaFilter)
import Text.Pandoc.Lua.StackInstances ()
import Text.Pandoc.Lua.Util (loadScriptFromDataDir)
import Text.Pandoc.Walk (Walkable)
import Text.Pandoc.MIME (MimeType)
import Text.Pandoc.Options (ReaderOptions (readerExtensions))
import Text.Pandoc.Process (pipeProcess)
import Text.Pandoc.Readers (Reader (..), getReader)

import qualified Data.ByteString.Lazy as BL
import qualified Foreign.Lua as Lua
import qualified Text.Pandoc.MediaBag as MB

-- | Push the "pandoc" on the lua stack. Requires the `list` module to be
-- loaded.
pushPandocModule :: Maybe FilePath -> Lua NumResults
pushPandocModule datadir = do
  loadScriptFromDataDir datadir "pandoc.lua"
  addFunction "_pipe" pipeFn
  addFunction "_read" readDoc
  addFunction "sha1" sha1HashFn
  addFunction "walk_block" walkBlock
  addFunction "walk_inline" walkInline
  return 1

walkElement :: (ToLuaStack a, Walkable [Inline] a, Walkable [Block] a)
            => a -> LuaFilter -> Lua a
walkElement x f = walkInlines f x >>= walkBlocks f

walkInline :: Inline -> LuaFilter -> Lua Inline
walkInline = walkElement

walkBlock :: Block -> LuaFilter -> Lua Block
walkBlock = walkElement

readDoc :: String -> String -> Lua NumResults
readDoc formatSpec content = do
  case getReader formatSpec of
    Left  s      -> Lua.push s -- Unknown reader
    Right (reader, es) ->
      case reader of
        TextReader r -> do
          res <- liftIO $ runIO $ r def{ readerExtensions = es } (pack content)
          case res of
            Left s   -> Lua.push $ show s -- error while reading
            Right pd -> Lua.push pd       -- success, push Pandoc
        _  -> Lua.push "Only string formats are supported at the moment."
  return 1

--
-- MediaBag submodule
--
pushMediaBagModule :: CommonState -> IORef MB.MediaBag -> Lua NumResults
pushMediaBagModule commonState mediaBagRef = do
  Lua.newtable
  addFunction "insert" (insertMediaFn mediaBagRef)
  addFunction "lookup" (lookupMediaFn mediaBagRef)
  addFunction "list" (mediaDirectoryFn mediaBagRef)
  addFunction "fetch" (fetch commonState mediaBagRef)
  return 1

addFunction :: ToHaskellFunction a => String -> a -> Lua ()
addFunction name fn = do
  Lua.push name
  Lua.pushHaskellFunction fn
  Lua.rawset (-3)

sha1HashFn :: BL.ByteString
           -> Lua NumResults
sha1HashFn contents = do
  Lua.push $ showDigest (sha1 contents)
  return 1

pipeFn :: String
       -> [String]
       -> BL.ByteString
       -> Lua NumResults
pipeFn command args input = do
  (ec, output) <- liftIO $ pipeProcess Nothing command args input
  Lua.push $ case ec of
                  ExitSuccess   -> 0
                  ExitFailure n -> n
  Lua.push output
  return 2

insertMediaFn :: IORef MB.MediaBag
              -> FilePath
              -> OrNil MimeType
              -> BL.ByteString
              -> Lua NumResults
insertMediaFn mbRef fp nilOrMime contents = do
  liftIO . modifyIORef' mbRef $
    MB.insertMedia fp (toMaybe nilOrMime) contents
  return 0

lookupMediaFn :: IORef MB.MediaBag
              -> FilePath
              -> Lua NumResults
lookupMediaFn mbRef fp = do
  res <- MB.lookupMedia fp <$> liftIO (readIORef mbRef)
  case res of
    Nothing -> Lua.pushnil *> return 1
    Just (mimeType, contents) -> do
      Lua.push mimeType
      Lua.push contents
      return 2

mediaDirectoryFn :: IORef MB.MediaBag
                 -> Lua NumResults
mediaDirectoryFn mbRef = do
  dirContents <- MB.mediaDirectory <$> liftIO (readIORef mbRef)
  Lua.newtable
  zipWithM_ addEntry [1..] dirContents
  return 1
 where
  addEntry :: Int -> (FilePath, MimeType, Int) -> Lua ()
  addEntry idx (fp, mimeType, contentLength) = do
    Lua.newtable
    Lua.push "path" *> Lua.push fp *> Lua.rawset (-3)
    Lua.push "type" *> Lua.push mimeType *> Lua.rawset (-3)
    Lua.push "length" *> Lua.push contentLength *> Lua.rawset (-3)
    Lua.rawseti (-2) idx

fetch :: CommonState
      -> IORef MB.MediaBag
      -> String
      -> Lua NumResults
fetch commonState mbRef src = do
  mediaBag <- liftIO $ readIORef mbRef
  (bs, mimeType) <- liftIO . runIOorExplode $ do
    putCommonState commonState
    setMediaBag mediaBag
    fetchItem src
  Lua.push $ fromMaybe "" mimeType
  Lua.push bs
  return 2 -- returns 2 values: contents, mimetype

--
-- Helper types and orphan instances
--

newtype OrNil a = OrNil { toMaybe :: Maybe a }

instance FromLuaStack a => FromLuaStack (OrNil a) where
  peek idx = do
    noValue <- Lua.isnil idx
    if noValue
      then return (OrNil Nothing)
      else OrNil . Just <$> Lua.peek idx
