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
{- |
   Module      : Text.Pandoc.Lua.Module.Pandoc
   Copyright   : Copyright © 2017 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc module for lua.
-}
module Text.Pandoc.Lua.Module.Pandoc
  ( pushModule
  , pushMediaBagModule
  ) where

import Control.Monad (when, zipWithM_)
import Data.Default (Default (..))
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.IORef (IORef, modifyIORef', readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Foreign.Lua (ToLuaStack, FromLuaStack, Lua, NumResults, liftIO)
import System.Exit (ExitCode (..))
import Text.Pandoc.Class (CommonState (..), fetchItem, putCommonState,
                          runIO, runIOorExplode, setMediaBag)
import Text.Pandoc.Definition (Block, Inline)
import Text.Pandoc.Lua.Filter (walkInlines, walkBlocks, LuaFilter)
import Text.Pandoc.Lua.StackInstances ()
import Text.Pandoc.Lua.Util (OrNil (toMaybe), addFunction, addValue,
                             loadScriptFromDataDir, raiseError)
import Text.Pandoc.Walk (Walkable)
import Text.Pandoc.MIME (MimeType)
import Text.Pandoc.Options (ReaderOptions (readerExtensions))
import Text.Pandoc.Process (pipeProcess)
import Text.Pandoc.Readers (Reader (..), getReader)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Foreign.Lua as Lua
import qualified Text.Pandoc.MediaBag as MB

-- | Push the "pandoc" on the lua stack. Requires the `list` module to be
-- loaded.
pushModule :: Maybe FilePath -> Lua NumResults
pushModule datadir = do
  loadScriptFromDataDir datadir "pandoc.lua"
  addFunction "read" readDoc
  addFunction "pipe" pipeFn
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

readDoc :: String -> OrNil String -> Lua NumResults
readDoc content formatSpecOrNil = do
  let formatSpec = fromMaybe "markdown" (toMaybe formatSpecOrNil)
  case getReader formatSpec of
    Left  s      -> raiseError s -- Unknown reader
    Right (reader, es) ->
      case reader of
        TextReader r -> do
          res <- liftIO $ runIO $ r def{ readerExtensions = es } (pack content)
          case res of
            Right pd -> (1 :: NumResults) <$ Lua.push pd -- success, push Pandoc
            Left s   -> raiseError (show s)              -- error while reading
        _  -> raiseError "Only string formats are supported at the moment."

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

sha1HashFn :: BL.ByteString
           -> Lua NumResults
sha1HashFn contents = do
  Lua.push $ showDigest (sha1 contents)
  return 1

-- | Pipes input through a command.
pipeFn :: String
       -> [String]
       -> BL.ByteString
       -> Lua NumResults
pipeFn command args input = do
  (ec, output) <- liftIO $ pipeProcess Nothing command args input
  case ec of
    ExitSuccess -> 1 <$ Lua.push output
    ExitFailure n -> raiseError (PipeError command n output)

data PipeError = PipeError
  { pipeErrorCommand :: String
  , pipeErrorCode :: Int
  , pipeErrorOutput :: BL.ByteString
  }

instance FromLuaStack PipeError where
  peek idx =
    PipeError
    <$> (Lua.getfield idx "command"    *> Lua.peek (-1) <* Lua.pop 1)
    <*> (Lua.getfield idx "error_code" *> Lua.peek (-1) <* Lua.pop 1)
    <*> (Lua.getfield idx "output"     *> Lua.peek (-1) <* Lua.pop 1)

instance ToLuaStack PipeError where
  push pipeErr = do
    Lua.newtable
    addValue "command" (pipeErrorCommand pipeErr)
    addValue "error_code" (pipeErrorCode pipeErr)
    addValue "output" (pipeErrorOutput pipeErr)
    pushPipeErrorMetaTable
    Lua.setmetatable (-2)
      where
        pushPipeErrorMetaTable :: Lua ()
        pushPipeErrorMetaTable = do
          v <- Lua.newmetatable "pandoc pipe error"
          when v $ addFunction "__tostring" pipeErrorMessage

        pipeErrorMessage :: PipeError -> Lua BL.ByteString
        pipeErrorMessage (PipeError cmd errorCode output) = return $ mconcat
          [ BSL.pack "Error running "
          , BSL.pack cmd
          , BSL.pack " (error code "
          , BSL.pack $ show errorCode
          , BSL.pack "): "
          , if output == mempty then BSL.pack "<no output>" else output
          ]
-- end: pipe

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
