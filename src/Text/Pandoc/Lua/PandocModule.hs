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
#if !MIN_VERSION_hslua(0,9,0)
{-# OPTIONS_GHC -fno-warn-orphans #-}
#endif
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

import Control.Monad (unless, zipWithM_)
import Data.ByteString.Char8 (unpack)
import Data.Default (Default (..))
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Foreign.Lua (Lua, FromLuaStack, ToLuaStack, NumResults, liftIO)
import Text.Pandoc.Class (readDataFile, runIO,
                          runIOorExplode, setUserDataDir, CommonState(..),
                          putCommonState, fetchItem, setMediaBag)
import Text.Pandoc.Options (ReaderOptions(readerExtensions))
import Text.Pandoc.Lua.StackInstances ()
import Text.Pandoc.Readers (Reader (..), getReader)
import Text.Pandoc.MIME (MimeType)
import Text.Pandoc.Process (pipeProcess)
import System.Exit (ExitCode(..))
import Data.Digest.Pure.SHA (sha1, showDigest)

import qualified Foreign.Lua as Lua
import qualified Data.ByteString.Lazy as BL
import qualified Text.Pandoc.MediaBag as MB

-- | Push the "pandoc" on the lua stack.
pushPandocModule :: Maybe FilePath -> Lua ()
pushPandocModule datadir = do
  script <- liftIO (pandocModuleScript datadir)
  status <- Lua.loadstring script
  unless (status /= Lua.OK) $ Lua.call 0 1
  Lua.push "__read"
  Lua.pushHaskellFunction readDoc
  Lua.rawset (-3)
  Lua.push "sha1"
  Lua.pushHaskellFunction sha1HashFn
  Lua.rawset (-3)
  Lua.push "pipe"
  Lua.pushHaskellFunction pipeFn
  Lua.rawset (-3)

-- | Get the string representation of the pandoc module
pandocModuleScript :: Maybe FilePath -> IO String
pandocModuleScript datadir = unpack <$>
  runIOorExplode (setUserDataDir datadir >> readDataFile "pandoc.lua")

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
pushMediaBagModule :: CommonState -> IORef MB.MediaBag -> Lua ()
pushMediaBagModule commonState mediaBagRef = do
  Lua.newtable
  addFunction "insert" (insertMediaFn mediaBagRef)
  addFunction "lookup" (lookupMediaFn mediaBagRef)
  addFunction "list" (mediaDirectoryFn mediaBagRef)
  addFunction "fetch" (fetch commonState mediaBagRef)
  return ()
 where
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

#if !MIN_VERSION_hslua(0,9,0)
instance ToLuaStack BL.ByteString where
  push = Lua.push . BL.toStrict

instance FromLuaStack BL.ByteString where
  peek = fmap BL.fromStrict . Lua.peek
#endif
