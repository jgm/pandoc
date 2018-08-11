{-# LANGUAGE NoImplicitPrelude #-}
{-
Copyright © 2017-2018 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

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
   Copyright   : Copyright © 2017-2018 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc module for lua.
-}
module Text.Pandoc.Lua.Module.Pandoc
  ( pushModule
  ) where

import Prelude
import Control.Monad (when)
import Data.Default (Default (..))
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Foreign.Lua (ToLuaStack, FromLuaStack, Lua, NumResults, Optional, liftIO)
import System.Exit (ExitCode (..))
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Definition (Block, Inline)
import Text.Pandoc.Lua.Filter (walkInlines, walkBlocks, LuaFilter)
import Text.Pandoc.Lua.StackInstances ()
import Text.Pandoc.Lua.Util (addFunction, loadScriptFromDataDir)
import Text.Pandoc.Walk (Walkable)
import Text.Pandoc.Options (ReaderOptions (readerExtensions))
import Text.Pandoc.Process (pipeProcess)
import Text.Pandoc.Readers (Reader (..), getReader)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Foreign.Lua as Lua
import qualified Text.Pandoc.Lua.Util as LuaUtil

-- | Push the "pandoc" on the lua stack. Requires the `list` module to be
-- loaded.
pushModule :: Maybe FilePath -> Lua NumResults
pushModule datadir = do
  loadScriptFromDataDir datadir "pandoc.lua"
  addFunction "read" readDoc
  addFunction "pipe" pipeFn
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

readDoc :: String -> Optional String -> Lua NumResults
readDoc content formatSpecOrNil = do
  let formatSpec = fromMaybe "markdown" (Lua.fromOptional formatSpecOrNil)
  case getReader formatSpec of
    Left  s      -> Lua.raiseError s -- Unknown reader
    Right (reader, es) ->
      case reader of
        TextReader r -> do
          res <- liftIO $ runIO $ r def{ readerExtensions = es } (pack content)
          case res of
            Right pd -> (1 :: NumResults) <$ Lua.push pd -- success, push Pandoc
            Left s   -> Lua.raiseError (show s)          -- error while reading
        _  -> Lua.raiseError "Only string formats are supported at the moment."

-- | Pipes input through a command.
pipeFn :: String
       -> [String]
       -> BL.ByteString
       -> Lua NumResults
pipeFn command args input = do
  (ec, output) <- liftIO $ pipeProcess Nothing command args input
  case ec of
    ExitSuccess -> 1 <$ Lua.push output
    ExitFailure n -> Lua.raiseError (PipeError command n output)

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
    LuaUtil.addField "command" (pipeErrorCommand pipeErr)
    LuaUtil.addField "error_code" (pipeErrorCode pipeErr)
    LuaUtil.addField "output" (pipeErrorOutput pipeErr)
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
