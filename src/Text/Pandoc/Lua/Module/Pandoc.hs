{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{- |
   Module      : Text.Pandoc.Lua.Module.Pandoc
   Copyright   : Copyright © 2017-2019 Albert Krewinkel
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
import Control.Monad.Except (throwError)
import Data.Default (Default (..))
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Foreign.Lua (Lua, NumResults, Optional, Peekable, Pushable)
import System.Exit (ExitCode (..))
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Definition (Block, Inline)
import Text.Pandoc.Lua.Filter (walkInlines, walkBlocks, LuaFilter, SingletonsList (..))
import Text.Pandoc.Lua.Marshaling ()
import Text.Pandoc.Walk (Walkable)
-- import Text.Pandoc.Options (ReaderOptions (readerExtensions)) TODO text: restore
import Text.Pandoc.Process (pipeProcess)
import Text.Pandoc.Readers (Reader (..), getReader)

-- TODO text: remove
import Text.Pandoc.Legacy.Options
--

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Foreign.Lua as Lua
import qualified Text.Pandoc.Lua.Util as LuaUtil
import Text.Pandoc.Error

-- | Push the "pandoc" on the lua stack. Requires the `list` module to be
-- loaded.
pushModule :: Maybe FilePath -> Lua NumResults
pushModule datadir = do
  LuaUtil.loadScriptFromDataDir datadir "pandoc.lua"
  LuaUtil.addFunction "read" readDoc
  LuaUtil.addFunction "pipe" pipeFn
  LuaUtil.addFunction "walk_block" walkBlock
  LuaUtil.addFunction "walk_inline" walkInline
  return 1

walkElement :: (Walkable (SingletonsList Inline) a,
                Walkable (SingletonsList Block) a)
            => a -> LuaFilter -> Lua a
walkElement x f = walkInlines f x >>= walkBlocks f

walkInline :: Inline -> LuaFilter -> Lua Inline
walkInline = walkElement

walkBlock :: Block -> LuaFilter -> Lua Block
walkBlock = walkElement

readDoc :: String -> Optional String -> Lua NumResults
readDoc content formatSpecOrNil = do
  let formatSpec = fromMaybe "markdown" (Lua.fromOptional formatSpecOrNil)
  res <- Lua.liftIO . runIO $
           getReader formatSpec >>= \(rdr,es) ->
             case rdr of
               TextReader r ->
                 r def{ readerExtensions = es } (pack content)
               _ -> throwError $ PandocSomeError $
                      "Only textual formats are supported"
  case res of
    Right pd -> (1 :: NumResults) <$ Lua.push pd -- success, push Pandoc
    Left  (PandocUnknownReaderError f) -> Lua.raiseError $
       "Unknown reader: " ++ f
    Left  (PandocUnsupportedExtensionError e f) -> Lua.raiseError $
       "Extension " ++ e ++ " not supported for " ++ f
    Left  e      -> Lua.raiseError $ show e

-- | Pipes input through a command.
pipeFn :: String
       -> [String]
       -> BL.ByteString
       -> Lua NumResults
pipeFn command args input = do
  (ec, output) <- Lua.liftIO $ pipeProcess Nothing command args input
  case ec of
    ExitSuccess -> 1 <$ Lua.push output
    ExitFailure n -> Lua.raiseError (PipeError command n output)

data PipeError = PipeError
  { pipeErrorCommand :: String
  , pipeErrorCode :: Int
  , pipeErrorOutput :: BL.ByteString
  }

instance Peekable PipeError where
  peek idx =
    PipeError
    <$> (Lua.getfield idx "command"    *> Lua.peek (-1) <* Lua.pop 1)
    <*> (Lua.getfield idx "error_code" *> Lua.peek (-1) <* Lua.pop 1)
    <*> (Lua.getfield idx "output"     *> Lua.peek (-1) <* Lua.pop 1)

instance Pushable PipeError where
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
          when v $ LuaUtil.addFunction "__tostring" pipeErrorMessage

        pipeErrorMessage :: PipeError -> Lua BL.ByteString
        pipeErrorMessage (PipeError cmd errorCode output) = return $ mconcat
          [ BSL.pack "Error running "
          , BSL.pack cmd
          , BSL.pack " (error code "
          , BSL.pack $ show errorCode
          , BSL.pack "): "
          , if output == mempty then BSL.pack "<no output>" else output
          ]
