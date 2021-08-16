{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Lua.Module.Pandoc
   Copyright   : Copyright © 2017-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Pandoc module for lua.
-}
module Text.Pandoc.Lua.Module.Pandoc
  ( pushModule
  ) where

import Prelude hiding (read)
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Data.Default (Default (..))
import Data.Maybe (fromMaybe)
import Foreign.Lua (Lua, NumResults, Optional, Peekable, Pushable)
import System.Exit (ExitCode (..))
import Text.Pandoc.Class.PandocIO (runIO)
import Text.Pandoc.Definition (Block, Inline)
import Text.Pandoc.Lua.Filter (LuaFilter, SingletonsList (..), walkInlines,
                               walkInlineLists, walkBlocks, walkBlockLists)
import Text.Pandoc.Lua.Marshaling ()
import Text.Pandoc.Lua.Marshaling.List (List (..))
import Text.Pandoc.Lua.PandocLua (PandocLua, addFunction, liftPandocLua,
                                  loadDefaultModule)
import Text.Pandoc.Walk (Walkable)
import Text.Pandoc.Options (ReaderOptions (readerExtensions))
import Text.Pandoc.Process (pipeProcess)
import Text.Pandoc.Readers (Reader (..), getReader)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Foreign.Lua as Lua
import qualified Text.Pandoc.Lua.Util as LuaUtil
import Text.Pandoc.Error

-- | Push the "pandoc" package to the Lua stack. Requires the `List`
-- module to be loadable.
pushModule :: PandocLua NumResults
pushModule = do
  loadDefaultModule "pandoc"
  addFunction "read" read
  addFunction "pipe" pipe
  addFunction "walk_block" walk_block
  addFunction "walk_inline" walk_inline
  return 1

walkElement :: (Walkable (SingletonsList Inline) a,
                Walkable (SingletonsList Block) a,
                Walkable (List Inline) a,
                Walkable (List Block) a)
            => a -> LuaFilter -> PandocLua a
walkElement x f = liftPandocLua $
  walkInlines f x >>= walkInlineLists f >>= walkBlocks f >>= walkBlockLists f

walk_inline :: Inline -> LuaFilter -> PandocLua Inline
walk_inline = walkElement

walk_block :: Block -> LuaFilter -> PandocLua Block
walk_block = walkElement

read :: T.Text -> Optional T.Text -> PandocLua NumResults
read content formatSpecOrNil = liftPandocLua $ do
  let formatSpec = fromMaybe "markdown" (Lua.fromOptional formatSpecOrNil)
  res <- Lua.liftIO . runIO $
           getReader formatSpec >>= \(rdr,es) ->
             case rdr of
               TextReader r ->
                 r def{ readerExtensions = es } content
               _ -> throwError $ PandocSomeError
                      "Only textual formats are supported"
  case res of
    Right pd -> (1 :: NumResults) <$ Lua.push pd -- success, push Pandoc
    Left  (PandocUnknownReaderError f) -> Lua.raiseError $
       "Unknown reader: " <> f
    Left  (PandocUnsupportedExtensionError e f) -> Lua.raiseError $
       "Extension " <> e <> " not supported for " <> f
    Left  e      -> Lua.raiseError $ show e

-- | Pipes input through a command.
pipe :: String           -- ^ path to executable
     -> [String]         -- ^ list of arguments
     -> BL.ByteString    -- ^ input passed to process via stdin
     -> PandocLua NumResults
pipe command args input = liftPandocLua $ do
  (ec, output) <- Lua.liftIO $ pipeProcess Nothing command args input
  case ec of
    ExitSuccess -> 1 <$ Lua.push output
    ExitFailure n -> Lua.raiseError (PipeError (T.pack command) n output)

data PipeError = PipeError
  { pipeErrorCommand :: T.Text
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
          , BSL.pack $ T.unpack cmd
          , BSL.pack " (error code "
          , BSL.pack $ show errorCode
          , BSL.pack "): "
          , if output == mempty then BSL.pack "<no output>" else output
          ]
