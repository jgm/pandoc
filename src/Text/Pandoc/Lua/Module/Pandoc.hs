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
import Control.Monad ((>=>), when)
import Control.Monad.Except (throwError)
import Data.Default (Default (..))
import Data.Maybe (fromMaybe)
import HsLua as Lua hiding (pushModule)
import HsLua.Class.Peekable (PeekError)
import System.Exit (ExitCode (..))
import Text.Pandoc.Class.PandocIO (runIO)
import Text.Pandoc.Definition (Block, Inline)
import Text.Pandoc.Lua.Filter (SingletonsList (..), walkInlines,
                               walkInlineLists, walkBlocks, walkBlockLists)
import Text.Pandoc.Lua.Marshaling ()
import Text.Pandoc.Lua.Marshaling.AST
import Text.Pandoc.Lua.Marshaling.List (List (..))
import Text.Pandoc.Lua.PandocLua (PandocLua, addFunction, liftPandocLua,
                                  loadDefaultModule)
import Text.Pandoc.Options (ReaderOptions (readerExtensions))
import Text.Pandoc.Process (pipeProcess)
import Text.Pandoc.Readers (Reader (..), getReader)
import Text.Pandoc.Walk (Walkable)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Text.Pandoc.Lua.Util as LuaUtil
import Text.Pandoc.Error

-- | Push the "pandoc" package to the Lua stack. Requires the `List`
-- module to be loadable.
pushModule :: PandocLua NumResults
pushModule = do
  loadDefaultModule "pandoc"
  addFunction "read" read
  addFunction "pipe" pipe
  addFunction "walk_block" (walkElement peekBlock pushBlock)
  addFunction "walk_inline" (walkElement peekInline pushInline)
  return 1

walkElement :: (Walkable (SingletonsList Inline) a,
                Walkable (SingletonsList Block) a,
                Walkable (List Inline) a,
                Walkable (List Block) a)
            => Peeker PandocError a -> Pusher PandocError a
            -> LuaE PandocError NumResults
walkElement peek' push' = do
  x <- forcePeek $ peek' (nthBottom 1)
  f <- peek (nthBottom 2)
  let walk' =  walkInlines f
           >=> walkInlineLists f
           >=> walkBlocks f
           >=> walkBlockLists f
  walk' x >>= push'
  return (NumResults 1)

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
    ExitFailure n -> do
      pushPipeError (PipeError (T.pack command) n output)
      Lua.error

data PipeError = PipeError
  { pipeErrorCommand :: T.Text
  , pipeErrorCode :: Int
  , pipeErrorOutput :: BL.ByteString
  }

peekPipeError :: PeekError e => StackIndex -> LuaE e PipeError
peekPipeError idx =
  PipeError
  <$> (Lua.getfield idx "command"    *> Lua.peek (-1) <* Lua.pop 1)
  <*> (Lua.getfield idx "error_code" *> Lua.peek (-1) <* Lua.pop 1)
  <*> (Lua.getfield idx "output"     *> Lua.peek (-1) <* Lua.pop 1)

pushPipeError :: PeekError e => Pusher e PipeError
pushPipeError pipeErr = do
  Lua.newtable
  LuaUtil.addField "command" (pipeErrorCommand pipeErr)
  LuaUtil.addField "error_code" (pipeErrorCode pipeErr)
  LuaUtil.addField "output" (pipeErrorOutput pipeErr)
  pushPipeErrorMetaTable
  Lua.setmetatable (-2)
    where
      pushPipeErrorMetaTable :: PeekError e => LuaE e ()
      pushPipeErrorMetaTable = do
        v <- Lua.newmetatable "pandoc pipe error"
        when v $ do
          pushName "__tostring"
          pushHaskellFunction pipeErrorMessage
          rawset (nth 3)

      pipeErrorMessage :: PeekError e => LuaE e NumResults
      pipeErrorMessage = do
        (PipeError cmd errorCode output) <- peekPipeError (nthBottom 1)
        pushByteString . BSL.toStrict . BSL.concat $
          [ BSL.pack "Error running "
          , BSL.pack $ T.unpack cmd
          , BSL.pack " (error code "
          , BSL.pack $ show errorCode
          , BSL.pack "): "
          , if output == mempty then BSL.pack "<no output>" else output
          ]
        return (NumResults 1)
