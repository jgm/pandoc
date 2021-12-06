{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications  #-}
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
  , documentedModule
  ) where

import Prelude hiding (read)
import Control.Monad (forM_, when)
import Control.Monad.Catch (catch, throwM)
import Control.Monad.Except (throwError)
import Data.Data (Data, dataTypeConstrs, dataTypeOf, showConstr)
import Data.Default (Default (..))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import HsLua hiding (pushModule)
import HsLua.Class.Peekable (PeekError)
import System.Exit (ExitCode (..))
import Text.Pandoc.Class.PandocIO (runIO)
import Text.Pandoc.Definition
import Text.Pandoc.Lua.Orphans ()
import Text.Pandoc.Lua.Marshal.AST
import Text.Pandoc.Lua.Marshal.Filter (peekFilter)
import Text.Pandoc.Lua.Marshal.ReaderOptions ( peekReaderOptions
                                             , pushReaderOptions)
import Text.Pandoc.Lua.Module.Utils (sha1)
import Text.Pandoc.Lua.PandocLua (PandocLua, liftPandocLua)
import Text.Pandoc.Options (ReaderOptions (readerExtensions))
import Text.Pandoc.Process (pipeProcess)
import Text.Pandoc.Readers (Reader (..), getReader)

import qualified HsLua as Lua
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Text.Pandoc.Lua.Util as LuaUtil
import Text.Pandoc.Error

-- | Push the "pandoc" package to the Lua stack. Requires the `List`
-- module to be loadable.
pushModule :: PandocLua NumResults
pushModule = do
  liftPandocLua $ Lua.pushModule documentedModule
  return 1

documentedModule :: Module PandocError
documentedModule = Module
  { moduleName = "pandoc"
  , moduleDescription = T.unlines
    [ "Lua functions for pandoc scripts; includes constructors for"
    , "document elements, functions to parse text in a given"
    , "format, and functions to filter and modify a subtree."
    ]
  , moduleFields = stringConstants ++ [inlineField, blockField]
  , moduleOperations = []
  , moduleFunctions = mconcat
      [ functions
      , otherConstructors
      , blockConstructors
      , inlineConstructors
      , metaValueConstructors
      ]
  }

-- | Inline table field
inlineField :: Field PandocError
inlineField = Field
  { fieldName = "Inline"
  , fieldDescription = "Inline constructors, nested under 'constructors'."
  -- the nesting happens for historical reasons and should probably be
  -- changed.
  , fieldPushValue = pushWithConstructorsSubtable inlineConstructors
  }

-- | @Block@ module field
blockField :: Field PandocError
blockField = Field
  { fieldName = "Block"
  , fieldDescription = "Inline constructors, nested under 'constructors'."
  -- the nesting happens for historical reasons and should probably be
  -- changed.
  , fieldPushValue = pushWithConstructorsSubtable blockConstructors
  }

pushWithConstructorsSubtable :: [DocumentedFunction PandocError]
                             -> LuaE PandocError ()
pushWithConstructorsSubtable constructors = do
  newtable -- Field table
  newtable -- constructor table
  pushName "constructor" *> pushvalue (nth 2) *> rawset (nth 4)
  forM_ constructors $ \fn -> do
    pushName (functionName fn)
    pushDocumentedFunction fn
    rawset (nth 3)
  pop 1 -- pop constructor table

otherConstructors :: LuaError e => [DocumentedFunction e]
otherConstructors =
  [ mkPandoc
  , mkMeta
  , mkAttr
  , mkAttributeList
  , mkBlocks
  , mkCitation
  , mkInlines
  , mkListAttributes
  , mkSimpleTable

  , defun "ReaderOptions"
    ### liftPure id
    <#> parameter peekReaderOptions "ReaderOptions|table" "opts" "reader options"
    =#> functionResult pushReaderOptions "ReaderOptions" "new object"
    #? "Creates a new ReaderOptions value."
  ]

stringConstants :: [Field e]
stringConstants =
  let constrs :: forall a. Data a => Proxy a -> [String]
      constrs _ = map showConstr . dataTypeConstrs . dataTypeOf @a $ undefined
      nullaryConstructors = mconcat
        [ constrs (Proxy @ListNumberStyle)
        , constrs (Proxy @ListNumberDelim)
        , constrs (Proxy @QuoteType)
        , constrs (Proxy @MathType)
        , constrs (Proxy @Alignment)
        , constrs (Proxy @CitationMode)
        ]
      toField s = Field
        { fieldName = T.pack s
        , fieldDescription = T.pack s
        , fieldPushValue = pushString s
        }
  in map toField nullaryConstructors

functions :: [DocumentedFunction PandocError]
functions =
  [ defun "pipe"
    ### (\command args input -> do
            (ec, output) <- Lua.liftIO $ pipeProcess Nothing command args input
                            `catch` (throwM . PandocIOError "pipe")
            case ec of
              ExitSuccess -> 1 <$ Lua.pushLazyByteString output
              ExitFailure n -> do
                pushPipeError (PipeError (T.pack command) n output)
                Lua.error)
    <#> parameter peekString "string" "command" "path to executable"
    <#> parameter (peekList peekString) "{string,...}" "args"
          "list of arguments"
    <#> parameter peekLazyByteString "string" "input"
          "input passed to process via stdin"
    =?> "output string, or error triple"

  , defun "read"
    ### (\content mformatspec mreaderOptions -> do
            let formatSpec = fromMaybe "markdown" mformatspec
                readerOptions = fromMaybe def mreaderOptions
            res <- Lua.liftIO . runIO $ getReader formatSpec >>= \case
              (TextReader r, es) -> r readerOptions{ readerExtensions = es }
                                      content
              _ -> throwError $ PandocSomeError
                   "Only textual formats are supported"
            case res of
              Right pd -> return pd -- success, got a Pandoc document
              Left  (PandocUnknownReaderError f) ->
                Lua.failLua . T.unpack $ "Unknown reader: " <> f
              Left  (PandocUnsupportedExtensionError e f) ->
                Lua.failLua . T.unpack $
                "Extension " <> e <> " not supported for " <> f
              Left e ->
                throwM e)
    <#> parameter peekText "string" "content" "text to parse"
    <#> optionalParameter peekText "string" "formatspec" "format and extensions"
    <#> optionalParameter peekReaderOptions "ReaderOptions" "reader_options"
          "reader options"
    =#> functionResult pushPandoc "Pandoc" "result document"

  , sha1

  , defun "walk_block"
    ### walkElement
    <#> parameter peekBlockFuzzy "Block" "block" "element to traverse"
    <#> parameter peekFilter "Filter" "lua_filter" "filter functions"
    =#> functionResult pushBlock "Block" "modified Block"

  , defun "walk_inline"
    ### walkElement
    <#> parameter peekInlineFuzzy "Inline" "inline" "element to traverse"
    <#> parameter peekFilter "Filter" "lua_filter" "filter functions"
    =#> functionResult pushInline "Inline" "modified Inline"
  ]
 where
  walkElement x f =
        walkInlineSplicing f x
    >>= walkInlinesStraight f
    >>= walkBlockSplicing f
    >>= walkBlocksStraight f

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
