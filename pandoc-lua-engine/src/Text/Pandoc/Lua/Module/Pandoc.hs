{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications  #-}
{- |
   Module      : Text.Pandoc.Lua.Module.Pandoc
   Copyright   : Copyright © 2017-2022 Albert Krewinkel
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
import Control.Applicative ((<|>))
import Control.Monad (forM_, when)
import Control.Monad.Catch (catch, throwM)
import Data.Data (Data, dataTypeConstrs, dataTypeOf, showConstr)
import Data.Default (Default (..))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import HsLua hiding (pushModule)
import System.Exit (ExitCode (..))
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError (..))
import Text.Pandoc.Lua.Orphans ()
import Text.Pandoc.Lua.Marshal.AST
import Text.Pandoc.Lua.Marshal.Filter (peekFilter)
import Text.Pandoc.Lua.Marshal.ReaderOptions ( peekReaderOptions
                                             , pushReaderOptions)
import Text.Pandoc.Lua.Marshal.Sources (peekSources)
import Text.Pandoc.Lua.Marshal.WriterOptions ( peekWriterOptions
                                             , pushWriterOptions)
import Text.Pandoc.Lua.Module.Utils (sha1)
import Text.Pandoc.Lua.PandocLua (PandocLua (unPandocLua), liftPandocLua)
import Text.Pandoc.Options ( ReaderOptions (readerExtensions)
                           , WriterOptions (writerExtensions) )
import Text.Pandoc.Process (pipeProcess)
import Text.Pandoc.Readers (Reader (..), getReader, readers)
import Text.Pandoc.Sources (toSources)
import Text.Pandoc.Writers (Writer (..), getWriter, writers)

import qualified HsLua as Lua
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Text.Pandoc.UTF8 as UTF8

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
  , moduleFields = readersField : writersField :
                   stringConstants ++ [inlineField, blockField]
  , moduleOperations = []
  , moduleFunctions = mconcat
      [ functions
      , otherConstructors
      , blockConstructors
      , inlineConstructors
      , metaValueConstructors
      ]
  }

-- | Set of input formats accepted by @read@.
readersField :: Field PandocError
readersField = Field
  { fieldName = "readers"
  , fieldDescription = T.unlines
    [ "Set of formats that pandoc can parse. All keys in this table can"
    , "be used as the `format` value in `pandoc.read`."
    ]
  , fieldPushValue = pushSet pushText $
                     Set.fromList (map fst (readers @PandocLua))
  }

-- | Set of input formats accepted by @write@.
writersField :: Field PandocError
writersField = Field
  { fieldName = "writers"
  , fieldDescription = T.unlines
    [ "Set of formats that pandoc can generate. All keys in this table"
    , "can be used as the `format` value in `pandoc.write`."
    ]
  , fieldPushValue = pushSet pushText $
                     Set.fromList (map fst (writers @PandocLua))
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
  , mkCell
  , mkRow
  , mkTableHead
  , mkTableFoot
  , mkInlines
  , mkListAttributes
  , mkSimpleTable

  , defun "ReaderOptions"
    ### liftPure id
    <#> parameter peekReaderOptions "ReaderOptions|table" "opts" "reader options"
    =#> functionResult pushReaderOptions "ReaderOptions" "new object"
    #? "Creates a new ReaderOptions value."

  , defun "WriterOptions"
    ### liftPure id
    <#> parameter peekWriterOptions "WriterOptions|table" "opts"
          "writer options"
    =#> functionResult pushWriterOptions "WriterOptions" "new object"
    #? "Creates a new WriterOptions value."
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
                readerOpts = fromMaybe def mreaderOptions
                readAction = getReader formatSpec >>= \case
                  (TextReader r, es)       ->
                    r readerOpts{readerExtensions = es}
                      (case content of
                         Left bs       -> toSources $ UTF8.toText bs
                         Right sources -> sources)
                  (ByteStringReader r, es) ->
                    case content of
                      Left bs -> r readerOpts{readerExtensions = es}
                                   (BSL.fromStrict bs)
                      Right _ -> liftPandocLua $ Lua.failLua
                                 "Cannot use bytestring reader with Sources"
            try (unPandocLua readAction) >>= \case
              Right pd ->
                -- success, got a Pandoc document
                return pd
              Left  (PandocUnknownReaderError f) ->
                Lua.failLua . T.unpack $ "Unknown reader: " <> f
              Left  (PandocUnsupportedExtensionError e f) ->
                Lua.failLua . T.unpack $
                "Extension " <> e <> " not supported for " <> f
              Left e ->
                throwM e)
    <#> parameter (\idx -> (Left  <$> peekByteString idx)
                       <|> (Right <$> peekSources idx))
          "string|Sources" "content" "text to parse"
    <#> opt (textParam "formatspec" "format and extensions")
    <#> opt (parameter peekReaderOptions "ReaderOptions" "reader_options"
             "reader options")
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

  , defun "write"
    ### (\doc mformatspec mwriterOpts -> do
            let formatSpec = fromMaybe "html" mformatspec
                writerOpts = fromMaybe def mwriterOpts
            unPandocLua $ getWriter formatSpec >>= \case
              (TextWriter w, es)      -> Right <$>
                w writerOpts{ writerExtensions = es } doc
              (ByteStringWriter w, es) -> Left <$>
                w writerOpts{ writerExtensions = es } doc)
    <#> parameter peekPandoc "Pandoc" "doc" "document to convert"
    <#> opt (textParam "formatspec" "format and extensions")
    <#> opt (parameter peekWriterOptions "WriterOptions" "writer_options"
              "writer options")
    =#> functionResult (either pushLazyByteString pushText) "string"
          "result document"
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

peekPipeError :: LuaError e => StackIndex -> LuaE e PipeError
peekPipeError idx =
  PipeError
  <$> (Lua.getfield idx "command"    *> Lua.peek (-1) <* Lua.pop 1)
  <*> (Lua.getfield idx "error_code" *> Lua.peek (-1) <* Lua.pop 1)
  <*> (Lua.getfield idx "output"     *> Lua.peek (-1) <* Lua.pop 1)

pushPipeError :: LuaError e => Pusher e PipeError
pushPipeError pipeErr = do
  pushAsTable [ ("command"    , pushText . pipeErrorCommand)
              , ("error_code" , pushIntegral . pipeErrorCode)
              , ("output"     , pushLazyByteString . pipeErrorOutput)
              ] pipeErr
  pushPipeErrorMetaTable
  Lua.setmetatable (nth 2)
    where
      pushPipeErrorMetaTable :: LuaError e => LuaE e ()
      pushPipeErrorMetaTable = do
        v <- Lua.newmetatable "pandoc pipe error"
        when v $ do
          pushName "__tostring"
          pushHaskellFunction pipeErrorMessage
          rawset (nth 3)

      pipeErrorMessage :: LuaError e => LuaE e NumResults
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
