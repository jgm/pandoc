{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications  #-}
{- |
   Module      : Text.Pandoc.Lua.Module.Pandoc
   Copyright   : Copyright © 2017-2024 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <albert+pandoc@tarleb.com>

Main @pandoc@ module, containing element constructors and central functions.
-}
module Text.Pandoc.Lua.Module.Pandoc
  ( documentedModule
  ) where

import Prelude hiding (read)
import Control.Applicative ((<|>))
import Control.Monad (foldM, forM_, when)
import Control.Monad.Catch (catch, handle, throwM)
import Control.Monad.Except (MonadError (throwError))
import Data.Data (Data, dataTypeConstrs, dataTypeOf, showConstr)
import Data.Default (Default (..))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Text.Encoding.Error (UnicodeException)
import Data.Version (makeVersion)
import HsLua
import System.Exit (ExitCode (..))
import Text.Pandoc.Class ( PandocMonad, FileInfo (..), FileTree
                         , addToFileTree, getCurrentTime
                         , getRequestHeaders, getResourcePath, getUserDataDir
                         , setRequestHeaders, setResourcePath, setUserDataDir
                         , insertInFileTree, sandboxWithFileTree
                         )
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError (..))
import Text.Pandoc.Format (FlavoredFormat, parseFlavoredFormat)
import Text.Pandoc.Lua.Orphans ()
import Text.Pandoc.Lua.Marshal.AST
import Text.Pandoc.Lua.Marshal.Format (peekFlavoredFormat)
import Text.Pandoc.Lua.Marshal.Filter (peekFilter)
import Text.Pandoc.Lua.Marshal.ReaderOptions ( peekReaderOptions
                                             , pushReaderOptions)
import Text.Pandoc.Lua.Marshal.Sources (peekSources)
import Text.Pandoc.Lua.Marshal.WriterOptions ( peekWriterOptions
                                             , pushWriterOptions)
import Text.Pandoc.Lua.Module.Utils (sha1)
import Text.Pandoc.Lua.PandocLua (PandocLua (unPandocLua))
import Text.Pandoc.Lua.Writer.Classic (runCustom)
import Text.Pandoc.Options ( ReaderOptions (readerExtensions)
                           , WriterOptions (writerExtensions) )
import Text.Pandoc.Process (pipeProcess)
import Text.Pandoc.Readers (Reader (..), getReader, readers)
import Text.Pandoc.Sources (toSources)
import Text.Pandoc.Writers (Writer (..), getWriter, writers)

import qualified HsLua as Lua
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Text.Pandoc.UTF8 as UTF8

documentedModule :: Module PandocError
documentedModule = defmodule "pandoc"
  `withDescription` T.unlines
    [ "Fields and functions for pandoc scripts; includes constructors for"
    , "document tree elements, functions to parse text in a given"
    , "format, and functions to filter and modify a subtree."
    ]
  `withFields` readersField : writersField :
                   stringConstants ++ [inlineField, blockField]
  `withFunctions` mconcat
      [ [mkPandoc, mkMeta]
      , metaValueConstructors
      , blockConstructors
      , [mkBlocks]
      , inlineConstructors
      , [mkInlines]
      , otherConstructors
      , functions
      ]
  `associateType` typePandoc
  `associateType` typeBlock
  `associateType` typeInline

-- | Set of input formats accepted by @read@.
readersField :: Field PandocError
readersField = deffield "readers"
  `withType` "table"
  `withDescription` T.unlines
    [ "Set of formats that pandoc can parse. All keys in this table can"
    , "be used as the `format` value in `pandoc.read`."
    ]
  `withValue` pushKeyValuePairs pushText (pushText . readerType)
                     (readers @PandocLua)
 where
  readerType = \case
    TextReader {} -> "text"
    ByteStringReader {} -> "bytestring"

-- | Set of input formats accepted by @write@.
writersField :: Field PandocError
writersField = deffield "writers"
  `withType` "table"
  `withDescription` T.unlines
    [ "Set of formats that pandoc can generate. All keys in this table"
    , "can be used as the `format` value in `pandoc.write`."
    ]
  `withValue` pushKeyValuePairs pushText (pushText . writerType)
                     (writers @PandocLua)
 where
  writerType = \case
    TextWriter {} -> "text"
    ByteStringWriter {} -> "bytestring"

-- | Inline table field
inlineField :: Field PandocError
inlineField = deffield "Inline"
  `withType` "table"
  `withDescription` "Inline constructors, nested under 'constructors'."
  -- the nesting happens for historical reasons and should probably be
  -- changed.
  `withValue` pushWithConstructorsSubtable inlineConstructors

-- | @Block@ module field
blockField :: Field PandocError
blockField = deffield "Block"
  `withType` "table"
  `withDescription` "Inline constructors, nested under 'constructors'."
  -- the nesting happens for historical reasons and should probably be
  -- changed.
  `withValue` pushWithConstructorsSubtable blockConstructors

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

otherConstructors :: [DocumentedFunction PandocError]
otherConstructors =
  [ mkAttr
  , mkCaption `since` makeVersion [3,6,1]
  , mkCell
  , mkAttributeList
  , mkCitation
  , mkListAttributes
  , mkRow
  , mkTableFoot
  , mkTableHead
  , mkSimpleTable

  , defun "ReaderOptions"
    ### liftPure id
    <#> parameter peekReaderOptions "ReaderOptions|table" "opts"
        (T.unlines
         [ "Either a table with a subset of the properties of a"
         , "[[ReaderOptions]] object, or another ReaderOptions object."
         , "Uses the defaults specified in the manual for all"
         , "properties that are not explicitly specified. Throws an"
         , "error if a table contains properties which are not present"
         , "in a ReaderOptions object."
        ]
        )
    =#> functionResult pushReaderOptions "ReaderOptions" "new object"
    #? T.unlines
    [ "Creates a new ReaderOptions value."
    , ""
    , "Usage:"
    , ""
    , "    -- copy of the reader options that were defined on the command line."
    , "    local cli_opts = pandoc.ReaderOptions(PANDOC_READER_OPTIONS)"
    , "    -- default reader options, but columns set to 66."
    , "    local short_colums_opts = pandoc.ReaderOptions {columns = 66}"
    ]

  , defun "WriterOptions"
    ### liftPure id
    <#> parameter peekWriterOptions "WriterOptions|table" "opts"
        (T.unlines
        [ "Either a table with a subset of the properties of a"
        , "[[WriterOptions]] object, or another WriterOptions object."
        , "Uses the defaults specified in the manual for all"
        , "properties that are not explicitly specified. Throws an"
        , "error if a table contains properties which are not present"
        , "in a WriterOptions object."
        ])
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
      toField s = deffield (Name $ UTF8.fromString s)
        `withType` "string"
        `withDescription` T.pack s
        `withValue` pushString s
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
    ### (\content mformatspec mreaderOptions mreadEnv -> do
            let readerOpts = fromMaybe def mreaderOptions

                readAction :: PandocMonad m => FlavoredFormat -> m Pandoc
                readAction flvrd = getReader flvrd >>= \case
                  (TextReader r, es)       ->
                    r readerOpts{readerExtensions = es} $
                    case content of
                      Left bs       -> toSources $ UTF8.toText bs
                      Right sources -> sources
                  (ByteStringReader r, es) ->
                    case content of
                      Left bs -> r readerOpts{readerExtensions = es}
                                 (BSL.fromStrict bs)
                      Right _ -> throwError $ PandocLuaError
                                 "Cannot use bytestring reader with Sources"

            handle (failLua . show @UnicodeException) . unPandocLua $ do
              flvrd <- maybe (parseFlavoredFormat "markdown") pure mformatspec
              case mreadEnv of
                Nothing   -> readAction flvrd
                Just tree -> sandboxWithFileTree tree (readAction flvrd))
    <#> parameter (\idx -> (Left  <$> peekByteString idx)
                       <|> (Right <$> peekSources idx))
          "string|Sources" "content" "text to parse"
    <#> opt (parameter peekFlavoredFormat "string|table"
                       "formatspec" "format and extensions")
    <#> opt (parameter peekReaderOptions "ReaderOptions" "reader_options"
             "reader options")
    <#> opt (parameter peekReadEnv "table" "read_env" $ T.unlines
            [ "If the value is not given or `nil`, then the global environment"
            , "is used. Passing a list of filenames causes the reader to"
            , "be run in a sandbox. The given files are read from the file"
            , "system and provided to the sandbox via an ersatz file system."
            , "The table can also contain mappings from filenames to"
            , "contents, which will be used to populate the ersatz file"
            , "system."
            ])
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

  , defun "with_state"
    ### with_state
    <#> parameter peekStateOptions "table" "options" "state options"
    <#> parameter pure "function" "callback"
          "The action to run with the given state."
    =?> "The results of the call to *callback*."
    #? "Runs a function with a modified pandoc state.\n\
       \\n\
       \The given callback is invoked after setting the pandoc state to the\
       \ given values. The modifiable options are restored to their original\
       \ values once the callback has returned.\n\
       \\n\
       \The following state variables can be controlled:\n\
       \\n\
       \  - `request_headers` (list of key-value tuples)\n\
       \  - `resource_path` (list of filepaths)\n\
       \  - `user_data_dir` (string)\n\
       \\n\
       \Other options are ignored, and the rest of the state is not modified.\n\
       \\n\
       \Usage:\n\
       \\n\
       \    local opts = {\n\
       \      request_headers = {\n\
       \        {'Authorization', 'Basic my-secret'}\n\
       \      }\n\
       \    }\n\
       \    pandoc.with_state(opts, function ()\n\
       \      local mime, contents = pandoc.mediabag.fetch(image_url)\n\
       \    )\n"

  , defun "write"
    ### (\doc mformatspec mwriterOpts -> unPandocLua $ do
            flvrd <- maybe (parseFlavoredFormat "markdown") pure mformatspec
            let writerOpts = fromMaybe def mwriterOpts
            getWriter flvrd >>= \case
              (TextWriter w, es)      -> Right <$>
                w writerOpts{ writerExtensions = es } doc
              (ByteStringWriter w, es) -> Left <$>
                w writerOpts{ writerExtensions = es } doc)
    <#> parameter peekPandoc "Pandoc" "doc" "document to convert"
    <#> opt (parameter peekFlavoredFormat "string|table" "formatspec"
             (T.unlines
              [ "format specification; defaults to `\"html\"`. See the"
              , "documentation of [`pandoc.read`](#pandoc.read) for a complete"
              , "description of this parameter."
              ]))
    <#> opt (parameter peekWriterOptions "WriterOptions|table" "writer_options"
            (T.unlines
            [ "options passed to the writer; may be a WriterOptions object"
            , "or a table with a subset of the keys and values of a"
            , "WriterOptions object; defaults to the default values"
            , "documented in the manual."
            ])
            )
    =#> functionResult (either pushLazyByteString pushText) "string"
          "result document"
    #? T.unlines
    [ "Converts a document to the given target format."
    , ""
    , "Usage:"
    , ""
    , "    local doc = pandoc.Pandoc("
    , "      {pandoc.Para {pandoc.Strong 'Tea'}}"
    , "    )"
    , "    local html = pandoc.write(doc, 'html')"
    , "    assert(html == '<p><strong>Tea</strong></p>')"
    ]

  , defun "write_classic"
    ### (\doc mwopts -> runCustom (fromMaybe def mwopts) doc)
    <#> parameter peekPandoc "Pandoc" "doc" "document to convert"
    <#> opt (parameter peekWriterOptions "WriterOptions" "writer_options"
             (T.unlines
              [ "options passed to the writer; may be a WriterOptions object"
              , "or a table with a subset of the keys and values of a"
              , "WriterOptions object; defaults to the default values"
              , "documented in the manual."
              ]))
    =#> functionResult pushText "string" "rendered document"
    #? (T.unlines
       [ "Runs a classic custom Lua writer, using the functions defined"
       , "in the current environment."
       , ""
       , "Usage:"
       , ""
       , "    -- Adding this function converts a classic writer into a"
       , "    -- new-style custom writer."
       , "    function Writer (doc, opts)"
       , "      PANDOC_DOCUMENT = doc"
       , "      PANDOC_WRITER_OPTIONS = opts"
       , "      loadfile(PANDOC_SCRIPT_FILE)()"
       , "      return pandoc.write_classic(doc, opts)"
       , "    end"
       ])
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

-- | Peek the environment in which the `read` function operates.
peekReadEnv :: Peeker PandocError FileTree
peekReadEnv idx = do
  mtime <- liftLua . unPandocLua $ getCurrentTime

  -- Add files from file system
  files <- peekList peekString idx
  tree1 <- liftLua $
           foldM (\tree fp -> liftIO $ addToFileTree tree fp) mempty files

  -- Add files from key-value pairs
  let toFileInfo contents = FileInfo
        { infoFileMTime = mtime
        , infoFileContents = contents
        }
  pairs <- peekKeyValuePairs peekString (fmap toFileInfo . peekByteString) idx
  let tree2 = foldr (uncurry insertInFileTree) tree1 pairs

  -- Return ersatz file system.
  pure tree2

-- | Helper type that holds all common state values that can be controlled.
--
-- This is closely related to "CommonState", but that's an opaque value
-- that can only be read and modified through accessor functions. All
-- fields in this type can be modified through accessors.
data StateOptions = StateOptions
  { stateOptsRequestHeaders :: [(T.Text, T.Text)]
  , stateOptsResourcePath :: [String]
  , stateOptsUserDataDir :: Maybe String
  }

-- | Peek pandoc state options; the current state properties are used for
-- unspecified values.
peekStateOptions :: Peeker PandocError StateOptions
peekStateOptions idx = do
  absidx <- liftLua $ absindex idx
  let setOptions opts = do
        liftLua (next absidx) >>= \case
          False -> return opts
          True -> do
            key <- peekByteString (nth 2)
            case key of
              "request_headers" -> do
                let peekHeaderPair = peekPair peekText peekText
                value <- peekList peekHeaderPair top `lastly` pop 1
                setOptions $ opts { stateOptsRequestHeaders = value }
              "resource_path" -> do
                value <- peekList peekString top `lastly` pop 1
                setOptions $ opts { stateOptsResourcePath = value }
              "user_data_dir" -> do
                value <- peekNilOr peekString top `lastly` pop 1
                setOptions $ opts { stateOptsUserDataDir = value }
              _ -> do
                liftLua $ pop 2 -- remove key and value
                failPeek $ "Unknown or unsupported state option: " <> key

  liftLua pushnil -- first "key"
  liftLua getStateOptions >>= setOptions

-- | Get the current options values from the pandoc state.
getStateOptions :: LuaE PandocError StateOptions
getStateOptions = unPandocLua $ StateOptions
  <$> getRequestHeaders
  <*> getResourcePath
  <*> getUserDataDir

-- | Update the pandoc state with the new options.
setStateOptions :: StateOptions -> LuaE PandocError ()
setStateOptions opts = unPandocLua $ do
  setRequestHeaders $ stateOptsRequestHeaders opts
  setResourcePath $ stateOptsResourcePath opts
  setUserDataDir $ stateOptsUserDataDir opts

-- | Run a callback with a modified pandoc state.
with_state :: StateOptions -> StackIndex -> LuaE PandocError NumResults
with_state options callback_idx = do
  origState <- getStateOptions
  setStateOptions options
  -- Invoke the callback
  oldTop <- gettop
  pushvalue callback_idx
  call 0 multret
  newTop <- gettop
  setStateOptions origState
  return . NumResults . fromStackIndex $ newTop - oldTop
