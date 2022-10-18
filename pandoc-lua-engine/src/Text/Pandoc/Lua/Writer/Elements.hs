{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{- |
   Module      : Text.Pandoc.Lua.Writer.Elements
   Copyright   : © 2022 Albert Krewinkel
   License     : GPL-2.0-or-later
   Maintainer  : Albert Krewinkel <pandoc@tarleb.com>

Conversion of Pandoc documents using a custom Lua writer.
-}
module Text.Pandoc.Lua.Writer.Elements
  ( pushElementWriter
  ) where

import Control.Monad ((<$!>), void)
import Data.ByteString (ByteString)
import Data.Data (dataTypeConstrs, dataTypeOf, showConstr, toConstr)
import Data.Default (def)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.String (IsString (fromString))
import HsLua
import HsLua.Module.DocLayout (peekDoc, pushDoc)
import Text.DocLayout (Doc, blankline, render)
import Text.DocTemplates (Context)
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError (..))
import Text.Pandoc.Options (WriterOptions (..), WrapOption(..))
import Text.Pandoc.Lua.PandocLua ()
import Text.Pandoc.Lua.Marshal.AST
import Text.Pandoc.Lua.Marshal.Context (peekContext)
import Text.Pandoc.Lua.Marshal.WriterOptions ( peekWriterOptions
                                             , pushWriterOptions)
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Writers.Shared (metaToContext, setField)
import qualified Data.Text as T
import qualified Text.Pandoc.UTF8 as UTF8

-- | Convert Pandoc to custom markup.
pushElementWriter :: LuaE PandocError NumResults
pushElementWriter = do
  newtable
    *> pushWriterMT *> setmetatable (nth 2)
  writer <- toWriterTable top
  addField "Blocks"  $ pushDocumentedFunction (blocksFn writer)
  addField "Inlines" $ pushDocumentedFunction (inlinesFn writer)
  addField "Block"   $ newtable *> pushBlockMT  writer *> setmetatable (nth 2)
  addField "Inline"  $ newtable *> pushInlineMT writer *> setmetatable (nth 2)
  addField "Pandoc"  $ pushDocumentedFunction $ lambda
    ### (\(Pandoc _ blks) -> do
            pushWriterTable writer
            getfield' top "Blocks" <* remove (nth 2)
            pushBlocks blks
            callTrace 1 1
            pure (NumResults 1))
    <#> parameter peekPandoc "Pandoc" "doc" ""
    =?> "rendered doc"
  freeWriter writer
  return 1
 where
  blocksFn w = lambda
    ### (\blocks msep -> blockListToCustom w msep blocks)
    <#> parameter peekBlocks "Blocks" "blocks" ""
    <#> opt (parameter peekDocFuzzy "Doc" "sep" "")
    =#> functionResult pushDoc "Doc" ""
  inlinesFn w = lambda
    ### inlineListToCustom w
    <#> parameter peekInlines "Inlines" "inlines" ""
    =#> functionResult pushDoc "Doc" ""
  pushBlockMT writer = do
    newtable
    addField "__call" $ pushDocumentedFunction $ lambda
      ### blockToCustom
      <#> parameter peekWriter "table" "writer" ""
      <#> parameter peekBlockFuzzy "Block" "block" ""
      =#> functionResult pushDoc "Doc" "rendered blocks"
    addField "__index" $
      -- lookup missing fields in the main Writer table
      pushWriterTable writer
  pushInlineMT writer = do
    newtable
    addField "__call" $ pushDocumentedFunction $ lambda
      ### inlineToCustom
      <#> parameter peekWriter "table" "writer" ""
      <#> parameter peekInlineFuzzy "Inline" "inline" ""
      =#> functionResult pushDoc "Doc" "rendered inline"
    addField "__index" $ do
      -- lookup missing fields in the main Writer table
      pushWriterTable writer

pushWriterMT :: LuaE PandocError ()
pushWriterMT = do
  newtable
  addField "__call" $ pushDocumentedFunction $ lambda
    ### (\writer doc mopts -> runWriter writer doc mopts)
    <#> parameter peekWriter "table" "writer" ""
    <#> parameter peekPandoc "Pandoc" "doc" ""
    <#> opt (parameter peekWriterOptions "WriterOptions" "opts" "")
    =#> functionResult pushText "string" "rendered document"
  addField "__index" . pushDocumentedFunction $ lambda
    ### (\_writer key -> handleMissingField key)
    <#> parameter pure "table"  "writer" ""
    <#> parameter (liftLua . tostring') "string" "key" ""
    =#> functionResult (const pushnil) "string" ""


addField :: LuaError e => Name -> LuaE e a -> LuaE e ()
addField name action = do
  pushName name
  action
  rawset (nth 3)

getfield' :: LuaError e => StackIndex -> Name -> LuaE e HsLua.Type
getfield' idx name = do
  aidx <- absindex idx
  pushName name
  rawget aidx >>= \case
    TypeNil -> pop 1 *> getfield aidx name
    ty      -> pure ty

-- | A writer table is just an absolute stack index.
newtype WriterTable = WriterTable Reference

toWriterTable :: LuaError e => StackIndex -> LuaE e WriterTable
toWriterTable idx = WriterTable <$!> do
  pushvalue idx
  ref registryindex

peekWriter :: LuaError e => Peeker e WriterTable
peekWriter = liftLua . toWriterTable

pushWriterTable :: LuaError e => Pusher e WriterTable
pushWriterTable (WriterTable wref) = void $ getref registryindex wref

writerOptionsField :: Name
writerOptionsField = "Pandoc Writer WriterOptions"

freeWriter :: WriterTable -> LuaE e ()
freeWriter (WriterTable wref) = unref registryindex wref

pushOpts :: LuaE PandocError ()
pushOpts = void $ getfield' registryindex writerOptionsField

runWriter :: WriterTable -> Pandoc -> Maybe WriterOptions
          -> LuaE PandocError Text
runWriter writer doc@(Pandoc meta _blks) mopts = do
  let opts = fromMaybe def mopts
  pushWriterOptions opts *>
    setfield registryindex writerOptionsField

  (body, mcontext) <- runPeek (pandocToCustom writer doc) >>= force . \case
    Failure msg contexts -> Failure (cleanupTrace msg) contexts
    s -> s

  -- convert metavalues to a template context (variables)
  defaultContext <- metaToContext opts
                    (blockListToCustom writer Nothing)
                    (inlineListToCustom writer)
                    meta
  let context = setField "body" body
              $ fromMaybe defaultContext mcontext

  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing

  return $ render colwidth $
    case writerTemplate opts of
       Nothing  -> body
       Just tpl -> renderTemplate tpl context

-- | Keep exactly one traceback and clean it up. This wouldn't be
-- necessary if the @pcallTrace@ function would do nothing whenever the
-- error already included a trace, but that would require some bigger
-- changes; removing the additional traces in this post-process step is
-- much easier (for now).
cleanupTrace :: ByteString -> ByteString
cleanupTrace msg = UTF8.fromText . T.intercalate "\n" $
  let tmsg = T.lines $ UTF8.toText msg
      traceStart = (== "stack traceback:")
  in case break traceStart tmsg of
        (x, t:traces) -> (x <>) . (t:) $
                         let (firstTrace, rest) = break traceStart traces
                             isPeekContext = ("\twhile " `T.isPrefixOf`)
                             isUnknownCFn = (== "\t[C]: in ?")
                         in filter (not . isUnknownCFn) firstTrace <>
                            filter isPeekContext rest
        _ -> tmsg

-- | Pushes the field in the writer table.
getWriterField :: LuaError e
               => WriterTable -> Name -> LuaE e HsLua.Type
getWriterField writer name = do
  pushWriterTable writer
  getfield' top name <* remove (nth 2)

-- | Looks up @Writer.subtable.field@; tries @Writer.field@ as a fallback if the
-- subtable field is @nil@.
getNestedWriterField :: LuaError e
                     => WriterTable -> Name -> Name -> LuaE e HsLua.Type
getNestedWriterField writer subtable field = do
  pushWriterTable writer
  getfield' top subtable >>= \case
    TypeNil -> TypeNil <$ remove (nth 2) -- remove Writer table
    _       -> getfield' top field
               -- remove Writer and subtable
               <* remove (nth 3) <* remove (nth 2)

pandocToCustom :: WriterTable -> Pandoc
               -> Peek PandocError (Doc Text, Maybe (Context Text))
pandocToCustom writer doc = withContext "rendering Pandoc" $ do
  callStatus <- liftLua $ do
    getWriterField writer "Pandoc"
    pushPandoc doc
    pushOpts
    pcallTrace 2 2
  case callStatus of
    OK -> ((,) <$> peekDocFuzzy (nth 2) <*> orNil peekContext top)
          `lastly` pop 2
    _  -> failPeek =<< liftLua (tostring' top)


blockToCustom :: WriterTable -> Block -> LuaE PandocError (Doc Text)
blockToCustom writer blk = forcePeek $ renderBlock writer blk

renderBlock :: WriterTable -> Block -> Peek PandocError (Doc Text)
renderBlock writer blk = do
  let constrName = fromString . showConstr . toConstr $ blk
  withContext ("rendering Block `" <> constrName <> "`") $
    liftLua (getNestedWriterField writer "Block" constrName) >>= \case
      TypeNil -> failPeek =<< typeMismatchMessage "function or Doc" top
      _       -> callOrDoc (pushBlock blk)

inlineToCustom :: WriterTable -> Inline -> LuaE PandocError (Doc Text)
inlineToCustom writer inln = forcePeek $ renderInline writer inln

renderInline :: WriterTable -> Inline -> Peek PandocError (Doc Text)
renderInline writer inln = do
  let constrName = fromString . showConstr . toConstr $ inln
  withContext ("rendering Inline `" <> constrName <> "`") $ do
    liftLua (getNestedWriterField writer "Inline" constrName) >>= \case
      TypeNil -> failPeek =<< typeMismatchMessage "function or Doc" top
      _       -> callOrDoc (pushInline inln)

-- | If the value at the top of the stack can be called as a function,
-- then push the element and writer options to the stack and call it;
-- otherwise treat it as a plain Doc value
callOrDoc :: LuaE PandocError ()
          -> Peek PandocError (Doc Text)
callOrDoc pushElement = do
  liftLua (ltype top) >>= \case
    TypeFunction -> peekCall
    _            -> do
      isCallable <- liftLua $ getmetafield top "__call" >>= \case
        TypeNil -> pure False
        _       -> True <$ pop 1
      if isCallable
        then peekCall
        else peekDocFuzzy top
 where
   peekCall :: Peek PandocError (Doc Text)
   peekCall =
     liftLua (pushElement *> pushOpts *> pcallTrace 2 1) >>= \case
       OK -> peekDocFuzzy top
       _  -> failPeek =<< liftLua (tostring' top)

blockListToCustom :: WriterTable -> Maybe (Doc Text) -> [Block]
                  -> LuaE PandocError (Doc Text)
blockListToCustom writer msep blocks = forcePeek $
  renderBlockList writer msep blocks

inlineListToCustom :: WriterTable -> [Inline] -> LuaE PandocError (Doc Text)
inlineListToCustom writer inlines = forcePeek $
  renderInlineList writer inlines

renderBlockList :: WriterTable -> Maybe (Doc Text) -> [Block]
                -> Peek PandocError (Doc Text)
renderBlockList writer msep blocks = withContext "rendering Blocks" $ do
  let addSeps = intersperse $ fromMaybe blankline msep
  mconcat . addSeps <$> mapM (renderBlock writer) blocks

renderInlineList :: WriterTable -> [Inline] -> Peek PandocError (Doc Text)
renderInlineList writer inlines = withContext "rendering Inlines" $ do
  mconcat <$> mapM (renderInline writer) inlines

orNil :: Peeker e a -> Peeker e (Maybe a)
orNil p idx = liftLua (ltype idx) >>= \case
  TypeNil  -> pure Nothing
  TypeNone -> pure Nothing
  _        -> Just <$> p idx

peekDocFuzzy :: LuaError e => Peeker e (Doc Text)
peekDocFuzzy idx = liftLua (ltype idx) >>= \case
  TypeTable -> mconcat <$!> peekList peekDoc idx
  _         -> peekDoc idx

handleMissingField :: LuaError e => ByteString -> LuaE e ()
handleMissingField key' =
  let key = UTF8.toString key'
      blockNames  = map (fromString . show) . dataTypeConstrs . dataTypeOf
                      $ HorizontalRule
      inlineNames = map (fromString . show) . dataTypeConstrs . dataTypeOf
                      $ Space
      mtypeName = case () of
       _ | key `elem` blockNames  -> Just "Block"
       _ | key `elem` inlineNames -> Just "Inline"
       _                          -> Nothing
  in case mtypeName of
       Just typeName  -> failLua $
                         "No render function for " <> typeName <> " value " <>
                         "'" <> key <> "';\ndefine a function `Writer." <>
                         typeName <> "." <> key <> "` that returns " <>
                         "a string or Doc."
       _ -> pure ()
