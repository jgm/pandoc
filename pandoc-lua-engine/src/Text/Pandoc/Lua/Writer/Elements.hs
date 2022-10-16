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

import Control.Applicative ((<|>))
import Control.Monad ((<$!>), void)
import Data.Data ( showConstr, toConstr )
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
    ### (\(Pandoc _ blks) -> blockListToCustom writer Nothing blks)
    <#> parameter peekPandoc "Pandoc" "doc" ""
    =#> functionResult pushDoc "Doc" "rendered doc"
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

  (body, mcontext) <- pandocToCustom writer doc

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
    _       -> do
      getfield' top field
        -- remove Writer and subtable
        <* remove (nth 3) <* remove (nth 2)

pandocToCustom :: WriterTable -> Pandoc
               -> LuaE PandocError (Doc Text, Maybe (Context Text))
pandocToCustom writer doc = do
  getWriterField writer "Pandoc"
  pushPandoc doc
  pushOpts
  callTrace 2 2
  forcePeek $ ((,) <$> peekDocFuzzy (nth 2) <*> orNil peekContext top)
    `lastly` pop 2

blockToCustom :: WriterTable -> Block -> LuaE PandocError (Doc Text)
blockToCustom writer blk = do
  let constrName = fromString . showConstr . toConstr $ blk
  getNestedWriterField writer "Block" constrName
  forcePeek . (`lastly` pop 1) $ -- remove final Doc value
    -- try to use the value as a Doc; if that fails, use it as a function.
    peekDocFuzzy top <|> do
      -- try to call value as a function
      liftLua $ pushBlock blk *> pushOpts *> call 2 1
      peekDocFuzzy top

inlineToCustom :: WriterTable -> Inline -> LuaE PandocError (Doc Text)
inlineToCustom writer blk = do
  let constrName = fromString . showConstr . toConstr $ blk
  getNestedWriterField writer "Inline" constrName
  forcePeek . (`lastly` pop 1) $ -- remove final Doc value
    -- try to use the value as a Doc; if that fails, use it as a function.
    peekDocFuzzy top <|> do
      -- try to call value as a function
      liftLua $ pushInline blk *> pushOpts *> call 2 1
      peekDocFuzzy top

blockListToCustom :: WriterTable -> Maybe (Doc Text) -> [Block]
                  -> LuaE PandocError (Doc Text)
blockListToCustom writer msep blocks = do
  let addSeps = intersperse $ fromMaybe blankline msep
  mconcat . addSeps <$> mapM (blockToCustom writer) blocks

inlineListToCustom :: WriterTable -> [Inline] -> LuaE PandocError (Doc Text)
inlineListToCustom writer inlines = do
  mconcat <$> mapM (inlineToCustom writer) inlines

orNil :: Peeker e a -> Peeker e (Maybe a)
orNil p idx = liftLua (ltype idx) >>= \case
  TypeNil  -> pure Nothing
  TypeNone -> pure Nothing
  _        -> Just <$> p idx

peekDocFuzzy :: LuaError e => Peeker e (Doc Text)
peekDocFuzzy idx = liftLua (ltype idx) >>= \case
  TypeTable -> mconcat <$!> peekList peekDoc idx
  _         -> peekDoc idx
