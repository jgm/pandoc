{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{- |
   Module      : Text.Pandoc.Writers.Custom
   Copyright   : Copyright (C) 2012-2021 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to custom markup using
a Lua writer.
-}
module Text.Pandoc.Writers.Custom ( writeCustom ) where
import Control.Applicative (optional)
import Control.Arrow ((***))
import Control.Exception
import Control.Monad (when)
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text, pack)
import HsLua as Lua hiding (Operation (Div), render)
import HsLua.Class.Peekable (PeekError)
import Text.DocLayout (render, literal)
import Text.DocTemplates (Context)
import Control.Monad.IO.Class (MonadIO)
import Text.Pandoc.Definition
import Text.Pandoc.Lua (Global (..), runLua, setGlobals)
import Text.Pandoc.Lua.Util (addField, dofileWithTraceback, peekViaJSON)
import Text.Pandoc.Options
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Writers.Shared

attrToMap :: Attr -> M.Map T.Text T.Text
attrToMap (id',classes,keyvals) = M.fromList
    $ ("id", id')
    : ("class", T.unwords classes)
    : keyvals

newtype Stringify e a = Stringify a

instance Pushable (Stringify e Format) where
  push (Stringify (Format f)) = Lua.push (T.toLower f)

instance PeekError e => Pushable (Stringify e [Inline]) where
  push (Stringify ils) = Lua.push =<<
    changeErrorType ((inlineListToCustom @e) ils)

instance PeekError e => Pushable (Stringify e [Block]) where
  push (Stringify blks) = Lua.push =<<
    changeErrorType ((blockListToCustom @e) blks)

instance PeekError e => Pushable (Stringify e MetaValue) where
  push (Stringify (MetaMap m))       = Lua.push (fmap (Stringify @e) m)
  push (Stringify (MetaList xs))     = Lua.push (map (Stringify @e) xs)
  push (Stringify (MetaBool x))      = Lua.push x
  push (Stringify (MetaString s))    = Lua.push s
  push (Stringify (MetaInlines ils)) = Lua.push (Stringify @e ils)
  push (Stringify (MetaBlocks bs))   = Lua.push (Stringify @e bs)

instance PeekError e => Pushable (Stringify e Citation) where
  push (Stringify cit) = do
    Lua.createtable 6 0
    addField "citationId" $ citationId cit
    addField "citationPrefix" . Stringify @e $ citationPrefix cit
    addField "citationSuffix" . Stringify @e $ citationSuffix cit
    addField "citationMode" $ show (citationMode cit)
    addField "citationNoteNum" $ citationNoteNum cit
    addField "citationHash" $ citationHash cit

-- | Key-value pair, pushed as a table with @a@ as the only key and @v@ as the
-- associated value.
newtype KeyValue a b = KeyValue (a, b)

instance (Pushable a, Pushable b) => Pushable (KeyValue a b) where
  push (KeyValue (k, v)) = do
    Lua.newtable
    Lua.push k
    Lua.push v
    Lua.rawset (Lua.nth 3)

-- | Convert Pandoc to custom markup.
writeCustom :: (PandocMonad m, MonadIO m)
            => FilePath -> WriterOptions -> Pandoc -> m Text
writeCustom luaFile opts doc@(Pandoc meta _) = do
  let globals = [ PANDOC_DOCUMENT doc
                , PANDOC_SCRIPT_FILE luaFile
                , PANDOC_WRITER_OPTIONS opts
                ]
  res <- runLua $ do
    setGlobals globals
    stat <- dofileWithTraceback luaFile
    -- check for error in lua script (later we'll change the return type
    -- to handle this more gracefully):
    when (stat /= Lua.OK)
      Lua.throwErrorAsException
    (rendered, context) <- docToCustom opts doc
    metaContext <- metaToContext opts
                   (fmap (literal . pack) . blockListToCustom)
                   (fmap (literal . pack) . inlineListToCustom)
                   meta
    return (pack rendered, context <> metaContext)
  case res of
    Left msg -> throw msg
    Right (body, context) -> return $
      case writerTemplate opts of
        Nothing  -> body
        Just tpl -> render Nothing $
                    renderTemplate tpl $ setField "body" body context

docToCustom :: forall e. PeekError e
            => WriterOptions -> Pandoc -> LuaE e (String, Context Text)
docToCustom opts (Pandoc (Meta metamap) blocks) = do
  body <- blockListToCustom blocks
  -- invoke doesn't work with multiple return values, so we have to call
  -- `Doc` manually.
  Lua.getglobal "Doc"                 -- function
  push body                           -- argument 1
  push (fmap (Stringify @e) metamap)  -- argument 2
  push (writerVariables opts)         -- argument 3
  call 3 2
  rendered  <- peek (nth 2)           -- first return value
  context <- forcePeek . optional $ peekViaJSON top  -- snd return value
  return (rendered, fromMaybe mempty context)

-- | Convert Pandoc block element to Custom.
blockToCustom :: forall e. PeekError e
              => Block         -- ^ Block element
              -> LuaE e String

blockToCustom Null = return ""

blockToCustom (Plain inlines) = invoke @e "Plain" (Stringify @e inlines)

blockToCustom (Para [Image attr txt (src,tit)]) =
  invoke @e "CaptionedImage" src tit (Stringify @e txt) (attrToMap attr)

blockToCustom (Para inlines) = invoke @e "Para" (Stringify @e inlines)

blockToCustom (LineBlock linesList) =
  invoke @e "LineBlock" (map (Stringify @e) linesList)

blockToCustom (RawBlock format str) =
  invoke @e "RawBlock" (Stringify @e format) str

blockToCustom HorizontalRule = invoke @e "HorizontalRule"

blockToCustom (Header level attr inlines) =
  invoke @e "Header" level (Stringify @e inlines) (attrToMap attr)

blockToCustom (CodeBlock attr str) =
  invoke @e "CodeBlock" str (attrToMap attr)

blockToCustom (BlockQuote blocks) =
  invoke @e "BlockQuote" (Stringify @e blocks)

blockToCustom (Table _ blkCapt specs thead tbody tfoot) =
  let (capt, aligns, widths, headers, rows) = toLegacyTable blkCapt specs thead tbody tfoot
      aligns' = map show aligns
      capt' = Stringify @e capt
      headers' = map (Stringify @e) headers
      rows' = map (map (Stringify @e)) rows
  in invoke @e "Table" capt' aligns' widths headers' rows'

blockToCustom (BulletList items) =
  invoke @e "BulletList" (map (Stringify @e) items)

blockToCustom (OrderedList (num,sty,delim) items) =
  invoke @e "OrderedList" (map (Stringify @e) items) num (show sty) (show delim)

blockToCustom (DefinitionList items) =
  invoke @e "DefinitionList"
               (map (KeyValue . (Stringify @e *** map (Stringify @e))) items)

blockToCustom (Div attr items) =
  invoke @e "Div" (Stringify @e items) (attrToMap attr)

-- | Convert list of Pandoc block elements to Custom.
blockListToCustom :: forall e. PeekError e
                  => [Block]       -- ^ List of block elements
                  -> LuaE e String
blockListToCustom xs = do
  blocksep <- invoke @e "Blocksep"
  bs <- mapM blockToCustom xs
  return $ mconcat $ intersperse blocksep bs

-- | Convert list of Pandoc inline elements to Custom.
inlineListToCustom :: forall e. PeekError e => [Inline] -> LuaE e String
inlineListToCustom lst = do
  xs <- mapM (inlineToCustom @e) lst
  return $ mconcat xs

-- | Convert Pandoc inline element to Custom.
inlineToCustom :: forall e. PeekError e => Inline -> LuaE e String

inlineToCustom (Str str) = invoke @e "Str" str

inlineToCustom Space = invoke @e "Space"

inlineToCustom SoftBreak = invoke @e "SoftBreak"

inlineToCustom (Emph lst) = invoke @e "Emph" (Stringify @e lst)

inlineToCustom (Underline lst) = invoke @e "Underline" (Stringify @e lst)

inlineToCustom (Strong lst) = invoke @e "Strong" (Stringify @e lst)

inlineToCustom (Strikeout lst) = invoke @e "Strikeout" (Stringify @e lst)

inlineToCustom (Superscript lst) = invoke @e "Superscript" (Stringify @e lst)

inlineToCustom (Subscript lst) = invoke @e "Subscript" (Stringify @e lst)

inlineToCustom (SmallCaps lst) = invoke @e "SmallCaps" (Stringify @e lst)

inlineToCustom (Quoted SingleQuote lst) =
  invoke @e "SingleQuoted" (Stringify @e lst)

inlineToCustom (Quoted DoubleQuote lst) =
  invoke @e "DoubleQuoted" (Stringify @e lst)

inlineToCustom (Cite cs lst) =
  invoke @e "Cite" (Stringify @e lst) (map (Stringify @e) cs)

inlineToCustom (Code attr str) =
  invoke @e "Code" str (attrToMap attr)

inlineToCustom (Math DisplayMath str) =
  invoke @e "DisplayMath" str

inlineToCustom (Math InlineMath str) =
  invoke @e "InlineMath" str

inlineToCustom (RawInline format str) =
  invoke @e "RawInline" (Stringify @e format) str

inlineToCustom LineBreak = invoke @e "LineBreak"

inlineToCustom (Link attr txt (src,tit)) =
  invoke @e "Link" (Stringify @e txt) src tit (attrToMap attr)

inlineToCustom (Image attr alt (src,tit)) =
  invoke @e "Image" (Stringify @e alt) src tit (attrToMap attr)

inlineToCustom (Note contents) = invoke @e "Note" (Stringify @e contents)

inlineToCustom (Span attr items) =
  invoke @e "Span" (Stringify @e items) (attrToMap attr)
