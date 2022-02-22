{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{- |
   Module      : Text.Pandoc.Lua.Writer.Classic
   Copyright   : Copyright (C) 2012-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of Pandoc documents using a \"classic\" custom Lua writer.
-}
module Text.Pandoc.Lua.Writer.Classic
  ( runCustom
  ) where
import Control.Applicative (optional)
import Control.Arrow ((***))
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text (Text, pack)
import HsLua as Lua hiding (Operation (Div))
#if !MIN_VERSION_hslua(2,2,0)
import HsLua.Aeson (peekViaJSON)
#endif
import Text.DocLayout (literal, render)
import Text.DocTemplates (Context)
import Text.Pandoc.Definition
import Text.Pandoc.Lua.Marshal.Attr (pushAttributeList)
import Text.Pandoc.Lua.Orphans ()
import Text.Pandoc.Options
import Text.Pandoc.Templates (renderTemplate)
import Text.Pandoc.Writers.Shared

-- | List of key-value pairs that is pushed to Lua as AttributeList
-- userdata.
newtype AttributeList = AttributeList [(Text, Text)]
instance Pushable AttributeList where
  push (AttributeList kvs) = pushAttributeList kvs

attrToMap :: Attr -> AttributeList
attrToMap (id',classes,keyvals) = AttributeList
    $ ("id", id')
    : ("class", T.unwords classes)
    : keyvals

newtype Stringify a = Stringify a

instance Pushable (Stringify Format) where
  push (Stringify (Format f)) = Lua.push (T.toLower f)

instance Pushable (Stringify [Inline]) where
  push (Stringify ils) = Lua.push =<< inlineListToCustom ils

instance Pushable (Stringify [Block]) where
  push (Stringify blks) = Lua.push =<< blockListToCustom blks

instance Pushable (Stringify MetaValue) where
  push (Stringify (MetaMap m))       = Lua.push (fmap Stringify m)
  push (Stringify (MetaList xs))     = Lua.push (map Stringify xs)
  push (Stringify (MetaBool x))      = Lua.push x
  push (Stringify (MetaString s))    = Lua.push s
  push (Stringify (MetaInlines ils)) = Lua.push (Stringify ils)
  push (Stringify (MetaBlocks bs))   = Lua.push (Stringify bs)

instance Pushable (Stringify Citation) where
  push (Stringify cit) = flip pushAsTable cit
    [ ("citationId", push . citationId)
    , ("citationPrefix",  push . Stringify . citationPrefix)
    , ("citationSuffix",  push . Stringify . citationSuffix)
    , ("citationMode",    push . citationMode)
    , ("citationNoteNum", push . citationNoteNum)
    , ("citationHash",    push . citationHash)
    ]

-- | Key-value pair, pushed as a table with @a@ as the only key and @v@ as the
-- associated value.
newtype KeyValue a b = KeyValue (a, b)

instance (Pushable a, Pushable b) => Pushable (KeyValue a b) where
  push (KeyValue (k, v)) = do
    Lua.newtable
    Lua.push k
    Lua.push v
    Lua.rawset (Lua.nth 3)

-- | Convert Pandoc to custom markup using a classic Lua writer.
runCustom :: LuaError e
          => WriterOptions
          -> Pandoc
          -> LuaE e Text
runCustom opts doc@(Pandoc meta _) = do
  (body, context) <- docToCustom opts doc
  -- convert metavalues to a template context (variables)
  metaContext <- metaToContext opts
                   (fmap (literal . pack) . blockListToCustom)
                   (fmap (literal . pack) . inlineListToCustom)
                   meta
  -- merge contexts from metadata and variables
  let renderContext = context <> metaContext
  return $ case writerTemplate opts of
    Nothing  -> body
    Just tpl -> render Nothing $
                renderTemplate tpl $ setField "body" body renderContext

-- | Converts a Pandoc value to custom markup using a classic Lua writer.
docToCustom :: forall e. LuaError e
            => WriterOptions -> Pandoc -> LuaE e (Text, Context Text)
docToCustom opts (Pandoc (Meta metamap) blocks) = do
  body <- blockListToCustom blocks
  -- invoke doesn't work with multiple return values, so we have to call
  -- `Doc` manually.
  Lua.getglobal "Doc"                 -- function
  push body                           -- argument 1
  push (fmap Stringify  metamap)      -- argument 2
  push (writerVariables opts)         -- argument 3
  call 3 2
  rendered  <- peek (nth 2)       -- first return value
  context <- forcePeek . optional $ peekViaJSON top  -- snd return value
  return (rendered, fromMaybe mempty context)


-- | Convert Pandoc block element to Custom.
blockToCustom :: forall e. LuaError e
              => Block         -- ^ Block element
              -> LuaE e String

blockToCustom Null = return ""

blockToCustom (Plain inlines) = invoke "Plain" (Stringify inlines)

blockToCustom (Para [Image attr txt (src,tit)]) =
  invoke "CaptionedImage" src tit (Stringify txt) (attrToMap attr)

blockToCustom (Para inlines) = invoke "Para" (Stringify inlines)

blockToCustom (LineBlock linesList) =
  invoke "LineBlock" (map (Stringify) linesList)

blockToCustom (RawBlock format str) =
  invoke "RawBlock" (Stringify format) str

blockToCustom HorizontalRule = invoke "HorizontalRule"

blockToCustom (Header level attr inlines) =
  invoke "Header" level (Stringify inlines) (attrToMap attr)

blockToCustom (CodeBlock attr str) =
  invoke "CodeBlock" str (attrToMap attr)

blockToCustom (BlockQuote blocks) =
  invoke "BlockQuote" (Stringify blocks)

blockToCustom (Table _ blkCapt specs thead tbody tfoot) =
  let (capt, aligns, widths, headers, rows) = toLegacyTable blkCapt specs thead tbody tfoot
      aligns' = map show aligns
      capt' = Stringify capt
      headers' = map (Stringify) headers
      rows' = map (map (Stringify)) rows
  in invoke "Table" capt' aligns' widths headers' rows'

blockToCustom (BulletList items) =
  invoke "BulletList" (map (Stringify) items)

blockToCustom (OrderedList (num,sty,delim) items) =
  invoke "OrderedList" (map (Stringify) items) num (show sty) (show delim)

blockToCustom (DefinitionList items) =
  invoke "DefinitionList"
               (map (KeyValue . (Stringify *** map (Stringify))) items)

blockToCustom (Div attr items) =
  invoke "Div" (Stringify items) (attrToMap attr)

-- | Convert list of Pandoc block elements to Custom.
blockListToCustom :: forall e. LuaError e
                  => [Block]       -- ^ List of block elements
                  -> LuaE e String
blockListToCustom xs = do
  blocksep <- invoke "Blocksep"
  bs <- mapM blockToCustom xs
  return $ mconcat $ intersperse blocksep bs

-- | Convert list of Pandoc inline elements to Custom.
inlineListToCustom :: forall e. LuaError e => [Inline] -> LuaE e String
inlineListToCustom lst = do
  xs <- mapM (inlineToCustom @e) lst
  return $ mconcat xs

-- | Convert Pandoc inline element to Custom.
inlineToCustom :: forall e. LuaError e => Inline -> LuaE e String

inlineToCustom (Str str) = invoke "Str" str

inlineToCustom Space = invoke "Space"

inlineToCustom SoftBreak = invoke "SoftBreak"

inlineToCustom (Emph lst) = invoke "Emph" (Stringify lst)

inlineToCustom (Underline lst) = invoke "Underline" (Stringify lst)

inlineToCustom (Strong lst) = invoke "Strong" (Stringify lst)

inlineToCustom (Strikeout lst) = invoke "Strikeout" (Stringify lst)

inlineToCustom (Superscript lst) = invoke "Superscript" (Stringify lst)

inlineToCustom (Subscript lst) = invoke "Subscript" (Stringify lst)

inlineToCustom (SmallCaps lst) = invoke "SmallCaps" (Stringify lst)

inlineToCustom (Quoted SingleQuote lst) =
  invoke "SingleQuoted" (Stringify lst)

inlineToCustom (Quoted DoubleQuote lst) =
  invoke "DoubleQuoted" (Stringify lst)

inlineToCustom (Cite cs lst) =
  invoke "Cite" (Stringify lst) (map (Stringify) cs)

inlineToCustom (Code attr str) =
  invoke "Code" str (attrToMap attr)

inlineToCustom (Math DisplayMath str) =
  invoke "DisplayMath" str

inlineToCustom (Math InlineMath str) =
  invoke "InlineMath" str

inlineToCustom (RawInline format str) =
  invoke "RawInline" (Stringify format) str

inlineToCustom LineBreak = invoke "LineBreak"

inlineToCustom (Link attr txt (src,tit)) =
  invoke "Link" (Stringify txt) src tit (attrToMap attr)

inlineToCustom (Image attr alt (src,tit)) =
  invoke "Image" (Stringify alt) src tit (attrToMap attr)

inlineToCustom (Note contents) = invoke "Note" (Stringify contents)

inlineToCustom (Span attr items) =
  invoke "Span" (Stringify items) (attrToMap attr)
