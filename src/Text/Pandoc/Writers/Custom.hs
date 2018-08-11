{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
{- Copyright (C) 2012-2018 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Writers.Custom
   Copyright   : Copyright (C) 2012-2018 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to custom markup using
a lua writer.
-}
module Text.Pandoc.Writers.Custom ( writeCustom ) where
import Prelude
import Control.Arrow ((***))
import Control.Exception
import Control.Monad (when)
import Control.Monad.Trans (MonadIO (liftIO))
import Data.Char (toLower)
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Text (Text, pack)
import Data.Typeable
import Foreign.Lua (Lua, ToLuaStack (..), callFunc)
import Foreign.Lua.Api
import Text.Pandoc.Class (PandocIO)
import Text.Pandoc.Definition
import Text.Pandoc.Error
import Text.Pandoc.Lua.Init (runPandocLua, registerScriptPath)
import Text.Pandoc.Lua.StackInstances ()
import Text.Pandoc.Lua.Util (addField, addValue, dostring')
import Text.Pandoc.Options
import Text.Pandoc.Templates
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Writers.Shared

attrToMap :: Attr -> M.Map String String
attrToMap (id',classes,keyvals) = M.fromList
    $ ("id", id')
    : ("class", unwords classes)
    : keyvals

newtype Stringify a = Stringify a

instance ToLuaStack (Stringify Format) where
  push (Stringify (Format f)) = push (map toLower f)

instance ToLuaStack (Stringify [Inline]) where
  push (Stringify ils) = push =<< inlineListToCustom ils

instance ToLuaStack (Stringify [Block]) where
  push (Stringify blks) = push =<< blockListToCustom blks

instance ToLuaStack (Stringify MetaValue) where
  push (Stringify (MetaMap m))       = push (fmap Stringify m)
  push (Stringify (MetaList xs))     = push (map Stringify xs)
  push (Stringify (MetaBool x))      = push x
  push (Stringify (MetaString s))    = push s
  push (Stringify (MetaInlines ils)) = push (Stringify ils)
  push (Stringify (MetaBlocks bs))   = push (Stringify bs)

instance ToLuaStack (Stringify Citation) where
  push (Stringify cit) = do
    createtable 6 0
    addField "citationId" $ citationId cit
    addField "citationPrefix" . Stringify $ citationPrefix cit
    addField "citationSuffix" . Stringify $ citationSuffix cit
    addField "citationMode" $ show (citationMode cit)
    addField "citationNoteNum" $ citationNoteNum cit
    addField "citationHash" $ citationHash cit

-- | Key-value pair, pushed as a table with @a@ as the only key and @v@ as the
-- associated value.
newtype KeyValue a b = KeyValue (a, b)

instance (ToLuaStack a, ToLuaStack b) => ToLuaStack (KeyValue a b) where
  push (KeyValue (k, v)) = do
    newtable
    addValue k v

data PandocLuaException = PandocLuaException String
    deriving (Show, Typeable)

instance Exception PandocLuaException

-- | Convert Pandoc to custom markup.
writeCustom :: FilePath -> WriterOptions -> Pandoc -> PandocIO Text
writeCustom luaFile opts doc@(Pandoc meta _) = do
  luaScript <- liftIO $ UTF8.readFile luaFile
  res <- runPandocLua $ do
    registerScriptPath luaFile
    stat <- dostring' luaScript
    -- check for error in lua script (later we'll change the return type
    -- to handle this more gracefully):
    when (stat /= OK) $
      tostring (-1) >>= throw . PandocLuaException . UTF8.toString
    -- TODO - call hierarchicalize, so we have that info
    rendered <- docToCustom opts doc
    context <- metaToJSON opts
               blockListToCustom
               inlineListToCustom
               meta
    return (rendered, context)
  let (body, context) = case res of
        Left e -> throw (PandocLuaException (show e))
        Right x -> x
  case writerTemplate opts of
       Nothing  -> return $ pack body
       Just tpl ->
         case applyTemplate (pack tpl) $ setField "body" body context of
              Left e  -> throw (PandocTemplateError e)
              Right r -> return (pack r)

docToCustom :: WriterOptions -> Pandoc -> Lua String
docToCustom opts (Pandoc (Meta metamap) blocks) = do
  body <- blockListToCustom blocks
  callFunc "Doc" body (fmap Stringify metamap) (writerVariables opts)

-- | Convert Pandoc block element to Custom.
blockToCustom :: Block         -- ^ Block element
              -> Lua String

blockToCustom Null = return ""

blockToCustom (Plain inlines) = callFunc "Plain" (Stringify inlines)

blockToCustom (Para [Image attr txt (src,tit)]) =
  callFunc "CaptionedImage" src tit (Stringify txt) (attrToMap attr)

blockToCustom (Para inlines) = callFunc "Para" (Stringify inlines)

blockToCustom (LineBlock linesList) = callFunc "LineBlock" (map Stringify linesList)

blockToCustom (RawBlock format str) =
  callFunc "RawBlock" (Stringify format) str

blockToCustom HorizontalRule = callFunc "HorizontalRule"

blockToCustom (Header level attr inlines) =
  callFunc "Header" level (Stringify inlines) (attrToMap attr)

blockToCustom (CodeBlock attr str) =
  callFunc "CodeBlock" str (attrToMap attr)

blockToCustom (BlockQuote blocks) = callFunc "BlockQuote" (Stringify blocks)

blockToCustom (Table capt aligns widths headers rows) =
  let aligns' = map show aligns
      capt' = Stringify capt
      headers' = map Stringify headers
      rows' = map (map Stringify) rows
  in callFunc "Table" capt' aligns' widths headers' rows'

blockToCustom (BulletList items) = callFunc "BulletList" (map Stringify items)

blockToCustom (OrderedList (num,sty,delim) items) =
  callFunc "OrderedList" (map Stringify items) num (show sty) (show delim)

blockToCustom (DefinitionList items) =
  callFunc "DefinitionList"
           (map (KeyValue . (Stringify *** map Stringify)) items)

blockToCustom (Div attr items) =
  callFunc "Div" (Stringify items) (attrToMap attr)

-- | Convert list of Pandoc block elements to Custom.
blockListToCustom :: [Block]       -- ^ List of block elements
                  -> Lua String
blockListToCustom xs = do
  blocksep <- callFunc "Blocksep"
  bs <- mapM blockToCustom xs
  return $ mconcat $ intersperse blocksep bs

-- | Convert list of Pandoc inline elements to Custom.
inlineListToCustom :: [Inline] -> Lua String
inlineListToCustom lst = do
  xs <- mapM inlineToCustom lst
  return $ mconcat xs

-- | Convert Pandoc inline element to Custom.
inlineToCustom :: Inline -> Lua String

inlineToCustom (Str str) = callFunc "Str" str

inlineToCustom Space = callFunc "Space"

inlineToCustom SoftBreak = callFunc "SoftBreak"

inlineToCustom (Emph lst) = callFunc "Emph" (Stringify lst)

inlineToCustom (Strong lst) = callFunc "Strong" (Stringify lst)

inlineToCustom (Strikeout lst) = callFunc "Strikeout" (Stringify lst)

inlineToCustom (Superscript lst) = callFunc "Superscript" (Stringify lst)

inlineToCustom (Subscript lst) = callFunc "Subscript" (Stringify lst)

inlineToCustom (SmallCaps lst) = callFunc "SmallCaps" (Stringify lst)

inlineToCustom (Quoted SingleQuote lst) = callFunc "SingleQuoted" (Stringify lst)

inlineToCustom (Quoted DoubleQuote lst) = callFunc "DoubleQuoted" (Stringify lst)

inlineToCustom (Cite cs lst) = callFunc "Cite" (Stringify lst) (map Stringify cs)

inlineToCustom (Code attr str) =
  callFunc "Code" str (attrToMap attr)

inlineToCustom (Math DisplayMath str) =
  callFunc "DisplayMath" str

inlineToCustom (Math InlineMath str) =
  callFunc "InlineMath" str

inlineToCustom (RawInline format str) =
  callFunc "RawInline" (Stringify format) str

inlineToCustom LineBreak = callFunc "LineBreak"

inlineToCustom (Link attr txt (src,tit)) =
  callFunc "Link" (Stringify txt) src tit (attrToMap attr)

inlineToCustom (Image attr alt (src,tit)) =
  callFunc "Image" (Stringify alt) src tit (attrToMap attr)

inlineToCustom (Note contents) = callFunc "Note" (Stringify contents)

inlineToCustom (Span attr items) =
  callFunc "Span" (Stringify items) (attrToMap attr)
