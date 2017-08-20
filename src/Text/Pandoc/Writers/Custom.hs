{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleInstances    #-}
#if MIN_VERSION_base(4,8,0)
#else
{-# LANGUAGE OverlappingInstances #-}
#endif
{- Copyright (C) 2012-2017 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2012-2017 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to custom markup using
a lua writer.
-}
module Text.Pandoc.Writers.Custom ( writeCustom ) where
import Control.Exception
import Control.Monad (when)
import Data.Char (toLower)
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Text (Text, pack)
import Data.Typeable
import GHC.IO.Encoding (getForeignEncoding, setForeignEncoding, utf8)
import Foreign.Lua (Lua, ToLuaStack (..), callFunc, runLua)
import Foreign.Lua.Api
import Text.Pandoc.Error
import Text.Pandoc.Lua.Util ( addValue )
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Templates
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Writers.Shared

attrToMap :: Attr -> M.Map String String
attrToMap (id',classes,keyvals) = M.fromList
    $ ("id", id')
    : ("class", unwords classes)
    : keyvals

instance ToLuaStack Double where
  push = push . (realToFrac :: Double -> LuaNumber)

instance ToLuaStack Int where
  push = push . (fromIntegral :: Int -> LuaInteger)

instance ToLuaStack Format where
  push (Format f) = push (map toLower f)

#if MIN_VERSION_base(4,8,0)
instance {-# OVERLAPS #-} ToLuaStack [Inline] where
#else
instance ToLuaStack [Inline] where
#endif
  push ils = push =<< inlineListToCustom ils

#if MIN_VERSION_base(4,8,0)
instance {-# OVERLAPS #-} ToLuaStack [Block] where
#else
instance ToLuaStack [Block] where
#endif
  push ils = push =<< blockListToCustom ils

instance ToLuaStack MetaValue where
  push (MetaMap m)       = push m
  push (MetaList xs)     = push xs
  push (MetaBool x)      = push x
  push (MetaString s)    = push s
  push (MetaInlines ils) = push ils
  push (MetaBlocks bs)   = push bs

instance ToLuaStack Citation where
  push cit = do
    createtable 6 0
    addValue "citationId" $ citationId cit
    addValue "citationPrefix" $ citationPrefix cit
    addValue "citationSuffix" $ citationSuffix cit
    addValue "citationMode" $ show (citationMode cit)
    addValue "citationNoteNum" $ citationNoteNum cit
    addValue "citationHash" $ citationHash cit

data PandocLuaException = PandocLuaException String
    deriving (Show, Typeable)

instance Exception PandocLuaException

-- | Convert Pandoc to custom markup.
writeCustom :: FilePath -> WriterOptions -> Pandoc -> IO Text
writeCustom luaFile opts doc@(Pandoc meta _) = do
  luaScript <- UTF8.readFile luaFile
  enc <- getForeignEncoding
  setForeignEncoding utf8
  (body, context) <- runLua $ do
    openlibs
    stat <- loadstring luaScript
    -- check for error in lua script (later we'll change the return type
    -- to handle this more gracefully):
    when (stat /= OK) $
      tostring 1 >>= throw . PandocLuaException . UTF8.toString
    call 0 0
  -- TODO - call hierarchicalize, so we have that info
    rendered <- docToCustom opts doc
    context <- metaToJSON opts
               blockListToCustom
               inlineListToCustom
               meta
    return (rendered, context)
  setForeignEncoding enc
  case writerTemplate opts of
       Nothing  -> return $ pack body
       Just tpl ->
         case applyTemplate (pack tpl) $ setField "body" body context of
              Left e  -> throw (PandocTemplateError e)
              Right r -> return (pack r)

docToCustom :: WriterOptions -> Pandoc -> Lua String
docToCustom opts (Pandoc (Meta metamap) blocks) = do
  body <- blockListToCustom blocks
  callFunc "Doc" body metamap (writerVariables opts)

-- | Convert Pandoc block element to Custom.
blockToCustom :: Block         -- ^ Block element
              -> Lua String

blockToCustom Null = return ""

blockToCustom (Plain inlines) = callFunc "Plain" inlines

blockToCustom (Para [Image attr txt (src,tit)]) =
  callFunc "CaptionedImage" src tit txt (attrToMap attr)

blockToCustom (Para inlines) = callFunc "Para" inlines

blockToCustom (LineBlock linesList) = callFunc "LineBlock" linesList

blockToCustom (RawBlock format str) =
  callFunc "RawBlock" format str

blockToCustom HorizontalRule = callFunc "HorizontalRule"

blockToCustom (Header level attr inlines) =
  callFunc "Header" level inlines (attrToMap attr)

blockToCustom (CodeBlock attr str) =
  callFunc "CodeBlock" str (attrToMap attr)

blockToCustom (BlockQuote blocks) = callFunc "BlockQuote" blocks

blockToCustom (Table capt aligns widths headers rows') =
  callFunc "Table" capt (map show aligns) widths headers rows'

blockToCustom (BulletList items) = callFunc "BulletList" items

blockToCustom (OrderedList (num,sty,delim) items) =
  callFunc "OrderedList" items num (show sty) (show delim)

blockToCustom (DefinitionList items) =
  callFunc "DefinitionList" items

blockToCustom (Div attr items) =
  callFunc "Div" items (attrToMap attr)

blockToCustom (Figure attr (Caption short long) items) =
  callFunc "Figure" short long items (attrToMap attr)

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

inlineToCustom (Emph lst) = callFunc "Emph" lst

inlineToCustom (Strong lst) = callFunc "Strong" lst

inlineToCustom (Strikeout lst) = callFunc "Strikeout" lst

inlineToCustom (Superscript lst) = callFunc "Superscript" lst

inlineToCustom (Subscript lst) = callFunc "Subscript" lst

inlineToCustom (SmallCaps lst) = callFunc "SmallCaps" lst

inlineToCustom (Quoted SingleQuote lst) = callFunc "SingleQuoted" lst

inlineToCustom (Quoted DoubleQuote lst) = callFunc "DoubleQuoted" lst

inlineToCustom (Cite cs lst) = callFunc "Cite" lst cs

inlineToCustom (Code attr str) =
  callFunc "Code" str (attrToMap attr)

inlineToCustom (Math DisplayMath str) =
  callFunc "DisplayMath" str

inlineToCustom (Math InlineMath str) =
  callFunc "InlineMath" str

inlineToCustom (RawInline format str) =
  callFunc "RawInline" format str

inlineToCustom (LineBreak) = callFunc "LineBreak"

inlineToCustom (Link attr txt (src,tit)) =
  callFunc "Link" txt src tit (attrToMap attr)

inlineToCustom (Image attr alt (src,tit)) =
  callFunc "Image" alt src tit (attrToMap attr)

inlineToCustom (Note contents) = callFunc "Note" contents

inlineToCustom (Span attr items) =
  callFunc "Span" items (attrToMap attr)
