{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, OverloadedStrings,
    ScopedTypeVariables, DeriveDataTypeable, CPP #-}
#if MIN_VERSION_base(4,8,0)
#else
{-# LANGUAGE OverlappingInstances #-}
#endif
{- Copyright (C) 2012-2015 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2012-2015 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to custom markup using
a lua writer.
-}
module Text.Pandoc.Writers.Custom ( writeCustom ) where
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Data.List ( intersperse )
import Data.Char ( toLower )
import Data.Typeable
import Scripting.Lua (LuaState, StackValue, callfunc)
import Text.Pandoc.Writers.Shared
import qualified Scripting.Lua as Lua
import qualified Text.Pandoc.UTF8 as UTF8
import Control.Monad (when)
import Control.Exception
import qualified Data.Map as M
import Text.Pandoc.Templates
import GHC.IO.Encoding (getForeignEncoding,setForeignEncoding, utf8)

attrToMap :: Attr -> M.Map String String
attrToMap (id',classes,keyvals) = M.fromList
    $ ("id", id')
    : ("class", unwords classes)
    : keyvals

#if MIN_VERSION_hslua(0,4,0)
#if MIN_VERSION_base(4,8,0)
instance {-# OVERLAPS #-} StackValue [Char] where
#else
instance StackValue [Char] where
#endif
  push lua cs = Lua.push lua (UTF8.fromString cs)
  peek lua i = do
                 res <- Lua.peek lua i
                 return $ UTF8.toString `fmap` res
  valuetype _ = Lua.TSTRING
#else
#if MIN_VERSION_base(4,8,0)
instance {-# OVERLAPS #-} StackValue a => StackValue [a] where
#else
instance StackValue a => StackValue [a] where
#endif
  push lua xs = do
    Lua.createtable lua (length xs + 1) 0
    let addValue (i, x) = Lua.push lua x >> Lua.rawseti lua (-2) i
    mapM_ addValue $ zip [1..] xs
  peek lua i = do
    top <- Lua.gettop lua
    let i' = if i < 0 then top + i + 1 else i
    Lua.pushnil lua
    lst <- getList lua i'
    Lua.pop lua 1
    return (Just lst)
  valuetype _ = Lua.TTABLE

getList :: StackValue a => LuaState -> Int -> IO [a]
getList lua i' = do
  continue <- Lua.next lua i'
  if continue
     then do
       next <- Lua.peek lua (-1)
       Lua.pop lua 1
       x <- maybe (fail "peek returned Nothing") return next
       rest <- getList lua i'
       return (x : rest)
     else return []
#endif

instance StackValue Format where
  push lua (Format f) = Lua.push lua (map toLower f)
  peek l n = fmap Format `fmap` Lua.peek l n
  valuetype _ = Lua.TSTRING

instance (StackValue a, StackValue b) => StackValue (M.Map a b) where
  push lua m = do
    let xs = M.toList m
    Lua.createtable lua (length xs + 1) 0
    let addValue (k, v) = Lua.push lua k >> Lua.push lua v >>
                          Lua.rawset lua (-3)
    mapM_ addValue xs
  peek _ _ = undefined -- not needed for our purposes
  valuetype _ = Lua.TTABLE

instance (StackValue a, StackValue b) => StackValue (a,b) where
  push lua (k,v) = do
    Lua.createtable lua 2 0
    Lua.push lua k
    Lua.push lua v
    Lua.rawset lua (-3)
  peek _ _ = undefined -- not needed for our purposes
  valuetype _ = Lua.TTABLE

#if MIN_VERSION_base(4,8,0)
instance {-# OVERLAPS #-} StackValue [Inline] where
#else
instance StackValue [Inline] where
#endif
  push l ils = Lua.push l =<< inlineListToCustom l ils
  peek _ _ = undefined
  valuetype _ = Lua.TSTRING

#if MIN_VERSION_base(4,8,0)
instance {-# OVERLAPS #-} StackValue [Block] where
#else
instance StackValue [Block] where
#endif
  push l ils = Lua.push l =<< blockListToCustom l ils
  peek _ _ = undefined
  valuetype _ = Lua.TSTRING

instance StackValue MetaValue where
  push l (MetaMap m) = Lua.push l m
  push l (MetaList xs) = Lua.push l xs
  push l (MetaBool x) = Lua.push l x
  push l (MetaString s) = Lua.push l s
  push l (MetaInlines ils) = Lua.push l ils
  push l (MetaBlocks bs) = Lua.push l bs
  peek _ _ = undefined
  valuetype (MetaMap _) = Lua.TTABLE
  valuetype (MetaList _) = Lua.TTABLE
  valuetype (MetaBool _) = Lua.TBOOLEAN
  valuetype (MetaString _) = Lua.TSTRING
  valuetype (MetaInlines _) = Lua.TSTRING
  valuetype (MetaBlocks _) = Lua.TSTRING

instance StackValue Citation where
  push lua cit = do
    Lua.createtable lua 6 0
    let addValue (k :: String, v) = Lua.push lua k >> Lua.push lua v >>
                          Lua.rawset lua (-3)
    addValue ("citationId", citationId cit)
    addValue ("citationPrefix", citationPrefix cit)
    addValue ("citationSuffix", citationSuffix cit)
    addValue ("citationMode", show (citationMode cit))
    addValue ("citationNoteNum", citationNoteNum cit)
    addValue ("citationHash", citationHash cit)
  peek = undefined
  valuetype _ = Lua.TTABLE

data PandocLuaException = PandocLuaException String
    deriving (Show, Typeable)

instance Exception PandocLuaException

-- | Convert Pandoc to custom markup.
writeCustom :: FilePath -> WriterOptions -> Pandoc -> IO String
writeCustom luaFile opts doc@(Pandoc meta _) = do
  luaScript <- UTF8.readFile luaFile
  enc <- getForeignEncoding
  setForeignEncoding utf8
  lua <- Lua.newstate
  Lua.openlibs lua
  status <- Lua.loadstring lua luaScript luaFile
  -- check for error in lua script (later we'll change the return type
  -- to handle this more gracefully):
  when (status /= 0) $
#if MIN_VERSION_hslua(0,4,0)
    Lua.tostring lua 1 >>= throw . PandocLuaException . UTF8.toString
#else
    Lua.tostring lua 1 >>= throw . PandocLuaException
#endif
  Lua.call lua 0 0
  -- TODO - call hierarchicalize, so we have that info
  rendered <- docToCustom lua opts doc
  context <- metaToJSON opts
             (blockListToCustom lua)
             (inlineListToCustom lua)
             meta
  Lua.close lua
  setForeignEncoding enc
  let body = rendered
  if writerStandalone opts
     then do
       let context' = setField "body" body context
       return $ renderTemplate' (writerTemplate opts) context'
     else return body

docToCustom :: LuaState -> WriterOptions -> Pandoc -> IO String
docToCustom lua opts (Pandoc (Meta metamap) blocks) = do
  body <- blockListToCustom lua blocks
  callfunc lua "Doc" body metamap (writerVariables opts)

-- | Convert Pandoc block element to Custom.
blockToCustom :: LuaState      -- ^ Lua state
              -> Block         -- ^ Block element
              -> IO String

blockToCustom _ Null = return ""

blockToCustom lua (Plain inlines) = callfunc lua "Plain" inlines

blockToCustom lua (Para [Image attr txt (src,tit)]) =
  callfunc lua "CaptionedImage" src tit txt (attrToMap attr)

blockToCustom lua (Para inlines) = callfunc lua "Para" inlines

blockToCustom lua (RawBlock format str) =
  callfunc lua "RawBlock" format str

blockToCustom lua HorizontalRule = callfunc lua "HorizontalRule"

blockToCustom lua (Header level attr inlines) =
  callfunc lua "Header" level inlines (attrToMap attr)

blockToCustom lua (CodeBlock attr str) =
  callfunc lua "CodeBlock" str (attrToMap attr)

blockToCustom lua (BlockQuote blocks) = callfunc lua "BlockQuote" blocks

blockToCustom lua (Table capt aligns widths headers rows') =
  callfunc lua "Table" capt (map show aligns) widths headers rows'

blockToCustom lua (BulletList items) = callfunc lua "BulletList" items

blockToCustom lua (OrderedList (num,sty,delim) items) =
  callfunc lua "OrderedList" items num (show sty) (show delim)

blockToCustom lua (DefinitionList items) =
  callfunc lua "DefinitionList" items

blockToCustom lua (Div attr items) =
  callfunc lua "Div" items (attrToMap attr)

-- | Convert list of Pandoc block elements to Custom.
blockListToCustom :: LuaState -- ^ Options
                  -> [Block]       -- ^ List of block elements
                  -> IO String
blockListToCustom lua xs = do
  blocksep <- callfunc lua "Blocksep"
  bs <- mapM (blockToCustom lua) xs
  return $ mconcat $ intersperse blocksep bs

-- | Convert list of Pandoc inline elements to Custom.
inlineListToCustom :: LuaState -> [Inline] -> IO String
inlineListToCustom lua lst = do
  xs <- mapM (inlineToCustom lua) lst
  return $ concat xs

-- | Convert Pandoc inline element to Custom.
inlineToCustom :: LuaState -> Inline -> IO String

inlineToCustom lua (Str str) = callfunc lua "Str" str

inlineToCustom lua Space = callfunc lua "Space"

inlineToCustom lua SoftBreak = callfunc lua "SoftBreak"

inlineToCustom lua (Emph lst) = callfunc lua "Emph" lst

inlineToCustom lua (Strong lst) = callfunc lua "Strong" lst

inlineToCustom lua (Strikeout lst) = callfunc lua "Strikeout" lst

inlineToCustom lua (Superscript lst) = callfunc lua "Superscript" lst

inlineToCustom lua (Subscript lst) = callfunc lua "Subscript" lst

inlineToCustom lua (SmallCaps lst) = callfunc lua "SmallCaps" lst

inlineToCustom lua (Quoted SingleQuote lst) = callfunc lua "SingleQuoted" lst

inlineToCustom lua (Quoted DoubleQuote lst) = callfunc lua "DoubleQuoted" lst

inlineToCustom lua (Cite cs lst) = callfunc lua "Cite" lst cs

inlineToCustom lua (Code attr str) =
  callfunc lua "Code" str (attrToMap attr)

inlineToCustom lua (Math DisplayMath str) =
  callfunc lua "DisplayMath" str

inlineToCustom lua (Math InlineMath str) =
  callfunc lua "InlineMath" str

inlineToCustom lua (RawInline format str) =
  callfunc lua "RawInline" format str

inlineToCustom lua (LineBreak) = callfunc lua "LineBreak"

inlineToCustom lua (Link attr txt (src,tit)) =
  callfunc lua "Link" txt src tit (attrToMap attr)

inlineToCustom lua (Image attr alt (src,tit)) =
  callfunc lua "Image" alt src tit (attrToMap attr)

inlineToCustom lua (Note contents) = callfunc lua "Note" contents

inlineToCustom lua (Span attr items) =
  callfunc lua "Span" items (attrToMap attr)
