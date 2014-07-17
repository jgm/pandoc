{-# LANGUAGE OverlappingInstances, FlexibleInstances, OverloadedStrings,
    ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- Copyright (C) 2012-2014 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2012-2014 John MacFarlane
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
import Scripting.Lua (LuaState, StackValue, callfunc)
import Text.Pandoc.Writers.Shared
import qualified Scripting.Lua as Lua
import Text.Pandoc.UTF8 (fromString, toString)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Monoid
import qualified Data.Map as M
import Text.Pandoc.Templates

attrToMap :: Attr -> M.Map ByteString ByteString
attrToMap (id',classes,keyvals) = M.fromList
    $ ("id", fromString id')
    : ("class", fromString $ unwords classes)
    : map (\(x,y) -> (fromString x, fromString y)) keyvals

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

instance StackValue ByteString where
    push l x = Lua.push l $ C8.unpack x
    peek l n = (fmap . fmap) C8.pack (Lua.peek l n)
    valuetype _ = Lua.TSTRING

instance StackValue a => StackValue [a] where
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

instance StackValue [Inline] where
  push l ils = Lua.push l . C8.unpack =<< inlineListToCustom l ils
  peek _ _ = undefined
  valuetype _ = Lua.TSTRING

instance StackValue [Block] where
  push l ils = Lua.push l . C8.unpack =<< blockListToCustom l ils
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
    let addValue ((k :: String), v) = Lua.push lua k >> Lua.push lua v >>
                          Lua.rawset lua (-3)
    addValue ("citationId", citationId cit)
    addValue ("citationPrefix", citationPrefix cit)
    addValue ("citationSuffix", citationSuffix cit)
    addValue ("citationMode", show (citationMode cit))
    addValue ("citationNoteNum", citationNoteNum cit)
    addValue ("citationHash", citationHash cit)
  peek = undefined
  valuetype _ = Lua.TTABLE

-- | Convert Pandoc to custom markup.
writeCustom :: FilePath -> WriterOptions -> Pandoc -> IO String
writeCustom luaFile opts doc@(Pandoc meta _) = do
  luaScript <- C8.unpack `fmap` C8.readFile luaFile
  lua <- Lua.newstate
  Lua.openlibs lua
  Lua.loadstring lua luaScript "custom"
  Lua.call lua 0 0
  -- TODO - call hierarchicalize, so we have that info
  rendered <- docToCustom lua opts doc
  context <- metaToJSON opts
             (fmap toString . blockListToCustom lua)
             (fmap toString . inlineListToCustom lua)
             meta
  Lua.close lua
  let body = toString rendered
  if writerStandalone opts
     then do
       let context' = setField "body" body context
       return $ renderTemplate' (writerTemplate opts) context'
     else return body

docToCustom :: LuaState -> WriterOptions -> Pandoc -> IO ByteString
docToCustom lua opts (Pandoc (Meta metamap) blocks) = do
  body <- blockListToCustom lua blocks
  callfunc lua "Doc" body metamap (writerVariables opts)

-- | Convert Pandoc block element to Custom.
blockToCustom :: LuaState      -- ^ Lua state
              -> Block         -- ^ Block element
              -> IO ByteString

blockToCustom _ Null = return ""

blockToCustom lua (Plain inlines) = callfunc lua "Plain" inlines

blockToCustom lua (Para [Image txt (src,tit)]) =
  callfunc lua "CaptionedImage" src tit txt

blockToCustom lua (Para inlines) = callfunc lua "Para" inlines

blockToCustom lua (RawBlock format str) =
  callfunc lua "RawBlock" format (fromString str)

blockToCustom lua HorizontalRule = callfunc lua "HorizontalRule"

blockToCustom lua (Header level attr inlines) =
  callfunc lua "Header" level inlines (attrToMap attr)

blockToCustom lua (CodeBlock attr str) =
  callfunc lua "CodeBlock" (fromString str) (attrToMap attr)

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
                  -> IO ByteString
blockListToCustom lua xs = do
  blocksep <- callfunc lua "Blocksep"
  bs <- mapM (blockToCustom lua) xs
  return $ mconcat $ intersperse blocksep bs

-- | Convert list of Pandoc inline elements to Custom.
inlineListToCustom :: LuaState -> [Inline] -> IO ByteString
inlineListToCustom lua lst = do
  xs <- mapM (inlineToCustom lua) lst
  return $ C8.concat xs

-- | Convert Pandoc inline element to Custom.
inlineToCustom :: LuaState -> Inline -> IO ByteString

inlineToCustom lua (Str str) = callfunc lua "Str" $ fromString str

inlineToCustom lua Space = callfunc lua "Space"

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
  callfunc lua "Code" (fromString str) (attrToMap attr)

inlineToCustom lua (Math DisplayMath str) =
  callfunc lua "DisplayMath" (fromString str)

inlineToCustom lua (Math InlineMath str) =
  callfunc lua "InlineMath" (fromString str)

inlineToCustom lua (RawInline format str) =
  callfunc lua "RawInline" format (fromString str)

inlineToCustom lua (LineBreak) = callfunc lua "LineBreak"

inlineToCustom lua (Link txt (src,tit)) =
  callfunc lua "Link" txt (fromString src) (fromString tit)

inlineToCustom lua (Image alt (src,tit)) =
  callfunc lua "Image" alt (fromString src) (fromString tit)

inlineToCustom lua (Note contents) = callfunc lua "Note" contents

inlineToCustom lua (Span attr items) =
  callfunc lua "Span" items (attrToMap attr)
