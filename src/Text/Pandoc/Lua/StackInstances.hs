{-# LANGUAGE NoImplicitPrelude #-}
{-
Copyright © 2012-2018 John MacFarlane <jgm@berkeley.edu>
            2017-2018 Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>

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
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{- |
   Module      : Text.Pandoc.Lua.StackInstances
   Copyright   : © 2012-2018 John MacFarlane
                 © 2017-2018 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

StackValue instances for pandoc types.
-}
module Text.Pandoc.Lua.StackInstances () where

import Prelude
import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.Catch (finally)
import Data.Data (showConstr, toConstr)
import Data.Foldable (forM_)
import Foreign.Lua (FromLuaStack (peek), Lua, LuaInteger, LuaNumber, StackIndex,
                    ToLuaStack (push), Type (..), throwLuaError, tryLua)
import Text.Pandoc.Definition
import Text.Pandoc.Extensions (Extensions)
import Text.Pandoc.Lua.Util (pushViaConstructor, typeCheck)
import Text.Pandoc.Options (ReaderOptions (..), TrackChanges)
import Text.Pandoc.Shared (Element (Blk, Sec), safeRead)

import qualified Foreign.Lua as Lua
import qualified Data.Set as Set
import qualified Text.Pandoc.Lua.Util as LuaUtil

defineHowTo :: String -> Lua a -> Lua a
defineHowTo ctx op = op `Lua.modifyLuaError` (("Could not " ++ ctx ++ ": ") ++)

instance ToLuaStack Pandoc where
  push (Pandoc meta blocks) =
    pushViaConstructor "Pandoc" blocks meta

instance FromLuaStack Pandoc where
  peek idx = defineHowTo "get Pandoc value" $ do
    typeCheck idx Lua.TypeTable
    blocks <- LuaUtil.rawField idx "blocks"
    meta   <- Lua.getfield idx "meta" *> (Lua.peek Lua.stackTop `finally` Lua.pop 1)
    return $ Pandoc meta blocks

instance ToLuaStack Meta where
  push (Meta mmap) =
    pushViaConstructor "Meta" mmap
instance FromLuaStack Meta where
  peek idx = defineHowTo "get Meta value" $ do
    typeCheck idx Lua.TypeTable
    Meta <$> peek idx

instance ToLuaStack MetaValue where
  push = pushMetaValue
instance FromLuaStack MetaValue where
  peek = peekMetaValue

instance ToLuaStack Block where
  push = pushBlock

instance FromLuaStack Block where
  peek = peekBlock

-- Inline
instance ToLuaStack Inline where
  push = pushInline

instance FromLuaStack Inline where
  peek = peekInline

-- Citation
instance ToLuaStack Citation where
  push (Citation cid prefix suffix mode noteNum hash) =
    pushViaConstructor "Citation" cid mode prefix suffix noteNum hash

instance FromLuaStack Citation where
  peek idx = do
    id' <- LuaUtil.rawField idx "id"
    prefix <- LuaUtil.rawField idx "prefix"
    suffix <- LuaUtil.rawField idx "suffix"
    mode <- LuaUtil.rawField idx "mode"
    num <- LuaUtil.rawField idx "note_num"
    hash <- LuaUtil.rawField idx "hash"
    return $ Citation id' prefix suffix mode num hash

instance ToLuaStack Alignment where
  push = push . show
instance FromLuaStack Alignment where
  peek idx = safeRead' =<< peek idx

instance ToLuaStack CitationMode where
  push = push . show
instance FromLuaStack CitationMode where
  peek idx = safeRead' =<< peek idx

instance ToLuaStack Format where
  push (Format f) = push f
instance FromLuaStack Format where
  peek idx = Format <$> peek idx

instance ToLuaStack ListNumberDelim where
  push = push . show
instance FromLuaStack ListNumberDelim where
  peek idx = safeRead' =<< peek idx

instance ToLuaStack ListNumberStyle where
  push = push . show
instance FromLuaStack ListNumberStyle where
  peek idx = safeRead' =<< peek idx

instance ToLuaStack MathType where
  push = push . show
instance FromLuaStack MathType where
  peek idx = safeRead' =<< peek idx

instance ToLuaStack QuoteType where
  push = push . show
instance FromLuaStack QuoteType where
  peek idx = safeRead' =<< peek idx

instance ToLuaStack Double where
  push = push . (realToFrac :: Double -> LuaNumber)
instance FromLuaStack Double where
  peek = fmap (realToFrac :: LuaNumber -> Double) . peek

instance ToLuaStack Int where
  push = push . (fromIntegral :: Int -> LuaInteger)
instance FromLuaStack Int where
  peek = fmap (fromIntegral :: LuaInteger-> Int) . peek

safeRead' :: Read a => String -> Lua a
safeRead' s = case safeRead s of
  Nothing -> throwLuaError ("Could not read: " ++ s)
  Just x  -> return x

-- | Push an meta value element to the top of the lua stack.
pushMetaValue :: MetaValue -> Lua ()
pushMetaValue = \case
  MetaBlocks blcks  -> pushViaConstructor "MetaBlocks" blcks
  MetaBool bool     -> push bool
  MetaInlines inlns -> pushViaConstructor "MetaInlines" inlns
  MetaList metalist -> pushViaConstructor "MetaList" metalist
  MetaMap metamap   -> pushViaConstructor "MetaMap" metamap
  MetaString str    -> push str

-- | Interpret the value at the given stack index as meta value.
peekMetaValue :: StackIndex -> Lua MetaValue
peekMetaValue idx = defineHowTo "get MetaValue" $ do
  -- Get the contents of an AST element.
  let elementContent :: FromLuaStack a => Lua a
      elementContent = peek idx
  luatype <- Lua.ltype idx
  case luatype of
    TypeBoolean -> MetaBool <$> peek idx
    TypeString  -> MetaString <$> peek idx
    TypeTable   -> do
      tag <- tryLua $ LuaUtil.getTag idx
      case tag of
        Right "MetaBlocks"  -> MetaBlocks  <$> elementContent
        Right "MetaBool"    -> MetaBool    <$> elementContent
        Right "MetaMap"     -> MetaMap     <$> elementContent
        Right "MetaInlines" -> MetaInlines <$> elementContent
        Right "MetaList"    -> MetaList    <$> elementContent
        Right "MetaString"  -> MetaString  <$> elementContent
        Right t             -> throwLuaError ("Unknown meta tag: " ++ t)
        Left _ -> do
          -- no meta value tag given, try to guess.
          len <- Lua.rawlen idx
          if len <= 0
            then MetaMap <$> peek idx
            else  (MetaInlines <$> peek idx)
                  <|> (MetaBlocks <$> peek idx)
                  <|> (MetaList <$> peek idx)
    _        -> throwLuaError "could not get meta value"

-- | Push an block element to the top of the lua stack.
pushBlock :: Block -> Lua ()
pushBlock = \case
  BlockQuote blcks         -> pushViaConstructor "BlockQuote" blcks
  BulletList items         -> pushViaConstructor "BulletList" items
  CodeBlock attr code      -> pushViaConstructor "CodeBlock" code (LuaAttr attr)
  DefinitionList items     -> pushViaConstructor "DefinitionList" items
  Div attr blcks           -> pushViaConstructor "Div" blcks (LuaAttr attr)
  Header lvl attr inlns    -> pushViaConstructor "Header" lvl inlns (LuaAttr attr)
  HorizontalRule           -> pushViaConstructor "HorizontalRule"
  LineBlock blcks          -> pushViaConstructor "LineBlock" blcks
  OrderedList lstAttr list -> pushViaConstructor "OrderedList" list lstAttr
  Null                     -> pushViaConstructor "Null"
  Para blcks               -> pushViaConstructor "Para" blcks
  Plain blcks              -> pushViaConstructor "Plain" blcks
  RawBlock f cs            -> pushViaConstructor "RawBlock" f cs
  Table capt aligns widths headers rows ->
    pushViaConstructor "Table" capt aligns widths headers rows

-- | Return the value at the given index as block if possible.
peekBlock :: StackIndex -> Lua Block
peekBlock idx = defineHowTo "get Block value" $ do
  typeCheck idx Lua.TypeTable
  tag <- LuaUtil.getTag idx
  case tag of
      "BlockQuote"     -> BlockQuote <$> elementContent
      "BulletList"     -> BulletList <$> elementContent
      "CodeBlock"      -> withAttr CodeBlock <$> elementContent
      "DefinitionList" -> DefinitionList <$> elementContent
      "Div"            -> withAttr Div <$> elementContent
      "Header"         -> (\(lvl, LuaAttr attr, lst) -> Header lvl attr lst)
                          <$> elementContent
      "HorizontalRule" -> return HorizontalRule
      "LineBlock"      -> LineBlock <$> elementContent
      "OrderedList"    -> uncurry OrderedList <$> elementContent
      "Null"           -> return Null
      "Para"           -> Para <$> elementContent
      "Plain"          -> Plain <$> elementContent
      "RawBlock"       -> uncurry RawBlock <$> elementContent
      "Table"          -> (\(capt, aligns, widths, headers, body) ->
                                  Table capt aligns widths headers body)
                          <$> elementContent
      _ -> throwLuaError ("Unknown block type: " ++ tag)
 where
   -- Get the contents of an AST element.
   elementContent :: FromLuaStack a => Lua a
   elementContent = LuaUtil.rawField idx "c"

-- | Push an inline element to the top of the lua stack.
pushInline :: Inline -> Lua ()
pushInline = \case
  Cite citations lst       -> pushViaConstructor "Cite" lst citations
  Code attr lst            -> pushViaConstructor "Code" lst (LuaAttr attr)
  Emph inlns               -> pushViaConstructor "Emph" inlns
  Image attr alt (src,tit) -> pushViaConstructor "Image" alt src tit (LuaAttr attr)
  LineBreak                -> pushViaConstructor "LineBreak"
  Link attr lst (src,tit)  -> pushViaConstructor "Link" lst src tit (LuaAttr attr)
  Note blcks               -> pushViaConstructor "Note" blcks
  Math mty str             -> pushViaConstructor "Math" mty str
  Quoted qt inlns          -> pushViaConstructor "Quoted" qt inlns
  RawInline f cs           -> pushViaConstructor "RawInline" f cs
  SmallCaps inlns          -> pushViaConstructor "SmallCaps" inlns
  SoftBreak                -> pushViaConstructor "SoftBreak"
  Space                    -> pushViaConstructor "Space"
  Span attr inlns          -> pushViaConstructor "Span" inlns (LuaAttr attr)
  Str str                  -> pushViaConstructor "Str" str
  Strikeout inlns          -> pushViaConstructor "Strikeout" inlns
  Strong inlns             -> pushViaConstructor "Strong" inlns
  Subscript inlns          -> pushViaConstructor "Subscript" inlns
  Superscript inlns        -> pushViaConstructor "Superscript" inlns

-- | Return the value at the given index as inline if possible.
peekInline :: StackIndex -> Lua Inline
peekInline idx = defineHowTo "get Inline value" $ do
  typeCheck idx Lua.TypeTable
  tag <- LuaUtil.getTag idx
  case tag of
    "Cite"       -> uncurry Cite <$> elementContent
    "Code"       -> withAttr Code <$> elementContent
    "Emph"       -> Emph <$> elementContent
    "Image"      -> (\(LuaAttr attr, lst, tgt) -> Image attr lst tgt)
                    <$> elementContent
    "Link"       -> (\(LuaAttr attr, lst, tgt) -> Link attr lst tgt)
                    <$> elementContent
    "LineBreak"  -> return LineBreak
    "Note"       -> Note <$> elementContent
    "Math"       -> uncurry Math <$> elementContent
    "Quoted"     -> uncurry Quoted <$> elementContent
    "RawInline"  -> uncurry RawInline <$> elementContent
    "SmallCaps"  -> SmallCaps <$> elementContent
    "SoftBreak"  -> return SoftBreak
    "Space"      -> return Space
    "Span"       -> withAttr Span <$> elementContent
    "Str"        -> Str <$> elementContent
    "Strikeout"  -> Strikeout <$> elementContent
    "Strong"     -> Strong <$> elementContent
    "Subscript"  -> Subscript <$> elementContent
    "Superscript"-> Superscript <$> elementContent
    _ -> throwLuaError ("Unknown inline type: " ++ tag)
 where
   -- Get the contents of an AST element.
   elementContent :: FromLuaStack a => Lua a
   elementContent = LuaUtil.rawField idx "c"

withAttr :: (Attr -> a -> b) -> (LuaAttr, a) -> b
withAttr f (attributes, x) = f (fromLuaAttr attributes) x

-- | Wrapper for Attr
newtype LuaAttr = LuaAttr { fromLuaAttr :: Attr }

instance ToLuaStack LuaAttr where
  push (LuaAttr (id', classes, kv)) =
    pushViaConstructor "Attr" id' classes kv

instance FromLuaStack LuaAttr where
  peek idx = defineHowTo "get Attr value" (LuaAttr <$> peek idx)

--
-- Hierarchical elements
--
instance ToLuaStack Element where
  push (Blk blk) = push blk
  push (Sec lvl num attr label contents) = do
    Lua.newtable
    LuaUtil.addField "level" lvl
    LuaUtil.addField "numbering" num
    LuaUtil.addField "attr" (LuaAttr attr)
    LuaUtil.addField "label" label
    LuaUtil.addField "contents" contents
    pushSecMetaTable
    Lua.setmetatable (-2)
      where
        pushSecMetaTable :: Lua ()
        pushSecMetaTable = do
          inexistant <- Lua.newmetatable "PandocElementSec"
          when inexistant $ do
            LuaUtil.addField "t" "Sec"
            Lua.push "__index"
            Lua.pushvalue (-2)
            Lua.rawset (-3)


--
-- Reader Options
--
instance ToLuaStack Extensions where
  push exts = push (show exts)

instance ToLuaStack TrackChanges where
  push = push . showConstr . toConstr

instance ToLuaStack a => ToLuaStack (Set.Set a) where
  push set = do
    Lua.newtable
    forM_ set (`LuaUtil.addValue` True)

instance ToLuaStack ReaderOptions where
  push ro = do
    let ReaderOptions
          (extensions            :: Extensions)
          (standalone            :: Bool)
          (columns               :: Int)
          (tabStop               :: Int)
          (indentedCodeClasses   :: [String])
          (abbreviations         :: Set.Set String)
          (defaultImageExtension :: String)
          (trackChanges          :: TrackChanges)
          (stripComments         :: Bool)
          = ro
    Lua.newtable
    LuaUtil.addField "extensions" extensions
    LuaUtil.addField "standalone" standalone
    LuaUtil.addField "columns" columns
    LuaUtil.addField "tabStop" tabStop
    LuaUtil.addField "indentedCodeClasses" indentedCodeClasses
    LuaUtil.addField "abbreviations" abbreviations
    LuaUtil.addField "defaultImageExtension" defaultImageExtension
    LuaUtil.addField "trackChanges" trackChanges
    LuaUtil.addField "stripComments" stripComments
