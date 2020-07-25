{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE LambdaCase           #-}
{- |
   Module      : Text.Pandoc.Lua.Marshaling.AST
   Copyright   : © 2012-2020 John MacFarlane
                 © 2017-2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Marshaling/unmarshaling instances for document AST elements.
-}
module Text.Pandoc.Lua.Marshaling.AST
  ( LuaAttr (..)
  , LuaListAttributes (..)
  ) where

import Control.Applicative ((<|>))
import Foreign.Lua (Lua, Peekable, Pushable, StackIndex)
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Lua.Util (defineHowTo, pushViaConstructor)
import Text.Pandoc.Lua.Marshaling.CommonState ()

import qualified Control.Monad.Catch as Catch
import qualified Foreign.Lua as Lua
import qualified Text.Pandoc.Lua.Util as LuaUtil

instance Pushable Pandoc where
  push (Pandoc meta blocks) =
    pushViaConstructor "Pandoc" blocks meta

instance Peekable Pandoc where
  peek idx = defineHowTo "get Pandoc value" $ do
    blocks <- LuaUtil.rawField idx "blocks"
    meta   <- LuaUtil.rawField idx "meta"
    return $ Pandoc meta blocks

instance Pushable Meta where
  push (Meta mmap) =
    pushViaConstructor "Meta" mmap
instance Peekable Meta where
  peek idx = defineHowTo "get Meta value" $
    Meta <$> Lua.peek idx

instance Pushable MetaValue where
  push = pushMetaValue
instance Peekable MetaValue where
  peek = peekMetaValue

instance Pushable Block where
  push = pushBlock

instance Peekable Block where
  peek = peekBlock

-- Inline
instance Pushable Inline where
  push = pushInline

instance Peekable Inline where
  peek = peekInline

-- Citation
instance Pushable Citation where
  push (Citation cid prefix suffix mode noteNum hash) =
    pushViaConstructor "Citation" cid mode prefix suffix noteNum hash

instance Peekable Citation where
  peek idx = do
    id' <- LuaUtil.rawField idx "id"
    prefix <- LuaUtil.rawField idx "prefix"
    suffix <- LuaUtil.rawField idx "suffix"
    mode <- LuaUtil.rawField idx "mode"
    num <- LuaUtil.rawField idx "note_num"
    hash <- LuaUtil.rawField idx "hash"
    return $ Citation id' prefix suffix mode num hash

instance Pushable Alignment where
  push = Lua.push . show
instance Peekable Alignment where
  peek = Lua.peekRead

instance Pushable CitationMode where
  push = Lua.push . show
instance Peekable CitationMode where
  peek = Lua.peekRead

instance Pushable Format where
  push (Format f) = Lua.push f
instance Peekable Format where
  peek idx = Format <$> Lua.peek idx

instance Pushable ListNumberDelim where
  push = Lua.push . show
instance Peekable ListNumberDelim where
  peek = Lua.peekRead

instance Pushable ListNumberStyle where
  push = Lua.push . show
instance Peekable ListNumberStyle where
  peek = Lua.peekRead

instance Pushable MathType where
  push = Lua.push . show
instance Peekable MathType where
  peek = Lua.peekRead

instance Pushable QuoteType where
  push = Lua.push . show
instance Peekable QuoteType where
  peek = Lua.peekRead

-- | Push an meta value element to the top of the lua stack.
pushMetaValue :: MetaValue -> Lua ()
pushMetaValue = \case
  MetaBlocks blcks  -> pushViaConstructor "MetaBlocks" blcks
  MetaBool bool     -> Lua.push bool
  MetaInlines inlns -> pushViaConstructor "MetaInlines" inlns
  MetaList metalist -> pushViaConstructor "MetaList" metalist
  MetaMap metamap   -> pushViaConstructor "MetaMap" metamap
  MetaString str    -> Lua.push str

-- | Interpret the value at the given stack index as meta value.
peekMetaValue :: StackIndex -> Lua MetaValue
peekMetaValue idx = defineHowTo "get MetaValue" $ do
  -- Get the contents of an AST element.
  let elementContent :: Peekable a => Lua a
      elementContent = Lua.peek idx
  luatype <- Lua.ltype idx
  case luatype of
    Lua.TypeBoolean -> MetaBool <$> Lua.peek idx
    Lua.TypeString  -> MetaString <$> Lua.peek idx
    Lua.TypeTable   -> do
      tag <- try $ LuaUtil.getTag idx
      case tag of
        Right "MetaBlocks"  -> MetaBlocks  <$> elementContent
        Right "MetaBool"    -> MetaBool    <$> elementContent
        Right "MetaMap"     -> MetaMap     <$> elementContent
        Right "MetaInlines" -> MetaInlines <$> elementContent
        Right "MetaList"    -> MetaList    <$> elementContent
        Right "MetaString"  -> MetaString  <$> elementContent
        Right t             -> Lua.throwMessage ("Unknown meta tag: " <> t)
        Left _ -> do
          -- no meta value tag given, try to guess.
          len <- Lua.rawlen idx
          if len <= 0
            then MetaMap <$> Lua.peek idx
            else  (MetaInlines <$> Lua.peek idx)
                  <|> (MetaBlocks <$> Lua.peek idx)
                  <|> (MetaList <$> Lua.peek idx)
    _        -> Lua.throwMessage "could not get meta value"

-- | Push a block element to the top of the Lua stack.
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
  OrderedList lstAttr list -> pushViaConstructor "OrderedList" list
                                                 (LuaListAttributes lstAttr)
  Null                     -> pushViaConstructor "Null"
  Para blcks               -> pushViaConstructor "Para" blcks
  Plain blcks              -> pushViaConstructor "Plain" blcks
  RawBlock f cs            -> pushViaConstructor "RawBlock" f cs
  Table attr blkCapt specs thead tbody tfoot ->
    pushViaConstructor "Table" blkCapt specs thead tbody tfoot attr

-- | Return the value at the given index as block if possible.
peekBlock :: StackIndex -> Lua Block
peekBlock idx = defineHowTo "get Block value" $ do
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
      "OrderedList"    -> (\(LuaListAttributes lstAttr, lst) ->
                             OrderedList lstAttr lst)
                          <$> elementContent
      "Null"           -> return Null
      "Para"           -> Para <$> elementContent
      "Plain"          -> Plain <$> elementContent
      "RawBlock"       -> uncurry RawBlock <$> elementContent
      "Table"          -> (\(attr, capt, colSpecs, thead, tbodies, tfoot) ->
                              Table (fromLuaAttr attr)
                                    capt
                                    colSpecs
                                    thead
                                    tbodies
                                    tfoot)
                          <$> elementContent
      _ -> Lua.throwMessage ("Unknown block type: " <> tag)
 where
   -- Get the contents of an AST element.
   elementContent :: Peekable a => Lua a
   elementContent = LuaUtil.rawField idx "c"

instance Pushable Caption where
  push = pushCaption

instance Peekable Caption where
  peek = peekCaption

-- | Push Caption element
pushCaption :: Caption -> Lua ()
pushCaption (Caption shortCaption longCaption) = do
  Lua.newtable
  LuaUtil.addField "short" (Lua.Optional shortCaption)
  LuaUtil.addField "long" longCaption

-- | Peek Caption element
peekCaption :: StackIndex -> Lua Caption
peekCaption idx = do
  short <- Lua.fromOptional <$> LuaUtil.rawField idx "short"
  long  <- LuaUtil.rawField idx "long"
  return $ Caption short long

instance Peekable ColWidth where
  peek idx = do
    width <- Lua.fromOptional <$> Lua.peek idx
    return $ case width of
      Nothing -> ColWidthDefault
      Just w  -> ColWidth w

instance Pushable ColWidth where
  push = \case
    (ColWidth w)    -> Lua.push w
    ColWidthDefault -> Lua.pushnil

instance Pushable Row where
  push (Row attr cells) = Lua.push (attr, cells)

instance Peekable Row where
  peek = fmap (uncurry Row) . Lua.peek

instance Pushable TableBody where
  push (TableBody attr (RowHeadColumns rowHeadColumns) head' body) = do
    Lua.newtable
    LuaUtil.addField "attr" attr
    LuaUtil.addField "row_head_columns" rowHeadColumns
    LuaUtil.addField "head" head'
    LuaUtil.addField "body" body

instance Peekable TableBody where
  peek idx = do
    attr <- LuaUtil.rawField idx "attr"
    rowHeadColumns <- LuaUtil.rawField idx "row_head_columns"
    head' <- LuaUtil.rawField idx "head"
    body <- LuaUtil.rawField idx "body"
    return $ TableBody attr (RowHeadColumns rowHeadColumns) head' body

instance Pushable TableHead where
  push (TableHead attr cells) = Lua.push (attr, cells)

instance Peekable TableHead where
  peek = fmap (uncurry TableHead) . Lua.peek

instance Pushable TableFoot where
  push (TableFoot attr cells) = Lua.push (attr, cells)

instance Peekable TableFoot where
  peek = fmap (uncurry TableFoot) . Lua.peek

instance Pushable Cell where
  push = pushCell

instance Peekable Cell where
  peek = peekCell

pushCell :: Cell -> Lua ()
pushCell (Cell attr align (RowSpan rowSpan) (ColSpan colSpan) contents) = do
  Lua.newtable
  LuaUtil.addField "attr" attr
  LuaUtil.addField "alignment" align
  LuaUtil.addField "row_span" rowSpan
  LuaUtil.addField "col_span" colSpan
  LuaUtil.addField "contents" contents

peekCell :: StackIndex -> Lua Cell
peekCell idx = do
  attr <- fromLuaAttr <$> LuaUtil.rawField idx "attr"
  align <- LuaUtil.rawField idx "alignment"
  rowSpan <- LuaUtil.rawField idx "row_span"
  colSpan <- LuaUtil.rawField idx "col_span"
  contents <- LuaUtil.rawField idx "contents"
  return $ Cell attr align (RowSpan rowSpan) (ColSpan colSpan) contents

-- | Push an inline element to the top of the lua stack.
pushInline :: Inline -> Lua ()
pushInline = \case
  Cite citations lst       -> pushViaConstructor "Cite" lst citations
  Code attr lst            -> pushViaConstructor "Code" lst (LuaAttr attr)
  Emph inlns               -> pushViaConstructor "Emph" inlns
  Underline inlns          -> pushViaConstructor "Underline" inlns
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
  tag <- LuaUtil.getTag idx
  case tag of
    "Cite"       -> uncurry Cite <$> elementContent
    "Code"       -> withAttr Code <$> elementContent
    "Emph"       -> Emph <$> elementContent
    "Underline"  -> Underline <$> elementContent
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
    _ -> Lua.throwMessage ("Unknown inline type: " <> tag)
 where
   -- Get the contents of an AST element.
   elementContent :: Peekable a => Lua a
   elementContent = LuaUtil.rawField idx "c"

try :: Lua a -> Lua (Either PandocError a)
try = Catch.try

withAttr :: (Attr -> a -> b) -> (LuaAttr, a) -> b
withAttr f (attributes, x) = f (fromLuaAttr attributes) x

-- | Wrapper for Attr
newtype LuaAttr = LuaAttr { fromLuaAttr :: Attr }

instance Pushable LuaAttr where
  push (LuaAttr (id', classes, kv)) =
    pushViaConstructor "Attr" id' classes kv

instance Peekable LuaAttr where
  peek idx = defineHowTo "get Attr value" (LuaAttr <$> Lua.peek idx)

-- | Wrapper for ListAttributes
newtype LuaListAttributes = LuaListAttributes  ListAttributes

instance Pushable LuaListAttributes where
  push (LuaListAttributes (start, style, delimiter)) =
    pushViaConstructor "ListAttributes" start style delimiter

instance Peekable LuaListAttributes where
  peek = defineHowTo "get ListAttributes value" .
         fmap LuaListAttributes . Lua.peek
