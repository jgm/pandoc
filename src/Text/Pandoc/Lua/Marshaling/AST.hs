{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{- |
   Module      : Text.Pandoc.Lua.Marshaling.AST
   Copyright   : © 2012-2021 John MacFarlane
                 © 2017-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb+pandoc@moltkeplatz.de>
   Stability   : alpha

Marshaling/unmarshaling instances for document AST elements.
-}
module Text.Pandoc.Lua.Marshaling.AST
  ( peekAttr
  , peekBlock
  , peekBlocks
  , peekCaption
  , peekCitation
  , peekInline
  , peekInlines
  , peekListAttributes
  , peekMeta
  , peekMetaValue
  , peekPandoc

  , pushAttr
  , pushBlock
  , pushInline
  , pushListAttributes
  , pushMetaValue
  , pushPandoc
  ) where

import Control.Applicative ((<|>), optional)
import Control.Monad ((<$!>), (>=>))
import HsLua hiding (Operation (Div))
import Text.Pandoc.Definition
import Text.Pandoc.Lua.Util (pushViaConstr', pushViaConstructor)
import Text.Pandoc.Lua.Marshaling.Attr (peekAttr, pushAttr)
import Text.Pandoc.Lua.Marshaling.List (pushPandocList)

import qualified HsLua as Lua
import qualified Text.Pandoc.Lua.Util as LuaUtil

instance Pushable Pandoc where
  push = pushPandoc

pushPandoc :: LuaError e => Pusher e Pandoc
pushPandoc = pushUD typePandoc

peekPandoc :: LuaError e => Peeker e Pandoc
peekPandoc = retrieving "Pandoc value" . peekUD typePandoc

typePandoc :: LuaError e => DocumentedType e Pandoc
typePandoc = deftype "Pandoc"
  [ operation Eq $ defun "__eq"
     ### liftPure2 (==)
     <#> parameter (optional . peekPandoc) "doc1" "pandoc" ""
     <#> parameter (optional . peekPandoc) "doc2" "pandoc" ""
     =#> functionResult pushBool "boolean" "true iff the two values are equal"
  ]
  [ property "blocks" "list of blocks"
      (pushPandocList pushBlock, \(Pandoc _ blks) -> blks)
      (peekList peekBlock, \(Pandoc m _) blks -> Pandoc m blks)
  , property "meta" "document metadata"
      (pushMeta, \(Pandoc meta _) -> meta)
      (peekMeta, \(Pandoc _ blks) meta -> Pandoc meta blks)
  ]

instance Pushable Meta where
  push = pushMeta

pushMeta :: LuaError e => Pusher e Meta
pushMeta (Meta mmap) = pushViaConstr' "Meta" [push mmap]

peekMeta :: LuaError e => Peeker e Meta
peekMeta idx = retrieving "Meta" $
  Meta <$!> peekMap peekText peekMetaValue idx

instance Pushable MetaValue where
  push = pushMetaValue

instance Pushable Block where
  push = pushBlock

-- Inline
instance Pushable Inline where
  push = pushInline

-- Citation
instance Pushable Citation where
  push (Citation cid prefix suffix mode noteNum hash) =
    pushViaConstr' "Citation"
    [ push cid, push mode, push prefix, push suffix, push noteNum, push hash
    ]

peekCitation :: LuaError e => Peeker e Citation
peekCitation = fmap (retrieving "Citation")
  . typeChecked "table" Lua.istable $ \idx -> do
      idx' <- liftLua $ absindex idx
      Citation
        <$!> peekFieldRaw peekText "id" idx'
        <*>  peekFieldRaw (peekList peekInline) "prefix" idx'
        <*>  peekFieldRaw (peekList peekInline) "suffix" idx'
        <*>  peekFieldRaw peekRead "mode" idx'
        <*>  peekFieldRaw peekIntegral "note_num" idx'
        <*>  peekFieldRaw peekIntegral "hash" idx'


instance Pushable Alignment where
  push = Lua.pushString . show

instance Pushable CitationMode where
  push = Lua.push . show

instance Pushable Format where
  push (Format f) = Lua.push f

peekFormat :: LuaError e => Peeker e Format
peekFormat idx = Format <$!> peekText idx

instance Pushable ListNumberDelim where
  push = Lua.push . show

instance Pushable ListNumberStyle where
  push = Lua.push . show

instance Pushable MathType where
  push = Lua.push . show

instance Pushable QuoteType where
  push = Lua.push . show

-- | Push an meta value element to the top of the lua stack.
pushMetaValue :: LuaError e => MetaValue -> LuaE e ()
pushMetaValue = \case
  MetaBlocks blcks  -> pushViaConstr' "MetaBlocks" [pushList pushBlock blcks]
  MetaBool bool     -> Lua.push bool
  MetaInlines inlns -> pushViaConstr' "MetaInlines"
                       [pushList pushInline inlns]
  MetaList metalist -> pushViaConstr' "MetaList"
                       [pushList pushMetaValue metalist]
  MetaMap metamap   -> pushViaConstr' "MetaMap"
                       [pushMap pushText pushMetaValue metamap]
  MetaString str    -> Lua.push str

-- | Interpret the value at the given stack index as meta value.
peekMetaValue :: forall e. LuaError e => Peeker e MetaValue
peekMetaValue = retrieving "MetaValue $ " . \idx -> do
  -- Get the contents of an AST element.
  let mkMV :: (a -> MetaValue) -> Peeker e a -> Peek e MetaValue
      mkMV f p = f <$!> p idx

      peekTagged = \case
        "MetaBlocks"  -> mkMV MetaBlocks $
          retrieving "MetaBlocks" . peekBlocks
        "MetaBool"    -> mkMV MetaBool $
          retrieving "MetaBool" . peekBool
        "MetaMap"     -> mkMV MetaMap $
          retrieving "MetaMap" . peekMap peekText peekMetaValue
        "MetaInlines" -> mkMV MetaInlines $
          retrieving "MetaInlines" . peekInlines
        "MetaList"    -> mkMV MetaList $
          retrieving "MetaList" . peekList peekMetaValue
        "MetaString"  -> mkMV MetaString $
          retrieving "MetaString" . peekText
        (Name t)      -> failPeek ("Unknown meta tag: " <> t)

      peekUntagged = do
        -- no meta value tag given, try to guess.
        len <- liftLua $ Lua.rawlen idx
        if len <= 0
          then MetaMap <$!> peekMap peekText peekMetaValue idx
          else  (MetaInlines <$!> peekInlines idx)
            <|> (MetaBlocks <$!> peekBlocks idx)
            <|> (MetaList <$!> peekList peekMetaValue idx)
  luatype <- liftLua $ Lua.ltype idx
  case luatype of
    Lua.TypeBoolean -> MetaBool <$!> peekBool idx
    Lua.TypeString  -> MetaString <$!> peekText idx
    Lua.TypeTable   -> do
      optional (LuaUtil.getTag idx) >>= \case
        Just tag -> peekTagged tag
        Nothing  -> peekUntagged
    _        -> failPeek "could not get meta value"

-- | Push a block element to the top of the Lua stack.
pushBlock :: forall e. LuaError e => Block -> LuaE e ()
pushBlock = \case
  BlockQuote blcks         -> pushViaConstructor @e "BlockQuote" blcks
  BulletList items         -> pushViaConstructor @e "BulletList" items
  CodeBlock attr code      -> pushViaConstr' @e "CodeBlock"
                              [ push code, pushAttr attr ]
  DefinitionList items     -> pushViaConstructor @e "DefinitionList" items
  Div attr blcks           -> pushViaConstr' @e "Div"
                              [push blcks, pushAttr attr]
  Header lvl attr inlns    -> pushViaConstr' @e "Header"
                              [push lvl, push inlns, pushAttr attr]
  HorizontalRule           -> pushViaConstructor @e "HorizontalRule"
  LineBlock blcks          -> pushViaConstructor @e "LineBlock" blcks
  OrderedList lstAttr list -> pushViaConstr' @e "OrderedList"
                              [ push list, pushListAttributes @e lstAttr ]
  Null                     -> pushViaConstructor @e "Null"
  Para blcks               -> pushViaConstructor @e "Para" blcks
  Plain blcks              -> pushViaConstructor @e "Plain" blcks
  RawBlock f cs            -> pushViaConstructor @e "RawBlock" f cs
  Table attr blkCapt specs thead tbody tfoot ->
    pushViaConstr' @e "Table"
    [ pushCaption blkCapt, push specs, push thead, push tbody
    , push tfoot, pushAttr attr]

-- | Return the value at the given index as block if possible.
peekBlock :: forall e. LuaError e => Peeker e Block
peekBlock = fmap (retrieving "Block")
  . typeChecked "table" Lua.istable
  $ \idx -> do
  -- Get the contents of an AST element.
  let mkBlock :: (a -> Block) -> Peeker e a -> Peek e Block
      mkBlock f p = f <$!> peekFieldRaw p "c" idx
  LuaUtil.getTag idx >>= \case
      "BlockQuote"     -> mkBlock BlockQuote peekBlocks
      "BulletList"     -> mkBlock BulletList (peekList peekBlocks)
      "CodeBlock"      -> mkBlock (uncurry CodeBlock)
                                  (peekPair peekAttr peekText)
      "DefinitionList" -> mkBlock DefinitionList
                          (peekList (peekPair peekInlines (peekList peekBlocks)))
      "Div"            -> mkBlock (uncurry Div) (peekPair peekAttr peekBlocks)
      "Header"         -> mkBlock (\(lvl, attr, lst) -> Header lvl attr lst)
                          (peekTriple peekIntegral peekAttr peekInlines)
      "HorizontalRule" -> return HorizontalRule
      "LineBlock"      -> mkBlock LineBlock (peekList peekInlines)
      "OrderedList"    -> mkBlock (uncurry OrderedList)
                          (peekPair peekListAttributes (peekList peekBlocks))
      "Null"           -> return Null
      "Para"           -> mkBlock Para peekInlines
      "Plain"          -> mkBlock Plain peekInlines
      "RawBlock"       -> mkBlock (uncurry RawBlock)
                                  (peekPair peekFormat peekText)
      "Table"          -> mkBlock id
        (retrieving "Table" . (liftLua . absindex >=> (\idx' -> cleanup $ do
          attr  <- liftLua (rawgeti idx' 1) *> peekAttr top
          capt  <- liftLua (rawgeti idx' 2) *> peekCaption top
          cs    <- liftLua (rawgeti idx' 3) *> peekList peekColSpec top
          thead <- liftLua (rawgeti idx' 4) *> peekTableHead top
          tbods <- liftLua (rawgeti idx' 5) *> peekList peekTableBody top
          tfoot <- liftLua (rawgeti idx' 6) *> peekTableFoot top
          return $! Table attr capt cs thead tbods tfoot)))
      Name tag -> failPeek ("Unknown block type: " <> tag)

peekBlocks :: LuaError e => Peeker e [Block]
peekBlocks = peekList peekBlock

peekInlines :: LuaError e => Peeker e [Inline]
peekInlines = peekList peekInline

-- | Push Caption element
pushCaption :: LuaError e => Caption -> LuaE e ()
pushCaption (Caption shortCaption longCaption) = do
  Lua.newtable
  LuaUtil.addField "short" (Lua.Optional shortCaption)
  LuaUtil.addField "long" longCaption

-- | Peek Caption element
peekCaption :: LuaError e => Peeker e Caption
peekCaption = retrieving "Caption" . \idx -> do
  short <- optional $ peekFieldRaw peekInlines "short" idx
  long <- peekFieldRaw peekBlocks "long" idx
  return $! Caption short long

peekColWidth :: LuaError e => Peeker e ColWidth
peekColWidth = retrieving "ColWidth" . \idx -> do
  maybe ColWidthDefault ColWidth <$!> optional (peekRealFloat idx)

peekColSpec :: LuaError e => Peeker e ColSpec
peekColSpec = peekPair peekRead peekColWidth

instance Pushable ColWidth where
  push = \case
    (ColWidth w)    -> Lua.push w
    ColWidthDefault -> Lua.pushnil

instance Pushable Row where
  push (Row attr cells) = Lua.push (attr, cells)

instance Peekable Row where
  peek = forcePeek . peekRow

peekRow :: LuaError e => Peeker e Row
peekRow = ((uncurry Row) <$!>)
  . retrieving "Row"
  . peekPair peekAttr (peekList peekCell)

instance Pushable TableBody where
  push (TableBody attr (RowHeadColumns rowHeadColumns) head' body) = do
    Lua.newtable
    LuaUtil.addField "attr" attr
    LuaUtil.addField "row_head_columns" rowHeadColumns
    LuaUtil.addField "head" head'
    LuaUtil.addField "body" body

peekTableBody :: LuaError e => Peeker e TableBody
peekTableBody = fmap (retrieving "TableBody")
  . typeChecked "table" Lua.istable
  $ \idx -> TableBody
  <$!> peekFieldRaw peekAttr "attr" idx
  <*>  peekFieldRaw ((fmap RowHeadColumns) . peekIntegral) "row_head_columns" idx
  <*>  peekFieldRaw (peekList peekRow) "head" idx
  <*>  peekFieldRaw (peekList peekRow) "body" idx

instance Pushable TableHead where
  push (TableHead attr rows) = Lua.push (attr, rows)

peekTableHead :: LuaError e => Peeker e TableHead
peekTableHead = ((uncurry TableHead) <$!>)
  . retrieving "TableHead"
  . peekPair peekAttr (peekList peekRow)

instance Pushable TableFoot where
  push (TableFoot attr cells) = Lua.push (attr, cells)

peekTableFoot :: LuaError e => Peeker e TableFoot
peekTableFoot = ((uncurry TableFoot) <$!>)
  . retrieving "TableFoot"
  . peekPair peekAttr (peekList peekRow)

instance Pushable Cell where
  push = pushCell

instance Peekable Cell where
  peek = forcePeek . peekCell

pushCell :: LuaError e => Cell -> LuaE e ()
pushCell (Cell attr align (RowSpan rowSpan) (ColSpan colSpan) contents) = do
  Lua.newtable
  LuaUtil.addField "attr" attr
  LuaUtil.addField "alignment" align
  LuaUtil.addField "row_span" rowSpan
  LuaUtil.addField "col_span" colSpan
  LuaUtil.addField "contents" contents

peekCell :: LuaError e => Peeker e Cell
peekCell = fmap (retrieving "Cell")
  . typeChecked "table" Lua.istable
  $ \idx -> do
  attr <- peekFieldRaw peekAttr "attr" idx
  algn <- peekFieldRaw peekRead "alignment" idx
  rs   <- RowSpan <$!> peekFieldRaw peekIntegral "row_span" idx
  cs   <- ColSpan <$!> peekFieldRaw peekIntegral "col_span" idx
  blks <- peekFieldRaw peekBlocks "contents" idx
  return $! Cell attr algn rs cs blks

-- | Push an inline element to the top of the lua stack.
pushInline :: forall e. LuaError e => Inline -> LuaE e ()
pushInline = \case
  Cite citations lst       -> pushViaConstructor @e "Cite" lst citations
  Code attr lst            -> pushViaConstr' @e "Code"
                              [push lst, pushAttr attr]
  Emph inlns               -> pushViaConstructor @e "Emph" inlns
  Underline inlns          -> pushViaConstructor @e "Underline" inlns
  Image attr alt (src,tit) -> pushViaConstr' @e "Image"
                              [push alt, push src, push tit, pushAttr attr]
  LineBreak                -> pushViaConstructor @e "LineBreak"
  Link attr lst (src,tit)  -> pushViaConstr' @e "Link"
                              [push lst, push src, push tit, pushAttr attr]
  Note blcks               -> pushViaConstructor @e "Note" blcks
  Math mty str             -> pushViaConstructor @e "Math" mty str
  Quoted qt inlns          -> pushViaConstructor @e "Quoted" qt inlns
  RawInline f cs           -> pushViaConstructor @e "RawInline" f cs
  SmallCaps inlns          -> pushViaConstructor @e "SmallCaps" inlns
  SoftBreak                -> pushViaConstructor @e "SoftBreak"
  Space                    -> pushViaConstructor @e "Space"
  Span attr inlns          -> pushViaConstr' @e "Span"
                              [push inlns, pushAttr attr]
  Str str                  -> pushViaConstructor @e "Str" str
  Strikeout inlns          -> pushViaConstructor @e "Strikeout" inlns
  Strong inlns             -> pushViaConstructor @e "Strong" inlns
  Subscript inlns          -> pushViaConstructor @e "Subscript" inlns
  Superscript inlns        -> pushViaConstructor @e "Superscript" inlns

-- | Return the value at the given index as inline if possible.
peekInline :: forall e. LuaError e => Peeker e Inline
peekInline = retrieving "Inline" . \idx -> do
  -- Get the contents of an AST element.
  let mkBlock :: (a -> Inline) -> Peeker e a -> Peek e Inline
      mkBlock f p = f <$!> peekFieldRaw p "c" idx
  LuaUtil.getTag idx >>= \case
    "Cite"       -> mkBlock (uncurry Cite) $
                    peekPair (peekList peekCitation) peekInlines
    "Code"       -> mkBlock (uncurry Code) (peekPair peekAttr peekText)
    "Emph"       -> mkBlock Emph peekInlines
    "Underline"  -> mkBlock Underline peekInlines
    "Image"      -> mkBlock (\(attr, lst, tgt) -> Image attr lst tgt)
                    $ peekTriple peekAttr peekInlines
                                 (peekPair peekText peekText)
    "Link"       -> mkBlock (\(attr, lst, tgt) -> Link attr lst tgt) $
                    peekTriple peekAttr peekInlines (peekPair peekText peekText)
    "LineBreak"  -> return LineBreak
    "Note"       -> mkBlock Note peekBlocks
    "Math"       -> mkBlock (uncurry Math) (peekPair peekRead peekText)
    "Quoted"     -> mkBlock (uncurry Quoted) (peekPair peekRead peekInlines)
    "RawInline"  -> mkBlock (uncurry RawInline) (peekPair peekFormat peekText)
    "SmallCaps"  -> mkBlock SmallCaps peekInlines
    "SoftBreak"  -> return SoftBreak
    "Space"      -> return Space
    "Span"       -> mkBlock (uncurry Span) (peekPair peekAttr peekInlines)
    "Str"        -> mkBlock Str peekText
    "Strikeout"  -> mkBlock Strikeout peekInlines
    "Strong"     -> mkBlock Strong peekInlines
    "Subscript"  -> mkBlock Subscript peekInlines
    "Superscript"-> mkBlock Superscript peekInlines
    Name tag -> Lua.failPeek ("Unknown inline type: " <> tag)

pushListAttributes :: forall e. LuaError e => ListAttributes -> LuaE e ()
pushListAttributes (start, style, delimiter) =
    pushViaConstr' "ListAttributes"
    [ push start, push style, push delimiter ]

peekListAttributes :: LuaError e => Peeker e ListAttributes
peekListAttributes = retrieving "ListAttributes" . peekTriple
  peekIntegral
  peekRead
  peekRead

-- These instances exist only for testing. It's a hack to avoid making
-- the marshalling modules public.
instance Peekable Inline where
  peek = forcePeek . peekInline

instance Peekable Block where
  peek = forcePeek . peekBlock

instance Peekable Meta where
  peek = forcePeek . peekMeta

instance Peekable Pandoc where
  peek = forcePeek . peekPandoc

instance {-# OVERLAPPING #-} Peekable Attr where
  peek = forcePeek . peekAttr
