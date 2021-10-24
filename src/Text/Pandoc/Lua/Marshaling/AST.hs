{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
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
  , peekFormat
  , peekInline
  , peekInlines
  , peekListAttributes
  , peekMeta
  , peekMetaValue
  , peekPandoc
  , peekMathType
  , peekQuoteType

  , peekFuzzyInlines
  , peekFuzzyBlocks

  , pushAttr
  , pushBlock
  , pushCitation
  , pushInline
  , pushListAttributes
  , pushMetaValue
  , pushPandoc
  ) where

import Control.Applicative ((<|>), optional)
import Control.Monad.Catch (throwM)
import Control.Monad ((<$!>), (>=>))
import Data.Data (showConstr, toConstr)
import Data.Text (Text)
import Data.Version (Version)
import HsLua hiding (Operation (Div))
import HsLua.Module.Version (peekVersionFuzzy)
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError (PandocLuaError))
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
  push = pushCitation

typeCitation :: LuaError e => DocumentedType e Citation
typeCitation = deftype "Citation" []
  [ property "id" "citation ID / key"
      (pushText, citationId)
      (peekText, \citation cid -> citation{ citationId = cid })
  , property "mode" "citation mode"
      (pushString . show, citationMode)
      (peekRead, \citation mode -> citation{ citationMode = mode })
  , property "prefix" "citation prefix"
      (pushInlines, citationPrefix)
      (peekInlines, \citation prefix -> citation{ citationPrefix = prefix })
  , property "suffix" "citation suffix"
      (pushInlines, citationSuffix)
      (peekInlines, \citation suffix -> citation{ citationPrefix = suffix })
  , property "note_num" "note number"
      (pushIntegral, citationNoteNum)
      (peekIntegral, \citation noteNum -> citation{ citationNoteNum = noteNum })
  , property "hash" "hash number"
      (pushIntegral, citationHash)
      (peekIntegral, \citation hash -> citation{ citationHash = hash })
  , method $ defun "clone" ### return <#> udparam typeCitation "obj" ""
    =#> functionResult pushCitation "Citation" "copy of obj"
  ]

pushCitation :: LuaError e => Pusher e Citation
pushCitation = pushUD typeCitation

peekCitation :: LuaError e => Peeker e Citation
peekCitation = peekUD typeCitation

instance Pushable Alignment where
  push = Lua.pushString . show

instance Pushable CitationMode where
  push = Lua.push . show

instance Pushable Format where
  push = pushFormat

pushFormat :: LuaError e => Pusher e Format
pushFormat (Format f) = pushText f

peekFormat :: LuaError e => Peeker e Format
peekFormat idx = Format <$!> peekText idx

instance Pushable ListNumberDelim where
  push = Lua.push . show

instance Pushable ListNumberStyle where
  push = Lua.push . show

instance Pushable MathType where
  push = Lua.push . show

instance Pushable QuoteType where
  push = pushQuoteType

pushMathType :: LuaError e => Pusher e MathType
pushMathType = pushString . show

peekMathType :: LuaError e => Peeker e MathType
peekMathType = peekRead

pushQuoteType :: LuaError e => Pusher e QuoteType
pushQuoteType = pushString . show

peekQuoteType :: LuaError e => Peeker e QuoteType
peekQuoteType = peekRead

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

pushInlines :: LuaError e => Pusher e [Inline]
pushInlines = pushPandocList pushInline

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

getInlineText :: Inline -> Possible Text
getInlineText = \case
  Code _ lst      -> Actual lst
  Math _ str      -> Actual str
  RawInline _ raw -> Actual raw
  Str s           -> Actual s
  _               -> Absent

setInlineText :: Inline -> Text -> Possible Inline
setInlineText = \case
  Code attr _     -> Actual . Code attr
  Math mt _       -> Actual . Math mt
  RawInline f _   -> Actual . RawInline f
  Str _           -> Actual . Str
  _               -> const Absent

data Content
  = ContentBlocks [Block]
  | ContentInlines [Inline]

setInlineContent :: Inline -> Content -> Possible Inline
setInlineContent = \case
  -- inline content
  Cite cs _     -> Actual . Cite cs . inlineContent
  Emph _        -> Actual . Emph . inlineContent
  Quoted qt _   -> Actual . Quoted qt . inlineContent
  SmallCaps _   -> Actual . SmallCaps . inlineContent
  Span attr _   -> Actual . Span attr . inlineContent
  Strong _      -> Actual . Strong . inlineContent
  Subscript _   -> Actual . Subscript . inlineContent
  Superscript _ -> Actual . Superscript . inlineContent
  Underline _   -> Actual . Underline . inlineContent
  -- block content
  Note _        -> Actual . Note . blockContent
  _             -> const Absent
  where
    inlineContent = \case
      ContentInlines inlns -> inlns
      ContentBlocks _      -> throwM $
                              PandocLuaError "expected Inlines, got Blocks"
    blockContent = \case
      ContentBlocks blks -> blks
      ContentInlines []  -> []
      ContentInlines _   -> throwM $
                            PandocLuaError "expected Blocks, got Inlines"

getInlineContent :: Inline -> Possible Content
getInlineContent = \case
  Cite _ inlns      -> Actual $ ContentInlines inlns
  Emph inlns        -> Actual $ ContentInlines inlns
  Quoted _ inlns    -> Actual $ ContentInlines inlns
  SmallCaps inlns   -> Actual $ ContentInlines inlns
  Span _ inlns      -> Actual $ ContentInlines inlns
  Strong inlns      -> Actual $ ContentInlines inlns
  Subscript inlns   -> Actual $ ContentInlines inlns
  Superscript inlns -> Actual $ ContentInlines inlns
  Underline inlns   -> Actual $ ContentInlines inlns
  Note blks         -> Actual $ ContentBlocks blks
  _                 -> Absent

-- title
getInlineTitle :: Inline -> Possible Text
getInlineTitle = \case
  Image _ _ (_, tit) -> Actual tit
  Link _ _ (_, tit)  -> Actual tit
  _                  -> Absent

setInlineTitle :: Inline -> Text -> Possible Inline
setInlineTitle = \case
  Image attr capt (src, _) -> Actual . Image attr capt . (src,)
  Link attr capt (src, _)  -> Actual . Link attr capt . (src,)
  _                        -> const Absent

-- attr
getInlineAttr :: Inline -> Possible Attr
getInlineAttr = \case
  Code attr _    -> Actual attr
  Image attr _ _ -> Actual attr
  Link attr _ _  -> Actual attr
  Span attr _    -> Actual attr
  _              -> Absent

setInlineAttr :: Inline -> Attr -> Possible Inline
setInlineAttr = \case
  Code _ cs       -> Actual . (`Code` cs)
  Image _ cpt tgt -> Actual . \attr -> Image attr cpt tgt
  Link _ cpt tgt  -> Actual . \attr -> Link attr cpt tgt
  Span _ inlns    -> Actual . (`Span` inlns)
  _               -> const Absent

showInline :: LuaError e => DocumentedFunction e
showInline = defun "show"
  ### liftPure (show @Inline)
  <#> parameter peekInline "inline" "Inline" "Object"
  =#> functionResult pushString "string" "stringified Inline"

pushContent :: LuaError e => Pusher e Content
pushContent = \case
  ContentBlocks blks -> pushPandocList pushBlock blks
  ContentInlines inlns -> pushPandocList pushInline inlns

peekContent :: LuaError e => Peeker e Content
peekContent idx =
  (ContentInlines <$!> peekList peekInline idx) <|>
  (ContentBlocks  <$!> peekList peekBlock idx)

typeInline :: LuaError e => DocumentedType e Inline
typeInline = deftype "Inline"
  [ operation Tostring showInline
  , operation Eq $ defun "__eq"
      ### liftPure2 (==)
      <#> parameter peekInline "a" "Inline" ""
      <#> parameter peekInline "b" "Inline" ""
      =#> functionResult pushBool "boolean" "whether the two are equal"
  ]
  [ possibleProperty "attr" "element attributes"
      (pushAttr, getInlineAttr)
      (peekAttr, setInlineAttr)
  , possibleProperty "caption" "image caption"
      (pushPandocList pushInline, \case
          Image _ capt _ -> Actual capt
          _              -> Absent)
      (peekInlines, \case
          Image attr _ target -> Actual . (\capt -> Image attr capt target)
          _                   -> const Absent)
  , possibleProperty "citations" "list of citations"
      (pushPandocList pushCitation, \case {Cite cs _ -> Actual cs; _ -> Absent})
      (peekList peekCitation, \case
          Cite _ inlns -> Actual . (`Cite` inlns)
          _            -> const Absent)
  , possibleProperty "content" "element contents"
      (pushContent, getInlineContent)
      (peekContent, setInlineContent)
  , possibleProperty "format" "format of raw text"
      (pushFormat, \case {RawInline fmt _ -> Actual fmt; _ -> Absent})
      (peekFormat, \case
          RawInline _ txt -> Actual . (`RawInline` txt)
          _ -> const Absent)
  , possibleProperty "mathtype" "math rendering method"
      (pushMathType, \case {Math mt _ -> Actual mt; _ -> Absent})
      (peekMathType, \case
          Math _ txt -> Actual . (`Math` txt)
          _          -> const Absent)
  , possibleProperty "quotetype" "type of quotes (single or double)"
      (pushQuoteType, \case {Quoted qt _ -> Actual qt; _ -> Absent})
      (peekQuoteType, \case
          Quoted _ inlns  -> Actual . (`Quoted` inlns)
          _               -> const Absent)
  , possibleProperty "src" "image source"
      (pushText, \case
          Image _ _ (src, _) -> Actual src
          _                  -> Absent)
      (peekText, \case
          Image attr capt (_, title) -> Actual . Image attr capt . (,title)
          _                          -> const Absent)
  , possibleProperty "target" "link target URL"
      (pushText, \case
          Link _ _ (tgt, _) -> Actual tgt
          _                 -> Absent)
      (peekText, \case
          Link attr capt (_, title) -> Actual . Image attr capt . (,title)
          _                         -> const Absent)
  , possibleProperty "title" "title text"
      (pushText, getInlineTitle)
      (peekText, setInlineTitle)
  , possibleProperty "text" "text contents"
      (pushText, getInlineText)
      (peekText, setInlineText)
  , readonly "tag" "type of Inline"
      (pushString, showConstr . toConstr )

  , alias "t" "tag" ["tag"]
  , alias "c" "content" ["content"]
  , alias "identifier" "element identifier"       ["attr", "identifier"]
  , alias "classes"    "element classes"          ["attr", "classes"]
  , alias "attributes" "other element attributes" ["attr", "attributes"]

  , method $ defun "clone"
      ### return
      <#> parameter peekInline "inline" "Inline" "self"
      =#> functionResult pushInline "Inline" "cloned Inline"
  ]

-- | Push an inline element to the top of the lua stack.
pushInline :: forall e. LuaError e => Inline -> LuaE e ()
pushInline = pushUD typeInline

-- | Return the value at the given index as inline if possible.
peekInline :: forall e. LuaError e => Peeker e Inline
peekInline = retrieving "Inline" . \idx -> peekUD typeInline idx

-- | Try extra-hard to return the value at the given index as a list of
-- inlines.
peekFuzzyInlines :: LuaError e => Peeker e [Inline]
peekFuzzyInlines = choice
  [ peekList peekInline
  , fmap pure . peekInline
  , \idx -> pure . Str <$!> peekText idx
  ]

peekFuzzyBlocks :: LuaError e => Peeker e [Block]
peekFuzzyBlocks = choice
  [ peekList peekBlock
  , fmap pure . peekBlock
  , \idx -> pure . Plain . pure . Str <$!> peekText idx
  ]


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

instance Peekable Version where
  peek = forcePeek . peekVersionFuzzy

instance {-# OVERLAPPING #-} Peekable Attr where
  peek = forcePeek . peekAttr
