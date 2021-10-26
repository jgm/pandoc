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
  , peekBlockFuzzy
  , peekBlocks
  , peekBlocksFuzzy
  , peekCaption
  , peekCitation
  , peekColSpec
  , peekDefinitionItem
  , peekFormat
  , peekInline
  , peekInlineFuzzy
  , peekInlines
  , peekInlinesFuzzy
  , peekListAttributes
  , peekMeta
  , peekMetaValue
  , peekPandoc
  , peekMathType
  , peekQuoteType
  , peekTableBody
  , peekTableHead
  , peekTableFoot

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
import Control.Monad ((<$!>))
import Data.Data (showConstr, toConstr)
import Data.Text (Text)
import Data.Version (Version)
import HsLua hiding (Operation (Div))
import HsLua.Module.Version (peekVersionFuzzy)
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError (PandocLuaError))
import Text.Pandoc.Lua.Util (pushViaConstr')
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

typeBlock :: LuaError e => DocumentedType e Block
typeBlock = deftype "Block"
  [ operation Eq $ lambda
    ### liftPure2 (==)
    <#> parameter peekBlockFuzzy "Block" "a" ""
    <#> parameter peekBlockFuzzy "Block" "b" ""
    =#> boolResult "whether the two values are equal"
  , operation Tostring $ lambda
    ### liftPure show
    <#> udparam typeBlock "self" ""
    =#> functionResult pushString "string" "Haskell representation"
  ]
  [ possibleProperty "attr" "element attributes"
      (pushAttr, \case
          CodeBlock attr _     -> Actual attr
          Div attr _           -> Actual attr
          Header _ attr _      -> Actual attr
          Table attr _ _ _ _ _ -> Actual attr
          _                    -> Absent)
      (peekAttr, \case
          CodeBlock _ code     -> Actual . flip CodeBlock code
          Div _ blks           -> Actual . flip Div blks
          Header lvl _ blks    -> Actual . (\attr -> Header lvl attr blks)
          Table _ c cs h bs f  -> Actual . (\attr -> Table attr c cs h bs f)
          _                    -> const Absent)
  , possibleProperty "bodies" "table bodies"
      (pushPandocList pushTableBody, \case
          Table _ _ _ _ bs _ -> Actual bs
          _                  -> Absent)
      (peekList peekTableBody, \case
          Table attr c cs h _ f -> Actual . (\bs -> Table attr c cs h bs f)
          _                     -> const Absent)
  , possibleProperty "caption" "element caption"
      (pushCaption, \case {Table _ capt _ _ _ _ -> Actual capt; _ -> Absent})
      (peekCaption, \case
          Table attr _ cs h bs f -> Actual . (\c -> Table attr c cs h bs f)
          _                      -> const Absent)
  , possibleProperty "colspecs" "column alignments and widths"
      (pushPandocList pushColSpec, \case
          Table _ _ cs _ _ _     -> Actual cs
          _                      -> Absent)
      (peekList peekColSpec, \case
          Table attr c _ h bs f  -> Actual . (\cs -> Table attr c cs h bs f)
          _                      -> const Absent)
  , possibleProperty "content" "element content"
      (pushContent, getBlockContent)
      (peekContent, setBlockContent)
  , possibleProperty "foot" "table foot"
      (pushTableFoot, \case {Table _ _ _ _ _ f -> Actual f; _ -> Absent})
      (peekTableFoot, \case
          Table attr c cs h bs _ -> Actual . (\f -> Table attr c cs h bs f)
          _                      -> const Absent)
  , possibleProperty "format" "format of raw content"
      (pushFormat, \case {RawBlock f _ -> Actual f; _ -> Absent})
      (peekFormat, \case
          RawBlock _ txt -> Actual . (`RawBlock` txt)
          _              -> const Absent)
  , possibleProperty "head" "table head"
      (pushTableHead, \case {Table _ _ _ h _ _ -> Actual h; _ -> Absent})
      (peekTableHead, \case
          Table attr c cs _ bs f  -> Actual . (\h -> Table attr c cs h bs f)
          _                       -> const Absent)
  , possibleProperty "level" "heading level"
      (pushIntegral, \case {Header lvl _ _ -> Actual lvl; _ -> Absent})
      (peekIntegral, \case
          Header _ attr inlns -> Actual . \lvl -> Header lvl attr inlns
          _                   -> const Absent)
  , possibleProperty "listAttributes" "ordered list attributes"
      (pushListAttributes, \case
          OrderedList listAttr _ -> Actual listAttr
          _                      -> Absent)
      (peekListAttributes, \case
          OrderedList _ content -> Actual . (`OrderedList` content)
          _                     -> const Absent)
  , possibleProperty "text" "text contents"
      (pushText, getBlockText)
      (peekText, setBlockText)

  , readonly "tag" "type of Block"
      (pushString, showConstr . toConstr )

  , alias "t" "tag" ["tag"]
  , alias "c" "content" ["content"]
  , alias "identifier" "element identifier"       ["attr", "identifier"]
  , alias "classes"    "element classes"          ["attr", "classes"]
  , alias "attributes" "other element attributes" ["attr", "attributes"]
  , alias "start"      "ordered list start number" ["listAttributes", "start"]
  , alias "style"      "ordered list style"       ["listAttributes", "style"]
  , alias "delimiter"  "numbering delimiter"      ["listAttributes", "delimiter"]

  , method $ defun "clone"
    ### return
    <#> parameter peekBlock "Block" "block" "self"
    =#> functionResult pushBlock "Block" "cloned Block"

  , method $ defun "show"
    ### liftPure show
    <#> parameter peekBlock "Block" "self" ""
    =#> functionResult pushString "string" "Haskell string representation"
  ]
 where
  boolResult = functionResult pushBool "boolean"

getBlockContent :: Block -> Possible Content
getBlockContent = \case
  -- inline content
  Para inlns          -> Actual $ ContentInlines inlns
  Plain inlns         -> Actual $ ContentInlines inlns
  -- inline content
  BlockQuote blks     -> Actual $ ContentBlocks blks
  Div _ blks          -> Actual $ ContentBlocks blks
  -- lines content
  LineBlock lns       -> Actual $ ContentLines lns
  -- list items content
  BulletList itms     -> Actual $ ContentListItems itms
  OrderedList _ itms  -> Actual $ ContentListItems itms
  -- definition items content
  DefinitionList itms -> Actual $ ContentDefItems itms
  _                   -> Absent

setBlockContent :: Block -> Content -> Possible Block
setBlockContent = \case
  -- inline content
  Para _           -> Actual . Para . inlineContent
  Plain _          -> Actual . Plain . inlineContent
  -- block content
  BlockQuote _     -> Actual . BlockQuote . blockContent
  Div attr _       -> Actual . Div attr . blockContent
  -- lines content
  LineBlock _      -> Actual . LineBlock . lineContent
  -- list items content
  BulletList _     -> Actual . BulletList . listItemContent
  OrderedList la _ -> Actual . OrderedList la . listItemContent
  -- definition items content
  DefinitionList _ -> Actual . DefinitionList . defItemContent
  _                -> const Absent
  where
    inlineContent = \case
      ContentInlines inlns -> inlns
      c -> throwM . PandocLuaError $ "expected Inlines, got " <>
           contentTypeDescription c
    blockContent = \case
      ContentBlocks blks   -> blks
      ContentInlines inlns -> [Plain inlns]
      c -> throwM . PandocLuaError $ "expected Blocks, got " <>
           contentTypeDescription c
    lineContent = \case
      ContentLines lns     -> lns
      c -> throwM . PandocLuaError $ "expected list of lines, got " <>
           contentTypeDescription c
    defItemContent = \case
      ContentDefItems itms -> itms
      c -> throwM . PandocLuaError $ "expected definition items, got " <>
           contentTypeDescription c
    listItemContent = \case
      ContentBlocks blks    -> [blks]
      ContentLines lns      -> map ((:[]) . Plain) lns
      ContentListItems itms -> itms
      c -> throwM . PandocLuaError $ "expected list of items, got " <>
           contentTypeDescription c

getBlockText :: Block -> Possible Text
getBlockText = \case
  CodeBlock _ lst -> Actual lst
  RawBlock _ raw  -> Actual raw
  _               -> Absent

setBlockText :: Block -> Text -> Possible Block
setBlockText = \case
  CodeBlock attr _ -> Actual . CodeBlock attr
  RawBlock f _     -> Actual . RawBlock f
  _                -> const Absent

-- | Push a block element to the top of the Lua stack.
pushBlock :: forall e. LuaError e => Block -> LuaE e ()
pushBlock = pushUD typeBlock

-- | Return the value at the given index as block if possible.
peekBlock :: forall e. LuaError e => Peeker e Block
peekBlock = retrieving "Block" . peekUD typeBlock

-- | Retrieves a list of Block elements.
peekBlocks :: LuaError e => Peeker e [Block]
peekBlocks = peekList peekBlock

peekInlines :: LuaError e => Peeker e [Inline]
peekInlines = peekList peekInline

pushInlines :: LuaError e => Pusher e [Inline]
pushInlines = pushPandocList pushInline

-- | Retrieves a single definition item from a the stack; it is expected
-- to be a pair of a list of inlines and a list of list of blocks. Uses
-- fuzzy parsing, i.e., tries hard to convert mismatching types into the
-- expected result.
peekDefinitionItem :: LuaError e => Peeker e ([Inline], [[Block]])
peekDefinitionItem = peekPair peekInlinesFuzzy $ choice
  [ peekList peekBlocksFuzzy
  , \idx -> (:[]) <$!> peekBlocksFuzzy idx
  ]

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

-- | Push a ColSpec value as a pair of Alignment and ColWidth.
pushColSpec :: LuaError e => Pusher e ColSpec
pushColSpec = pushPair (pushString . show) pushColWidth

-- | Peek a ColSpec value as a pair of Alignment and ColWidth.
peekColSpec :: LuaError e => Peeker e ColSpec
peekColSpec = peekPair peekRead peekColWidth

peekColWidth :: LuaError e => Peeker e ColWidth
peekColWidth = retrieving "ColWidth" . \idx -> do
  maybe ColWidthDefault ColWidth <$!> optional (peekRealFloat idx)

-- | Push a ColWidth value by pushing the width as a plain number, or
-- @nil@ for ColWidthDefault.
pushColWidth :: LuaError e => Pusher e ColWidth
pushColWidth = \case
  (ColWidth w)    -> Lua.push w
  ColWidthDefault -> Lua.pushnil

-- | Push a table row as a pair of attr and the list of cells.
pushRow :: LuaError e => Pusher e Row
pushRow (Row attr cells) =
  pushPair pushAttr (pushPandocList pushCell) (attr, cells)

-- | Push a table row from a pair of attr and the list of cells.
peekRow :: LuaError e => Peeker e Row
peekRow = ((uncurry Row) <$!>)
  . retrieving "Row"
  . peekPair peekAttr (peekList peekCell)

-- | Pushes a 'TableBody' value as a Lua table with fields @attr@,
-- @row_head_columns@, @head@, and @body@.
pushTableBody :: LuaError e => Pusher e TableBody
pushTableBody (TableBody attr (RowHeadColumns rowHeadColumns) head' body) = do
    Lua.newtable
    LuaUtil.addField "attr" attr
    LuaUtil.addField "row_head_columns" rowHeadColumns
    LuaUtil.addField "head" head'
    LuaUtil.addField "body" body

-- | Retrieves a 'TableBody' value from a Lua table with fields @attr@,
-- @row_head_columns@, @head@, and @body@.
peekTableBody :: LuaError e => Peeker e TableBody
peekTableBody = fmap (retrieving "TableBody")
  . typeChecked "table" Lua.istable
  $ \idx -> TableBody
  <$!> peekFieldRaw peekAttr "attr" idx
  <*>  peekFieldRaw ((fmap RowHeadColumns) . peekIntegral) "row_head_columns" idx
  <*>  peekFieldRaw (peekList peekRow) "head" idx
  <*>  peekFieldRaw (peekList peekRow) "body" idx

-- | Push a table head value as the pair of its Attr and rows.
pushTableHead :: LuaError e => Pusher e TableHead
pushTableHead (TableHead attr rows) =
  pushPair pushAttr (pushPandocList pushRow) (attr, rows)

-- | Peek a table head value from a pair of Attr and rows.
peekTableHead :: LuaError e => Peeker e TableHead
peekTableHead = ((uncurry TableHead) <$!>)
  . retrieving "TableHead"
  . peekPair peekAttr (peekList peekRow)

-- | Pushes a 'TableFoot' value as a pair of the Attr value and the list
-- of table rows.
pushTableFoot :: LuaError e => Pusher e TableFoot
pushTableFoot (TableFoot attr rows) =
  pushPair pushAttr (pushPandocList pushRow) (attr, rows)

-- | Retrieves a 'TableFoot' value from a pair containing an Attr value
-- and a list of table rows.
peekTableFoot :: LuaError e => Peeker e TableFoot
peekTableFoot = ((uncurry TableFoot) <$!>)
  . retrieving "TableFoot"
  . peekPair peekAttr (peekList peekRow)

instance Pushable Cell where
  push = pushCell

instance Peekable Cell where
  peek = forcePeek . peekCell

-- | Push a table cell as a table with fields @attr@, @alignment@,
-- @row_span@, @col_span@, and @contents@.
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

-- | Helper type to represent all the different types a `content`
-- attribute can have.
data Content
  = ContentBlocks [Block]
  | ContentInlines [Inline]
  | ContentLines [[Inline]]
  | ContentDefItems [([Inline], [[Block]])]
  | ContentListItems [[Block]]

contentTypeDescription :: Content -> Text
contentTypeDescription = \case
  ContentBlocks {}    -> "list of Block items"
  ContentInlines {}   -> "list of Inline items"
  ContentLines {}     -> "list of Inline lists (i.e., a list of lines)"
  ContentDefItems {}  -> "list of definition items items"
  ContentListItems {} -> "list items (i.e., list of list of Block elements)"

pushContent :: LuaError e => Pusher e Content
pushContent = \case
  ContentBlocks blks -> pushPandocList pushBlock blks
  ContentInlines inlns -> pushPandocList pushInline inlns
  ContentLines lns -> pushPandocList (pushPandocList pushInline) lns
  ContentDefItems itms ->
    let pushItem = pushPair (pushPandocList pushInline)
                            (pushPandocList (pushPandocList pushBlock))
    in pushPandocList pushItem itms
  ContentListItems itms ->
    pushPandocList (pushPandocList pushBlock) itms

peekContent :: LuaError e => Peeker e Content
peekContent idx =
  (ContentInlines <$!> peekInlinesFuzzy idx) <|>
  (ContentLines  <$!> peekList (peekList peekInlineFuzzy) idx) <|>
  (ContentBlocks  <$!> peekBlocksFuzzy idx ) <|>
  (ContentListItems <$!> peekList peekBlocksFuzzy idx) <|>
  (ContentDefItems  <$!> peekList (peekDefinitionItem) idx)

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
      c -> throwM . PandocLuaError $ "expected Inlines, got " <>
           contentTypeDescription c
    blockContent = \case
      ContentBlocks blks -> blks
      ContentInlines []  -> []
      c -> throwM . PandocLuaError $ "expected Blocks, got " <>
           contentTypeDescription c

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

-- | Try extra hard to retrieve an Inline value from the stack. Treats
-- bare strings as @Str@ values.
peekInlineFuzzy :: LuaError e => Peeker e Inline
peekInlineFuzzy = retrieving "Inline" . choice
  [ peekUD typeInline
  , \idx -> Str <$!> peekText idx
  ]

-- | Try extra-hard to return the value at the given index as a list of
-- inlines.
peekInlinesFuzzy :: LuaError e => Peeker e [Inline]
peekInlinesFuzzy = choice
  [ peekList peekInlineFuzzy
  , fmap pure . peekInlineFuzzy
  ]

-- | Try extra hard to retrieve a Block value from the stack. Treats bar
-- Inline elements as if they were wrapped in 'Plain'.
peekBlockFuzzy :: LuaError e => Peeker e Block
peekBlockFuzzy = choice
  [ peekBlock
  , (\idx -> Plain <$!> peekInlinesFuzzy idx)
  ]

-- | Try extra-hard to return the value at the given index as a list of
-- blocks.
peekBlocksFuzzy :: LuaError e => Peeker e [Block]
peekBlocksFuzzy = choice
  [ peekList peekBlockFuzzy
  , (<$!>) pure . peekBlockFuzzy
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

-- * Orphan Instances

instance Pushable Inline where
  push = pushInline

instance Pushable Citation where
  push = pushCitation

instance Pushable Row where
  push = pushRow

instance Pushable TableBody where
  push = pushTableBody

instance Pushable TableFoot where
  push = pushTableFoot

instance Pushable TableHead where
  push = pushTableHead

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

instance Peekable Row where
  peek = forcePeek . peekRow

instance Peekable Version where
  peek = forcePeek . peekVersionFuzzy

instance {-# OVERLAPPING #-} Peekable Attr where
  peek = forcePeek . peekAttr
