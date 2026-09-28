{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- |
--   Module      : Text.Pandoc.Writers.XML
--   Copyright   : Copyright (C) 2025- Massimiliano Farinella and John MacFarlane
--   License     : GNU GPL, version 2 or above
--
--   Maintainer  : Massimiliano Farinella <massifrg@gmail.com>
--   Stability   : WIP
--   Portability : portable
--
-- Conversion of 'Pandoc' documents to (pandoc specific) xml markup.
module Text.Pandoc.Writers.XML (writeXML) where

import Data.Map (Map, toList)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Version (versionBranch)
import GHC.Generics
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options (WriterOptions (..))
import Text.Pandoc.XML.Light
import qualified Text.Pandoc.XML.Light as XML
import Text.Pandoc.XMLFormat

type PandocAttr = Text.Pandoc.Definition.Attr

writeXML :: (PandocMonad m) => WriterOptions -> Pandoc -> m T.Text
writeXML opts doc = do
  return $ pandocToXmlText opts doc

text_node :: T.Text -> Content
text_node text = Text (CData CDataText text Nothing)

emptyElement :: T.Text -> Element
emptyElement tag =
  Element
    { elName = unqual tag,
      elAttribs = [],
      elContent = [],
      elLine = Nothing
    }

elementWithContents :: T.Text -> [Content] -> Element
elementWithContents tag contents =
  Element
    { elName = unqual tag,
      elAttribs = [],
      elContent = contents,
      elLine = Nothing
    }

elementWithAttributes :: T.Text -> [XML.Attr] -> Element
elementWithAttributes tag attributes =
  Element
    { elName = unqual tag,
      elAttribs = attributes,
      elContent = [],
      elLine = Nothing
    }

elementWithAttrAndContents :: T.Text -> PandocAttr -> [Content] -> Element
elementWithAttrAndContents tag attr contents = addAttrAttributes attr $ elementWithContents tag contents

-- | Extract the name of a value's constructor via GHC.Generics.
class GConName f where
  gConName :: f p -> String

instance (GConName f) => GConName (M1 D d f) where
  gConName (M1 x) = gConName x

instance (GConName f, GConName g) => GConName (f :+: g) where
  gConName (L1 x) = gConName x
  gConName (R1 x) = gConName x

instance (Constructor c) => GConName (M1 C c f) where
  gConName = conName

itemName :: (Generic a, GConName (Rep a)) => a -> T.Text
itemName = T.pack . gConName . from

intAsText :: Int -> T.Text
intAsText i = T.pack $ show i

itemAsEmptyElement :: (Generic a, GConName (Rep a)) => a -> Element
itemAsEmptyElement item = emptyElement $ itemName item

pandocToXmlText :: WriterOptions -> Pandoc -> T.Text
pandocToXmlText opts (Pandoc (Meta meta) blocks) =
  case writerTemplate opts of
       Just _ -> -- standalone document; include Pandoc and Meta
         ppcTopElement configPP . with_blocks . with_meta . with_version $ el
       Nothing -> -- fragment; just include blocks, as native writer does
         mconcat $ map (ppcContent configPP) block_contents
 where
   el = emptyElement "Pandoc"
   with_version = addAttribute atNameApiVersion version
   version = (T.intercalate "," $ map (T.pack . show)
                                $ versionBranch pandocTypesVersion)
   with_meta = appendContents (metaMapToXML meta "meta")
   with_blocks = appendContents $ asContents block_element
   block_element = elementWithContents "blocks" block_contents
   block_contents = blocksToXML blocks

-- | Pretty-printing configuration: the contents of elements that
-- contain inline content are kept on a single line, so that no
-- significant whitespace is added inside them.
configPP :: ConfigPP
configPP = useInlineTags (isInlineTag . qName) prettyConfigPP

-- | Check whether a tag is for an element with inline content.
isInlineTag :: T.Text -> Bool
isInlineTag t =
  case t of
    "Para" -> True
    "Plain" -> True
    "Header" -> True
    "MetaInlines" -> True
    "Emph" -> True
    "Strong" -> True
    "Strikeout" -> True
    "Superscript" -> True
    "Subscript" -> True
    "SmallCaps" -> True
    "Underline" -> True
    "Quoted" -> True
    "Cite" -> True
    "Link" -> True
    "Image" -> True
    "Span" -> True
    _ ->
      t == tgNameLineItem
        || t == tgNameDefListTerm
        || t == tgNameCitationPrefix
        || t == tgNameCitationSuffix
        || t == tgNameShortCaption

metaMapToXML :: Map T.Text MetaValue -> T.Text -> [Content]
metaMapToXML mmap tag = asContents $ elementWithContents tag entries
  where
    entries = concatMap to_entry $ toList mmap
    to_entry :: (T.Text, MetaValue) -> [Content]
    to_entry (text, metavalue) = asContents with_key
      where
        entry = elementWithContents tgNameMetaMapEntry $ metaValueToXML metavalue
        with_key = addAttribute atNameMetaMapEntryKey text entry

metaValueToXML :: MetaValue -> [Content]
metaValueToXML value =
  let name = itemName value
      el = itemAsEmptyElement value
   in case (value) of
        MetaBool b -> asContents $ addAttribute atNameMetaBoolValue bool_value el
          where
            bool_value = if b then "true" else "false"
        MetaString s -> asContents $ appendContents [text_node s] el
        MetaInlines inlines -> asContents $ appendContents (inlinesToXML inlines) el
        MetaBlocks blocks -> asContents $ appendContents (blocksToXML blocks) el
        MetaList items -> asContents $ appendContents (concatMap metaValueToXML items) el
        MetaMap mm -> metaMapToXML mm name

blocksToXML :: [Block] -> [Content]
blocksToXML blocks = concatMap blockToXML blocks

inlinesToXML :: [Inline] -> [Content]
inlinesToXML inlines = concatMap wsRunsAsElements $ mergeTextNodes $ concatMap inlineContentToContents (ilsToIlsContent inlines [])

-- | Merge consecutive text nodes into a single text node; otherwise
-- the pretty-printer would render each one on a line of its own.
mergeTextNodes :: [Content] -> [Content]
mergeTextNodes (Text (CData CDataText t _) : rest) =
  text_node (T.concat (t : ts)) : mergeTextNodes rest'
  where
    (ts, rest') = textRun rest
    textRun (Text (CData CDataText t' _) : cs) =
      let (ts', cs') = textRun cs in (t' : ts', cs')
    textRun cs = ([], cs)
mergeTextNodes (c : rest) = c : mergeTextNodes rest
mergeTextNodes [] = []

-- | A whitespace run in a text node is read back as a single Space
-- (a run of spaces) or a single SoftBreak (a run containing a
-- newline), so runs like " \n" or "\n\n" would not roundtrip: encode
-- them as sequences of Space and SoftBreak elements instead.
wsRunsAsElements :: Content -> [Content]
wsRunsAsElements (Text (CData CDataText t _))
  | hasLongWsRun = go [] (T.groupBy (\a b -> isWs a == isWs b) t)
  where
    isWs ch = ch == ' ' || ch == '\n'
    -- a whitespace run needs encoding only if it is longer than one
    -- character (single-character runs are " " or "\n", which are
    -- kept); the common case of no such run avoids the work below
    hasLongWsRun = fst $ T.foldl' adjacent (False, False) t
    adjacent (found, prevWs) ch =
      let ws = isWs ch in (found || (prevWs && ws), ws)
    keep r = r == " " || r == "\n" || not (T.any isWs r)
    go acc (r : rs)
      | keep r = go (r : acc) rs
      | otherwise = flush acc ++ map toElem (T.unpack r) ++ go [] rs
    go acc [] = flush acc
    flush [] = []
    flush acc = [text_node $ T.concat $ reverse acc]
    toElem '\n' = Elem $ emptyElement "SoftBreak"
    toElem _ = Elem $ emptyElement "Space"
wsRunsAsElements c = [c]

data InlineContent
  = NormalInline Inline
  | ElSpace Int
  | ElStr T.Text

ilsToIlsContent :: [Inline] -> [InlineContent] -> [InlineContent]
ilsToIlsContent (Space : xs) [] = ilsToIlsContent xs [ElSpace 1]
ilsToIlsContent (Space : xs) (NormalInline Space : cs) = ilsToIlsContent xs (ElSpace 2 : cs)
ilsToIlsContent (Space : xs) (ElSpace n : cs) = ilsToIlsContent xs (ElSpace (n + 1) : cs)
-- empty Str are always encoded as <Str />
ilsToIlsContent (Str "" : xs) ilct = ilsToIlsContent xs (ElStr "" : ilct)
-- Str s1, Str s2 -> s1<Str content="s2">
ilsToIlsContent (Str s2 : xs) (NormalInline str1@(Str _) : ilct) = ilsToIlsContent xs (ElStr s2 : NormalInline str1 : ilct)
--
ilsToIlsContent (Str s : xs) ilct =
  if T.any (== ' ') s
    then ilsToIlsContent xs (ElStr s : ilct)
    else ilsToIlsContent xs (NormalInline (Str s) : ilct)
ilsToIlsContent (x : xs) ilct = ilsToIlsContent xs (NormalInline x : ilct)
ilsToIlsContent [] ilct = reverse $ lastSpaceAsElem ilct
  where
    lastSpaceAsElem :: [InlineContent] -> [InlineContent]
    lastSpaceAsElem (NormalInline Space : xs) = ElSpace 1 : xs
    lastSpaceAsElem ilcts = ilcts

inlineContentToContents :: InlineContent -> [Content]
inlineContentToContents (NormalInline il) = inlineToXML il
inlineContentToContents (ElSpace 1) = [Elem $ emptyElement "Space"]
inlineContentToContents (ElSpace n) = [Elem $ addAttribute atNameSpaceCount (intAsText n) (emptyElement "Space")]
inlineContentToContents (ElStr "") = [Elem $ emptyElement "Str"]
inlineContentToContents (ElStr s) = [Elem $ addAttribute atNameStrContent s (emptyElement "Str")]

asContents :: Element -> [Content]
asContents el = [Elem el]

wrapBlocks :: T.Text -> [Block] -> [Content]
wrapBlocks tag blocks = asContents $ elementWithContents tag $ blocksToXML blocks

wrapArrayOfBlocks :: T.Text -> [[Block]] -> [Content]
wrapArrayOfBlocks tag array = concatMap (wrapBlocks tag) array

-- wrapInlines :: T.Text -> [Inline] -> [Content]
-- wrapInlines tag inlines = asContents $ element_with_contents tag $ inlinesToXML inlines

blockToXML :: Block -> [Content]
blockToXML block =
  let el = itemAsEmptyElement block
   in case (block) of
        Para inlines -> asContents $ appendContents (inlinesToXML inlines) el
        Header level (idn, cls, attrs) inlines -> asContents $ appendContents (inlinesToXML inlines) with_attr
          where
            with_attr = addAttrAttributes (idn, cls, attrs ++ [(atNameLevel, intAsText level)]) el
        Plain inlines -> asContents $ appendContents (inlinesToXML inlines) el
        Div attr blocks -> asContents $ appendContents (blocksToXML blocks) with_attr
          where
            with_attr = addAttrAttributes attr el
        BulletList items -> asContents $ appendContents (wrapArrayOfBlocks tgNameListItem items) el
        OrderedList (start, style, delim) items -> asContents $ with_contents . with_attrs $ el
          where
            with_attrs =
              addAttributes
                ( validAttributes
                    [ (atNameStart, intAsText start),
                      (atNameNumberStyle, itemName style),
                      (atNameNumberDelim, itemName delim)
                    ]
                )
            with_contents = appendContents (wrapArrayOfBlocks tgNameListItem items)
        BlockQuote blocks -> asContents $ appendContents (blocksToXML blocks) el
        HorizontalRule -> asContents el
        CodeBlock attr text -> asContents $ with_contents . with_attr $ el
          where
            with_contents = appendContents [text_node text]
            with_attr = addAttrAttributes attr
        LineBlock lins -> asContents $ appendContents (concatMap wrapInlines lins) el
          where
            wrapInlines inlines = asContents $ appendContents (inlinesToXML inlines) $ emptyElement tgNameLineItem
        Table attr caption colspecs thead tbodies tfoot -> asContents $ with_foot . with_bodies . with_head . with_colspecs . with_caption . with_attr $ el
          where
            with_attr = addAttrAttributes attr
            with_caption = appendContents (captionToXML caption)
            with_colspecs = appendContents (colSpecsToXML colspecs)
            with_head = appendContents (tableHeadToXML thead)
            with_bodies = appendContents (concatMap tableBodyToXML tbodies)
            with_foot = appendContents (tableFootToXML tfoot)
        Figure attr caption blocks -> asContents $ with_contents . with_caption . with_attr $ el
          where
            with_attr = addAttrAttributes attr
            with_caption = appendContents (captionToXML caption)
            with_contents = appendContents (blocksToXML blocks)
        RawBlock (Format format) text -> asContents $ appendContents [text_node text] raw
          where
            raw = addAttribute atNameFormat format el
        DefinitionList items -> asContents $ appendContents (map definitionListItemToXML items) el

inlineToXML :: Inline -> [Content]
inlineToXML inline =
  let el = itemAsEmptyElement inline
      wrapInlines inlines = asContents $ appendContents (inlinesToXML inlines) el
   in case (inline) of
        Space -> [text_node " "]
        Str s -> [text_node s]
        Emph inlines -> wrapInlines inlines
        Strong inlines -> wrapInlines inlines
        Quoted quote_type inlines -> asContents $ appendContents (inlinesToXML inlines) quoted
          where
            quoted = addAttribute atNameQuoteType (itemName quote_type) el
        Underline inlines -> wrapInlines inlines
        Strikeout inlines -> wrapInlines inlines
        SmallCaps inlines -> wrapInlines inlines
        Superscript inlines -> wrapInlines inlines
        Subscript inlines -> wrapInlines inlines
        SoftBreak -> [text_node "\n"]
        LineBreak -> asContents el
        Span attr inlines -> asContents $ appendContents (inlinesToXML inlines) with_attr
          where
            with_attr = addAttrAttributes attr el
        Link (idn, cls, attrs) inlines (url, title) -> asContents $ appendContents (inlinesToXML inlines) with_attr
          where
            with_attr = addAttrAttributes (idn, cls, attrs ++ optionalAttribute atNameLinkUrl url ++ optionalAttribute atNameTitle title) el
        Image (idn, cls, attrs) inlines (url, title) -> asContents $ appendContents (inlinesToXML inlines) with_attr
          where
            with_attr = addAttrAttributes (idn, cls, attrs ++ optionalAttribute atNameImageUrl url ++ optionalAttribute atNameTitle title) el
        RawInline (Format format) text -> asContents $ appendContents [text_node text] raw
          where
            raw = addAttribute atNameFormat format el
        Math math_type text -> asContents $ appendContents [text_node text] math
          where
            math = addAttribute atNameMathType (itemName math_type) el
        Code attr text -> asContents $ appendContents [text_node text] with_attr
          where
            with_attr = addAttrAttributes attr el
        Note blocks -> asContents $ appendContents (blocksToXML blocks) el
        Cite citations inlines -> asContents $ appendContents (inlinesToXML inlines) with_citations
          where
            with_citations = addCitations citations el

-- TODO: don't let an attribute overwrite id or class
maybeAttribute :: (T.Text, T.Text) -> Maybe XML.Attr
maybeAttribute ("", _) = Nothing
maybeAttribute (name, value) = Just $ XML.Attr (unqual $ encodeAttrName name) value

-- | An optional attribute, omitted when its value is empty (the
-- reader treats a missing attribute as an empty value).
optionalAttribute :: T.Text -> T.Text -> [(T.Text, T.Text)]
optionalAttribute _ "" = []
optionalAttribute name value = [(name, value)]

validAttributes :: [(T.Text, T.Text)] -> [XML.Attr]
validAttributes pairs = mapMaybe maybeAttribute pairs

appendContents :: [Content] -> Element -> Element
appendContents newContents el = el {elContent = (elContent el) ++ newContents}

prependContents :: [Content] -> Element -> Element
prependContents newContents el = el {elContent = newContents ++ (elContent el)}

addAttributes :: [XML.Attr] -> Element -> Element
addAttributes newAttrs el = el {elAttribs = newAttrs ++ elAttribs el}

addAttribute :: T.Text -> T.Text -> Element -> Element
addAttribute attr_name attr_value el = el {elAttribs = new_attr : elAttribs el}
  where
    new_attr = XML.Attr (unqual attr_name) attr_value

addAttrAttributes :: PandocAttr -> Element -> Element
addAttrAttributes (identifier, classes, attributes) el = addAttributes attrs' el
  where
    attrs' =
      mapMaybe maybeAttribute $
        optionalAttribute "id" identifier
          ++ optionalAttribute "class" (T.intercalate " " classes)
          ++ attributes

addCitations :: [Citation] -> Element -> Element
addCitations citations el = appendContents [Elem $ elementWithContents tgNameCitations $ concatMap citation_to_elem citations] el
  where
    citation_to_elem :: Citation -> [Content]
    citation_to_elem citation = asContents with_suffix
      where
        cit_elem = elementWithAttributes (itemName citation) attrs
        prefix = citationPrefix citation
        suffix = citationSuffix citation
        with_prefix =
          if null prefix
            then cit_elem
            else appendContents [Elem $ elementWithContents tgNameCitationPrefix $ inlinesToXML prefix] cit_elem
        with_suffix =
          if null suffix
            then with_prefix
            else appendContents [Elem $ elementWithContents tgNameCitationSuffix $ inlinesToXML suffix] with_prefix
        attrs =
          map
            (\(n, v) -> XML.Attr (unqual n) v)
            [ ("id", citationId citation),
              (atNameCitationMode, itemName $ citationMode citation),
              (atNameCitationNoteNum, intAsText $ citationNoteNum citation),
              (atNameCitationHash, intAsText $ citationHash citation)
            ]

definitionListItemToXML :: ([Inline], [[Block]]) -> Content
definitionListItemToXML (inlines, defs) = Elem $ elementWithContents tgNameDefListItem $ term ++ wrapArrayOfBlocks tgNameDefListDef defs
  where
    term = asContents $ appendContents (inlinesToXML inlines) $ emptyElement tgNameDefListTerm

captionToXML :: Caption -> [Content]
captionToXML (Caption short blocks) = asContents with_short_caption
  where
    el = elementWithContents "Caption" $ blocksToXML blocks
    with_short_caption = case (short) of
      Just inlines -> prependContents (asContents $ elementWithContents tgNameShortCaption $ inlinesToXML inlines) el
      _ -> el

colSpecToXML :: (Alignment, ColWidth) -> [Content]
colSpecToXML (align, cw) = asContents colspec
  where
    colspec = elementWithAttributes "ColSpec" $ validAttributes [(atNameAlignment, itemName align), (atNameColWidth, colwidth)]
    colwidth = case (cw) of
      ColWidth d -> T.pack $ show d
      ColWidthDefault -> "0"

colSpecsToXML :: [(Alignment, ColWidth)] -> [Content]
colSpecsToXML colspecs = asContents $ elementWithContents tgNameColspecs $ concatMap colSpecToXML colspecs

tableHeadToXML :: TableHead -> [Content]
tableHeadToXML (TableHead attr rows) = asContents $ elementWithAttrAndContents "TableHead" attr $ concatMap rowToXML rows

tableBodyToXML :: TableBody -> [Content]
tableBodyToXML (TableBody (idn, cls, attrs) (RowHeadColumns headcols) hrows brows) = asContents $ elementWithAttrAndContents "TableBody" attr children
  where
    attr = (idn, cls, (atNameRowHeadColumns, intAsText headcols) : attrs)
    header_rows = asContents $ elementWithContents tgNameBodyHeader $ concatMap rowToXML hrows
    body_rows = asContents $ elementWithContents tgNameBodyBody $ concatMap rowToXML brows
    children = header_rows ++ body_rows

tableFootToXML :: TableFoot -> [Content]
tableFootToXML (TableFoot attr rows) = asContents $ elementWithAttrAndContents "TableFoot" attr $ concatMap rowToXML rows

rowToXML :: Row -> [Content]
rowToXML (Row attr cells) = asContents $ elementWithAttrAndContents "Row" attr $ concatMap cellToXML cells

cellToXML :: Cell -> [Content]
cellToXML (Cell (idn, cls, attrs) alignment (RowSpan rowspan) (ColSpan colspan) blocks) = asContents $ elementWithAttrAndContents "Cell" attr $ blocksToXML blocks
  where
    with_alignment a = (atNameAlignment, itemName alignment) : a
    with_rowspan a = if rowspan > 1 then (atNameRowspan, intAsText rowspan) : a else a
    with_colspan a = if colspan > 1 then (atNameColspan, intAsText colspan) : a else a
    attrs' = (with_colspan . with_rowspan . with_alignment) attrs
    attr = (idn, cls, attrs')
