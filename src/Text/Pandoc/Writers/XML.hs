{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Writers.XML (writeXML) where

import Data.Map (Map, toList)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Version (versionBranch)
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options (WriterOptions (..))
import Text.Pandoc.XML.Light
import qualified Text.Pandoc.XML.Light as XML
import Text.XML.Light (xml_header)

type PandocAttr = Text.Pandoc.Definition.Attr

alignmentAttrName :: T.Text
alignmentAttrName = "alignment"

writeXML :: (PandocMonad m) => WriterOptions -> Pandoc -> m T.Text
writeXML _ doc = do
  return $ pandocToXmlText doc

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
elementWithAttrAndContents tag attr contents = addAttributes (elementWithContents tag contents) $ attrAttributes attr

asBlockOfInlines :: Element -> [Content]
asBlockOfInlines el = [Elem el, text_node "\n"]

asBlockOfBlocks :: Element -> [Content]
asBlockOfBlocks el = [Elem newline_before_first, newline]
  where
    newline = text_node "\n"
    newline_before_first = if null (elContent el) then el else prependContents el [newline]

itemName :: (Show a) => a -> T.Text
itemName a = T.pack $ takeWhile (/= ' ') (show a)

intAsText :: Int -> T.Text
intAsText i = T.pack $ show i

itemAsEmptyElement :: (Show a) => a -> Element
itemAsEmptyElement item = emptyElement $ itemName item

pandocToXmlText :: Pandoc -> T.Text
pandocToXmlText (Pandoc (Meta meta) blocks) = with_header
  where
    el = prependContents (emptyElement "Pandoc") [text_node "\n"]
    with_version = addAttribute el "api-version" $ T.intercalate "," $ map (T.pack . show) $ versionBranch pandocTypesVersion
    with_meta = appendContents with_version $ metaMapToXML meta "meta"
    with_blocks = appendContents with_meta $ asBlockOfBlocks $ elementWithContents "blocks" $ blocksToXML blocks
    with_header = T.concat [T.pack xml_header, "\n", showElement with_blocks]

metaMapToXML :: Map T.Text MetaValue -> T.Text -> [Content]
metaMapToXML mmap tag = asBlockOfBlocks $ elementWithContents tag entries
  where
    entries = concatMap to_entry $ toList mmap
    to_entry :: (T.Text, MetaValue) -> [Content]
    to_entry (text, metavalue) = asBlockOfBlocks with_key
      where
        entry = elementWithContents "entry" $ metaValueToXML metavalue
        with_key = addAttribute entry "text" text

metaValueToXML :: MetaValue -> [Content]
metaValueToXML value =
  let name = itemName value
      el = itemAsEmptyElement value
   in case (value) of
        MetaBool b -> asBlockOfInlines $ addAttribute el "value" bool_value
          where
            bool_value = if b then "true" else "false"
        MetaString s -> asBlockOfInlines $ appendContents el $ [text_node s]
        MetaInlines inlines -> asBlockOfInlines $ appendContents el $ inlinesToXML inlines
        MetaBlocks blocks -> asBlockOfBlocks $ appendContents el $ blocksToXML blocks
        MetaList items -> asBlockOfBlocks $ appendContents el $ concatMap metaValueToXML items
        MetaMap mm -> metaMapToXML mm name

blocksToXML :: [Block] -> [Content]
blocksToXML blocks = concatMap blockToXML blocks

inlinesToXML :: [Inline] -> [Content]
inlinesToXML inlines = concatMap inlineToXML inlines

asContents :: Element -> [Content]
asContents el = [Elem el]

wrapBlocks :: T.Text -> [Block] -> [Content]
wrapBlocks tag blocks = asBlockOfBlocks $ elementWithContents tag $ blocksToXML blocks

wrapArrayOfBlocks :: T.Text -> [[Block]] -> [Content]
wrapArrayOfBlocks tag array = concatMap (wrapBlocks tag) array

-- wrapInlines :: T.Text -> [Inline] -> [Content]
-- wrapInlines tag inlines = asBlockOfInlines $ element_with_contents tag $ inlinesToXML inlines

blockToXML :: Block -> [Content]
blockToXML block =
  let el = itemAsEmptyElement block
   in case (block) of
        Para inlines -> asBlockOfInlines $ appendContents el $ inlinesToXML inlines
        Header level (idn, cls, attrs) inlines -> asBlockOfInlines $ appendContents with_attr $ inlinesToXML inlines
          where
            with_attr = addAttributes el $ attrAttributes (idn, cls, attrs ++ [("level", intAsText level)])
        Plain inlines -> asBlockOfInlines $ appendContents el $ inlinesToXML inlines
        Div attr blocks -> asBlockOfBlocks $ appendContents with_attr $ blocksToXML blocks
          where
            with_attr = addAttributes el $ attrAttributes attr
        BulletList items -> asBlockOfBlocks $ appendContents el $ wrapArrayOfBlocks "item" items
        OrderedList (start, style, delim) items -> asBlockOfBlocks $ appendContents with_attrs $ wrapArrayOfBlocks "item" items
          where
            with_attrs =
              addAttributes el $
                validAttributes
                  [ ("start", intAsText start),
                    ("number-style", itemName style),
                    ("number-delim", itemName delim)
                  ]
        BlockQuote blocks -> asBlockOfBlocks $ appendContents el $ blocksToXML blocks
        HorizontalRule -> asBlockOfInlines el
        CodeBlock attr text -> asBlockOfInlines $ appendContents with_attr [text_node text]
          where
            with_attr = addAttributes el $ attrAttributes attr
        LineBlock lins -> asBlockOfBlocks $ appendContents el $ concatMap wrapInlines lins
          where
            wrapInlines inlines = asContents $ appendContents (emptyElement "line") $ inlinesToXML inlines
        Table attr caption colspecs thead tbodies tfoot -> asBlockOfBlocks with_foot
          where
            with_attr = addAttributes el $ attrAttributes attr
            with_caption = appendContents with_attr $ captionToXML caption
            with_colspecs = appendContents with_caption $ colSpecsToXML colspecs
            with_head = appendContents with_colspecs $ tableHeadToXML thead
            with_bodies = appendContents with_head $ concatMap tableBodyToXML tbodies
            with_foot = appendContents with_bodies $ tableFootToXML tfoot
        Figure attr caption blocks -> asBlockOfBlocks $ appendContents with_caption $ blocksToXML blocks
          where
            with_attr = addAttributes el $ attrAttributes attr
            with_caption = appendContents with_attr $ captionToXML caption
        RawBlock (Format format) text -> asContents $ appendContents raw [text_node text]
          where
            raw = addAttribute el "format" format
        DefinitionList items -> asBlockOfBlocks $ appendContents el $ map definitionListItemToXML items

inlineToXML :: Inline -> [Content]
inlineToXML inline =
  let el = itemAsEmptyElement inline
      wrapInlines inlines = asContents $ appendContents el $ inlinesToXML inlines
   in case (inline) of
        Space -> [text_node " "]
        Str s -> [text_node s]
        Emph inlines -> wrapInlines inlines
        Strong inlines -> wrapInlines inlines
        Quoted quote_type inlines -> asContents $ appendContents quoted $ inlinesToXML inlines
          where
            quoted = addAttribute el "quote-type" $ itemName quote_type
        Underline inlines -> wrapInlines inlines
        Strikeout inlines -> wrapInlines inlines
        SmallCaps inlines -> wrapInlines inlines
        Superscript inlines -> wrapInlines inlines
        Subscript inlines -> wrapInlines inlines
        SoftBreak -> asContents el
        LineBreak -> asContents el
        Span attr inlines -> asContents $ appendContents with_attr $ inlinesToXML inlines
          where
            with_attr = addAttributes el $ attrAttributes attr
        Link (idn, cls, attrs) inlines (url, title) -> asContents $ appendContents with_attr $ inlinesToXML inlines
          where
            with_attr = addAttributes el $ attrAttributes (idn, cls, attrs ++ [("href", url), ("title", title)])
        Image (idn, cls, attrs) inlines (url, title) -> asContents $ appendContents with_attr $ inlinesToXML inlines
          where
            with_attr = addAttributes el $ attrAttributes (idn, cls, attrs ++ [("src", url), ("title", title)])
        RawInline (Format format) text -> asContents $ appendContents raw [text_node text]
          where
            raw = addAttribute el "format" format
        Math math_type text -> asContents $ appendContents math [text_node text]
          where
            math = addAttribute el "math-type" $ itemName math_type
        Code attr text -> asContents $ appendContents with_attr [text_node text]
          where
            with_attr = addAttributes el $ attrAttributes attr
        Note blocks -> asContents $ appendContents el $ blocksToXML blocks
        Cite citations inlines -> asContents $ appendContents with_citations $ inlinesToXML inlines
          where
            with_citations = addCitations el citations

-- TODO: don't let an attribute overwrite id or class
maybeAttribute :: (T.Text, T.Text) -> Maybe XML.Attr
maybeAttribute (_, "") = Nothing
maybeAttribute ("", _) = Nothing
maybeAttribute (name, value) = Just $ XML.Attr (unqual name) value

validAttributes :: [(T.Text, T.Text)] -> [XML.Attr]
validAttributes pairs = mapMaybe maybeAttribute pairs

attrAttributes :: PandocAttr -> [XML.Attr]
attrAttributes (identifier, classes, attributes) =
  mapMaybe
    maybeAttribute
    ( ("id", identifier)
        : ("class", T.intercalate " " classes)
        : attributes
    )

appendContents :: Element -> [Content] -> Element
appendContents el newContents = el {elContent = (elContent el) ++ newContents}

prependContents :: Element -> [Content] -> Element
prependContents el newContents = el {elContent = newContents ++ (elContent el)}

addAttributes :: Element -> [XML.Attr] -> Element
addAttributes el newAttrs = el {elAttribs = newAttrs ++ elAttribs el}

addAttribute :: Element -> T.Text -> T.Text -> Element
addAttribute el attr_name attr_value = el {elAttribs = new_attr : elAttribs el}
  where
    new_attr = XML.Attr (unqual attr_name) attr_value

addCitations :: Element -> [Citation] -> Element
addCitations el citations = appendContents el [Elem $ elementWithContents "citations" $ (text_node "\n") : concatMap citation_to_elem citations]
  where
    citation_to_elem :: Citation -> [Content]
    citation_to_elem citation = asBlockOfInlines with_suffix
      where
        cit_elem = elementWithAttributes (itemName citation) attrs
        prefix = citationPrefix citation
        suffix = citationSuffix citation
        with_prefix = if null prefix then cit_elem else appendContents cit_elem [Elem $ elementWithContents "prefix" $ inlinesToXML prefix]
        with_suffix = if null suffix then with_prefix else appendContents with_prefix [Elem $ elementWithContents "suffix" $ inlinesToXML suffix]
        attrs =
          map
            (\(n, v) -> XML.Attr (unqual n) v)
            [ ("id", citationId citation),
              ("mode", T.pack $ show $ citationMode citation),
              ("note-num", intAsText $ citationNoteNum citation),
              ("hash", intAsText $ citationHash citation)
            ]

definitionListItemToXML :: ([Inline], [[Block]]) -> Content
definitionListItemToXML (inlines, defs) = Elem $ elementWithContents "item" $ term ++ wrapArrayOfBlocks "def" defs
  where
    term = asBlockOfInlines $ appendContents (emptyElement "term") $ inlinesToXML inlines

captionToXML :: Caption -> [Content]
captionToXML (Caption short blocks) = asBlockOfBlocks with_short_caption
  where
    el = elementWithContents "Caption" $ blocksToXML blocks
    with_short_caption = case (short) of
      Just inlines -> prependContents el $ asBlockOfInlines $ elementWithContents "ShortCaption" $ inlinesToXML inlines
      _ -> el

colSpecToXML :: (Alignment, ColWidth) -> [Content]
colSpecToXML (align, cw) = asBlockOfInlines colspec
  where
    colspec = elementWithAttributes "ColSpec" $ validAttributes [(alignmentAttrName, itemName align), ("col-width", colwidth)]
    colwidth = case (cw) of
      ColWidth d -> T.pack $ show d
      ColWidthDefault -> "0"

colSpecsToXML :: [(Alignment, ColWidth)] -> [Content]
colSpecsToXML colspecs = asBlockOfBlocks $ elementWithContents "colspecs" $ concatMap colSpecToXML colspecs

tableHeadToXML :: TableHead -> [Content]
tableHeadToXML (TableHead attr rows) = asBlockOfBlocks $ elementWithAttrAndContents "TableHead" attr $ concatMap rowToXML rows

tableBodyToXML :: TableBody -> [Content]
tableBodyToXML (TableBody (idn, cls, attrs) (RowHeadColumns headcols) hrows brows) = asBlockOfBlocks $ elementWithAttrAndContents "TableBody" attr children
  where
    attr = (idn, cls, ("row-head-columns", intAsText headcols) : attrs)
    header_rows = asBlockOfBlocks $ elementWithContents "header" $ concatMap rowToXML hrows
    body_rows = asBlockOfBlocks $ elementWithContents "body" $ concatMap rowToXML brows
    children = header_rows ++ body_rows

tableFootToXML :: TableFoot -> [Content]
tableFootToXML (TableFoot attr rows) = asBlockOfBlocks $ elementWithAttrAndContents "TableFoot" attr $ concatMap rowToXML rows

rowToXML :: Row -> [Content]
rowToXML (Row attr cells) = asBlockOfBlocks $ elementWithAttrAndContents "Row" attr $ concatMap cellToXML cells

cellToXML :: Cell -> [Content]
cellToXML (Cell (idn, cls, attrs) alignment (RowSpan rowspan) (ColSpan colspan) blocks) = asBlockOfBlocks $ elementWithAttrAndContents "Cell" attr $ blocksToXML blocks
  where
    attrs' = (alignmentAttrName, itemName alignment) : attrs
    attrs'' = if rowspan > 1 then ("rowspan", intAsText rowspan) : attrs' else attrs'
    attrs''' = if colspan > 1 then ("colspan", intAsText colspan) : attrs'' else attrs''
    attr = (idn, cls, attrs''')
