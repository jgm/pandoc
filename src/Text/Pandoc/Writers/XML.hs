{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Pandoc.Writers.XML (writeXML) where

import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (mapMaybe)
import Data.Scientific (FPFormat (Fixed), Scientific, formatScientific, isInteger)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Version (versionBranch)
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options (WriterOptions (..))
import Text.Pandoc.XML.Light
import qualified Text.Pandoc.XML.Light as XML

data Context
  = CtxNone
  | CtxPandocObject
  | CtxMeta
  | CtxMetaValue
  | CtxBlock
  | CtxInline
  | CtxAttribute T.Text
  | CtxAttr
  | CtxClasses T.Text
  | CtxAttributes
  | CtxList
  | CtxListAttributes
  | CtxListItem
  | CtxCaption
  | CtxShortCaption
  | CtxColSpec
  | CtxTableHead
  | CtxTableBody
  | CtxTableFoot
  | CtxRow
  | CtxCell
  | CtxDefListItem
  | CtxDefListTerm
  | CtxDefListDef
  | CtxLineBlock
  | CtxLinkTarget
  | CtxImageTarget
  | CtxCitation
  | CtxCitationPrefix
  | CtxCitationSuffix
  | CtxText
  | CtxArrayOf Context
  | CtxWrapArrayOf Context T.Text
  deriving (Eq, Show)

writeXML :: (PandocMonad m) => WriterOptions -> Pandoc -> m T.Text
writeXML _ doc = do
  return $ showTopElement $ processValue (empty_element "Pandoc") CtxPandocObject $ toJSON doc

text_node :: T.Text -> Content
text_node text = Text (CData CDataText text Nothing)

empty_element :: T.Text -> Element
empty_element tag =
  Element
    { elName = unqual tag,
      elAttribs = [],
      elContent = [],
      elLine = Nothing
    }

scientific_to_text :: Scientific -> T.Text
scientific_to_text sc
  | isInteger sc = T.pack (show (round sc :: Integer)) -- No decimals if it's an integer
  | otherwise = T.pack (formatScientific Fixed Nothing sc) -- Format with Fixed otherwise

appendContentsToElement :: Element -> [Content] -> Element
appendContentsToElement el newContents = el {elContent = (elContent el) ++ newContents}

-- prependContentsToElement :: Element -> [Content] -> Element
-- prependContentsToElement el newContents = el {elContent = newContents ++ (elContent el)}

addAttributesToElement :: Element -> [XML.Attr] -> Element
addAttributesToElement el newAttrs = el {elAttribs = newAttrs ++ elAttribs el}

arrayAtKey :: Object -> T.Text -> Array
arrayAtKey obj key = case (KM.lookup (K.fromText key) obj) of
  Just (Array arr) -> arr
  _ -> V.empty

objectAtKey :: Object -> T.Text -> Object
objectAtKey obj key = case (KM.lookup (K.fromText key) obj) of
  Just (Object o) -> o
  _ -> KM.empty

processPandocObject :: Element -> Object -> Element
processPandocObject element obj =
  let meta = Elem $ processObject (empty_element "meta") CtxMeta $ objectAtKey obj "meta"
      blocks = newElementWithContents "blocks" [CtxBlock] $ arrayAtKey obj "blocks"
      maybe_api_version = valueToArrayOfStrings $ KM.lookup (K.fromText "pandoc-api-version") obj
      api_version = case (maybe_api_version) of
        Just (vv) -> T.intercalate "," vv
        Nothing -> T.intercalate "," $ map (T.pack . show) $ versionBranch pandocTypesVersion
      with_api_version = addAttributesToElement element [Attr (unqual "api-version") api_version]
   in appendContentsToElement with_api_version [meta, blocks]

valueToArrayOfStrings :: Maybe Value -> Maybe [T.Text]
valueToArrayOfStrings (Just (Array arr)) =
  let strings = [str | String str <- V.toList arr]
   in if length strings == V.length arr
        then Just (strings)
        else Nothing
valueToArrayOfStrings _ = Nothing

newElementWithContents :: T.Text -> [Context] -> Array -> Content
newElementWithContents tag contexts contents =
  Elem $ processArray (empty_element tag) contexts contents

processObject :: Element -> Context -> Object -> Element
processObject el context obj = case (context, t, c) of
  (CtxPandocObject, _, Nothing) -> processPandocObject el obj -- outermost object with "meta", "blocks" and "pandoc-api-version" fields
  (_, "Str", Just (String (text))) -> appendContentsToElement el [text_node text] -- text in Str
  (_, "Space", Nothing) -> appendContentsToElement el [text_node " "] -- spaces as Space
  -- (_, "", Just (Array (contents))) -> processArray el [context] contents -- should not happen
  (_, _, Just (Array (contents))) -> processObjectWithArrayContents el t contents -- any object with a non-empty t field
  (CtxAttribute ("col-width"), "ColWidthDefault", Nothing) -> addAttributesToElement el [Attr (unqual "col-width") "0"] -- ColSpec col-width attribute with a ColWidthDefault value
  (CtxAttribute (name), _, Nothing) -> addAttributesToElement el [Attr (unqual name) t] -- alignments, number-style, number-delim
  (CtxCitation, _, Nothing) -> appendContentsToElement el [Elem $ processCitationObject obj] -- Citation
  (_, "MetaMap", Nothing) -> processMetaMap (empty_element "MetaMap") obj -- MetaMap
  (CtxMeta, _, Nothing) -> processMetaMap el obj -- Meta
  (_, tag, Nothing) -> appendContentsToElement el [Elem $ empty_element tag] -- HorizontalRule, SoftBreak, LineBreak
  _ -> el
  where
    t = case KM.lookup (K.fromText "t") obj of
      Just (String (text)) -> text
      _ -> ""
    c = KM.lookup (K.fromText "c") obj

processObjectWithArrayContents :: Element -> T.Text -> Array -> Element
processObjectWithArrayContents el tag contents = case (tag) of
  "Para" -> createElemAndProcessValuesAs [CtxInline]
  "Div" -> createElemAndProcessValuesAs [CtxAttr, CtxArrayOf CtxBlock]
  "Header" -> createElemAndProcessValuesAs [CtxAttribute "level", CtxAttr, CtxArrayOf CtxInline]
  "Plain" -> createElemAndProcessValuesAs [CtxInline]
  "Span" -> createElemAndProcessValuesAs [CtxAttr, CtxArrayOf CtxInline]
  "BulletList" -> createElemAndProcessValuesAs [CtxListItem]
  "OrderedList" -> createElemAndProcessValuesAs [CtxListAttributes, CtxArrayOf CtxListItem]
  "Table" -> createElemAndProcessValuesAs [CtxAttr, CtxCaption, CtxWrapArrayOf CtxColSpec "colspecs", CtxTableHead, CtxArrayOf CtxTableBody, CtxTableFoot]
  "Figure" -> createElemAndProcessValuesAs [CtxAttr, CtxCaption, CtxArrayOf CtxBlock]
  "BlockQuote" -> createElemAndProcessValuesAs [CtxBlock]
  "DefinitionList" -> createElemAndProcessValuesAs [CtxDefListItem]
  "LineBlock" -> createElemAndProcessValuesAs [CtxLineBlock]
  "CodeBlock" -> createElemAndProcessValuesAs [CtxAttr, CtxText]
  "RawBlock" -> createElemAndProcessValuesAs [CtxAttribute "format", CtxText]
  "Quoted" -> createElemAndProcessValuesAs [CtxAttribute "quote-type", CtxArrayOf CtxInline]
  "Cite" -> createElemAndProcessValuesAs [CtxWrapArrayOf CtxCitation "citations", CtxArrayOf CtxInline]
  "Code" -> createElemAndProcessValuesAs [CtxAttr, CtxText]
  "Math" -> createElemAndProcessValuesAs [CtxAttribute "math-type", CtxText]
  "RawInline" -> createElemAndProcessValuesAs [CtxAttribute "format", CtxText]
  "Link" -> createElemAndProcessValuesAs [CtxAttr, CtxArrayOf CtxInline, CtxLinkTarget]
  "Image" -> createElemAndProcessValuesAs [CtxAttr, CtxArrayOf CtxInline, CtxImageTarget]
  "Note" -> createElemAndProcessValuesAs [CtxBlock]
  "MetaBlocks" -> createElemAndProcessValuesAs [CtxBlock]
  "MetaList" -> createElemAndProcessValuesAs [CtxMetaValue]
  "MetaBool" -> createElemAndProcessValuesAs [CtxAttribute "value"]
  "MetaString" -> createElemAndProcessValuesAs [CtxText]
  _ -> createElemAndProcessValuesAs [CtxInline] -- Emph, Underline, Strong, Strikeout, Superscript, Subscript, SmallCaps, MetaInlines
  where
    createElemAndProcessValuesAs :: [Context] -> Element
    createElemAndProcessValuesAs contexts =
      let before = text_node ""
          after = text_node ""
       in appendContentsToElement el [before, newElementWithContents tag contexts contents, after]

processValue :: Element -> Context -> Value -> Element
processValue el context v = case (context, v) of
  (_, Object (obj)) -> processObject el context obj
  (CtxArrayOf ctx, Array (a)) -> processArray el [ctx] a
  (CtxWrapArrayOf ctx tag, Array (a)) -> appendContentsToElement el [newElementWithContents tag [ctx] a]
  (CtxAttr, Array (arr)) -> processAttr el $ V.toList arr
  (CtxAttribute attr_name, Number (number)) -> addAttributesToElement el [Attr (unqual attr_name) (scientific_to_text number)]
  (CtxAttribute attr_name, String ("")) -> el -- no empty attributes
  (CtxAttribute attr_name, String (text)) -> addAttributesToElement el [Attr (unqual attr_name) text]
  (CtxAttribute attr_name, Bool (b)) -> addAttributesToElement el [Attr (unqual attr_name) $ if b then "true" else "false"]
  (CtxClasses attr_name, Array (wrds)) -> addAttributesToElement el [Attr (unqual attr_name) $ wordsAttrValue (V.toList wrds)]
  (CtxAttributes, Array (a)) -> addAttributesToElement el $ arrayToAttributes $ V.toList a
  (CtxListAttributes, Array (a)) -> processArray el [CtxAttribute "start", CtxAttribute "number-style", CtxAttribute "number-delim"] a
  (CtxListItem, Array (a)) -> appendContentsToElement el [newElementWithContents "item" [CtxBlock] a]
  (CtxImageTarget, Array (a)) -> processArray el [CtxAttribute "src", CtxAttribute "title"] a
  (CtxLinkTarget, Array (a)) -> processArray el [CtxAttribute "href", CtxAttribute "title"] a
  (CtxCaption, Array (a)) -> appendContentsToElement el [newElementWithContents "Caption" [CtxShortCaption, CtxArrayOf CtxBlock] a]
  (CtxShortCaption, Array (a)) -> appendContentsToElement el [newElementWithContents "ShortCaption" [CtxArrayOf CtxInline] a]
  (CtxColSpec, Array (a)) -> appendContentsToElement el [newElementWithContents "ColSpec" [CtxAttribute "alignment", CtxAttribute "col-width"] a]
  (CtxTableHead, Array (a)) -> appendContentsToElement el [newElementWithContents "TableHead" [CtxAttr, CtxArrayOf CtxRow] a]
  (CtxTableBody, Array (a)) -> appendContentsToElement el [newElementWithContents "TableBody" [CtxAttr, CtxAttribute "row-head-columns", CtxArrayOf CtxRow, CtxArrayOf CtxRow] a]
  (CtxTableFoot, Array (a)) -> appendContentsToElement el [newElementWithContents "TableFoot" [CtxAttr, CtxArrayOf CtxRow] a]
  (CtxRow, Array (a)) -> appendContentsToElement el [newElementWithContents "Row" [CtxAttr, CtxArrayOf CtxCell] a]
  (CtxCell, Array (a)) -> appendContentsToElement el [newElementWithContents "Cell" [CtxAttr, CtxAttribute "alignment", CtxAttribute "row-span", CtxAttribute "col-span", CtxArrayOf CtxBlock] a]
  (CtxDefListItem, Array (a)) -> appendContentsToElement el [newElementWithContents "item" [CtxDefListTerm, CtxArrayOf CtxDefListDef] a]
  (CtxDefListTerm, Array (a)) -> appendContentsToElement el [newElementWithContents "term" [CtxInline] a]
  (CtxDefListDef, Array (a)) -> appendContentsToElement el [newElementWithContents "def" [CtxBlock] a]
  (CtxLineBlock, Array (a)) -> appendContentsToElement el [newElementWithContents "line" [CtxInline] a]
  (CtxCitationPrefix, Array (a)) -> if V.length a > 0 then appendContentsToElement el [newElementWithContents "prefix" [CtxInline] a] else el
  (CtxCitationSuffix, Array (a)) -> if V.length a > 0 then appendContentsToElement el [newElementWithContents "suffix" [CtxInline] a] else el
  (_, Array (a)) -> processArray el [context] a
  (_, String (text)) -> appendContentsToElement el [text_node text]
  _ -> el

processArray :: Element -> [Context] -> Array -> Element
processArray element contexts contents = case (V.toList contents) of
  [] -> element
  x : xs -> processArray (processValue element contexts_head x) contexts_tail $ V.fromList xs
    where
      contexts_head = case (contexts) of
        [] -> CtxNone
        c : cs -> c
      contexts_tail = case (contexts) of
        [] -> [CtxNone]
        [c] -> [c]
        _ : cs -> cs

wordsAttrValue :: [Value] -> T.Text
wordsAttrValue wrds =
  if areAllStrings wrds
    then
      T.unwords $ map extractText wrds
    else ""
  where
    extractText (String t) = t
    extractText _ = T.empty
    areAllStrings values = not (null values) && (all valueIsString values)

arrayToAttributes :: [Value] -> [XML.Attr]
arrayToAttributes values = mapMaybe valueToAttr values
  where
    valueToAttr :: Value -> Maybe XML.Attr
    valueToAttr (Array arr')
      | V.length arr' == 2 =
          case V.toList arr' of
            ["id", _] -> Nothing
            ["class", _] -> Nothing
            [String name, String value] -> Just (Attr (unqual name) value)
            _ -> Nothing
      | otherwise = Nothing
    valueToAttr _ = Nothing

processAttr :: Element -> [Value] -> Element
processAttr element arr = case (arr) of
  [String identifier, Array classes, Array attributes] ->
    let withId = addIdToElement element identifier
        withClasses = addClassesToElement withId (V.toList classes)
        withAttributes = addAttributesToElement withClasses (valuesToAttrs $ V.toList attributes)
     in withAttributes
  _ -> element
  where
    addIdToElement :: Element -> T.Text -> Element
    addIdToElement el identifier = case (identifier) of
      "" -> el
      value -> addAttributesToElement el [Attr (unqual "id") value]

    addClassesToElement :: Element -> [Value] -> Element
    addClassesToElement el classes =
      if areAllStrings classes
        then
          addAttributesToElement el [Attr (unqual "class") (T.unwords $ map extractText classes)]
        else el
      where
        extractText (String t) = t
        extractText _ = T.empty
        areAllStrings values = not (null values) && (all valueIsString values)

    valueToAttr :: Value -> Maybe XML.Attr
    valueToAttr (Array arr')
      | V.length arr' == 2 =
          case V.toList arr' of
            [String name, String value] -> Just (Attr (unqual name) value)
            _ -> Nothing
      | otherwise = Nothing
    valueToAttr _ = Nothing

    valuesToAttrs :: [Value] -> [XML.Attr]
    valuesToAttrs values = mapMaybe valueToAttr values

valueIsString :: Value -> Bool
valueIsString (String _) = True
valueIsString _ = False

processCitationObject :: Object -> Element
processCitationObject obj = foldl process_citation_field el fields
  where
    el = empty_element "Citation"
    fields =
      [ ("citationHash", (CtxAttribute "hash")),
        ("citationId", (CtxAttribute "id")),
        ("citationMode", (CtxAttribute "mode")),
        ("citationNoteNum", (CtxAttribute "note-num")),
        ("citationPrefix", CtxCitationPrefix),
        ("citationSuffix", CtxCitationSuffix)
      ]
    value_at_key :: T.Text -> Value
    value_at_key key = case (KM.lookup (K.fromText key) obj) of
      Just (v) -> v
      Nothing -> String ""
    process_citation_field :: Element -> (T.Text, Context) -> Element
    process_citation_field e (key, context) = processValue e context $ value_at_key key

processMetaMap :: Element -> Object -> Element
processMetaMap el obj = foldl process_meta_entry el $ KM.toList obj
  where
    entry_element :: KM.Key -> Element
    entry_element k = addAttributesToElement (empty_element "entry") [XML.Attr (unqual "text") $ K.toText k]
    process_meta_entry :: Element -> (KM.Key, Value) -> Element
    process_meta_entry e (k, v) = appendContentsToElement e [Elem $ processValue (entry_element k) CtxMetaValue v]
