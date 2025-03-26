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
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options (WriterOptions (..))
import Text.Pandoc.XML.Light
import qualified Text.Pandoc.XML.Light as XML

data Context
  = None
  | PandocObject
  | MetaData
  | BlockElem
  | InlineElem
  | Attribute T.Text
  | AttrData
  | WordsAttribute T.Text
  | Attributes
  | ListElem
  | ListAttributes
  | ListContents
  | ListItem
  | TextData
  deriving (Eq, Show)

data ElementType
  = IsBlockOfBlocks
  | IsBlockOfInlines
  | IsInline
  | IsListElem
  | IsLineBlockElem
  deriving (Eq, Show)

writeXML :: (PandocMonad m) => WriterOptions -> Pandoc -> m T.Text
writeXML _ doc = do
  return $ showTopElement $ processValue (empty_element "Pandoc") PandocObject $ toJSON doc

text_node :: T.Text -> Content
text_node text = Text (CData CDataText text Nothing)

empty_text :: Content
empty_text = text_node ""

empty_element :: T.Text -> Element
empty_element tag =
  Element
    { elName = unqual tag,
      elAttribs = [],
      elContent = [],
      elLine = Nothing
    }

element_name :: Element -> T.Text
element_name element = qName $ elName element

scientific_to_text :: Scientific -> T.Text
scientific_to_text sc
  | isInteger sc = T.pack (show (round sc :: Integer)) -- No decimals if it's an integer
  | otherwise = T.pack (formatScientific Fixed Nothing sc) -- Format with Fixed otherwise

appendContentsToElement :: Element -> [Content] -> Element
appendContentsToElement el newContents = el {elContent = (elContent el) ++ newContents}

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
  let meta = Elem $ processObject (empty_element "meta") MetaData $ objectAtKey obj "meta"
      blocks = newElementWithContents "blocks" [BlockElem] $ arrayAtKey obj "blocks"
   in -- add apiVersion attribute
      appendContentsToElement element [meta, blocks]

newElementWithContents :: T.Text -> [Context] -> Array -> Content
newElementWithContents tag contexts contents =
  Elem $ processArray (empty_element tag) contexts $ V.toList contents

-- process Object that has the "t" key
processObject :: Element -> Context -> Object -> Element
processObject element context obj = case (context, t, c) of
  (PandocObject, _, Nothing) -> processPandocObject element obj
  (_, "Str", Just (String (text))) -> appendContentsToElement element [text_node text]
  (_, "Space", Nothing) -> appendContentsToElement element [text_node " "]
  -- (_, "", _, Just (Array (contents))) -> processArray element [context] $ V.toList contents -- should not happen
  (_, _, Just (Array (contents))) -> processObjectWithArrayContents element t contents
  (Attribute ("col-width"), "ColWidthDefault", Nothing) -> addAttributesToElement element [Attr (unqual "col-width") "0"]
  (Attribute (name), _, Nothing) -> addAttributesToElement element [Attr (unqual name) t]
  (_, tag, Nothing) -> appendContentsToElement element [Elem $ empty_element tag] -- HorizontalRule, SoftBreak, LineBreak
  _ -> element
  where
    t = case KM.lookup (K.fromText "t") obj of
      Just (String (text)) -> text
      _ -> ""
    c = KM.lookup (K.fromText "c") obj

processObjectWithArrayContents :: Element -> T.Text -> Array -> Element
processObjectWithArrayContents el tag contents = case (tag, el_type) of
  (_, IsBlockOfInlines) -> createElemAndProcessValuesAs [InlineElem] -- Plain, Para
  (_, IsBlockOfBlocks) -> createElemAndProcessValuesAs [BlockElem] -- BlockQuote
  ("Div", _) -> createElemAndProcessValuesAs [AttrData, BlockElem]
  ("Span", _) -> createElemAndProcessValuesAs [AttrData, InlineElem]
  ("Header", _) -> createElemAndProcessValuesAs [Attribute "level", AttrData, InlineElem]
  ("BulletList", _) -> createElemAndProcessValuesAs [ListContents]
  ("OrderedList", _) -> createElemAndProcessValuesAs [ListAttributes, ListContents]
  _ -> createElemAndProcessValuesAs [BlockElem]
  where
    el_type = elementType el
    createElemAndProcessValuesAs :: [Context] -> Element
    createElemAndProcessValuesAs contexts = appendContentsToElement el [newElementWithContents tag contexts contents]

processValue :: Element -> Context -> Value -> Element
processValue el context v = case (context, v) of
  (_, Object (obj)) -> processObject el context obj
  (AttrData, Array (arr)) -> processAttr el $ V.toList arr
  (Attribute attr_name, Number (number)) -> addAttributesToElement el [Attr (unqual attr_name) (scientific_to_text number)]
  (Attribute attr_name, String (text)) -> addAttributesToElement el [Attr (unqual attr_name) text]
  (WordsAttribute attr_name, Array (wrds)) -> addAttributesToElement el [Attr (unqual attr_name) $ wordsAttrValue (V.toList wrds)]
  (Attributes, Array (a)) -> addAttributesToElement el $ arrayToAttributes $ V.toList a
  (ListAttributes, Array (a)) -> processArray el [Attribute "start", Attribute "number-style", Attribute "number-delim"] $ V.toList a
  (ListContents, Array (a)) -> appendContentsToElement el $ wrapItems "item" BlockElem $ V.toList a
  (ListItem, Array (a)) -> appendContentsToElement el [newElementWithContents "item" [BlockElem] a]
  (_, Array (a)) -> processArray el [context] $ V.toList a
  (_, String (text)) -> appendContentsToElement el [text_node text]
  _ -> el

wrapItems :: T.Text -> Context -> [Value] -> [Content]
wrapItems tag context values = map wrapInElement values
  where
    wrapInElement :: Value -> Content
    wrapInElement (Array a) = Elem $ processArray item_el [context] $ V.toList a
      where
        item_el = empty_element tag
    wrapInElement _ = empty_text

processArray :: Element -> [Context] -> [Value] -> Element
processArray element contexts contents = case (contents) of
  [] -> element
  x : xs -> processArray (processValue element (head contexts) x) contexts_tail xs
    where
      contexts_tail = case (contexts) of
        [] -> [None]
        [c] -> [c]
        _ : cs -> cs

elementType :: Element -> ElementType
elementType el = tagType $ element_name el

tagType :: T.Text -> ElementType
tagType name = case (name) of
  "Str" -> IsInline
  "Space" -> IsInline
  "Para" -> IsBlockOfInlines
  "Header" -> IsBlockOfInlines
  "Plain" -> IsBlockOfInlines
  "Emph" -> IsInline
  "Underline" -> IsInline
  "Strong" -> IsInline
  "Strikeout" -> IsInline
  "Superscript" -> IsInline
  "Subscript" -> IsInline
  "SmallCaps" -> IsInline
  "Quoted" -> IsInline
  "Cite" -> IsInline
  "Code" -> IsInline
  "SoftBreak" -> IsInline
  "LineBreak" -> IsInline
  "Math" -> IsInline
  "RawInline" -> IsInline
  "Link" -> IsInline
  "Image" -> IsInline
  "Note" -> IsInline
  "Span" -> IsInline
  "BulletList" -> IsListElem
  "OrderedList" -> IsListElem
  "LineBlock" -> IsLineBlockElem
  "line" -> IsBlockOfInlines
  _ -> IsBlockOfBlocks

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
