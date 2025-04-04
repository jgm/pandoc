{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Text.Pandoc.Readers.XML (readXML) where

import Codec.Picture.Metadata (Value (Int))
import Control.Monad (forM_, unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.State.Strict (StateT (runStateT), gets, modify)
import Data.Char (isDigit, isSpace)
import Data.Default (Default (..))
import Data.Generics
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set ((\\))
import qualified Data.Set as S (Set, fromList, member)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (fromStrict)
import Data.Version (Version, makeVersion, versionBranch)
import Text.Blaze.Html4.FrameSet (blockquote)
import Text.Pandoc.Builder
import Text.Pandoc.Class.PandocMonad
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError (..))
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Parsing (ToSources, toSources)
import Text.Pandoc.Shared (extractSpaces)
import Text.Pandoc.Sources (sourcesToText)
import Text.Pandoc.Version (pandocVersion)
import Text.Pandoc.XML (lookupEntity)
import Text.Pandoc.XML.Light
import qualified Text.Pandoc.XML.Light as XML
import Text.Read (readMaybe)

type XMLReader m = StateT XMLReaderState m

data XMLReaderState = XMLReaderState
  { xmlApiVersion :: Version,
    xmlMeta :: Meta,
    xmlContent :: [Content],
    xmlPath :: [Text]
  }
  deriving (Show)

instance Default XMLReaderState where
  def =
    XMLReaderState
      { xmlApiVersion = pandocVersion,
        xmlMeta = mempty,
        xmlContent = [],
        xmlPath = ["root"]
      }

readXML :: (PandocMonad m, ToSources a) => ReaderOptions -> a -> m Pandoc
readXML _ inp = do
  let sources = toSources inp
  tree <-
    either (throwError . PandocXMLError "") return $
      parseXMLContents (fromStrict . sourcesToText $ sources)
  (bs, st') <- flip runStateT (def {xmlContent = tree}) $ mapM parseContents tree
  let blockList = toList $ mconcat bs
  return $ Pandoc (xmlMeta st') blockList

getBlocks :: (PandocMonad m) => Element -> XMLReader m Blocks
getBlocks e = mconcat <$> mapM parseBlock (elContent e)

attrValue :: Text -> Element -> Text
attrValue attr =
  fromMaybe "" . maybeAttrValue attr

maybeAttrValue :: Text -> Element -> Maybe Text
maybeAttrValue attr elt =
  lookupAttrBy (\x -> qName x == attr) (elAttribs elt)

reportUnexpected :: (PandocMonad m, Eq b) => Text -> Text -> b -> XMLReader m b
reportUnexpected what context whatever = do
  path <- gets xmlPath
  let parent = headOr "" path
   in do
        report $ UnexpectedXmlElement (what <> " \"" <> context <> "\"") parent
        return whatever

parseBlock :: (PandocMonad m) => Content -> XMLReader m Blocks
parseBlock (Text (CData CDataRaw _ _)) = return mempty -- DOCTYPE
parseBlock (Text (CData _ s _)) =
  if T.all isSpace s
    then return mempty
    else reportUnexpected "text" s mempty
-- parseBlock (CRef x) = return $ plain $ str $ T.toUpper x
parseBlock (CRef x) = reportUnexpected "reference" x mempty
parseBlock (Elem e) = do
  let name = qName $ elName e
  -- modify $ \st -> st {xmlContextElement = name, xmlContextParent = parent}
  case (name) of
    "Pandoc" -> parsePandoc
    "?xml" -> return mempty
    "blocks" -> getBlocks e
    "Para" -> parseMixed name para (elContent e)
    "Plain" -> parseMixed name plain (elContent e)
    "Header" -> parseMixed name (headerWith attr level) (elContent e)
      where
        level = textToInt (attrValue "level" e) 1
        attr = filterAttrAttributes ["level"] $ attrFromElement e
    "HorizontalRule" -> return horizontalRule
    "BlockQuote" -> do
      contents <- getBlocks e
      return $ blockQuote contents
    "Div" -> do
      contents <- getBlocks e
      return $ divWith (attrFromElement e) contents
    _ -> reportUnexpected "element" name mempty
  where
    parsePandoc = do
      let version = maybeAttrValue "api-version" e
          apiversion = case (version) of
            Just (v) -> makeVersion $ map (read . T.unpack) $ T.splitOn "," v
            Nothing -> pandocVersion
       in modify $ \st -> st {xmlApiVersion = apiversion}
      getBlocks e

    parseMixed parent' container conts = do
      let (ils, rest) = break isBlockElement conts
      ils' <- trimInlines . mconcat <$> mapM (parseInline parent') ils
      let p = if ils' == mempty then mempty else container ils'
      case rest of
        [] -> return p
        (r : rs) -> do
          b <- parseBlock r
          x <- parseMixed parent' container rs
          return $ p <> b <> x

strContentRecursive :: Element -> Text
strContentRecursive =
  strContent
    . (\e' -> e' {elContent = map elementToStr $ elContent e'})

elementToStr :: Content -> Content
elementToStr (Elem e') = Text $ CData CDataText (strContentRecursive e') Nothing
elementToStr x = x

textToInt :: Text -> Int -> Int
textToInt t deflt =
  let safe_to_int :: Text -> Maybe Int
      safe_to_int s = readMaybe $ T.unpack s
   in case (safe_to_int t) of
        Nothing -> deflt
        Just (n) -> n

parseInline :: (PandocMonad m) => Text -> Content -> XMLReader m Inlines
parseInline _ (Text (CData _ s _)) = return $ text s
parseInline _ (CRef ref) =
  return $
    maybe (text $ T.toUpper ref) text $
      lookupEntity ref
parseInline parent (Elem e) =
  case qName (elName e) of
    "Emph" -> innerInlines parent emph
    "Strong" -> innerInlines parent strong
    "Strikeout" -> innerInlines parent strikeout
    "Subscript" -> innerInlines parent subscript
    "Superscript" -> innerInlines parent superscript
    "Underline" -> innerInlines parent underline
    "LineBreak" -> return linebreak
    "SmallCaps" -> innerInlines parent smallcaps
    "Code" -> codeWithLang
    -- "monospace" -> codeWithLang
    -- "inline-graphic" -> getGraphic e
    _ -> innerInlines parent id
  where
    innerInlines parent' f =
      extractSpaces f . mconcat
        <$> mapM (parseInline parent') (elContent e)
    codeWithLang = do
      let classes' = case attrValue "language" e of
            "" -> []
            l -> [l]
      return $ codeWith (attrValue "id" e, classes', []) $ strContentRecursive e

isBlockElement :: Content -> Bool
isBlockElement (Elem e) = qName (elName e) `S.member` blocktags
  where
    blocktags = S.fromList $ paragraphLevel ++ lists ++ other
    paragraphLevel = ["Para", "Plain", "Header"]
    lists = ["BulletList", "DefinitionList", "OrderedList"]
    other =
      [ "LineBlock",
        "CodeBlock",
        "RawBlock",
        "BlockQuote",
        "HorizontalRule",
        "Table",
        "Figure",
        "Div"
      ]
isBlockElement _ = False

-- onlyOneChild x = length (allChildren x) == 1
-- allChildren x = filterChildren (const True) x

isBlockTag :: Text -> Bool
isBlockTag tag = tag `S.member` blocktags
  where
    blocktags = S.fromList $ paragraphLevel ++ lists ++ other
    paragraphLevel = ["Para", "Plain", "Header"]
    lists = ["BulletList", "DefinitionList", "OrderedList"]
    other =
      [ "LineBlock",
        "CodeBlock",
        "RawBlock",
        "BlockQuote",
        "HorizontalRule",
        "Table",
        "Figure",
        "Div"
      ]

-- True for elements that can contain only elements or spaces, not text or references
isOnlyElementsTag :: Text -> Bool
isOnlyElementsTag tag =
  tag
    `S.member` S.fromList
      [ "meta",
        "blocks",
        "BulletList",
        "DefinitionList",
        "OrderedList",
        "LineBlock",
        "BlockQuote",
        "Table",
        "Figure",
        "Div",
        "Note",
        "Caption", -- container of an optional ShortCaption and the blocks of a (Figure or Table) Caption
        "TableHead",
        "TableBody",
        "TableFoot",
        "Row",
        "Cell",
        "item", -- container of list items
        "def", -- container of the definitions of an item in a DefinitionList
        "header", -- container of the header rows of a TableBody
        "body" -- container of the body rows of a TableBody
      ]

isMetaValueTag :: Text -> Bool
isMetaValueTag tag =
  tag
    `S.member` S.fromList
      [ "MetaBool",
        "MetaString",
        "MetaInlines",
        "MetaBlocks",
        "MetaList",
        "MetaMap"
      ]

isListItemTag :: Text -> Bool
isListItemTag tag = tag == "item"

isTableSectionTag :: Text -> Bool
isTableSectionTag tag = tag == "TableBody" || tag == "TableHead" || tag == "TableFoot"

isTableBodySectionTag :: Text -> Bool
isTableBodySectionTag tag = tag == "header" || tag == "body"

isInlineTag :: Text -> Bool
isInlineTag tag = tag `S.member` inlinetags
  where
    inlinetags =
      S.fromList
        [ "Underline",
          "Strong",
          "Strikeout",
          "Superscript",
          "Subscript",
          "SmallCaps",
          "Quoted",
          "Cite",
          "Code",
          "SoftBreak",
          "LineBreak",
          "Math",
          "RawInline",
          "Link",
          "Image",
          "Note",
          "Span"
        ]

expectedContent :: [Text] -> Content -> Bool
expectedContent path (Elem e) =
  let tag = qName $ elName e
      parent = if length path > 1 then head path else ""
      grandparent = if length path > 2 then head (tail path) else ""
   in case (parent) of
        "blocks" -> isBlockTag tag
        "meta" -> isMetaValueTag tag
        "Para" -> isInlineTag tag
        "Plain" -> isInlineTag tag
        "Header" -> isInlineTag tag
        "Div" -> isBlockTag tag
        "BlockQuote" -> isBlockTag tag
        "BulletList" -> isListItemTag tag
        "OrderedList" -> isListItemTag tag
        "DefinitionList" -> isListItemTag tag
        "item" ->
          if grandparent /= "DefinitionList"
            then isBlockTag tag
            else tag == "term" || tag == "def"
        "LineBlock" -> tag == "line"
        "Table" -> tag == "Caption" || tag == "colspecs" || isTableBodySectionTag tag
        "Figure" -> tag == "Caption" || isBlockTag "tag"
        "Caption" -> tag == "ShortCaption" || isBlockTag "tag"
        "TableBody" -> tag == "header" || tag == "body"
        "header" -> tag == "Row"
        "body" -> tag == "Row"
        "TableHead" -> tag == "Row"
        "TableFoot" -> tag == "Row"
        "Row" -> tag == "Cell"
        _ -> False
expectedContent path (Text (CData _ s _)) = non_space_text_allowed || T.all isSpace s
  where
    parent = if length path > 1 then head path else ""
    non_space_text_allowed = not (isOnlyElementsTag parent)
expectedContent _ _ = True

headOr :: a -> [a] -> a
headOr default_value [] = default_value
headOr _ (x : _) = x

parseContents :: (PandocMonad m) => Content -> XMLReader m Blocks
parseContents content = do
  path <- gets xmlPath
  let expected = expectedContent path
   in case (content) of
        (Text (CData CDataRaw _ _)) -> return mempty -- DOCTYPE
        (Text (CData _ s _)) -> do
          if T.all isSpace s
            then return mempty
            else reportUnexpected "text" s mempty
        (CRef x) -> reportUnexpected "reference" x mempty
        (Elem e) -> do
          let name = qName $ elName e
           in -- modify $ \st -> st {xmlContextElement = name, xmlContextParent = parent}
              case (name) of
                "Pandoc" -> parsePandoc
                "?xml" -> return mempty
                "blocks" -> getBlocks e
                "Para" -> parseMixed name para (elContent e)
                "Plain" -> parseMixed name plain (elContent e)
                "HorizontalRule" -> return horizontalRule
                _ -> reportUnexpected "element" name mempty
          where
            parsePandoc = do
              let version = maybeAttrValue "api-version" e
                  apiversion = case (version) of
                    Just (v) -> makeVersion $ map (read . T.unpack) $ T.splitOn "," v
                    Nothing -> pandocVersion
               in modify $ \st -> st {xmlApiVersion = apiversion}
              getBlocks e
            parseMixed parent' container conts = do
              let (ils, rest) = break isBlockElement conts
              ils' <- trimInlines . mconcat <$> mapM (parseInline parent') ils
              let p = if ils' == mempty then mempty else container ils'
              case rest of
                [] -> return p
                (r : rs) -> do
                  b <- parseBlock r
                  x <- parseMixed parent' container rs
                  return $ p <> b <> x

type PandocAttr = (Text, [Text], [(Text, Text)])

filterAttributes :: S.Set Text -> [(Text, Text)] -> [(Text, Text)]
filterAttributes to_be_removed a = filter keep_attr a
  where
    keep_attr (k, _) = not (k `S.member` to_be_removed)

filterAttrAttributes :: [Text] -> PandocAttr -> PandocAttr
filterAttrAttributes to_be_removed (idn, classes, a) = (idn, classes, filtered)
  where
    filtered = filterAttributes (S.fromList to_be_removed) a

attrFromElement :: Element -> PandocAttr
attrFromElement e = filterAttrAttributes ["id", "class"] (idn, classes, attributes)
  where
    idn = attrValue "id" e
    classes = T.words $ attrValue "class" e
    attributes = map (\a -> (qName $ attrKey a, attrVal a)) $ elAttribs e
