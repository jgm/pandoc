{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Text.Pandoc.Readers.XML (readXML) where

import Control.Monad (mfilter)
import Control.Monad.Except (throwError)
import Control.Monad.State.Strict (StateT (runStateT), gets, modify)
import Data.Char (isSpace)
import Data.Default (Default (..))
import Data.List (partition)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import qualified Data.Set as S (Set, fromList, member)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy (fromStrict)
import Data.Version (Version, makeVersion)
import Text.Pandoc.Builder
import Text.Pandoc.Class.PandocMonad
import Text.Pandoc.Error (PandocError (..))
import Text.Pandoc.FormatXML
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Parsing (ToSources, toSources)
import Text.Pandoc.Shared (extractSpaces)
import Text.Pandoc.Sources (sourcesToText)
import Text.Pandoc.Version (pandocVersion)
import Text.Pandoc.XML (lookupEntity)
import Text.Pandoc.XML.Light
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

elementName :: Element -> Text
elementName e = qName $ elName e

attrValue :: Text -> Element -> Text
attrValue attr =
  fromMaybe "" . maybeAttrValue attr

maybeAttrValue :: Text -> Element -> Maybe Text
maybeAttrValue attr elt =
  lookupAttrBy (\x -> qName x == attr) (elAttribs elt)

parseBlock :: (PandocMonad m) => Content -> XMLReader m Blocks
parseBlock (Text (CData CDataRaw _ _)) = return mempty -- DOCTYPE
parseBlock (Text (CData _ s _)) =
  if T.all isSpace s
    then return mempty
    else do
      report $ UnexpectedXmlCData s ""
      return mempty
parseBlock (CRef x) = do
  report $ UnexpectedXmlReference x ""
  return mempty
parseBlock (Elem e) = do
  let name = elementName e
   in -- modify $ \st -> st {xmlContextElement = name, xmlContextParent = parent}
      case (name) of
        "Pandoc" -> parsePandoc
        "?xml" -> return mempty
        "blocks" -> getBlocks e
        "Para" -> parseMixed para (elContent e)
        "Plain" -> parseMixed plain (elContent e)
        "Header" -> parseMixed (headerWith attr level) (elContent e)
          where
            level = textToInt (attrValue attrLevel e) 1
            attr = filterAttrAttributes [attrLevel] $ attrFromElement e
        "HorizontalRule" -> return horizontalRule
        "BlockQuote" -> do
          contents <- getBlocks e
          return $ blockQuote contents
        "Div" -> do
          contents <- getBlocks e
          return $ divWith (attrFromElement e) contents
        "BulletList" -> do
          items <- getArrayOfBlocks isListItem (elContent e)
          return $ bulletList items
        "OrderedList" -> do
          items <- getArrayOfBlocks isListItem (elContent e)
          return $ orderedListWith (getListAttributes e) items
        -- "DefinitionList" -> do
        -- "Figure" -> do
        --   let attr = attrFromElement e
        --    (citations_contents, contents) <- partitionCitationsAndInlines $ elContent e
        --   caption <- parseCaption e
        --   return $ figureWith attr caption
        "CodeBlock" -> do
          let attr = attrFromElement e
          return $ codeBlockWith attr $ strContentRecursive e
        "RawBlock" -> do
          let format = (attrValue attrFormat e)
          return $ rawBlock format $ strContentRecursive e
        "LineBlock" -> do
          lins <- getArrayOfInlines isLineItem (elContent e)
          return $ lineBlock lins
        -- "Table" -> do
        _ -> do
          report $ UnexpectedXmlElement name ""
          return mempty
  where
    parsePandoc = do
      let version = maybeAttrValue attrApiVersion e
          apiversion = case (version) of
            Just (v) -> makeVersion $ map (read . T.unpack) $ T.splitOn "," v
            Nothing -> pandocVersion
       in modify $ \st -> st {xmlApiVersion = apiversion}
      getBlocks e

    parseMixed container conts = do
      let (ils, rest) = break isBlockElement conts
      ils' <- trimInlines . mconcat <$> mapM parseInline ils
      let p = if ils' == mempty then mempty else container ils'
      case rest of
        [] -> return p
        (r : rs) -> do
          b <- parseBlock r
          x <- parseMixed container rs
          return $ p <> b <> x

getArrayOfBlocks :: (PandocMonad m) => (Element -> Bool) -> [Content] -> XMLReader m [Blocks]
getArrayOfBlocks filter_element contents = mfilter not_empty <$> mapM readBlocksSelectedElements contents
  where
    not_empty blocks = not (null blocks)
    readBlocksSelectedElements :: (PandocMonad m) => Content -> XMLReader m Blocks
    readBlocksSelectedElements content = do
      context <- gets xmlPath
      let parent = headOr "" context
       in case (content) of
            (Elem c) ->
              if filter_element c
                then do
                  mconcat <$> mapM parseBlock (elContent c)
                else do
                  report $ UnexpectedXmlElement (elementName c) ""
                  return mempty
            (Text (CData _ s _)) ->
              if T.all isSpace s
                then return mempty
                else do
                  report $ UnexpectedXmlCData s parent
                  return mempty
            (CRef ref) -> do
              report $ UnexpectedXmlReference ref parent
              return mempty

getArrayOfInlines :: (PandocMonad m) => (Element -> Bool) -> [Content] -> XMLReader m [Inlines]
getArrayOfInlines filter_element contents = mapM readInlinesSelectedElements contents
  where
    readInlinesSelectedElements :: (PandocMonad m) => Content -> XMLReader m Inlines
    readInlinesSelectedElements content = do
      context <- gets xmlPath
      let parent = headOr "" context
       in case (content) of
            (Elem c) ->
              if filter_element c
                then do
                  mconcat <$> mapM parseInline (elContent c)
                else do
                  report $ UnexpectedXmlElement (elementName c) parent
                  return mempty
            i -> parseInline i

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

parseInline :: (PandocMonad m) => Content -> XMLReader m Inlines
parseInline (Text (CData _ s _)) = return $ text s
parseInline (CRef ref) =
  return $
    maybe (text $ T.toUpper ref) text $
      lookupEntity ref
parseInline (Elem e) =
  let name = elementName e
   in case (name) of
        "Emph" -> innerInlines emph
        "Strong" -> innerInlines strong
        "Strikeout" -> innerInlines strikeout
        "Subscript" -> innerInlines subscript
        "Superscript" -> innerInlines superscript
        "Underline" -> innerInlines underline
        "SoftBreak" -> return softbreak
        "LineBreak" -> return linebreak
        "SmallCaps" -> innerInlines smallcaps
        "Quoted" -> case (attrValue attrQuoteType e) of
          "SingleQuoted" -> innerInlines singleQuoted
          _ -> innerInlines doubleQuoted
        "Math" -> case (attrValue attrMathType e) of
          "DisplayMath" -> do return $ displayMath $ strContentRecursive e
          _ -> do return $ math $ strContentRecursive e
        "Span" -> innerInlines $ spanWith (attrFromElement e)
        "Code" -> do
          let attr = attrFromElement e
          return $ codeWith attr $ strContentRecursive e
        "Link" -> innerInlines $ linkWith attr url title
          where
            url = attrValue attrLinkUrl e
            title = attrValue attrTitle e
            attr = filterAttrAttributes [attrLinkUrl, attrTitle] $ attrFromElement e
        "Image" -> innerInlines $ imageWith attr url title
          where
            url = attrValue attrImageUrl e
            title = attrValue attrTitle e
            attr = filterAttrAttributes [attrImageUrl, attrTitle] $ attrFromElement e
        "RawInline" -> do
          let format = (attrValue attrFormat e)
          return $ rawInline format $ strContentRecursive e
        "Note" -> do
          contents <- getBlocks e
          return $ note contents
        "Cite" -> do
          (citations_contents, contents) <- partitionCitationsAndInlines $ elContent e
          citations <- parseCitations citations_contents
          (innerInlines' contents) $ cite citations
        _ -> do
          report $ UnexpectedXmlElement name ""
          return mempty
  where
    innerInlines' contents f =
      extractSpaces f . mconcat
        <$> mapM parseInline contents
    innerInlines f = innerInlines' (elContent e) f

getListAttributes :: Element -> ListAttributes
getListAttributes e = (start, style, delim)
  where
    start = textToInt (attrValue attrStart e) 1
    style = case (attrValue attrNumberStyle e) of
      "Example" -> Example
      "Decimal" -> Decimal
      "LowerRoman" -> LowerRoman
      "UpperRoman" -> UpperRoman
      "LowerAlpha" -> LowerAlpha
      "UpperAlpha" -> UpperAlpha
      _ -> DefaultStyle
    delim = case (attrValue attrNumberDelim e) of
      "Period" -> Period
      "OneParen" -> OneParen
      "TwoParens" -> TwoParens
      _ -> DefaultDelim

partitionCitationsAndInlines :: (PandocMonad m) => [Content] -> XMLReader m ([Content], [Content])
partitionCitationsAndInlines contents =
  let (citations_el, inlines) = partition isCitations contents
      citations_children = contentsOfChildren tagCitations citations_el
   in case (length citations_children) of
        1 -> do return (concat citations_children, inlines)
        0 -> do
          report $ UnexpectedXmlElement ("missing <" <> tagCitations <> "> element") ""
          return (mempty, inlines)
        n -> do
          report $ UnexpectedXmlElement ("too many (" <> T.pack (show n) <> ") <" <> tagCitations <> "> elements") ""
          return (concat citations_children, inlines)
  where
    isCitations content = case (content) of
      Elem e -> elementName e == tagCitations
      _ -> False

contentsOfChildren :: Text -> [Content] -> [[Content]]
contentsOfChildren tag contents = mapMaybe childrenElementWithTag contents
  where
    childrenElementWithTag :: Content -> Maybe [Content]
    childrenElementWithTag c = case (c) of
      (Elem e) -> if tag == elementName e then Just (elContent e) else Nothing
      _ -> Nothing

parseCitations :: (PandocMonad m) => [Content] -> XMLReader m [Citation]
parseCitations contents = do
  maybecitations <- mapM getCitation contents
  return $ catMaybes maybecitations
  where
    getCitation :: (PandocMonad m) => Content -> XMLReader m (Maybe Citation)
    getCitation content = case (content) of
      (Elem e) ->
        if qName (elName e) == "Citation"
          then do
            p <- mconcat <$> prefix e
            s <- mconcat <$> suffix e
            return $
              Just
                ( Citation
                    { citationId = attrValue "id" e,
                      citationPrefix = toList p,
                      citationSuffix = toList s,
                      citationMode = case (attrValue attrCitationMode e) of
                        "AuthorInText" -> AuthorInText
                        "SuppressAuthor" -> SuppressAuthor
                        _ -> NormalCitation,
                      citationNoteNum = textToInt (attrValue attrCitationNoteNum e) 0,
                      citationHash = textToInt (attrValue attrCitationHash e) 0
                    }
                )
          else do
            return Nothing
      _ -> do
        return Nothing
      where
        prefix e = getArrayOfInlines (\c -> tagCitationPrefix == elementName c) $ elContent e
        suffix e = getArrayOfInlines (\c -> tagCitationSuffix == elementName c) $ elContent e

-- parseCaption :: (PandocMonad m) => [Content] -> XMLReader m Caption
-- parseCaption contents = let (caption_els, contents)

isListItem :: Element -> Bool
isListItem e = qName (elName e) == tagListItem

isLineItem :: Element -> Bool
isLineItem e = qName (elName e) == tagLineItem

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
      parent = headOr "" path
      grandparent = tailHeadOr "" path
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

tailHeadOr :: a -> [a] -> a
tailHeadOr default_value [] = default_value
tailHeadOr default_value [_] = default_value
tailHeadOr default_value (_ : xs) = headOr default_value xs

parseContents :: (PandocMonad m) => Content -> XMLReader m Blocks
parseContents content = do
  path <- gets xmlPath
  let expected = expectedContent path
   in case (content) of
        (Text (CData CDataRaw _ _)) -> return mempty -- DOCTYPE
        (Text (CData _ s _)) -> do
          if T.all isSpace s
            then return mempty
            else do
              report $ UnexpectedXmlCData s ""
              return mempty
        (CRef x) -> do
          report $ UnexpectedXmlReference x ""
          return mempty
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
                _ -> do
                  report $ UnexpectedXmlElement name ""
                  return mempty
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
              ils' <- trimInlines . mconcat <$> mapM parseInline ils
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
