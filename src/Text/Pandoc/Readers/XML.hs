{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Text.Pandoc.Readers.XML (readXML) where

import Control.Monad (mfilter, msum)
import Control.Monad.Except (throwError)
import Control.Monad.State.Strict (StateT (runStateT), gets, modify)
import Data.Char (isSpace)
import Data.Default (Default (..))
import qualified Data.List as L
import qualified Data.Map as M
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
  (bs, st') <- flip runStateT (def {xmlContent = tree}) $ mapM parseBlock tree
  let blockList = toList $ mconcat bs
  return $ Pandoc (xmlMeta st') blockList

parseBlocks :: (PandocMonad m) => [Content] -> XMLReader m Blocks
parseBlocks contents = mconcat <$> mapM parseBlock contents

getBlocks :: (PandocMonad m) => Element -> XMLReader m Blocks
getBlocks e = parseBlocks (elContent e)

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
   in case (name) of
        "Pandoc" -> parsePandoc
        "?xml" -> return mempty
        "blocks" -> getBlocks e
        "meta" ->
          let entry_els = childrenNamed tgNameMetaMapEntry e
           in do
                entries <- catMaybes <$> mapM parseMetaMapEntry entry_els
                mapM_ (uncurry addMeta) entries
                return mempty
        "Para" -> parseMixed para (elContent e)
        "Plain" -> parseMixed plain (elContent e)
        "Header" -> parseMixed (headerWith attr level) (elContent e)
          where
            level = textToInt (attrValue atNameLevel e) 1
            attr = filterAttrAttributes [atNameLevel] $ attrFromElement e
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
        "DefinitionList" -> do
          let items_contents = getContentsOfElements (isElementNamed tgNameDefListItem) (elContent e)
          items <- mapM parseDefinitionListItem items_contents
          return $ definitionList items
        "Figure" -> do
          let attr = attrFromElement e
              (maybe_caption_el, contents) = partitionFirstChildNamed "Caption" $ elContent e
          figure_caption <- case (maybe_caption_el) of
            Just (caption_el) -> parseCaption $ elContent caption_el
            Nothing -> pure emptyCaption
          blocks <- parseBlocks contents
          return $ figureWith attr figure_caption blocks
        "CodeBlock" -> do
          let attr = attrFromElement e
          return $ codeBlockWith attr $ strContentRecursive e
        "RawBlock" -> do
          let format = (attrValue atNameFormat e)
          return $ rawBlock format $ strContentRecursive e
        "LineBlock" -> do
          lins <- mapM getInlines (contentsOfChildren tgNameLineItem (elContent e))
          return $ lineBlock lins
        "Table" -> do
          -- TODO: check unexpected items
          let attr = attrFromElement e
              (maybe_caption_el, after_caption) = partitionFirstChildNamed "Caption" $ elContent e
              children = elementsWithNames (S.fromList [tgNameColspecs, "TableHead", "TableBody", "TableFoot"]) after_caption
              is_element tag el = tag == elementName el
          colspecs <- getColspecs $ L.find (is_element tgNameColspecs) children
          tbs <- getTableBodies $ filter (is_element "TableBody") children
          th <- getTableHead $ L.find (is_element "TableHead") children
          tf <- getTableFoot $ L.find (is_element "TableFoot") children
          capt <- parseMaybeCaptionElement maybe_caption_el
          case colspecs of
            Nothing -> return mempty
            Just cs -> return $ tableWith attr capt cs th tbs tf
        _ -> do
          report $ UnexpectedXmlElement name ""
          return mempty
  where
    parsePandoc = do
      let version = maybeAttrValue atNameApiVersion e
          apiversion = case (version) of
            Just (v) -> makeVersion $ map (read . T.unpack) $ T.splitOn "," v
            Nothing -> pandocVersion
       in modify $ \st -> st {xmlApiVersion = apiversion}
      getBlocks e

    parseMixed container conts = do
      let (ils, rest) = break isBlockElement conts
      ils' <- getInlines ils
      let p = if ils' == mempty then mempty else container ils'
      case rest of
        [] -> return p
        (r : rs) -> do
          b <- parseBlock r
          x <- parseMixed container rs
          return $ p <> b <> x

getContentsOfElements :: (Content -> Bool) -> [Content] -> [[Content]]
getContentsOfElements filter_element contents = mapMaybe element_contents $ filter filter_element contents
  where
    element_contents :: Content -> Maybe [Content]
    element_contents c = case (c) of
      Elem e -> Just (elContent e)
      _ -> Nothing

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
                  getInlines (elContent c)
                else do
                  -- report $ UnexpectedXmlElement (elementName c) parent
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
        "Quoted" -> case (attrValue atNameQuoteType e) of
          "SingleQuote" -> innerInlines singleQuoted
          _ -> innerInlines doubleQuoted
        "Math" -> case (attrValue atNameMathType e) of
          "DisplayMath" -> pure $ displayMath $ strContentRecursive e
          _ -> pure $ math $ strContentRecursive e
        "Span" -> innerInlines $ spanWith (attrFromElement e)
        "Code" -> do
          let attr = attrFromElement e
          return $ codeWith attr $ strContentRecursive e
        "Link" -> innerInlines $ linkWith attr url title
          where
            url = attrValue atNameLinkUrl e
            title = attrValue atNameTitle e
            attr = filterAttrAttributes [atNameLinkUrl, atNameTitle] $ attrFromElement e
        "Image" -> innerInlines $ imageWith attr url title
          where
            url = attrValue atNameImageUrl e
            title = attrValue atNameTitle e
            attr = filterAttrAttributes [atNameImageUrl, atNameTitle] $ attrFromElement e
        "RawInline" -> do
          let format = (attrValue atNameFormat e)
          return $ rawInline format $ strContentRecursive e
        "Note" -> do
          contents <- getBlocks e
          return $ note contents
        "Cite" ->
          let (maybe_citations_el, contents) = partitionFirstChildNamed tgNameCitations $ elContent e
           in case (maybe_citations_el) of
                Just citations_el -> do
                  citations <- parseCitations $ elContent citations_el
                  (innerInlines' contents) $ cite citations
                Nothing -> getInlines contents
        _ -> do
          report $ UnexpectedXmlElement name ""
          return mempty
  where
    innerInlines' contents f =
      extractSpaces f . mconcat
        <$> mapM parseInline contents
    innerInlines f = innerInlines' (elContent e) f

getInlines :: (PandocMonad m) => [Content] -> XMLReader m Inlines
getInlines contents = mconcat <$> mapM parseInline contents

getListAttributes :: Element -> ListAttributes
getListAttributes e = (start, style, delim)
  where
    start = textToInt (attrValue atNameStart e) 1
    style = case (attrValue atNameNumberStyle e) of
      "Example" -> Example
      "Decimal" -> Decimal
      "LowerRoman" -> LowerRoman
      "UpperRoman" -> UpperRoman
      "LowerAlpha" -> LowerAlpha
      "UpperAlpha" -> UpperAlpha
      _ -> DefaultStyle
    delim = case (attrValue atNameNumberDelim e) of
      "Period" -> Period
      "OneParen" -> OneParen
      "TwoParens" -> TwoParens
      _ -> DefaultDelim

contentsOfChildren :: Text -> [Content] -> [[Content]]
contentsOfChildren tag contents = mapMaybe childrenElementWithTag contents
  where
    childrenElementWithTag :: Content -> Maybe [Content]
    childrenElementWithTag c = case (c) of
      (Elem e) -> if tag == elementName e then Just (elContent e) else Nothing
      _ -> Nothing

alignmentFromText :: Text -> Alignment
alignmentFromText t = case t of
  "AlignLeft" -> AlignLeft
  "AlignRight" -> AlignRight
  "AlignCenter" -> AlignCenter
  _ -> AlignDefault

getColWidth :: Text -> ColWidth
getColWidth txt = case reads (T.unpack txt) of
  [(value, "")] -> if value == 0.0 then ColWidthDefault else ColWidth value
  _ -> ColWidthDefault

getColspecs :: (PandocMonad m) => Maybe Element -> XMLReader m (Maybe [ColSpec])
getColspecs Nothing = pure Nothing
getColspecs (Just cs) = do
  return $ Just $ map elementToColSpec (childrenNamed "ColSpec" cs)
  where
    elementToColSpec e = (alignmentFromText $ attrValue atNameAlignment e, getColWidth $ attrValue atNameColWidth e)

getTableBody :: (PandocMonad m) => Element -> XMLReader m (Maybe TableBody)
getTableBody body_el = do
  let attr =  filterAttrAttributes [atNameRowHeadColumns] $ attrFromElement body_el
      bh = childrenNamed tgNameBodyHeader body_el
      bb = childrenNamed tgNameBodyBody body_el
      headcols = textToInt (attrValue atNameRowHeadColumns body_el) 0
  hrows <- mconcat <$> mapM getRows bh
  brows <- mconcat <$> mapM getRows bb
  return $ Just $ TableBody attr (RowHeadColumns headcols) hrows brows

getTableBodies :: (PandocMonad m) => [Element] -> XMLReader m [TableBody]
getTableBodies body_elements = do
  catMaybes <$> mapM getTableBody body_elements

getTableHead :: (PandocMonad m) => Maybe Element -> XMLReader m TableHead
getTableHead maybe_e = case maybe_e of
  Just e -> do
    let attr = attrFromElement e
    rows <- getRows e
    return $ TableHead attr rows
  Nothing -> return $ TableHead nullAttr []

getTableFoot :: (PandocMonad m) => Maybe Element -> XMLReader m TableFoot
getTableFoot maybe_e = case maybe_e of
  Just e -> do
    let attr = attrFromElement e
    rows <- getRows e
    return $ TableFoot attr rows
  Nothing -> return $ TableFoot nullAttr []

getCell :: (PandocMonad m) => Element -> XMLReader m Cell
getCell c = do
  let alignment = alignmentFromText $ attrValue atNameAlignment c
      rowspan = RowSpan $ textToInt (attrValue atNameRowspan c) 1
      colspan = ColSpan $ textToInt (attrValue atNameColspan c) 1
      attr = filterAttrAttributes [atNameAlignment, atNameRowspan, atNameColspan] $ attrFromElement c
  blocks <- getBlocks c
  return $ Cell attr alignment rowspan colspan (toList blocks)

getRows :: (PandocMonad m) => Element -> XMLReader m [Row]
getRows e = mapM getRow $ childrenNamed "Row" e
  where
    getRow r = do
      cells <- mapM getCell (childrenNamed "Cell" r)
      return $ Row (attrFromElement r) cells

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
                      citationMode = case (attrValue atNameCitationMode e) of
                        "AuthorInText" -> AuthorInText
                        "SuppressAuthor" -> SuppressAuthor
                        _ -> NormalCitation,
                      citationNoteNum = textToInt (attrValue atNameCitationNoteNum e) 0,
                      citationHash = textToInt (attrValue atNameCitationHash e) 0
                    }
                )
          else do
            return Nothing
      _ -> do
        return Nothing
      where
        prefix e = getArrayOfInlines (\c -> tgNameCitationPrefix == elementName c) $ elContent e
        suffix e = getArrayOfInlines (\c -> tgNameCitationSuffix == elementName c) $ elContent e

parseMaybeCaptionElement :: (PandocMonad m) => Maybe Element -> XMLReader m Caption
parseMaybeCaptionElement Nothing = pure emptyCaption
parseMaybeCaptionElement (Just e) = parseCaption $ elContent e

parseCaption :: (PandocMonad m) => [Content] -> XMLReader m Caption
parseCaption contents =
  let (maybe_shortcaption_el, caption_contents) = partitionFirstChildNamed tgNameShortCaption contents
   in do
        blocks <- parseBlocks caption_contents
        case (maybe_shortcaption_el) of
          Just shortcaption_el -> do
            short_caption <- getInlines (elContent shortcaption_el)
            return $ caption (Just $ toList short_caption) blocks
          Nothing -> return $ caption Nothing blocks

parseDefinitionListItem :: (PandocMonad m) => [Content] -> XMLReader m (Inlines, [Blocks])
parseDefinitionListItem contents = do
  let term_contents = getContentsOfElements (isElementNamed tgNameDefListTerm) contents
      defs_elements = elementContents $ filter (isElementNamed tgNameDefListDef) contents
  term_inlines <- getInlines (concat term_contents)
  defs <- mapM getBlocks defs_elements
  return (term_inlines, defs)

elementContents :: [Content] -> [Element]
elementContents contents = mapMaybe toElement contents
  where
    toElement :: Content -> Maybe Element
    toElement (Elem e) = Just e
    toElement _ = Nothing

isElementNamed :: Text -> Content -> Bool
isElementNamed t c = case (c) of
  Elem e -> t == elementName e
  _ -> False

childrenNamed :: Text -> Element -> [Element]
childrenNamed tag e = elementContents $ filter (isElementNamed tag) (elContent e)

elementsWithNames :: S.Set Text -> [Content] -> [Element]
elementsWithNames tags contents = mapMaybe isElementWithNameInSet contents
  where
    isElementWithNameInSet c = case (c) of
      Elem el ->
        if (elementName el) `S.member` tags
          then Just el
          else Nothing
      _ -> Nothing

childrenWithNames :: S.Set Text -> Element -> [Element]
childrenWithNames tags e = elementsWithNames tags $ elContent e

partitionFirstChildNamed :: Text -> [Content] -> (Maybe Element, [Content])
partitionFirstChildNamed tag contents = case (contents) of
  (Text (CData _ s _) : rest) ->
    if T.all isSpace s
      then partitionFirstChildNamed tag rest
      else (Nothing, contents)
  (Elem e : rest) ->
    if tag == elementName e
      then (Just e, rest)
      else (Nothing, contents)
  _ -> (Nothing, contents)

ensureContentMatches :: (PandocMonad m) => (Content -> Maybe LogMessage) -> Element -> XMLReader m ()
ensureContentMatches check e = mapM_ reportUnexpectedContents (elContent e)
  where
    reportUnexpectedContents c = case (check c) of
      Just (message) -> do report message; return ()
      Nothing -> pure ()

ensureContentIsElementsOnly :: (PandocMonad m) => Element -> XMLReader m ()
ensureContentIsElementsOnly e = ensureContentMatches check e
  where
    check :: Content -> Maybe LogMessage
    check c = case (c) of
      Elem _ -> Nothing
      Text (CData CDataRaw s _) -> Just $ UnexpectedXmlCData s ""
      Text (CData _ s _) ->
        if T.all isSpace s
          then Nothing
          else Just $ UnexpectedXmlCData s ""
      CRef x -> Just $ UnexpectedXmlReference x ""

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

headOr :: a -> [a] -> a
headOr default_value [] = default_value
headOr _ (x : _) = x

-- tailHeadOr :: a -> [a] -> a
-- tailHeadOr default_value [] = default_value
-- tailHeadOr default_value [_] = default_value
-- tailHeadOr default_value (_ : xs) = headOr default_value xs

isListItem :: Element -> Bool
isListItem e = tgNameListItem == elementName e

isLineItem :: Element -> Bool
isLineItem e = tgNameLineItem == elementName e

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

addMeta :: (PandocMonad m) => (ToMetaValue a) => Text -> a -> XMLReader m ()
addMeta field val = modify (setMeta field val)

instance HasMeta XMLReaderState where
  setMeta field v s = s {xmlMeta = setMeta field v (xmlMeta s)}

  deleteMeta field s = s {xmlMeta = deleteMeta field (xmlMeta s)}

-- parseMetaContents :: (PandocMonad m) => [Content] -> XMLReader m (Maybe MetaValue)

parseMetaMapEntry :: (PandocMonad m) => Element -> XMLReader m (Maybe (Text, MetaValue))
parseMetaMapEntry e =
  let key = attrValue atNameMetaMapEntryKey e
   in case (key) of
        "" -> pure Nothing
        k -> do
          maybe_value <- parseMetaMapEntryContents $ elContent e
          case (maybe_value) of
            Nothing -> return Nothing
            Just v -> return $ Just (k, v)

parseMetaMapEntryContents :: (PandocMonad m) => [Content] -> XMLReader m (Maybe MetaValue)
parseMetaMapEntryContents cs = msum <$> mapM parseMeta cs

parseMeta :: (PandocMonad m) => Content -> XMLReader m (Maybe MetaValue)
parseMeta (Text (CData CDataRaw _ _)) = return Nothing
parseMeta (Text (CData _ s _)) =
  if T.all isSpace s
    then return Nothing
    else do
      report $ UnexpectedXmlCData s ""
      return Nothing
parseMeta (CRef x) = do
  report $ UnexpectedXmlReference x ""
  return Nothing
parseMeta (Elem e) = do
  let name = elementName e
   in case (name) of
        "MetaBool" -> case (attrValue atNameMetaBoolValue e) of
          "true" -> return $ Just $ MetaBool True
          _ -> return $ Just $ MetaBool False
        "MetaString" -> pure Nothing
        "MetaInlines" -> do
          inlines <- getInlines (elContent e)
          return $ Just $ MetaInlines $ toList inlines
        "MetaBlocks" -> do
          blocks <- getBlocks e
          return $ Just $ MetaBlocks $ toList blocks
        "MetaList" -> do
          maybe_items <- mapM parseMeta $ elContent e
          let items = catMaybes maybe_items
           in if null items
                then
                  -- TODO: report empty MetaList
                  return Nothing
                else return $ Just $ MetaList items
        "MetaMap" ->
          let entry_els = childrenNamed tgNameMetaMapEntry e
           in do
                entries <- catMaybes <$> mapM parseMetaMapEntry entry_els
                if null entries
                  then
                    -- TODO: report empty MetaMap
                    return Nothing
                  else return $ Just $ MetaMap $ M.fromList entries
        _ -> do
          report $ UnexpectedXmlElement name ""
          return Nothing

-- -- onlyOneChild x = length (allChildren x) == 1
-- -- allChildren x = filterChildren (const True) x

-- isBlockTag :: Text -> Bool
-- isBlockTag tag = tag `S.member` blocktags
--   where
--     blocktags = S.fromList $ paragraphLevel ++ lists ++ other
--     paragraphLevel = ["Para", "Plain", "Header"]
--     lists = ["BulletList", "DefinitionList", "OrderedList"]
--     other =
--       [ "LineBlock",
--         "CodeBlock",
--         "RawBlock",
--         "BlockQuote",
--         "HorizontalRule",
--         "Table",
--         "Figure",
--         "Div"
--       ]

-- -- True for elements that can contain only elements or spaces, not text or references
-- isOnlyElementsTag :: Text -> Bool
-- isOnlyElementsTag tag =
--   tag
--     `S.member` S.fromList
--       [ "meta",
--         "blocks",
--         "BulletList",
--         "DefinitionList",
--         "OrderedList",
--         "LineBlock",
--         "BlockQuote",
--         "Table",
--         "Figure",
--         "Div",
--         "Note",
--         "Caption", -- container of an optional ShortCaption and the blocks of a (Figure or Table) Caption
--         "TableHead",
--         "TableBody",
--         "TableFoot",
--         "Row",
--         "Cell",
--         "item", -- container of list items
--         "def", -- container of the definitions of an item in a DefinitionList
--         "header", -- container of the header rows of a TableBody
--         "body" -- container of the body rows of a TableBody
--       ]

-- isMetaValueTag :: Text -> Bool
-- isMetaValueTag tag =
--   tag
--     `S.member` S.fromList
--       [ "MetaBool",
--         "MetaString",
--         "MetaInlines",
--         "MetaBlocks",
--         "MetaList",
--         "MetaMap"
--       ]

-- isListItemTag :: Text -> Bool
-- isListItemTag tag = tag == "item"

-- isTableSectionTag :: Text -> Bool
-- isTableSectionTag tag = tag == "TableBody" || tag == "TableHead" || tag == "TableFoot"

-- isTableBodySectionTag :: Text -> Bool
-- isTableBodySectionTag tag = tag == "header" || tag == "body"

-- isInlineTag :: Text -> Bool
-- isInlineTag tag = tag `S.member` inlinetags
--   where
--     inlinetags =
--       S.fromList
--         [ "Underline",
--           "Strong",
--           "Strikeout",
--           "Superscript",
--           "Subscript",
--           "SmallCaps",
--           "Quoted",
--           "Cite",
--           "Code",
--           "SoftBreak",
--           "LineBreak",
--           "Math",
--           "RawInline",
--           "Link",
--           "Image",
--           "Note",
--           "Span"
--         ]

-- expectedContent :: [Text] -> Content -> Bool
-- expectedContent path (Elem e) =
--   let tag = qName $ elName e
--       parent = headOr "" path
--       grandparent = tailHeadOr "" path
--    in case (parent) of
--         "blocks" -> isBlockTag tag
--         "meta" -> isMetaValueTag tag
--         "Para" -> isInlineTag tag
--         "Plain" -> isInlineTag tag
--         "Header" -> isInlineTag tag
--         "Div" -> isBlockTag tag
--         "BlockQuote" -> isBlockTag tag
--         "BulletList" -> isListItemTag tag
--         "OrderedList" -> isListItemTag tag
--         "DefinitionList" -> isListItemTag tag
--         "item" ->
--           if grandparent /= "DefinitionList"
--             then isBlockTag tag
--             else tag == "term" || tag == "def"
--         "LineBlock" -> tag == "line"
--         "Table" -> tag == "Caption" || tag == "colspecs" || isTableBodySectionTag tag
--         "Figure" -> tag == "Caption" || isBlockTag "tag"
--         "Caption" -> tag == "ShortCaption" || isBlockTag "tag"
--         "TableBody" -> tag == "header" || tag == "body"
--         "header" -> tag == "Row"
--         "body" -> tag == "Row"
--         "TableHead" -> tag == "Row"
--         "TableFoot" -> tag == "Row"
--         "Row" -> tag == "Cell"
--         _ -> False
-- expectedContent path (Text (CData _ s _)) = non_space_text_allowed || T.all isSpace s
--   where
--     parent = if length path > 1 then head path else ""
--     non_space_text_allowed = not (isOnlyElementsTag parent)
-- expectedContent _ _ = True

-- parseContents :: (PandocMonad m) => Content -> XMLReader m Blocks
-- parseContents content = do
--   path <- gets xmlPath
--   let expected = expectedContent path
--    in case (content) of
--         (Text (CData CDataRaw _ _)) -> return mempty -- DOCTYPE
--         (Text (CData _ s _)) -> do
--           if T.all isSpace s
--             then return mempty
--             else do
--               report $ UnexpectedXmlCData s ""
--               return mempty
--         (CRef x) -> do
--           report $ UnexpectedXmlReference x ""
--           return mempty
--         (Elem e) -> do
--           let name = qName $ elName e
--            in -- modify $ \st -> st {xmlContextElement = name, xmlContextParent = parent}
--               case (name) of
--                 "Pandoc" -> parsePandoc
--                 "?xml" -> return mempty
--                 "blocks" -> getBlocks e
--                 "Para" -> parseMixed name para (elContent e)
--                 "Plain" -> parseMixed name plain (elContent e)
--                 "HorizontalRule" -> return horizontalRule
--                 _ -> do
--                   report $ UnexpectedXmlElement name ""
--                   return mempty
--           where
--             parsePandoc = do
--               let version = maybeAttrValue "api-version" e
--                   apiversion = case (version) of
--                     Just (v) -> makeVersion $ map (read . T.unpack) $ T.splitOn "," v
--                     Nothing -> pandocVersion
--                in modify $ \st -> st {xmlApiVersion = apiversion}
--               getBlocks e
--             parseMixed parent' container conts = do
--               let (ils, rest) = break isBlockElement conts
--               ils' <- trimInlines . mconcat <$> mapM parseInline ils
--               let p = if ils' == mempty then mempty else container ils'
--               case rest of
--                 [] -> return p
--                 (r : rs) -> do
--                   b <- parseBlock r
--                   x <- parseMixed parent' container rs
--                   return $ p <> b <> x
