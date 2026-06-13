{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.DocLang
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of DocLang XML to 'Pandoc' document.
-}
module Text.Pandoc.Readers.DocLang
  ( readDocLang
  ) where

import Control.Monad (void)
import Data.Char (isSpace)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Builder
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Error (PandocError (..))
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.XML.Light
import Text.Pandoc.XML (escapeStringForXML)

-- | Read DocLang XML and return a Pandoc document.
readDocLang :: PandocMonad m => ReaderOptions -> Text -> m Pandoc
readDocLang _opts inp = do
  let tree = parseXMLContents inp
  case tree of
    Left e  -> throwError $ PandocParseError
                 ("Could not parse DocLang XML: " <> T.pack (show e))
    Right [Elem doclang] -> parseDocLang doclang
    Right _ -> throwError $ PandocParseError
                 "DocLang document must have a single <doclang> root element"

-- | Parse the <doclang> root element.
parseDocLang :: PandocMonad m => Element -> m Pandoc
parseDocLang doclang = do
  let allContent = elContent doclang
  let (headEls, bodyEls) = partitionHead allContent
  meta <- case headEls of
           [Elem h] -> parseHead h
           _        -> return mempty
  blocks <- concat <$> mapM parseTopLevel bodyEls
  return $ Pandoc meta (toList blocks)

-- | Separate optional <head> from body content.
partitionHead :: [Content] -> (Maybe Element, [Content])
partitionHead (Elem e : rest)
  | qName (elName e) == "head" = (Just e, rest)
partitionHead cs = (Nothing, cs)

-- | Parse metadata <head> element.
parseHead :: PandocMonad m => Element -> m Meta
parseHead headEl = do
  let childs = onlyElems $ elContent headEl
      metaMap = foldr addMetaField mempty childs
  return $ Meta metaMap
  where
    addMetaField :: Element -> [(Text, MetaValue)] -> [(Text, MetaValue)]
    addMetaField e acc =
      let name = qName (elName e)
          textContent = T.strip $ strContent e
      in case name of
           "title" -> ("title", MetaInlines [Str textContent]) : acc
           "author" -> let existing = lookup "author" acc
                       in case existing of
                            Just (MetaList items) ->
                              ("author", MetaList (items ++ [MetaInlines [Str textContent]])) :
                              filter (\(k,_) -> k /= "author") acc
                            _ -> ("author", MetaList [MetaInlines [Str textContent]]) : acc
           "date" -> ("date", MetaInlines [Str textContent]) : acc
           "language" -> ("lang", MetaString textContent) : acc
           _ -> (name, MetaString textContent) : acc

-- | Parse top-level elements (children of <doclang> or inside <text>).
parseTopLevel :: PandocMonad m => Content -> m Blocks
parseTopLevel (Elem e) = parseTopLevelElem e
parseTopLevel _ = return mempty

parseTopLevelElem :: PandocMonad m => Element -> m Blocks
parseTopLevelElem e = case qName (elName e) of
  "text" -> do
    content <- parseContent $ elContent e
    return $ para content
  "heading" -> do
    let lvl = maybe 1 (read . T.unpack) $ attrVal "level" e
    content <- parseContent $ elContent e
    return $ header lvl (fromList content)
  "code" -> do
    let lang = maybe "" strContent $ findChild (byName "label") e
    let codeContent = getCodeContent e
    return $ codeBlockWith (nullAttr, [lang | not (T.null lang)], []) codeContent
  "formula" -> do
    let tex = T.strip $ strContent e
    return $ para (math tex)
  "picture" -> do
    let srcUri = maybe "" (attrVal "uri") $ findChild (byName "src") e
    if T.null srcUri
      then return $ para (text "[image]")
      else return $ para (image srcUri "" (text srcUri))
  "list" -> do
    let ordered = attrVal "class" e == Just "ordered"
    items <- getListItems e
    if ordered
      then return $ orderedList items
      else return $ bulletList items
  "table" -> parseTable e
  "footnote" -> do
    content <- parseContent $ elContent e
    return $ para (note $ plain content)
  "page_break" -> return mempty
  _ -> return mempty

-- | Get attribute value from an element.
attrVal :: Text -> Element -> Maybe Text
attrVal name e = lookupAttr (byNameAttr name) (elAttribs e)

byNameAttr :: Text -> Attr -> Bool
byNameAttr name attr = qName (attrKey attr) == name

-- | Parse content of a semantic element: text, formatting, and nested blocks.
parseContent :: PandocMonad m => [Content] -> m [Inline]
parseContent = fmap concat . mapM parseInlineContent

parseInlineContent :: PandocMonad m => Content -> m [Inline]
parseInlineContent (Text (CData _ s _)) =
  if T.all isSpace s then return [] else return [Str s]
parseInlineContent (Elem e) = parseInlineElement e
parseInlineContent _ = return []

parseInlineElement :: PandocMonad m => Element -> m [Inline]
parseInlineElement e = case qName (elName e) of
  "bold"          -> wrapInlines Strong e
  "italic"        -> wrapInlines Emph e
  "underline"     -> wrapInlines Underline e
  "strikethrough" -> wrapInlines Strikeout e
  "superscript"   -> wrapInlines Superscript e
  "subscript"     -> wrapInlines Subscript e
  "code"          -> return [Code nullAttr $ strContent e]
  "formula"       -> return [Math InlineMath $ T.strip $ strContent e]
  "content"       -> return [Str $ strContent e]
  "footnote"      -> parseFootnote e
  "href"          -> return []  -- element head property, handled at container level
  "label"         -> return []
  "location"      -> return []
  "caption"       -> return []
  _               -> return []

-- | Wrap element children in an inline constructor.
wrapInlines :: ([Inline] -> Inline) -> Element -> m [Inline]
wrapInlines ctor e = do
  children <- parseContent $ elContent e
  return [ctor children]

-- | Parse footnote inline element.
parseFootnote :: PandocMonad m => Element -> m [Inline]
parseFootnote e = do
  content <- parseContent $ elContent e
  return [Note [Plain content]]

-- | Get code content, preferring <content> child with whitespace preservation.
getCodeContent :: Element -> Text
getCodeContent e =
  case findChild (byName "content") e of
    Just c  -> strContent c
    Nothing -> strContent e

-- | Parse list items from a <list> element.
getListItems :: PandocMonad m => Element -> m [[Block]]
getListItems e = do
  let items = splitOnLdiv $ elContent e
  mapM parseListItem items

-- | Split list content into items at each <ldiv/> boundary.
splitOnLdiv :: [Content] -> [[Content]]
splitOnLdiv [] = []
splitOnLdiv cs =
  let (item, rest) = break isLdiv cs
  in case rest of
       (_:after) -> item : splitOnLdiv after
       []        -> if null item then [] else [item]

isLdiv :: Content -> Bool
isLdiv (Elem e) = qName (elName e) == "ldiv"
isLdiv _ = False

-- | Parse a single list item's content.
parseListItem :: PandocMonad m => [Content] -> m [Block]
parseListItem [] = return [Plain []]
parseListItem cs = do
  let blocks = mapMaybe elemToBlock cs
  if null blocks
    then do
      inlines <- parseContent cs
      return [Plain inlines]
    else return blocks

elemToBlock :: Content -> Maybe Block
elemToBlock (Elem e) = case qName (elName e) of
  "text"    -> Just $ Plain []  -- placeholder, handled by parseContent
  "list"    -> Nothing  -- nested lists handled separately
  "table"   -> Nothing
  _         -> Nothing
elemToBlock _ = Nothing

-- | Parse an OTSL table.
parseTable :: PandocMonad m => Element -> m Blocks
parseTable e = do
  let cells = collectTableCells $ elContent e
  let rows = splitRows cells
  let (headerRow, dataRows) = case rows of
        (r:rs) -> (r, rs)
        []     -> ([], [])
  let isHeader (OTSLHeader _) = True
      isHeader _ = False
  let hasHeaders = any isHeader headerRow
  let colCount = case rows of
        (r:_) -> length r
        _     -> 1
  let colspecs = replicate colCount (AlignDefault, ColWidthDefault)
  let alignments = replicate colCount AlignDefault
  let widths = replicate colCount ColWidthDefault
  let headerBlks = if hasHeaders
                   then [headerRowToBlocks headerRow]
                   else []
  let bodyBlks = map dataRowToBlocks dataRows
  let caption = emptyCaption
  return $ simpleTable (map simpleCell headerBlks) (map (map simpleCell) bodyBlks)
  where
    simpleCell blks = (AlignDefault, ColWidthDefault, blks)

data OTSLCell = OTSLHeader [Block]
              | OTSLData [Block]
              | OTSLEmpty

data OTSLRow = OTSLRow [OTSLCell]

-- | Collect table cells from flat token sequence.
collectTableCells :: [Content] -> [OTSLCell]
collectTableCells [] = []
collectTableCells (Elem e : rest) = case qName (elName e) of
  "ched" -> OTSLHeader (parseCellContent rest) : collectTableCells (dropCellContent rest)
  "fcel" -> OTSLData (parseCellContent rest) : collectTableCells (dropCellContent rest)
  "ecel" -> OTSLEmpty : collectTableCells rest
  "srow" -> OTSLEmpty : collectTableCells rest
  "nl"   -> collectTableCells rest
  _      -> collectTableCells rest
collectTableCells (_ : rest) = collectTableCells rest

-- | Parse cell content up to the next OTSL token or element head element.
parseCellContent :: [Content] -> [Block]
parseCellContent [] = [Plain []]
parseCellContent (Elem e : _)
  | qName (elName e) `elem` ["fcel","ched","ecel","srow","nl",
                              "label","thread","href","xref","layer",
                              "location","caption","custom"] = [Plain []]
parseCellContent cs = 
  let (texts, _) = span isCellContent cs
      str = T.concat [s | Text (CData _ s _) <- texts]
  in if T.all isSpace str then [Plain []]
     else [Plain [Str $ T.strip str]]

isCellContent :: Content -> Bool
isCellContent (Text _) = True
isCellContent (Elem e) = not $ qName (elName e) `elem`
  ["fcel","ched","ecel","srow","nl"]
isCellContent _ = True

-- | Drop cell content tokens up to the next OTSL token.
dropCellContent :: [Content] -> [Content]
dropCellContent [] = []
dropCellContent (c@(Elem e) : rest)
  | qName (elName e) `elem` ["fcel","ched","ecel","srow","nl"] = c : rest
dropCellContent (_ : rest) = dropCellContent rest

-- | Split cells into rows at <nl/> boundaries.
splitRows :: [OTSCell] -> [[OTSCell]]
splitRows [] = []
splitRows cs = 
  let (row, rest) = break isNewline cs
  in row : splitRows (drop 1 rest)

isNewline :: OTSCell -> Bool
isNewline _ = False  -- handled in collectTableCells

headerRowToBlocks :: [OTSCell] -> [Block]
headerRowToBlocks = concatMap cellToBlocks

dataRowToBlocks :: [OTSCell] -> [Block]
dataRowToBlocks = concatMap cellToBlocks

cellToBlocks :: OTSCell -> [Block]
cellToBlocks (OTSLHeader bs) = bs
cellToBlocks (OTSLData bs)   = bs
cellToBlocks OTSLEmpty       = [Plain []]

-- | Check if an element matches a tag name.
byName :: Text -> Element -> Bool
byName name e = qName (elName e) == name

-- | Extract the text content of an element (all text nodes concatenated).
strContent :: Element -> Text
strContent = T.concat . mapMaybe getText . elContent
  where
    getText (Text (CData _ s _)) = Just s
    getText _ = Nothing
