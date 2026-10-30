{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{- |
   Module      : Text.Pandoc.Readers.Pptx.Shapes
   Copyright   : © 2025 Anton Antic
   License     : GNU GPL, version 2 or above

   Maintainer  : Anton Antic <anton@everworker.ai>
   Stability   : alpha
   Portability : portable

Parsing of PPTX shapes (text boxes, images, tables, diagrams).
-}
module Text.Pandoc.Readers.Pptx.Shapes
  ( PptxShape(..)
  , PptxParagraph(..)
  , BulletType(..)
  , parseShapes
  , parseShape
  , parseShapeWith
  , inheritedBullets
  , shapeToBlocks
  , paragraphsToBlocks
  , isTitlePlaceholder
  , extractDrawingMLText
  ) where

import Codec.Archive.Zip (Archive, findEntryByPath, fromEntry)
import qualified Data.ByteString.Lazy as B
import Data.Char (isSpace)
import Data.List (find, intersperse)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import Text.Read (readMaybe)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import qualified Text.Pandoc.Class.PandocMonad as P
import Text.Pandoc.Definition
import Text.Pandoc.Readers.OOXML.Shared
import Text.Pandoc.Readers.Pptx.SmartArt
import Text.Pandoc.XML.Light

-- | Paragraph with bullet/numbering information
data PptxParagraph = PptxParagraph
  { paraLevel   :: Int            -- Bullet level (0, 1, 2...)
  , paraBullet  :: BulletType
  , paraText    :: [Inline]
  } deriving (Show)

-- | Bullet type
data BulletType
  = NoBullet
  | Bullet                        -- Has bullet (character detected or implicit)
  | WingdingsBullet              -- Detected via Wingdings symbol
  | Numbered ListAttributes      -- Automatic numbering (a:buAutoNum)
  deriving (Show, Eq)

-- | Shape types in PPTX slides
data PptxShape
  = PptxTextBox [PptxParagraph]         -- Parsed paragraphs with bullet info
  | PptxPicture
      { picRelId  :: Text               -- Relationship ID (lazy loading)
      , picTitle  :: Text
      , picAlt    :: Text
      }
  | PptxTable [[Text]]                  -- Simple text cells for now
  | PptxDiagramRef
      { dgmDataRelId   :: Text          -- Relationship to data.xml
      , dgmLayoutRelId :: Text          -- Relationship to layout.xml
      }
  | PptxGraphic Text                    -- Placeholder for other graphics
  deriving (Show)

-- | Parse all shapes from shape tree
parseShapes :: NameSpaces -> Element -> [PptxShape]
parseShapes ns spTreeElem =
  let shapeElems = onlyElems $ elContent spTreeElem
      -- Merge parent namespaces with element namespaces
      ns' = ns <> elemToNameSpaces spTreeElem
   in mapMaybe (parseShape ns') shapeElems

-- | Parse individual shape element
parseShape :: NameSpaces -> Element -> Maybe PptxShape
parseShape = parseShapeWith (const Nothing)

-- | Parse a shape, given the bullet its paragraphs inherit at each level
-- (0 for the first) when they don't set one themselves
parseShapeWith :: (Int -> Maybe BulletType) -> NameSpaces -> Element
               -> Maybe PptxShape
parseShapeWith inherited ns el
  -- Text box: <p:sp> with <p:txBody>
  | isElem ns "p" "sp" el =
      case findChildByName ns "p" "txBody" el of
        Just txBody ->
          let paras = parseParagraphs inherited ns txBody
           in if null paras
              then Nothing
              else Just $ PptxTextBox paras
        Nothing -> Nothing

  -- Picture: <p:pic>
  | isElem ns "p" "pic" el = do
      nvPicPr <- findChildByName ns "p" "nvPicPr" el
      cNvPr <- findChildByName ns "p" "cNvPr" nvPicPr

      let title = maybe "" id $ findAttr (unqual "name") cNvPr
          alt = maybe "" id $ findAttr (unqual "descr") cNvPr

      -- Get blip relationship ID
      blipFill <- findChildByName ns "p" "blipFill" el
      blip <- findChildByName ns "a" "blip" blipFill
      relId <- findAttrByName ns "r" "embed" blip

      return $ PptxPicture relId title alt

  -- GraphicFrame: table or diagram
  | isElem ns "p" "graphicFrame" el =
      case findChildByName ns "a" "graphic" el >>=
           findChildByName ns "a" "graphicData" of
        Nothing -> Nothing
        Just graphicData ->
          case findAttr (unqual "uri") graphicData of
            Nothing -> Just $ PptxGraphic "no-uri"
            Just uri ->
              if "table" `T.isInfixOf` uri
                then
                  -- Table
                  case findChildByName ns "a" "tbl" graphicData of
                    Just tbl ->
                      let rows = parseTableRows ns tbl
                       in Just $ PptxTable rows
                    Nothing -> Nothing
                else if "diagram" `T.isInfixOf` uri
                  then
                    -- SmartArt diagram - dgm namespace is declared inline on relIds element
                    let dgmRelIds = find (\e -> qName (elName e) == "relIds") (elChildren graphicData)
                     in case dgmRelIds of
                          Nothing -> Just $ PptxGraphic "diagram-no-relIds"
                          Just relIdsElem ->
                            -- Get r:dm and r:lo attributes (r namespace is in parent)
                            let ns' = ns <> elemToNameSpaces relIdsElem
                             in case (findAttrByName ns' "r" "dm" relIdsElem,
                                      findAttrByName ns' "r" "lo" relIdsElem) of
                                  (Just dataRelId, Just layoutRelId) ->
                                    Just $ PptxDiagramRef dataRelId layoutRelId
                                  _ -> Just $ PptxGraphic "diagram-missing-rels"
                  else
                    -- Other graphic (chart, etc.)
                    Just $ PptxGraphic ("other: " <> uri)

  -- Skip other shapes for now
  | otherwise = Nothing

-- | Parse table rows (simple text extraction)
parseTableRows :: NameSpaces -> Element -> [[Text]]
parseTableRows ns tblElem =
  let trElems = findChildrenByName ns "a" "tr" tblElem
   in map (parseTableRow ns) trElems

parseTableRow :: NameSpaces -> Element -> [Text]
parseTableRow ns trElem =
  let tcElems = findChildrenByName ns "a" "tc" trElem
   in map extractCellText tcElems
  where
    extractCellText tcElem =
      -- Get text from txBody/a:p/a:r/a:t
      case findChildByName ns "a" "txBody" tcElem of
        Just txBody -> extractDrawingMLText txBody
        Nothing -> ""

-- | Convert shape to Pandoc blocks
shapeToBlocks :: PandocMonad m => Archive -> [(Text, Text)] -> PptxShape -> m [Block]
shapeToBlocks _archive _rels (PptxTextBox paras) =
  return $ paragraphsToBlocks paras
shapeToBlocks archive rels (PptxPicture relId title alt) = do
  -- Resolve relationship to get media path
  case lookup relId rels of
    Nothing -> return []  -- Image not found
    Just target -> do
      let mediaPath = resolveMediaPath target

      -- Load image bytes and add to MediaBag
      case loadMediaFromArchive archive mediaPath of
        Nothing -> return []
        Just mediaBytes -> do
          P.insertMedia (T.unpack mediaPath) Nothing mediaBytes

          let altText = if T.null alt then [] else [Str alt]
          return [Para [Image nullAttr altText (mediaPath, title)]]

shapeToBlocks _archive _rels (PptxTable rows) =
  -- Simple table representation for now
  case rows of
    [] -> return []
    (headerRow:bodyRows) -> do
      let makeCell text = Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1)
                            [Plain (B.toList (B.text text))]
          headerCells = map makeCell headerRow
          bodyCells = map (map makeCell) bodyRows
          caption = Caption Nothing []
          colSpec = replicate (length headerRow) (AlignDefault, ColWidthDefault)
          headerRow' = Row nullAttr headerCells
          bodyRows' = map (Row nullAttr) bodyCells
          thead = TableHead nullAttr [headerRow']
          tbody = [TableBody nullAttr 0 [] bodyRows']
          tfoot = TableFoot nullAttr []
      return [Table nullAttr caption colSpec thead tbody tfoot]

shapeToBlocks archive rels (PptxDiagramRef dataRelId layoutRelId) = do
  -- Parse SmartArt diagram
  case parseDiagram archive rels dataRelId layoutRelId of
    Left err -> do
      -- Failed to parse diagram, return placeholder
      return [Para [Str $ "[Diagram parse error: " <> err <> "]"]]
    Right diagram ->
      return $ diagramToBlocks diagram
shapeToBlocks _archive _rels (PptxGraphic text) =
  -- Placeholder for other graphics (charts, etc.)
  return [Para [Str $ "[Graphic: " <> text <> "]"]]

-- | Resolve media path (handle relative paths)
resolveMediaPath :: Text -> Text
resolveMediaPath target =
  if "../media/" `T.isPrefixOf` target
    then "ppt/media/" <> T.drop 9 target  -- "../media/" = 9 chars
    else if "media/" `T.isPrefixOf` target
      then "ppt/" <> target
      else target

-- | Load media file from archive
loadMediaFromArchive :: Archive -> Text -> Maybe B.ByteString
loadMediaFromArchive archive path =
  case findEntryByPath (T.unpack path) archive of
    Just entry -> Just $ fromEntry entry
    Nothing -> Nothing

-- | Parse paragraphs from text box
parseParagraphs :: (Int -> Maybe BulletType) -> NameSpaces -> Element
                -> [PptxParagraph]
parseParagraphs inherited ns txBody =
  let pElems = findChildrenByName ns "a" "p" txBody
   in map (parseParagraph inherited ns) pElems

-- | Parse individual paragraph
parseParagraph :: (Int -> Maybe BulletType) -> NameSpaces -> Element
               -> PptxParagraph
parseParagraph inherited ns pElem =
  let level = parseBulletLevel ns pElem
      bullet = detectBulletType (inherited level) ns pElem
      text = paragraphInlines pElem
   in PptxParagraph level bullet text

-- | Parse bullet level from paragraph properties
parseBulletLevel :: NameSpaces -> Element -> Int
parseBulletLevel ns pElem =
  case findChildByName ns "a" "pPr" pElem >>=
       findAttr (unqual "lvl") >>=
       (\s -> readMaybe (T.unpack s) :: Maybe Int) of
    Just lvl -> lvl
    Nothing -> 0  -- Default to level 0

-- | Detect bullet type: the paragraph's own, or else the one it inherits
detectBulletType :: Maybe BulletType -> NameSpaces -> Element -> BulletType
detectBulletType inherited ns pElem =
  case findChildByName ns "a" "pPr" pElem >>= explicitBullet of
    Just bullet -> bullet
    Nothing
      -- Check for Wingdings symbol (common in PowerPoint)
      | hasWingdingsSymbol ns pElem -> WingdingsBullet
      | otherwise -> fromMaybe NoBullet inherited

-- | The bullet that paragraph properties (@a:pPr@ or @a:lvlNpPr@) set,
-- if they set one
explicitBullet :: Element -> Maybe BulletType
explicitBullet pPr = listToMaybe (mapMaybe bullet (elChildren pPr))
  where
    bullet el = case qName (elName el) of
      "buChar"    -> Just Bullet
      "buBlip"    -> Just Bullet
      "buNone"    -> Just NoBullet
      "buAutoNum" -> Just (Numbered (numbering el))
      _           -> Nothing
    numbering el =
      let scheme = fromMaybe "arabicPeriod" (findAttr (unqual "type") el)
          start = fromMaybe 1 $
                  findAttr (unqual "startAt") el >>= readMaybe . T.unpack
          style | "arabic" `T.isPrefixOf` scheme = Decimal
                | "alphaLc" `T.isPrefixOf` scheme = LowerAlpha
                | "alphaUc" `T.isPrefixOf` scheme = UpperAlpha
                | "romanLc" `T.isPrefixOf` scheme = LowerRoman
                | "romanUc" `T.isPrefixOf` scheme = UpperRoman
                | otherwise = DefaultStyle
          delim | "ParenBoth" `T.isSuffixOf` scheme = TwoParens
                | "ParenR" `T.isSuffixOf` scheme = OneParen
                | "Period" `T.isSuffixOf` scheme = Period
                | otherwise = DefaultDelim
       in (start, style, delim)

-- | The bullet a shape's paragraphs inherit at each level (0 for the
-- first), from the shape's own list style, then the matching placeholder
-- on the slide layout and on the slide master, then the master's text
-- style for that kind of placeholder (or for other text).
inheritedBullets :: Maybe Element  -- ^ Slide layout
                 -> Maybe Element  -- ^ Slide master
                 -> Element        -- ^ Shape
                 -> Int -> Maybe BulletType
inheritedBullets layout master shape level =
  listToMaybe $ mapMaybe (>>= explicitBullet) $
    [lstLevel shape] ++
    case placeholder shape of
      Just (phType, phIdx) ->
        [ layout >>= findPlaceholder phType phIdx >>= lstLevel
        , master >>= findPlaceholder (masterType phType) Nothing >>= lstLevel
        , textStyle (styleFor phType) ]
      Nothing -> [textStyle "otherStyle"]
  where
    lvlName = "lvl" <> T.pack (show (level + 1)) <> "pPr"
    child name el = find ((== name) . qName . elName) (elChildren el)
    lstLevel sp = child "txBody" sp >>= child "lstStyle" >>= child lvlName
    textStyle name = master >>= child "txStyles" >>= child name >>=
                     child lvlName
    placeholder sp = do
      ph <- child "nvSpPr" sp >>= child "nvPr" >>= child "ph"
      return ( fromMaybe "obj" (findAttr (unqual "type") ph)
             , findAttr (unqual "idx") ph )
    -- A placeholder is matched by index if it has one, else by type
    findPlaceholder phType phIdx root = do
      tree <- child "cSld" root >>= child "spTree"
      let phs = [ (sp, p) | sp <- elChildren tree
                          , Just p <- [placeholder sp] ]
          byIdx = [ sp | Just i <- [phIdx], (sp, (_, j)) <- phs, j == Just i ]
          byType = [ sp | (sp, (t, _)) <- phs, masterType t == phType ||
                                              t == phType ]
      listToMaybe (byIdx ++ byType)
    masterType t = if t `elem` ["obj", "body"] then "body" else t
    styleFor t
      | t `elem` ["title", "ctrTitle"] = "titleStyle"
      | t `elem` ["obj", "body", "subTitle"] = "bodyStyle"
      | otherwise = "otherStyle"

-- | Check if paragraph starts with Wingdings symbol
hasWingdingsSymbol :: NameSpaces -> Element -> Bool
hasWingdingsSymbol ns pElem =
  let runs = findChildrenByName ns "a" "r" pElem
      checkRun r = case findChildByName ns "a" "rPr" r >>=
                        findChildByName ns "a" "sym" of
                     Just symElem ->
                       case findAttr (unqual "typeface") symElem of
                         Just typeface -> "Wingdings" `T.isInfixOf` typeface
                         Nothing -> False
                     Nothing -> False
   in any checkRun runs

-- | Inlines of a paragraph, with its line breaks
paragraphInlines :: Element -> [Inline]
paragraphInlines = B.toList . mconcat . intersperse B.linebreak .
                   map B.text . drawingMLParagraphLines

-- | Extract text from DrawingML element (all its paragraphs)
extractDrawingMLText :: Element -> Text
extractDrawingMLText = drawingMLText

-- | Convert paragraphs to blocks, grouping bulleted and numbered
-- paragraphs into lists. Paragraphs at a deeper level than a list item
-- go inside that item, as a nested list or as further paragraphs.
paragraphsToBlocks :: [PptxParagraph] -> [Block]
paragraphsToBlocks [] = []
paragraphsToBlocks (p:ps)
  | paraBullet p == NoBullet = Para (paraText p) : paragraphsToBlocks ps
  -- PowerPoint shows no bullet or number on an empty paragraph
  | isEmpty p = paragraphsToBlocks ps
  | otherwise =
      let (items, rest) = listItems p (p:ps)
       in listBlock (paraBullet p) items : paragraphsToBlocks rest
  where
    -- The items of a list that starts with the paragraph first, with the
    -- deeper paragraphs that follow each item
    listItems first (q:qs)
      | isEmpty q, paraBullet q /= NoBullet = listItems first qs
      | paraLevel q == paraLevel first, sameKind q first =
          let (inner, rest) = span ((> paraLevel first) . paraLevel) qs
              (items, rest') = listItems first rest
           in ((Plain (paraText q) : paragraphsToBlocks inner) : items, rest')
    listItems _ qs = ([], qs)
    sameKind a b = case (paraBullet a, paraBullet b) of
      (Numbered _, Numbered _) -> True
      (Numbered _, _) -> False
      (_, Numbered _) -> False
      (x, y) -> (x == NoBullet) == (y == NoBullet)
    listBlock (Numbered attrs) items = OrderedList attrs items
    listBlock _ items = BulletList items
    isEmpty = all blank . paraText
    blank il = case il of
      Space -> True
      SoftBreak -> True
      LineBreak -> True
      Str t -> T.all isSpace t
      _ -> False

-- | Check if shape is title placeholder (also used in Slides module)
isTitlePlaceholder :: NameSpaces -> Element -> Bool
isTitlePlaceholder ns el =
  case findChildByName ns "p" "nvSpPr" el >>=
       findChildByName ns "p" "nvPr" >>=
       findChildByName ns "p" "ph" of
    Just phElem ->
      case findAttr (unqual "type") phElem of
        Just phType -> phType == "title" || phType == "ctrTitle"
        Nothing -> False
    Nothing -> False
