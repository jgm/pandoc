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
  , TableCell(..)
  , BulletType(..)
  , parseShapes
  , parseShape
  , shapeToBlocks
  , isTitlePlaceholder
  , extractDrawingMLText
  ) where

import Codec.Archive.Zip (Archive, findEntryByPath, fromEntry)
import qualified Data.ByteString.Lazy as B
import Data.List (find, groupBy, intersperse)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
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

-- | A table cell (@a:tc@): its text, its row and column span, and whether
-- another cell's span covers it from the left (@hMerge@) or from above
-- (@vMerge@)
data TableCell = TableCell
  { tcText    :: Text
  , tcRowSpan :: Int
  , tcColSpan :: Int
  , tcHMerge  :: Bool
  , tcVMerge  :: Bool
  } deriving (Show)

tcCovered :: TableCell -> Bool
tcCovered c = tcHMerge c || tcVMerge c

-- | Bullet type
data BulletType
  = NoBullet
  | Bullet                        -- Has bullet (character detected or implicit)
  | WingdingsBullet              -- Detected via Wingdings symbol
  deriving (Show, Eq)

-- | Shape types in PPTX slides
data PptxShape
  = PptxTextBox [PptxParagraph]         -- Parsed paragraphs with bullet info
  | PptxPicture
      { picRelId  :: Text               -- Relationship ID (lazy loading)
      , picTitle  :: Text
      , picAlt    :: Text
      }
  | PptxTable [[TableCell]]             -- Rows of cells
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
parseShape ns el
  -- Text box: <p:sp> with <p:txBody>
  | isElem ns "p" "sp" el =
      case findChildByName ns "p" "txBody" el of
        Just txBody ->
          let paras = parseParagraphs ns txBody
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
parseTableRows :: NameSpaces -> Element -> [[TableCell]]
parseTableRows ns tblElem =
  let trElems = findChildrenByName ns "a" "tr" tblElem
   in map (parseTableRow ns) trElems

parseTableRow :: NameSpaces -> Element -> [TableCell]
parseTableRow ns trElem =
  let tcElems = findChildrenByName ns "a" "tc" trElem
   in map parseCell tcElems
  where
    parseCell tcElem = TableCell
      { tcText = case findChildByName ns "a" "txBody" tcElem of
          -- Get text from txBody/a:p/a:r/a:t
          Just txBody -> extractDrawingMLText txBody
          Nothing -> ""
      , tcRowSpan = span' "rowSpan" tcElem
      , tcColSpan = span' "gridSpan" tcElem
      , tcHMerge = flag "hMerge" tcElem
      , tcVMerge = flag "vMerge" tcElem
      }
    span' name el = maybe 1 (max 1) $
      findAttr (unqual name) el >>= readMaybe . T.unpack
    flag name el = findAttr (unqual name) el `elem` [Just "1", Just "true"]

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
  case spannedRows rows of
    [] -> return []
    (headerRow:bodyRows) -> do
      let makeCell (TableCell text rs cs _ _) =
            Cell nullAttr AlignDefault (RowSpan rs) (ColSpan cs)
                 [Plain (B.toList (B.text text))]
          visible = map makeCell . filter (not . tcCovered)
          headerCells = visible headerRow
          bodyCells = map visible bodyRows
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

-- | Make the spans of a table's cells fit pandoc's table model. Each row
-- has a cell for every grid column, with the cells covered by a span
-- marked. As a cell can't span the header row and the body, the header
-- row's spans are kept to that row, and the body cells they covered
-- become empty cells of their own. Row spans are clipped to the rows
-- there are.
spannedRows :: [[TableCell]] -> [[TableCell]]
spannedRows [] = []
spannedRows (header:body) =
  map (\c -> c{ tcRowSpan = 1 }) header : go headerSpanCols (length body) body
  where
    headerSpanCols = S.fromList
      [ col' | (col, c) <- zip [0 :: Int ..] header
             , tcRowSpan c > 1, not (tcCovered c)
             , col' <- [col .. col + tcColSpan c - 1] ]
    -- cols: the columns still covered by a span from the header row;
    -- left: the number of rows from this one to the last
    go _ _ [] = []
    go cols left (row:rows) =
      let cells = zip [0 ..] row
          fix (col, c)
            | col `S.member` cols && tcVMerge c =
                TableCell "" 1 1 False False
            | otherwise = c{ tcRowSpan = min (tcRowSpan c) left }
          stillCovered col = maybe False tcVMerge (lookup col cells)
       in map fix cells : go (S.filter stillCovered cols) (left - 1) rows

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
parseParagraphs :: NameSpaces -> Element -> [PptxParagraph]
parseParagraphs ns txBody =
  let pElems = findChildrenByName ns "a" "p" txBody
   in map (parseParagraph ns) pElems

-- | Parse individual paragraph
parseParagraph :: NameSpaces -> Element -> PptxParagraph
parseParagraph ns pElem =
  let level = parseBulletLevel ns pElem
      bullet = detectBulletType ns pElem
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

-- | Detect bullet type
detectBulletType :: NameSpaces -> Element -> BulletType
detectBulletType ns pElem =
  -- Check for explicit <a:pPr><a:buChar>
  case findChildByName ns "a" "pPr" pElem >>=
       findChildByName ns "a" "buChar" of
    Just _buCharElem -> Bullet
    Nothing ->
      -- Check for Wingdings symbol (common in PowerPoint)
      if hasWingdingsSymbol ns pElem
        then WingdingsBullet
        else NoBullet

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

-- | Convert paragraphs to blocks, grouping bullets into lists
paragraphsToBlocks :: [PptxParagraph] -> [Block]
paragraphsToBlocks paras =
  -- If we have multiple paragraphs with bullets, group them
  let hasBullets = any (\p -> paraBullet p /= NoBullet) paras
   in if hasBullets
      then groupBulletParagraphs paras
      else map (Para . paraText) paras

-- | Group bullet paragraphs into lists
groupBulletParagraphs :: [PptxParagraph] -> [Block]
groupBulletParagraphs paras =
  let grouped = groupBy sameBulletLevel paras
   in concatMap groupToBlock grouped
  where
    sameBulletLevel p1 p2 =
      (paraBullet p1 /= NoBullet) &&
      (paraBullet p2 /= NoBullet) &&
      (paraLevel p1 == paraLevel p2)

    groupToBlock :: [PptxParagraph] -> [Block]
    groupToBlock [] = []
    groupToBlock ps@(p:_)
      | paraBullet p /= NoBullet =
          -- Bullet list
          let items = map (\para -> [Plain (paraText para)]) ps
           in [BulletList items]
      | otherwise =
          -- Plain paragraph
          map (Para . paraText) ps

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
