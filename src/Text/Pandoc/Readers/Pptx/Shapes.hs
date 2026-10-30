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
  , shapeToBlocks
  , isTitlePlaceholder
  , extractDrawingMLText
  ) where

import Codec.Archive.Zip (Archive, findEntryByPath, fromEntry)
import qualified Data.ByteString.Lazy as B
import Data.List (find, groupBy)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import Text.Read (readMaybe)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import qualified Text.Pandoc.Class.PandocMonad as P
import Text.Pandoc.Definition
import Text.Pandoc.Readers.OOXML.Shared
import Text.Pandoc.Readers.Pptx.SmartArt
import Text.Pandoc.Walk (walk)
import Text.Pandoc.XML.Light
import Text.TeXMath (writeTeX)
import Text.TeXMath.Readers.OMML (readOMML)

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
shapeToBlocks _archive rels (PptxTextBox paras) =
  return $ resolveLinks rels $ paragraphsToBlocks paras
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

-- | Inlines of a paragraph: the text of its runs and fields with their
-- bold, italic, superscript and subscript, its line breaks, its
-- hyperlinks, and its equations (@a14:m@) as math. A hyperlink's target
-- is a relationship id at this point, which 'resolveLinks' replaces.
paragraphInlines :: Element -> [Inline]
paragraphInlines pElem =
  B.toList $ foldMap linked $ groupBy (\a b -> fst a == fst b) $
    concatMap piece (elChildren pElem)
  where
    piece el = case qName (elName el) of
      "r"   -> [(runLink el, Left (runFormat el, runText el))]
      "fld" -> [(runLink el, Left (runFormat el, runText el))]
      "br"  -> [(Nothing, Right B.linebreak)]
      "m"   -> [(Nothing, Right (foldMap mathInlines (elChildren el)))]
      _     -> []
    linked grp =
      let inls = foldMap formatted $ groupBy sameFormat (map snd grp)
       in case grp of
            (Just relId, _) : _ ->
              B.linkWith ("", [], [(linkRelAttr, relId)]) "" "" inls
            _ -> inls
    sameFormat (Left (f, _)) (Left (g, _)) = f == g
    sameFormat _ _ = False
    -- Runs with the same format are joined before it is applied, so
    -- that adjacent bold runs make one strong span.
    formatted pieces@(Left (fmt, _) : _) =
      applyFormat fmt (T.concat [t | Left (_, t) <- pieces])
    formatted pieces = mconcat [ils | Right ils <- pieces]

-- | The format of a run: bold, italic, and its baseline shift (positive
-- for superscript, negative for subscript)
type RunFormat = (Bool, Bool, Int)

runFormat :: Element -> RunFormat
runFormat run =
  case find ((== "rPr") . qName . elName) (elChildren run) of
    Nothing -> (False, False, 0)
    Just rPr ->
      ( flag "b" rPr, flag "i" rPr
      , fromMaybe 0 $
          findAttr (unqual "baseline") rPr >>= readMaybe . T.unpack )
  where
    flag name el = findAttr (unqual name) el `elem` [Just "1", Just "true"]

runText :: Element -> Text
runText = T.concat . map strContent .
          filter ((== "t") . qName . elName) . elChildren

-- | Text with a run format. Spaces at the edges stay outside the
-- formatting, as Markdown emphasis can't start or end with a space.
applyFormat :: RunFormat -> Text -> B.Inlines
applyFormat (bold, italic, baseline) txt
  | not (bold || italic || baseline /= 0) || T.null core = B.text txt
  | otherwise = edge leading <> wrap (B.text core) <> edge trailing
  where
    isBlank = (`elem` (" \t\r\n" :: String))
    core = T.dropAround isBlank txt
    leading = T.takeWhile isBlank txt
    trailing = T.takeWhileEnd isBlank txt
    edge t = if T.null t then mempty else B.space
    wrap = (if bold then B.strong else id) .
           (if italic then B.emph else id) .
           (if baseline > 0 then B.superscript
              else if baseline < 0 then B.subscript else id)

-- | The relationship id of a run's hyperlink (@a:hlinkClick@). Links
-- that act within the presentation, such as a jump to another slide,
-- carry an @action@ and are not kept.
runLink :: Element -> Maybe Text
runLink run = do
  rPr <- find ((== "rPr") . qName . elName) (elChildren run)
  link <- find ((== "hlinkClick") . qName . elName) (elChildren rPr)
  case findAttr (unqual "action") link of
    Just _ -> Nothing
    Nothing -> fmap attrVal $ find isRelId (elAttribs link)
  where
    isRelId a = qName (attrKey a) == "id" && qURI (attrKey a) == Just relNs
    relNs =
      "http://schemas.openxmlformats.org/officeDocument/2006/relationships"

-- | The attribute that marks a hyperlink whose target is still a
-- relationship id
linkRelAttr :: Text
linkRelAttr = "pptx-relationship"

-- | Replace the relationship id of each hyperlink read by
-- 'paragraphInlines' with the relationship's target. A link whose
-- relationship is missing is replaced by its text.
resolveLinks :: [(Text, Text)] -> [Block] -> [Block]
resolveLinks rels = walk (concatMap resolve)
  where
    resolve (Link (_, _, [(key, relId)]) inls _)
      | key == linkRelAttr =
          case lookup relId rels of
            Just target -> [Link nullAttr inls (target, "")]
            Nothing -> inls
    resolve x = [x]

-- | An equation (@m:oMathPara@ or @m:oMath@) as math. If texmath can't
-- read it, its text is kept.
mathInlines :: Element -> B.Inlines
mathInlines el =
  case readOMML (showElement el) of
    Right exps
      | qName (elName el) == "oMathPara" -> B.displayMath (writeTeX exps)
      | otherwise -> B.math (writeTeX exps)
    Left _ -> B.text $ T.concat $ map strContent $
                filterElementsName ((== "t") . qName) el

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
