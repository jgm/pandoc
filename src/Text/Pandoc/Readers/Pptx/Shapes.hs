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
  , parseShapeWith
  , inheritedBullets
  , shapeToBlocks
  , paragraphsToBlocks
  , isTitlePlaceholder
  , extractDrawingMLText
  ) where

import Codec.Archive.Zip (Archive, findEntryByPath, fromEntry)
import qualified Data.ByteString.Lazy as B
import Data.List (find, groupBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Char (isSpace)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text (Text)
import Text.Read (readMaybe)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import qualified Text.Pandoc.Class.PandocMonad as P
import Text.Pandoc.Definition
import Text.Pandoc.Readers.OOXML.Shared
import Text.Pandoc.Readers.OOXML.Charts
import Text.Pandoc.Readers.Pptx.SmartArt
import Text.Pandoc.Shared (collapseFilePath)
import Text.Pandoc.Walk (walk)
import Text.Pandoc.XML.Light
import Text.TeXMath (writeTeX)
import Text.TeXMath.Readers.OMML (readOMML)
import qualified System.FilePath.Posix as Posix

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
  | PptxTable [[TableCell]]             -- Rows of cells
  | PptxDiagramRef
      { dgmDataRelId   :: Text          -- Relationship to data.xml
      , dgmLayoutRelId :: Text          -- Relationship to layout.xml
      }
  | PptxChartRef Text Text              -- Relationship to the chart, URI
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
  -- A slide number placeholder only holds the number PowerPoint shows
  | isSlideNumberPlaceholder ns el = Nothing
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
                  else if uri == chartUri
                    then
                      -- The c prefix is usually declared on c:chart itself
                      case find ((== "chart") . qName . elName)
                                (elChildren graphicData) of
                        Just chartElem
                          | Just relId <- findAttrByName
                              (ns <> elemToNameSpaces chartElem) "r" "id"
                              chartElem
                          -> Just $ PptxChartRef relId uri
                        _ -> Just $ PptxGraphic ("other: " <> uri)
                  else
                    -- Other graphic
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
shapeToBlocks archive rels (PptxChartRef relId uri) =
  -- A chart whose part can't be read keeps the placeholder
  return $ either (const [Para [Str $ "[Graphic: other: " <> uri <> "]"]])
                  chartToBlocks $ do
    target <- maybe (Left $ "Relationship not found: " <> relId) Right $
              lookup relId rels
    readChart archive $ case T.unpack target of
      '/':path -> path
      path -> collapseFilePath ("ppt/slides" Posix.</> path)
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

-- | Check if shape is a slide number placeholder
isSlideNumberPlaceholder :: NameSpaces -> Element -> Bool
isSlideNumberPlaceholder ns el =
  maybe False ((== Just "sldNum") . findAttr (unqual "type")) $
    findChildByName ns "p" "nvSpPr" el >>=
    findChildByName ns "p" "nvPr" >>=
    findChildByName ns "p" "ph"

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

-- | The graphic data URI of a chart
chartUri :: Text
chartUri = "http://schemas.openxmlformats.org/drawingml/2006/chart"
