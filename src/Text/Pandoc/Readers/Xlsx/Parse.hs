{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Xlsx.Parse
   Copyright   : © 2025 Anton Antic
   License     : GNU GPL, version 2 or above

   Maintainer  : Anton Antic <anton@everworker.ai>
   Stability   : alpha
   Portability : portable

Parsing of XLSX archive to intermediate representation.
-}
module Text.Pandoc.Readers.Xlsx.Parse
  ( Xlsx(..)
  , XlsxWorkbook(..)
  , XlsxSheet(..)
  , SheetId(..)
  , SharedStrings
  , Styles(..)
  , FontInfo(..)
  , archiveToXlsx
  ) where

import Codec.Archive.Zip (Archive, Entry, findEntryByPath, fromEntry)
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as TL
import Data.Text (Text)
import qualified Data.Vector as V
import System.FilePath (splitFileName)
import Text.Pandoc.Readers.OOXML.Shared
import Text.Pandoc.Readers.Xlsx.Cells
import Text.Pandoc.XML.Light
import Text.Read (readMaybe)

-- | Sheet identifier
newtype SheetId = SheetId Int deriving (Show, Eq, Ord)

-- | Shared strings table (Vector for O(1) lookup)
type SharedStrings = V.Vector Text

-- | Font information
data FontInfo = FontInfo
  { fontBold :: Bool
  , fontItalic :: Bool
  , fontUnderline :: Bool
  } deriving (Show)

-- | Style information
data Styles = Styles
  { styleFonts :: V.Vector FontInfo
  } deriving (Show)

-- | Complete XLSX document
data Xlsx = Xlsx
  { xlsxWorkbook :: XlsxWorkbook
  , xlsxSheets :: [XlsxSheet]
  , xlsxSharedStrings :: SharedStrings
  , xlsxStyles :: Styles
  } deriving (Show)

-- | Workbook information
data XlsxWorkbook = XlsxWorkbook
  { workbookSheetNames :: [(SheetId, Text, Text)]  -- (id, name, relId)
  } deriving (Show)

-- | Individual worksheet
data XlsxSheet = XlsxSheet
  { sheetId :: SheetId
  , sheetName :: Text
  , sheetCells :: M.Map CellRef XlsxCell
  } deriving (Show)

-- | Parse XLSX archive
archiveToXlsx :: Archive -> Either Text Xlsx
archiveToXlsx archive = do
  -- Find and parse workbook.xml
  workbookPath <- getWorkbookXmlPath archive
  workbookElem <- loadXMLFromArchive archive workbookPath
  workbook <- parseWorkbook workbookElem
    `addContext` ("Parsing workbook.xml from: " <> T.pack workbookPath)

  -- Load workbook relationships
  workbookRels <- loadRelationships archive (relsPathFor workbookPath)

  -- Parse shared strings (look for sharedStrings relationship)
  sharedStrings <- case findRelWithTarget workbookRels "sharedStrings" of
    Just (_, target) -> do
      let path = "xl/" ++ T.unpack target
      el <- loadXMLFromArchive archive path
      parseSharedStrings el
    Nothing -> Right V.empty

  -- Parse styles
  styles <- case findRelWithTarget workbookRels "styles" of
    Just (_, target) -> do
      let path = "xl/" ++ T.unpack target
      el <- loadXMLFromArchive archive path
      parseStyles el
    Nothing -> Right $ Styles V.empty

  -- Parse worksheets
  sheets <- mapM (\sheetInfo -> parseSheet archive workbookRels sharedStrings styles sheetInfo)
                 (workbookSheetNames workbook)

  return $ Xlsx workbook sheets sharedStrings styles

-- | Find workbook.xml via root relationships
getWorkbookXmlPath :: Archive -> Either Text FilePath
getWorkbookXmlPath archive = do
  relsEntry <- maybeToEither "Missing _rels/.rels" $
               findEntryByPath "_rels/.rels" archive
  relsElem <- parseXMLFromEntry relsEntry

  let relElems = onlyElems $ elContent relsElem
  case find isOfficeDocRel relElems of
    Nothing -> Left "No workbook.xml relationship found"
    Just rel -> do
      target <- maybeToEither "Missing Target" $ findAttr (unqual "Target") rel
      return $ T.unpack target
  where
    isOfficeDocRel el =
      case (findAttr (unqual "Type") el, findAttr (unqual "Target") el) of
        (Just relType, Just target) ->
          "officeDocument" `T.isInfixOf` relType && "workbook" `T.isInfixOf` target
        _ -> False

-- | Parse workbook.xml
parseWorkbook :: Element -> Either Text XlsxWorkbook
parseWorkbook wbElem = do
  let ns = elemToNameSpaces wbElem

  -- Find sheets element (match by local name only)
  sheets <- maybeToEither "Missing <sheets>" $
            find (\e -> qName (elName e) == "sheets") (onlyElems $ elContent wbElem)

  let sheetElems = filter (\e -> qName (elName e) == "sheet") (onlyElems $ elContent sheets)
  sheetRefs <- mapM (parseSheetRef ns) (zip [1..] sheetElems)

  return $ XlsxWorkbook sheetRefs

parseSheetRef :: NameSpaces -> (Int, Element) -> Either Text (SheetId, Text, Text)
parseSheetRef ns (idx, sheetElem) = do
  let name = fromMaybe ("Sheet" <> T.pack (show idx)) $
             findAttr (unqual "name") sheetElem
  relId <- maybeToEither "Missing r:id" $
           findAttrByName ns "r" "id" sheetElem
  return (SheetId idx, name, relId)

-- | Parse shared strings
parseSharedStrings :: Element -> Either Text SharedStrings
parseSharedStrings sstElem = do
  let siElems = filter (\e -> qName (elName e) == "si") (onlyElems $ elContent sstElem)
      strings = map extractString siElems
  return $ V.fromList strings
  where
    extractString siElem =
      case find (\e -> qName (elName e) == "t") (onlyElems $ elContent siElem) of
        Just tElem -> strContent tElem
        Nothing -> getAllText siElem

-- | Parse styles (fonts only for MVP)
parseStyles :: Element -> Either Text Styles
parseStyles stylesElem = do
  -- Parse fonts (match by local name)
  let fontsElem = find (\e -> qName (elName e) == "fonts") (onlyElems $ elContent stylesElem)
      fontElems = maybe [] (\fe -> filter (\e -> qName (elName e) == "font") (onlyElems $ elContent fe)) fontsElem
      fonts = V.fromList $ map (parseFont mempty) fontElems

  return $ Styles fonts

parseFont :: NameSpaces -> Element -> FontInfo
parseFont _ns fontElem =
  FontInfo
    { fontBold = any (\e -> qName (elName e) == "b") (onlyElems $ elContent fontElem)
    , fontItalic = any (\e -> qName (elName e) == "i") (onlyElems $ elContent fontElem)
    , fontUnderline = any (\e -> qName (elName e) == "u") (onlyElems $ elContent fontElem)
    }

-- | Parse individual worksheet
parseSheet :: Archive -> [(Text, Text)] -> SharedStrings -> Styles -> (SheetId, Text, Text) -> Either Text XlsxSheet
parseSheet archive rels sharedStrings styles (sid, name, relId) = do
  target <- maybeToEither ("Sheet relationship not found: " <> relId) $
            lookup relId rels

  let sheetPath = "xl/" ++ T.unpack target
  sheetElem <- loadXMLFromArchive archive sheetPath

  cells <- parseSheetCells sheetElem sharedStrings styles

  return $ XlsxSheet sid name cells

-- | Parse sheet cells
parseSheetCells :: Element -> SharedStrings -> Styles -> Either Text (M.Map CellRef XlsxCell)
parseSheetCells sheetElem sharedStrings styles = do
  -- Find sheetData by local name
  case find (\e -> qName (elName e) == "sheetData") (onlyElems $ elContent sheetElem) of
    Nothing -> return M.empty
    Just sheetData -> do
      let rowElems = filter (\e -> qName (elName e) == "row") (onlyElems $ elContent sheetData)
          cellElems = concatMap (\r -> filter (\e -> qName (elName e) == "c") (onlyElems $ elContent r)) rowElems
          cells = mapMaybe (parseCell sharedStrings styles) cellElems
      return $ M.fromList [(cellRef c, c) | c <- cells]

-- | Parse individual cell
parseCell :: SharedStrings -> Styles -> Element -> Maybe XlsxCell
parseCell sharedStrings styles cElem = do
  -- Get cell reference
  refText <- findAttr (unqual "r") cElem
  cellRefParsed <- either (const Nothing) Just $ parseCellRef refText

  -- Get cell type (default to number if missing)
  let cellType = fromMaybe "" $ findAttr (unqual "t") cElem
      styleIdx = findAttr (unqual "s") cElem >>= readMaybe . T.unpack

  -- Get value (match by local name)
  let vElem = find (\e -> qName (elName e) == "v") (onlyElems $ elContent cElem)
      vText = maybe "" strContent vElem

  -- Parse value based on type
  let value = if cellType == "s"
              then
                -- Shared string
                case readMaybe (T.unpack vText) of
                  Just idx | idx >= 0 && idx < V.length sharedStrings ->
                    TextValue (sharedStrings V.! idx)
                  _ -> EmptyValue
              else if T.null vText
                then EmptyValue
                else
                  -- Number
                  case readMaybe (T.unpack vText) of
                    Just n -> NumberValue n
                    Nothing -> TextValue vText

  -- Get formatting from style
  let (bold, italic) = case styleIdx of
        Just idx | idx >= 0 && idx < V.length (styleFonts styles) ->
          let font = styleFonts styles V.! idx
           in (fontBold font, fontItalic font)
        _ -> (False, False)

  return $ XlsxCell cellRefParsed value bold italic

-- Helper functions
loadXMLFromArchive :: Archive -> FilePath -> Either Text Element
loadXMLFromArchive archive path = do
  entry <- maybeToEither ("Entry not found: " <> T.pack path) $
           findEntryByPath path archive
  parseXMLFromEntry entry

parseXMLFromEntry :: Entry -> Either Text Element
parseXMLFromEntry entry =
  let lazyText = TL.decodeUtf8 $ fromEntry entry
   in parseXMLElement lazyText

loadRelationships :: Archive -> FilePath -> Either Text [(Text, Text)]
loadRelationships archive relsPath =
  case findEntryByPath relsPath archive of
    Nothing -> Right []
    Just entry -> do
      relsElem <- parseXMLFromEntry entry
      let relElems = onlyElems $ elContent relsElem
      return $ mapMaybe extractRel relElems
  where
    extractRel el = do
      relId <- findAttr (unqual "Id") el
      target <- findAttr (unqual "Target") el
      return (relId, target)

relsPathFor :: FilePath -> FilePath
relsPathFor path =
  let (dir, file) = splitFileName path
   in dir ++ "/_rels/" ++ file ++ ".rels"

findRelWithTarget :: [(Text, Text)] -> Text -> Maybe (Text, Text)
findRelWithTarget rels targetName =
  find (\(_, target) -> targetName `T.isInfixOf` target) rels

maybeToEither :: Text -> Maybe a -> Either Text a
maybeToEither err Nothing = Left err
maybeToEither _ (Just x) = Right x

getAllText :: Element -> Text
getAllText el =
  let textFromContent (Text cdata) = cdData cdata
      textFromContent (Elem e) = getAllText e
      textFromContent _ = ""
      texts = map textFromContent (elContent el)
   in T.unwords $ filter (not . T.null) texts

addContext :: Either Text a -> Text -> Either Text a
addContext (Right x) _ = Right x
addContext (Left err) ctx = Left (err <> " (context: " <> ctx <> ")")
