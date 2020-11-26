{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{- |
   Module      : Text.Pandoc.Readers.HTML.Table
   Copyright   : © 2006-2020 John MacFarlane,
                   2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
   Stability   : alpha
   Portability : portable

HTML table parser.
-}
module Text.Pandoc.Readers.HTML.Table (pTable) where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Text.HTML.TagSoup
import Text.Pandoc.Builder (Blocks)
import Text.Pandoc.CSS (cssAttributes)
import Text.Pandoc.Definition
import Text.Pandoc.Class.PandocMonad (PandocMonad (..))
import Text.Pandoc.Parsing
  ( eof, lookAhead, many, many1, manyTill, option, optional
  , optionMaybe, skipMany, try)
import Text.Pandoc.Readers.HTML.Parsing
import Text.Pandoc.Readers.HTML.Types (TagParser)
import Text.Pandoc.Shared (onlySimpleTableCells, safeRead)
import qualified Data.Text as T
import qualified Text.Pandoc.Builder as B

-- | Parses a @<col>@ element, returning the column's width. Defaults to
-- @'ColWidthDefault'@ if the width is not set or cannot be determined.
pCol :: PandocMonad m => TagParser m ColWidth
pCol = try $ do
  TagOpen _ attribs' <- pSatisfy (matchTagOpen "col" [])
  let attribs = toStringAttr attribs'
  skipMany pBlank
  optional $ pSatisfy (matchTagClose "col")
  skipMany pBlank
  let width = case lookup "width" attribs of
                Nothing -> case lookup "style" attribs of
                  Just (T.stripPrefix "width:" -> Just xs) | T.any (== '%') xs ->
                    fromMaybe 0.0 $ safeRead (T.filter
                      (`notElem` (" \t\r\n%'\";" :: [Char])) xs)
                  _ -> 0.0
                Just (T.unsnoc -> Just (xs, '%')) ->
                  fromMaybe 0.0 $ safeRead xs
                _ -> 0.0
  if width > 0.0
    then return $ ColWidth $ width / 100.0
    else return ColWidthDefault

pColgroup :: PandocMonad m => TagParser m [ColWidth]
pColgroup = try $ do
  pSatisfy (matchTagOpen "colgroup" [])
  skipMany pBlank
  manyTill pCol (pCloses "colgroup" <|> eof) <* skipMany pBlank

pCell :: PandocMonad m
      => TagParser m Blocks
      -> Text
      -> TagParser m [Cell]
pCell block celltype = try $ do
  skipMany pBlank
  TagOpen _ attribs <- lookAhead $ pSatisfy (matchTagOpen celltype [])
  let cssAttribs = maybe [] cssAttributes $ lookup "style" attribs
  let align = case lookup "align" attribs <|>
                   lookup "text-align" cssAttribs of
                Just "left"   -> AlignLeft
                Just "right"  -> AlignRight
                Just "center" -> AlignCenter
                _             -> AlignDefault
  let rowspan = RowSpan . fromMaybe 1 $
                safeRead =<< lookup "rowspan" attribs
  let colspan = ColSpan . fromMaybe 1 $
                safeRead =<< lookup "colspan" attribs
  res <- pInTags celltype block
  skipMany pBlank
  let handledAttribs = ["align", "colspan", "rowspan", "text-align"]
      attribs' = foldr go [] attribs
      go kv@(k, _) acc = case k of
        "style" -> case filter ((/= "text-align") . fst) cssAttribs of
                     [] -> acc
                     cs -> ("style", toStyleString cs) : acc
        -- drop attrib if it's already handled
        _ | k `elem` handledAttribs -> acc
        _ -> kv : acc
  return [B.cellWith (toAttr attribs') align rowspan colspan res]

-- | Create a style attribute string from a list of CSS attributes
toStyleString :: [(Text, Text)] -> Text
toStyleString = T.intercalate "; " . map (\(k, v) -> k <> ": " <> v)

data RowType
  = HeaderCells
  | AllCells

-- | Parses a table row
pRow :: PandocMonad m
     => TagParser m Blocks
     -> RowType
     -> TagParser m [B.Row]
pRow block rowType = try $ do
  skipMany pBlank
  case rowType of
    HeaderCells -> do
      maybeCells <- optionMaybe (pInTags "tr" (pCell block "th"))
      return $ case maybeCells of
        Nothing    -> []
        Just cells -> [Row nullAttr cells]
    AllCells    -> do
      cells <- pInTags "tr" (pCell block "td" <|> pCell block "th")
      return [Row nullAttr cells]

-- | Parses a table head
pTableHead :: PandocMonad m
           => TagParser m Blocks
           -> TagParser m TableHead
pTableHead block = try $ do
  skipMany pBlank
  (attribs, rows) <-  pInTagWithAttribs ClosingTagOptional "thead"
                                        (option [] $ pRow block AllCells)
                  <|> pInTagWithAttribs TagsOmittable "thead"
                                        (pRow block HeaderCells)
  let cells = concatMap (\(Row _ cs) -> cs) rows
  if null cells
    then TableHead nullAttr <$>
         pInTag TagsOmittable "tbody" (pRow block HeaderCells)
    else return $ TableHead (toAttr attribs) [Row nullAttr cells]

-- | Parses a table foot
pTableFoot :: PandocMonad m
           => TagParser m Blocks
           -> TagParser m TableFoot
pTableFoot block = try $ do
  skipMany pBlank
  TagOpen _ attribs <- pSatisfy (matchTagOpen "tfoot" []) <* skipMany pBlank
  rows <- mconcat <$> many (pRow block AllCells <* skipMany pBlank)
  optional $ pSatisfy (matchTagClose "tfoot")
  return $ TableFoot (toAttr attribs) rows

-- | Parses a table body
pTableBody :: PandocMonad m
           => TagParser m Blocks
           -> TagParser m TableBody
pTableBody block = do
  skipMany pBlank
  (attribs, rows) <- pInTagWithAttribs TagsOmittable "tbody"
                     (mconcat <$> many1 (pRow block AllCells))
  return $ TableBody (toAttr attribs) 0 [] rows


-- | Parses a simple HTML table
pTable :: PandocMonad m
       => TagParser m Blocks -- ^ Caption and cell contents parser
       -> TagParser m Blocks
pTable block = try $ do
  TagOpen _ attribs <- pSatisfy (matchTagOpen "table" [])  <* skipMany pBlank
  caption <- option mempty $ pInTags "caption" block       <* skipMany pBlank
  widths  <- ((mconcat <$> many1 pColgroup) <|> many pCol) <* skipMany pBlank
  thead   <- pTableHead block               <* skipMany pBlank
  topfoot <- optionMaybe (pTableFoot block) <* skipMany pBlank
  tbodies <- many (pTableBody block)        <* skipMany pBlank
  botfoot <- optionMaybe (pTableFoot block) <* skipMany pBlank
  TagClose _ <- pSatisfy (matchTagClose "table")
  let tfoot = fromMaybe (TableFoot nullAttr []) $ topfoot <|> botfoot
  case normalize widths thead tbodies tfoot of
    Left err -> fail err
    Right (colspecs, thead', tbodies', tfoot') -> return $
      B.tableWith (toAttr attribs)
                  (B.simpleCaption caption)
                  colspecs
                  thead'
                  tbodies'
                  tfoot'
data TableType
  = SimpleTable
  | NormalTable

tableType :: [[Cell]] -> TableType
tableType cells =
  if onlySimpleTableCells $ map (map cellContents) cells
  then SimpleTable
  else NormalTable
  where
    cellContents :: Cell -> [Block]
    cellContents (Cell _ _ _ _ bs) = bs

normalize :: [ColWidth] -> TableHead -> [TableBody] -> TableFoot
          -> Either String ([ColSpec], TableHead, [TableBody], TableFoot)
normalize widths head' bodies foot = do
  let rows = headRows head' <> concatMap bodyRows bodies <> footRows foot
  let rowLength = length . rowCells
  let ncols = maximum (map rowLength rows)
  let tblType = tableType (map rowCells rows)
  -- fail on empty table
  if null rows
    then Left "empty table"
    else Right
         ( zip (calculateAlignments ncols bodies)
               (normalizeColWidths ncols tblType widths)
         , head'
         , bodies
         , foot
         )

normalizeColWidths :: Int -> TableType -> [ColWidth] -> [ColWidth]
normalizeColWidths ncols tblType = \case
  [] -> case tblType of
          SimpleTable -> replicate ncols ColWidthDefault
          NormalTable -> replicate ncols (ColWidth $ 1 / fromIntegral ncols)
  widths -> widths

calculateAlignments :: Int -> [TableBody] -> [Alignment]
calculateAlignments cols tbodies =
  case cells of
    cs:_ -> take cols $ concatMap cellAligns cs ++ repeat AlignDefault
    _    -> replicate cols AlignDefault
  where
    cells :: [[Cell]]
    cells = concatMap bodyRowCells tbodies
    cellAligns :: Cell -> [Alignment]
    cellAligns (Cell _ align _ (ColSpan cs) _) = replicate cs align

bodyRowCells :: TableBody -> [[Cell]]
bodyRowCells = map rowCells . bodyRows

headRows :: TableHead -> [B.Row]
headRows (TableHead _ rows) = rows

bodyRows :: TableBody -> [B.Row]
bodyRows (TableBody _ _ headerRows bodyRows') = headerRows <> bodyRows'

footRows :: TableFoot -> [B.Row]
footRows (TableFoot _ rows) = rows

rowCells :: B.Row -> [Cell]
rowCells (Row _ cells) = cells
