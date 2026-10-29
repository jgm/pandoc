{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Xlsx.Sheets
   Copyright   : © 2025 Anton Antic
   License     : GNU GPL, version 2 or above

   Maintainer  : Anton Antic <anton@everworker.ai>
   Stability   : alpha
   Portability : portable

Conversion of XLSX sheets to Pandoc AST.
-}
module Text.Pandoc.Readers.Xlsx.Sheets
  ( xlsxToOutput
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Char (isSpace)
import Text.Pandoc.Definition
import Text.Pandoc.Options (ReaderOptions)
import Text.Pandoc.Readers.Xlsx.Parse
import Text.Pandoc.Readers.Xlsx.Cells
import qualified Text.Pandoc.Builder as B

-- | Convert XLSX to Pandoc output
xlsxToOutput :: ReaderOptions -> Xlsx -> (Meta, [Block])
xlsxToOutput _opts xlsx =
  let sheets = xlsxSheets xlsx
      sheetBlocks = concatMap sheetToBlocks sheets
   in (mempty, sheetBlocks)

-- | Convert sheet to blocks (header + table)
sheetToBlocks :: XlsxSheet -> [Block]
sheetToBlocks sheet =
  let SheetId n = sheetId sheet
      name = sheetName sheet
      sheetIdent = "sheet-" <> T.pack (show n)
      header = Header 2 (sheetIdent, [], []) (B.toList (B.text name))

      -- Convert cells to table
      tableBlock = case cellsToTable sheet of
        Just tbl -> [tbl]
        Nothing -> []  -- Empty sheet
   in header : tableBlock

-- | Convert cells to Pandoc Table. Only the rows and columns that hold a
-- non-blank cell are kept, so the size of the table follows the content
-- of the sheet rather than the distance between its outermost cells.
cellsToTable :: XlsxSheet -> Maybe Block
cellsToTable sheet
  | M.null cells = Nothing
  | otherwise =
      let rows = S.toAscList $ S.fromList $ map cellRefRow $ M.keys cells
          cols = S.toAscList $ S.fromList $ map cellRefCol $ M.keys cells

          grid = [ [ M.lookup (CellRef col row) cells | col <- cols ]
                 | row <- rows
                 ]

          -- First row is header (simple heuristic)
          (headerRow, bodyRows) = case grid of
            (h:bs) -> (h, bs)
            [] -> ([], [])

          makeCell mcell = case mcell of
            Just cell ->
              let inlines = cellToInlines cell
               in Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1) [Plain inlines]
            Nothing ->
              Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1) [Plain []]

          numCols = length headerRow
          colSpec = replicate numCols (AlignDefault, ColWidthDefault)
          thead = TableHead nullAttr [Row nullAttr $ map makeCell headerRow]
          tbody = [TableBody nullAttr 0 [] $
                     map (Row nullAttr . map makeCell) bodyRows]
          tfoot = TableFoot nullAttr []

       in Just $ Table nullAttr (Caption Nothing []) colSpec thead tbody tfoot
  where
    cells = M.filter (not . isBlankCell) (sheetCells sheet)

-- | Check if a cell is empty or holds only whitespace
isBlankCell :: XlsxCell -> Bool
isBlankCell cell = case cellValue cell of
  EmptyValue -> True
  TextValue t -> T.all isSpace t
  NumberValue _ -> False

-- | Convert cell to Pandoc inlines
cellToInlines :: XlsxCell -> [Inline]
cellToInlines cell =
  let base = case cellValue cell of
        TextValue t -> B.toList $ B.text t
        NumberValue n -> [Str $ T.pack $ show n]
        EmptyValue -> []

      applyBold inls = if cellBold cell then [Strong inls] else inls
      applyItalic inls = if cellItalic cell then [Emph inls] else inls

   in applyItalic $ applyBold base
