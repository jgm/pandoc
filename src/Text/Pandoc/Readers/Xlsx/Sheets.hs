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
import qualified Data.Text as T
import Data.List (sort, dropWhileEnd)
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

-- | Convert cells to Pandoc Table
cellsToTable :: XlsxSheet -> Maybe Block
cellsToTable sheet
  | M.null (sheetCells sheet) = Nothing
  | otherwise =
      let cells = sheetCells sheet
          -- Get bounds
          refs = sort $ M.keys cells
          minCol = minimum $ map cellRefCol refs
          maxCol = maximum $ map cellRefCol refs
          minRow = minimum $ map cellRefRow refs
          maxRow = maximum $ map cellRefRow refs

          -- Build dense grid
          grid = [ [ M.lookup (CellRef col row) cells
                   | col <- [minCol..maxCol]
                   ]
                 | row <- [minRow..maxRow]
                 ]

          -- First row is header (simple heuristic)
          (headerRow, bodyRows) = case grid of
            (h:bs) -> (h, bs)
            [] -> ([], [])

          -- Filter out trailing empty rows (rows with only whitespace)
          filteredBodyRows = dropWhileEnd isEmptyRow bodyRows

          makeCell mcell = case mcell of
            Just cell ->
              let inlines = cellToInlines cell
               in Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1) [Plain inlines]
            Nothing ->
              Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1) [Plain []]

          numCols = length headerRow
          colSpec = replicate numCols (AlignDefault, ColWidthDefault)
          thead = TableHead nullAttr [Row nullAttr $ map makeCell headerRow]
          tbody = [TableBody nullAttr 0 [] $ map (Row nullAttr . map makeCell) filteredBodyRows]
          tfoot = TableFoot nullAttr []

       in Just $ Table nullAttr (Caption Nothing []) colSpec thead tbody tfoot

-- | Check if a row contains only whitespace or empty cells
isEmptyRow :: [Maybe XlsxCell] -> Bool
isEmptyRow = all isEmptyCell
  where
    isEmptyCell Nothing = True
    isEmptyCell (Just cell) = case cellValue cell of
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
