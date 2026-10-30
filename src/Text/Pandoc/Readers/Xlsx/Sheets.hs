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
          grid = [ [ (CellRef col row, M.lookup (CellRef col row) cells)
                   | col <- [minCol..maxCol]
                   ]
                 | row <- [minRow..maxRow]
                 ]

          -- First row is header (simple heuristic)
          (headerRow, bodyRows) = case grid of
            (h:bs) -> (h, bs)
            [] -> ([], [])

          -- Filter out trailing empty rows (rows with only whitespace)
          filteredBodyRows = dropWhileEnd (isEmptyRow . map snd) bodyRows
          lastRow = minRow + length filteredBodyRows

          -- A merged range becomes one cell spanning its rows and
          -- columns, clipped to the table and kept within the header row
          -- or within the body, as a cell can't span both.
          spans = M.fromList
            [ (from, (rows, cols))
            | (from, to) <- sheetMerges sheet
            , cellRefCol from >= minCol, cellRefCol from <= maxCol
            , cellRefRow from >= minRow, cellRefRow from <= lastRow
            , let rows = if cellRefRow from == minRow
                            then 1
                            else min (cellRefRow to) lastRow
                                   - cellRefRow from + 1
                  cols = min (cellRefCol to) maxCol - cellRefCol from + 1
            , rows > 1 || cols > 1
            ]
          covered = S.fromList
            [ CellRef col row
            | (CellRef c r, (rows, cols)) <- M.toList spans
            , row <- [r .. r + rows - 1], col <- [c .. c + cols - 1]
            , (col, row) /= (c, r)
            ]

          makeCell (ref, mcell) =
            let (rows, cols) = M.findWithDefault (1, 1) ref spans
             in Cell nullAttr AlignDefault (RowSpan rows) (ColSpan cols)
                     [Plain (maybe [] cellToInlines mcell)]
          makeRow = Row nullAttr . map makeCell .
                    filter (\(ref, _) -> not (ref `S.member` covered))

          numCols = length headerRow
          colSpec = replicate numCols (AlignDefault, ColWidthDefault)
          thead = TableHead nullAttr [makeRow headerRow]
          tbody = [TableBody nullAttr 0 [] $ map makeRow filteredBodyRows]
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
