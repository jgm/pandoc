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
import Text.Pandoc.Extensions (Extension (Ext_cell_details))
import Text.Pandoc.Options (ReaderOptions, isEnabled)
import Text.Pandoc.Readers.Xlsx.Parse
import Text.Pandoc.Readers.Xlsx.Cells
import qualified Text.Pandoc.Builder as B

-- | Convert XLSX to Pandoc output
xlsxToOutput :: ReaderOptions -> Xlsx -> (Meta, [Block])
xlsxToOutput opts xlsx =
  let sheets = xlsxSheets xlsx
      details = isEnabled Ext_cell_details opts
      sheetBlocks = concatMap (sheetToBlocks details) sheets
   in (mempty, sheetBlocks)

-- | Convert sheet to blocks (header + table). With the @cell_details@
-- extension, hidden sheets, rows and columns get a @hidden@ class, and
-- cells get their reference, formula and comment as attributes.
sheetToBlocks :: Bool -> XlsxSheet -> [Block]
sheetToBlocks details sheet =
  let SheetId n = sheetId sheet
      name = sheetName sheet
      sheetIdent = "sheet-" <> T.pack (show n)
      classes = ["hidden" | details && sheetHidden sheet]
      header = Header 2 (sheetIdent, classes, []) (B.toList (B.text name))

      -- Convert cells to table
      tableBlock = case cellsToTable details sheet of
        Just tbl -> [tbl]
        Nothing -> []  -- Empty sheet
   in header : tableBlock

-- | Convert cells to Pandoc Table
cellsToTable :: Bool -> XlsxSheet -> Maybe Block
cellsToTable details sheet
  | M.null (sheetCells sheet) = Nothing
  | otherwise =
      let cells = sheetCells sheet
          -- Get bounds; with cell details, comments on empty cells count
          refs = sort $ M.keys cells ++
                 (if details then M.keys (sheetComments sheet) else [])
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

          makeCell (ref, mcell) =
            Cell (cellAttr ref mcell) AlignDefault (RowSpan 1) (ColSpan 1)
                 [Plain (maybe [] cellToInlines mcell)]

          cellAttr ref mcell
            | not details = nullAttr
            | otherwise =
                ( ""
                , ["hidden" | cellRefCol ref `S.member` sheetHiddenCols sheet]
                , [("ref", showCellRef ref)] ++
                  [("formula", f) | Just f <- [mcell >>= cellFormula]] ++
                  [("comment", c) | Just c <- [M.lookup ref
                                                 (sheetComments sheet)]]
                )
          makeRow cs = Row (rowAttr cs) (map makeCell cs)
          rowAttr cs = case cs of
            (ref, _) : _
              | details
              , cellRefRow ref `S.member` sheetHiddenRows sheet
              -> ("", ["hidden"], [])
            _ -> nullAttr

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
