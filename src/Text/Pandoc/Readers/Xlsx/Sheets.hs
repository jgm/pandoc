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
import Numeric (floatToDigits)
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
      BoolValue _ -> False

-- | Convert cell to Pandoc inlines
cellToInlines :: XlsxCell -> [Inline]
cellToInlines cell =
  let base = case cellValue cell of
        TextValue t -> B.toList $ B.text t
        NumberValue n -> [Str $ formatGeneral n]
        BoolValue b -> [Str $ if b then "TRUE" else "FALSE"]
        EmptyValue -> []

      applyBold inls = if cellBold cell then [Strong inls] else inls
      applyItalic inls = if cellItalic cell then [Emph inls] else inls

   in applyItalic $ applyBold base

-- | Render a number as Excel's General format does, without rounding
-- it to fit a column: at most 15 significant digits (Excel's precision)
-- and no trailing zeros. Numbers with 12 or more digits before the
-- decimal point use scientific notation (@1.23456789012E+11@), as General
-- does, keeping all their digits; so do numbers below 1e-9.
formatGeneral :: Double -> T.Text
formatGeneral x
  | isNaN x || isInfinite x = T.pack (show x)
  | x == 0 = "0"
  | otherwise = T.pack $ sign ++ body
  where
    sign = if x < 0 then "-" else ""
    (ds, e) = roundDigits 15 (floatToDigits 10 (abs x))
    digits = concatMap show ds
    -- abs x == 0.d1d2d3... * 10^e
    body
      | e > 11 || e < -8 = scientific
      | e <= 0 = "0." ++ replicate (negate e) '0' ++ digits
      | length ds <= e = digits ++ replicate (e - length ds) '0'
      | otherwise = take e digits ++ "." ++ drop e digits
    scientific =
      let mantissa = case digits of
            d:rest@(_:_) -> d : '.' : rest
            _ -> digits
          ex = e - 1
       in mantissa ++ "E" ++ (if ex < 0 then "-" else "+") ++
          (let a = show (abs ex) in if length a < 2 then '0' : a else a)

-- | Round a digit list (as from 'floatToDigits') to at most n digits,
-- dropping trailing zeros.
roundDigits :: Int -> ([Int], Int) -> ([Int], Int)
roundDigits n (ds, e) =
  case splitAt n ds of
    (_, []) -> (ds, e)
    (keep, d:_) ->
      case if d >= 5 then carry (reverse keep) else Just keep of
        Just ds' -> (dropWhileEnd (== 0) ds', e)
        Nothing -> ([1], e + 1)
  where
    -- Add one to a reversed digit list; Nothing if it overflows (all 9s).
    carry [] = Nothing
    carry (9:rest) = (++ [0]) <$> carry rest
    carry (k:rest) = Just (reverse (k + 1 : rest))

