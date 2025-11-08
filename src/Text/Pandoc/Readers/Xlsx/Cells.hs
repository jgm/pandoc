{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Readers.Xlsx.Cells
   Copyright   : © 2025 Anton Antic
   License     : GNU GPL, version 2 or above

   Maintainer  : Anton Antic <anton@everworker.ai>
   Stability   : alpha
   Portability : portable

Cell types and parsing for XLSX.
-}
module Text.Pandoc.Readers.Xlsx.Cells
  ( CellRef(..)
  , XlsxCell(..)
  , CellValue(..)
  , parseCellRef
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (ord, isAlpha)
import Text.Read (readMaybe)

-- | Cell reference (A1 notation)
data CellRef = CellRef
  { cellRefCol :: Int    -- 1-based (A=1, B=2, ..., AA=27)
  , cellRefRow :: Int    -- 1-based
  } deriving (Show, Eq, Ord)

-- | Cell value types
data CellValue
  = TextValue Text
  | NumberValue Double
  | EmptyValue
  deriving (Show, Eq)

-- | Parsed cell
data XlsxCell = XlsxCell
  { cellRef :: CellRef
  , cellValue :: CellValue
  , cellBold :: Bool
  , cellItalic :: Bool
  } deriving (Show)

-- | Parse cell reference (A1 → CellRef)
parseCellRef :: Text -> Either Text CellRef
parseCellRef ref = do
  let (colStr, rowStr) = T.span isAlpha ref

  row <- case readMaybe (T.unpack rowStr) of
    Just r | r > 0 -> Right r
    _ -> Left $ "Invalid row: " <> rowStr

  col <- parseColumn colStr

  return $ CellRef col row

-- | Parse column (A=1, Z=26, AA=27, etc.)
parseColumn :: Text -> Either Text Int
parseColumn colStr
  | T.null colStr = Left "Empty column"
  | otherwise = Right $ T.foldl' (\acc c -> acc * 26 + (ord c - ord 'A' + 1)) 0 colStr
