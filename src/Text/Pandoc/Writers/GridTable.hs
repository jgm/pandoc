{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TupleSections              #-}

{- |
Module      : Text.Pandoc.Writers.GridTable
Copyright   : © 2020-2022 Albert Krewinkel
License     : GNU GPL, version 2 or above

Maintainer  : Albert Krewinkel <albert@zeitkraut.de>

Grid representation of pandoc tables. The structures in this module
allow to describe 'Text.Pandoc.Definition.Table' elements without loss
of information. However, they are simpler to use when the grid layout of
a table must be known.

The "grid tables" handled here are conceptually similar to grid tables
in reStructuredText and Markdown, but are more general.
-}
module Text.Pandoc.Writers.GridTable
  ( Table (..)
  , GridCell (..)
  , RowIndex (..)
  , ColIndex (..)
  , CellIndex
  , Part (..)
  , toTable
  , rowArray
  ) where

import Control.Monad (forM_)
import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.Maybe (listToMaybe)
import Data.STRef
import Text.Pandoc.Definition hiding (Table)
import qualified Text.Pandoc.Builder as B

-- | A grid cell contains either a real table cell, or is the
-- continuation of a column or row-spanning cell. In the latter case,
-- the index of the continued cell is provided.
data GridCell
  = ContentCell Attr Alignment RowSpan ColSpan [Block]
  | ContinuationCell CellIndex
  deriving (Show)

-- | Row index in a table part.
newtype RowIndex = RowIndex Int deriving (Enum, Eq, Ix, Ord, Show)
-- | Column index in a table part.
newtype ColIndex = ColIndex Int deriving (Enum, Eq, Ix, Ord, Show)

-- | Index to a cell in a table part.
type CellIndex = (RowIndex, ColIndex)

-- | Cells are placed on a grid. Row attributes are stored in a separate
-- array.
data Part = Part
  { partAttr :: Attr
  , partCellArray :: Array (RowIndex,ColIndex) GridCell
  , partRowAttrs  :: Array RowIndex Attr
  }

data Table = Table
  { tableAttr     :: Attr
  , tableCaption  :: Caption
  , tableColSpecs :: Array ColIndex ColSpec
  , tableRowHeads :: RowHeadColumns
  , tableHead     :: Part
  , tableBodies   :: [Part]
  , tableFoot     :: Part
  }

toTable
  :: B.Attr
  -> B.Caption
  -> [B.ColSpec]
  -> B.TableHead
  -> [B.TableBody]
  -> B.TableFoot
  -> Table
toTable attr caption colSpecs  thead tbodies tfoot =
  Table attr caption colSpecs' rowHeads thGrid tbGrids tfGrid
  where
    colSpecs' = listArray (ColIndex 1, ColIndex $ length colSpecs) colSpecs
    rowHeads = case listToMaybe tbodies of
      Nothing -> RowHeadColumns 0
      Just (TableBody _attr rowHeadCols _headerRows _rows) -> rowHeadCols
    thGrid = let (TableHead headAttr rows) = thead
             in rowsToPart headAttr rows
    tbGrids = map bodyToGrid tbodies
    tfGrid = let (TableFoot footAttr rows) = tfoot
             in rowsToPart footAttr rows
    bodyToGrid (TableBody bodyAttr _rowHeadCols headRows rows) =
      rowsToPart bodyAttr (headRows ++ rows)

data BuilderCell
  = FilledCell GridCell
  | FreeCell

fromBuilderCell :: BuilderCell -> GridCell
fromBuilderCell = \case
  FilledCell c -> c
  FreeCell -> error "Found an unassigned cell. Please report this as a bug!"

rowsToPart :: Attr -> [B.Row] -> Part
rowsToPart attr = \case
  [] -> Part
        attr
        (listArray ((RowIndex 1, ColIndex 1), (RowIndex 0, ColIndex 0)) [])
        (listArray (RowIndex 1, RowIndex 0) [])
  rows@(Row _attr firstRow:_) ->
    let nrows = length rows
        ncols = sum $ map (\(Cell _ _ _ (ColSpan cs) _) -> cs) firstRow
        gbounds = ((RowIndex 1, ColIndex 1), (RowIndex nrows, ColIndex ncols))
        mutableGrid :: ST s (STArray s CellIndex GridCell)
        mutableGrid = do
          grid <- newArray gbounds FreeCell
          ridx <- newSTRef (RowIndex 1)
          forM_ rows $ \(Row _attr cells) -> do
            cidx <- newSTRef (ColIndex 1)
            forM_ cells $ \(Cell cellAttr align rs cs blks) -> do
              ridx' <- readSTRef ridx
              let nextFreeInRow colindex@(ColIndex c) = do
                    let idx = (ridx', colindex)
                    if gbounds `inRange` idx
                      then readArray grid idx >>= \case
                             FreeCell -> pure (Just colindex)
                             _ -> nextFreeInRow $ ColIndex (c + 1)
                      else pure Nothing  -- invalid table
              mcidx' <- readSTRef cidx >>= nextFreeInRow
              -- If there is a FreeCell in the current row, then fill it
              -- with the current cell and mark cells in this and the
              -- following rows as continuation cells if necessary.
              --
              -- Just skip the current table cell if no FreeCell was
              -- found; this can only happen with invalid tables.
              case mcidx' of
                Nothing -> pure () -- no FreeCell left in row -- skip cell
                Just cidx' -> do
                  writeArray grid (ridx', cidx') . FilledCell $
                    ContentCell cellAttr align rs cs blks
                  forM_ (continuationIndices ridx' cidx' rs cs) $ \idx -> do
                    writeArray grid idx . FilledCell $
                      ContinuationCell (ridx', cidx')
                  -- go to new column
                  writeSTRef cidx cidx'
            -- go to next row
            modifySTRef ridx (incrRowIndex 1)
          -- Swap BuilderCells with normal GridCells.
          mapArray fromBuilderCell grid
    in Part
       { partCellArray = runSTArray mutableGrid
       , partRowAttrs = listArray (RowIndex 1, RowIndex nrows) $
                        map (\(Row rowAttr _) -> rowAttr) rows
       , partAttr = attr
       }

continuationIndices :: RowIndex -> ColIndex -> RowSpan -> ColSpan -> [CellIndex]
continuationIndices (RowIndex ridx) (ColIndex cidx) rowspan colspan =
  let (RowSpan rs) = rowspan
      (ColSpan cs) = colspan
  in [ (RowIndex r, ColIndex c) | r <- [ridx..(ridx + rs - 1)]
                                , c <- [cidx..(cidx + cs - 1)]
                                , (r, c) /= (ridx, cidx)]

rowArray :: RowIndex -> Array CellIndex GridCell -> Array ColIndex GridCell
rowArray ridx grid =
  let ((_minRidx, minCidx), (_maxRidx, maxCidx)) = bounds grid
  in ixmap (minCidx, maxCidx) (ridx,) grid

incrRowIndex :: RowSpan -> RowIndex -> RowIndex
incrRowIndex (RowSpan n) (RowIndex r) = RowIndex $ r + n
