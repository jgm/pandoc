{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

{- |
   Module      : Text.Pandoc.Writers.Tables
   Copyright   : Copyright 2020 Christian Despres
   License     : GNU GPL, version 2 or above

   Maintainer  : Christian Despres <christian.j.j.despres@gmail.com>
   Stability   : alpha
   Portability : portable

Definitions and helper functions for an intermediate 'AnnTable' type,
which annotates the existing 'Table' types with additional inferred
information. For use in writers that need to know the details of
columns that cells span, row numbers, and the cells that are in the
row head.
-}

module Text.Pandoc.Writers.Tables
  ( toAnnTable
  , fromAnnTable
  , AnnTable(..)
  , AnnTableHead(..)
  , AnnTableBody(..)
  , AnnTableFoot(..)
  , AnnHeaderRow(..)
  , AnnBodyRow(..)
  , RowNumber(..)
  , AnnRowHead
  , AnnRowBody
  , AnnCell(..)
  , ColNumber(..)
  )
where

import           Control.Monad.RWS.Strict
import           Data.Generics                  ( Data
                                                , Typeable
                                                )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           GHC.Generics                   ( Generic )
import           Text.Pandoc.Builder

-- | An annotated table type, corresponding to the 'Table' constructor
-- and the HTML @\<table\>@ element. It records the data of the
-- columns that cells span, the cells in the row head, the row numbers
-- of rows, and the column numbers of cells, in addition to the data
-- in a 'Table'. The type itself does not enforce any guarantees about
-- the consistency of this data. Use 'toAnnTable' to produce an
-- 'AnnTable' from a pandoc 'Table'.
data AnnTable = AnnTable Attr Caption [ColSpec] AnnTableHead [AnnTableBody] AnnTableFoot
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | An annotated table head, corresponding to 'TableHead' and the
-- HTML @\<thead\>@ element.
data AnnTableHead = AnnTableHead Attr [AnnHeaderRow]
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | An annotated table body, with an intermediate head and body,
-- corresponding to 'TableBody' and the HTML @\<tbody\>@ element.
data AnnTableBody = AnnTableBody Attr RowHeadColumns [AnnHeaderRow] [AnnBodyRow]
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | An annotated table foot, corresponding to 'TableFoot' and the
-- HTML @\<tfoot\>@ element.
data AnnTableFoot = AnnTableFoot Attr [AnnHeaderRow]
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | An annotated header row, corresponding to 'Row' and the HTML
-- @\<tr\>@ element, and also recording the row number of the row. All
-- the cells in an 'AnnHeaderRow' are header (@\<th\>@) cells.
data AnnHeaderRow = AnnHeaderRow Attr RowNumber [AnnCell]
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | An annotated body row, corresponding to 'Row' and the HTML
-- @\<tr\>@ element, and also recording its row number and separating
-- the row head cells from the row body cells.
data AnnBodyRow = AnnBodyRow Attr RowNumber AnnRowHead AnnRowBody
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | The row number of a row. Note that rows are numbered continuously
-- from zero from the start of the table, so the first row in a table
-- body, for instance, may have a large 'RowNumber'.
newtype RowNumber = RowNumber Int
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, Num, Enum)

-- | The head of a body row; the portion of the row lying in the stub
-- of the 'TableBody'. Its cells correspond to HTML @\<th\>@ cells.
type AnnRowHead = [AnnCell]

-- | The body of a body row; the portion of the row lying after the
-- stub of the 'TableBody'. Its cells correspond to HTML @\<td\>@
-- cells.
type AnnRowBody = [AnnCell]

-- | An annotated table cell, wrapping a 'Cell' with its 'ColNumber'
-- and the 'ColSpec' data for the columns that the cell spans.
data AnnCell = AnnCell (NonEmpty ColSpec) ColNumber Cell
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | The column number of a cell, meaning the column number of the
-- first column that the cell spans, if the table were laid on a
-- grid. Columns are numbered starting from zero.
newtype ColNumber = ColNumber Int
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, Num, Enum)

-- | Convert a 'Table' to an 'AnnTable'. This function also performs
-- the same normalization that the 'table' builder does (fixing
-- overlapping cells, cells that protrude out of their table section,
-- and so on). If the input table happens to satisfy the conditions
-- that 'table' guarantees, then the resulting 'AnnTable' will be
-- identical, save for the addition of the inferred table information.
toAnnTable
  :: Attr
  -> Caption
  -> [ColSpec]
  -> TableHead
  -> [TableBody]
  -> TableFoot
  -> AnnTable
toAnnTable attr cap cs th tbs tf = AnnTable attr cap cs th' tbs' tf'
 where
  (th', tbs', tf') = fst $ evalRWS (annotateTable th tbs tf) (cs, length cs) 0

-- | Internal monad for annotating a table, passing in the 'ColSpec'
-- data for the table, the grid width, and the current 'RowNumber' to
-- be referenced or updated.
type AnnM a = RWS ([ColSpec], Int) () RowNumber a

incRowNumber :: AnnM RowNumber
incRowNumber = do
  rn <- get
  put $ rn + 1
  return rn

annotateTable
  :: TableHead
  -> [TableBody]
  -> TableFoot
  -> AnnM (AnnTableHead, [AnnTableBody], AnnTableFoot)
annotateTable th tbs tf = do
  th'  <- annotateTableHead th
  tbs' <- traverse annotateTableBody tbs
  tf'  <- annotateTableFoot tf
  return (th', tbs', tf')

annotateTableHead :: TableHead -> AnnM AnnTableHead
annotateTableHead (TableHead attr rows) =
  AnnTableHead attr <$> annotateHeaderSection rows

annotateTableBody :: TableBody -> AnnM AnnTableBody
annotateTableBody (TableBody attr rhc th tb) = do
  twidth <- asks snd
  let rhc' = max 0 $ min (RowHeadColumns twidth) rhc
  th' <- annotateHeaderSection th
  tb' <- annotateBodySection rhc' tb
  return $ AnnTableBody attr rhc' th' tb'

annotateTableFoot :: TableFoot -> AnnM AnnTableFoot
annotateTableFoot (TableFoot attr rows) =
  AnnTableFoot attr <$> annotateHeaderSection rows

annotateHeaderSection :: [Row] -> AnnM [AnnHeaderRow]
annotateHeaderSection rows = do
  colspec <- asks fst
  let hangcolspec = (1, ) <$> colspec
  annotateHeaderSection' hangcolspec id $ clipRows rows
 where
  annotateHeaderSection' oldHang acc (Row attr cells : rs) = do
    let (_, newHang, cells', _) =
          annotateRowSection 0 oldHang $ cells <> repeat emptyCell
    n <- incRowNumber
    let annRow = AnnHeaderRow attr n cells'
    annotateHeaderSection' newHang (acc . (annRow :)) rs
  annotateHeaderSection' _ acc [] = return $ acc []

annotateBodySection :: RowHeadColumns -> [Row] -> AnnM [AnnBodyRow]
annotateBodySection (RowHeadColumns rhc) rows = do
  colspec <- asks fst
  let colspec'             = (1, ) <$> colspec
  let (stubspec, bodyspec) = splitAt rhc colspec'
  normalizeBodySection' stubspec bodyspec id $ clipRows rows
 where
  normalizeBodySection' headHang bodyHang acc (Row attr cells : rs) = do
    let (colnum, headHang', rowStub, cells') =
          annotateRowSection 0 headHang $ cells <> repeat emptyCell
    let (_, bodyHang', rowBody, _) = annotateRowSection colnum bodyHang cells'
    n <- incRowNumber
    let annRow = AnnBodyRow attr n rowStub rowBody
    normalizeBodySection' headHang' bodyHang' (acc . (annRow :)) rs
  normalizeBodySection' _ _ acc [] = return $ acc []

-- | Lay out a section of a 'Table' row on a grid row, annotating the
-- cells with the 'ColSpec' data for the columns that they
-- span. Performs the same normalization as 'placeRowSection'.
annotateRowSection
  :: ColNumber -- ^ The current column number
  -> [(RowSpan, ColSpec)] -- ^ The overhang of the previous grid row,
                          -- with column data
  -> [Cell] -- ^ The cells to annotate
  -> (ColNumber, [(RowSpan, ColSpec)], [AnnCell], [Cell]) -- ^ The new
                                                          -- column
                                                          -- number,
                                                          -- overhang,
                                                          -- annotated
                                                          -- cells,
                                                          -- and
                                                          -- remaining
                                                          -- cells
annotateRowSection !colnum oldHang cells
  -- If the grid has overhang at our position, try to re-lay in
  -- the next position.
  | (o, colspec) : os <- oldHang
  , o > 1
  = let (colnum', newHang, newCell, cells') =
            annotateRowSection (colnum + 1) os cells
    in  (colnum', (o - 1, colspec) : newHang, newCell, cells')
  -- Otherwise if there is any available width, place the cell and
  -- continue.
  | c : cells' <- cells
  , (h, w) <- getDim c
  , w' <- max 1 w
  , (w'', cellHang@(chStart : chRest), oldHang') <- splitCellHang h w' oldHang
  = let c'      = setW w'' c
        annCell = AnnCell (snd <$> chStart :| chRest) colnum c'
        colnum' = colnum + ColNumber (getColSpan w'')
        (colnum'', newHang, newCells, remainCells) =
            annotateRowSection colnum' oldHang' cells'
    in  (colnum'', cellHang <> newHang, annCell : newCells, remainCells)
  -- Otherwise there is no room in the section
  | otherwise
  = (colnum, [], [], cells)
 where
  getColSpan (ColSpan x) = x
  getDim (Cell _ _ h w _) = (h, w)
  setW w (Cell a b h _ c) = Cell a b h w c

-- | In @'splitCellHang' rs cs coldata@, with @rs@ the height of a
-- cell that lies at the beginning of @coldata@, and @cs@ its width
-- (which is not assumed to fit in the available space), return the
-- actual width of the cell (what will fit in the available space),
-- the data for the columns that the cell spans (including updating
-- the overhang to equal @rs@), and the remaining column data.
splitCellHang
  :: RowSpan
  -> ColSpan
  -> [(RowSpan, ColSpec)]
  -> (ColSpan, [(RowSpan, ColSpec)], [(RowSpan, ColSpec)])
splitCellHang h n = go 0
 where
  go acc ((1, spec) : ls) | acc < n =
    let (acc', hang, ls') = go (acc + 1) ls in (acc', (h, spec) : hang, ls')
  go acc l = (acc, [], l)

-- | Convert an 'AnnTable' to a 'Table'. This is the inverse of
-- 'toAnnTable' on well-formed tables (i.e. tables satisfying the
-- guarantees of 'table').
fromAnnTable
  :: AnnTable -> (Attr, Caption, [ColSpec], TableHead, [TableBody], TableFoot)
fromAnnTable (AnnTable attr cap cs th tbs tf) = (attr, cap, cs, th', tbs', tf')
 where
  th'  = fromAnnTableHead th
  tbs' = map fromAnnTableBody tbs
  tf'  = fromAnnTableFoot tf

fromAnnTableHead :: AnnTableHead -> TableHead
fromAnnTableHead (AnnTableHead attr rows) =
  TableHead attr $ fromAnnHeaderRow <$> rows

fromAnnTableBody :: AnnTableBody -> TableBody
fromAnnTableBody (AnnTableBody attr rhc th tb) =
  TableBody attr rhc (fromAnnHeaderRow <$> th) (fromAnnBodyRow <$> tb)

fromAnnTableFoot :: AnnTableFoot -> TableFoot
fromAnnTableFoot (AnnTableFoot attr rows) =
  TableFoot attr $ fromAnnHeaderRow <$> rows

fromAnnHeaderRow :: AnnHeaderRow -> Row
fromAnnHeaderRow (AnnHeaderRow attr _ cells) = Row attr $ fromAnnCell <$> cells

fromAnnBodyRow :: AnnBodyRow -> Row
fromAnnBodyRow (AnnBodyRow attr _ rh rb) =
  Row attr ((fromAnnCell <$> rh) <> (fromAnnCell <$> rb))

fromAnnCell :: AnnCell -> Cell
fromAnnCell (AnnCell _ _ c) = c
