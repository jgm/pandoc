{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
   Module      : Text.Pandoc.Writers.AnnotatedTable
   Copyright   : Copyright 2020 Christian Despres
   License     : GNU GPL, version 2 or above

   Maintainer  : Christian Despres <christian.j.j.despres@gmail.com>
   Stability   : alpha
   Portability : portable

Definitions and conversion functions for an intermediate 'Table' and
related types, which annotates the existing Pandoc 'B.Table' types
with additional inferred information. For use in writers that need to
know the details of columns that cells span, row numbers, and the
cells that are in the row head.
-}

module Text.Pandoc.Writers.AnnotatedTable
  ( toTable
  , fromTable
  , Table(..)
  , TableHead(..)
  , TableBody(..)
  , TableFoot(..)
  , HeaderRow(..)
  , BodyRow(..)
  , RowNumber(..)
  , RowHead
  , RowBody
  , Cell(..)
  , ColNumber(..)
  )
where

import           Control.Monad.RWS.Strict
                                         hiding ( (<>) )
import           Data.Generics                  ( Data
                                                , Typeable
                                                )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           GHC.Generics                   ( Generic )
import qualified Text.Pandoc.Builder           as B
import           Text.Pandoc.Walk               ( Walkable (..) )

-- | An annotated table type, corresponding to the Pandoc 'B.Table'
-- constructor and the HTML @\<table\>@ element. It records the data
-- of the columns that cells span, the cells in the row head, the row
-- numbers of rows, and the column numbers of cells, in addition to
-- the data in a 'B.Table'. The type itself does not enforce any
-- guarantees about the consistency of this data. Use 'toTable' to
-- produce a 'Table' from a Pandoc 'B.Table'.
data Table = Table B.Attr B.Caption [B.ColSpec] TableHead [TableBody] TableFoot
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | An annotated table head, corresponding to a Pandoc 'B.TableHead'
-- and the HTML @\<thead\>@ element.
data TableHead = TableHead B.Attr [HeaderRow]
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | An annotated table body, with an intermediate head and body,
-- corresponding to a Pandoc 'B.TableBody' and the HTML @\<tbody\>@
-- element.
data TableBody = TableBody B.Attr B.RowHeadColumns [HeaderRow] [BodyRow]
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | An annotated table foot, corresponding to a Pandoc 'B.TableFoot'
-- and the HTML @\<tfoot\>@ element.
data TableFoot = TableFoot B.Attr [HeaderRow]
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | An annotated header row, corresponding to a Pandoc 'B.Row' and
-- the HTML @\<tr\>@ element, and also recording the row number of the
-- row. All the cells in a 'HeaderRow' are header (@\<th\>@) cells.
data HeaderRow = HeaderRow B.Attr RowNumber [Cell]
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | An annotated body row, corresponding to a Pandoc 'B.Row' and the
-- HTML @\<tr\>@ element, and also recording its row number and
-- separating the row head cells from the row body cells.
data BodyRow = BodyRow B.Attr RowNumber RowHead RowBody
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | The row number of a row. Note that rows are numbered continuously
-- from zero from the start of the table, so the first row in a table
-- body, for instance, may have a large 'RowNumber'.
newtype RowNumber = RowNumber Int
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, Num, Enum)

-- | The head of a body row; the portion of the row lying in the stub
-- of the 'TableBody'. Its cells correspond to HTML @\<th\>@ cells.
type RowHead = [Cell]

-- | The body of a body row; the portion of the row lying after the
-- stub of the 'TableBody'. Its cells correspond to HTML @\<td\>@
-- cells.
type RowBody = [Cell]

-- | An annotated table cell, wrapping a Pandoc 'B.Cell' with its
-- 'ColNumber' and the 'B.ColSpec' data for the columns that the cell
-- spans.
data Cell = Cell (NonEmpty B.ColSpec) ColNumber B.Cell
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | The column number of a cell, meaning the column number of the
-- first column that the cell spans, if the table were laid on a
-- grid. Columns are numbered starting from zero.
newtype ColNumber = ColNumber Int
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic, Num, Enum)

-- | Convert a Pandoc 'B.Table' to an annotated 'Table'. This function
-- also performs the same normalization that the 'B.table' builder
-- does (fixing overlapping cells, cells that protrude out of their
-- table section, and so on). If the input table happens to satisfy
-- the conditions that 'B.table' guarantees, then the resulting
-- 'Table' will be identical, save for the addition of the inferred
-- table information.
toTable
  :: B.Attr
  -> B.Caption
  -> [B.ColSpec]
  -> B.TableHead
  -> [B.TableBody]
  -> B.TableFoot
  -> Table
toTable attr cap cs th tbs tf = Table attr cap cs th' tbs' tf'
 where
  (th', tbs', tf') = fst $ evalRWS (annotateTable th tbs tf) (cs, length cs) 0

-- | Internal monad for annotating a table, passing in the 'B.ColSpec'
-- data for the table, the grid width, and the current 'RowNumber' to
-- be referenced or updated.
type AnnM a = RWS ([B.ColSpec], Int) () RowNumber a

incRowNumber :: AnnM RowNumber
incRowNumber = do
  rn <- get
  put $ rn + 1
  return rn

annotateTable
  :: B.TableHead
  -> [B.TableBody]
  -> B.TableFoot
  -> AnnM (TableHead, [TableBody], TableFoot)
annotateTable th tbs tf = do
  th'  <- annotateTableHead th
  tbs' <- traverse annotateTableBody tbs
  tf'  <- annotateTableFoot tf
  return (th', tbs', tf')

annotateTableHead :: B.TableHead -> AnnM TableHead
annotateTableHead (B.TableHead attr rows) =
  TableHead attr <$> annotateHeaderSection rows

annotateTableBody :: B.TableBody -> AnnM TableBody
annotateTableBody (B.TableBody attr rhc th tb) = do
  twidth <- asks snd
  let rhc' = max 0 $ min (B.RowHeadColumns twidth) rhc
  th' <- annotateHeaderSection th
  tb' <- annotateBodySection rhc' tb
  return $ TableBody attr rhc' th' tb'

annotateTableFoot :: B.TableFoot -> AnnM TableFoot
annotateTableFoot (B.TableFoot attr rows) =
  TableFoot attr <$> annotateHeaderSection rows

annotateHeaderSection :: [B.Row] -> AnnM [HeaderRow]
annotateHeaderSection rows = do
  colspec <- asks fst
  let hangcolspec = (1, ) <$> colspec
  annotateHeaderSection' hangcolspec id $ B.clipRows rows
 where
  annotateHeaderSection' oldHang acc (B.Row attr cells : rs) = do
    let (_, newHang, cells', _) =
          annotateRowSection 0 oldHang $ cells <> repeat B.emptyCell
    n <- incRowNumber
    let annRow = HeaderRow attr n cells'
    annotateHeaderSection' newHang (acc . (annRow :)) rs
  annotateHeaderSection' _ acc [] = return $ acc []

annotateBodySection :: B.RowHeadColumns -> [B.Row] -> AnnM [BodyRow]
annotateBodySection (B.RowHeadColumns rhc) rows = do
  colspec <- asks fst
  let colspec'             = (1, ) <$> colspec
  let (stubspec, bodyspec) = splitAt rhc colspec'
  normalizeBodySection' stubspec bodyspec id $ B.clipRows rows
 where
  normalizeBodySection' headHang bodyHang acc (B.Row attr cells : rs) = do
    let (colnum, headHang', rowStub, cells') =
          annotateRowSection 0 headHang $ cells <> repeat B.emptyCell
    let (_, bodyHang', rowBody, _) = annotateRowSection colnum bodyHang cells'
    n <- incRowNumber
    let annRow = BodyRow attr n rowStub rowBody
    normalizeBodySection' headHang' bodyHang' (acc . (annRow :)) rs
  normalizeBodySection' _ _ acc [] = return $ acc []

-- | Lay out a section of a 'Table' row on a grid row, annotating the
-- cells with the 'B.ColSpec' data for the columns that they
-- span. Performs the same normalization as 'B.placeRowSection'.
annotateRowSection
  :: ColNumber -- ^ The current column number
  -> [(B.RowSpan, B.ColSpec)] -- ^ The overhang of the previous grid row,
                              -- with column data
  -> [B.Cell] -- ^ The cells to annotate
  -> (ColNumber, [(B.RowSpan, B.ColSpec)], [Cell], [B.Cell]) -- ^ The new
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
        annCell = Cell (snd <$> chStart :| chRest) colnum c'
        colnum' = colnum + ColNumber (getColSpan w'')
        (colnum'', newHang, newCells, remainCells) =
            annotateRowSection colnum' oldHang' cells'
    in  (colnum'', cellHang <> newHang, annCell : newCells, remainCells)
  -- Otherwise there is no room in the section
  | otherwise
  = (colnum, [], [], cells)
 where
  getColSpan (B.ColSpan x) = x
  getDim (B.Cell _ _ h w _) = (h, w)
  setW w (B.Cell a b h _ c) = B.Cell a b h w c

-- | In @'splitCellHang' rs cs coldata@, with @rs@ the height of a
-- cell that lies at the beginning of @coldata@, and @cs@ its width
-- (which is not assumed to fit in the available space), return the
-- actual width of the cell (what will fit in the available space),
-- the data for the columns that the cell spans (including updating
-- the overhang to equal @rs@), and the remaining column data.
splitCellHang
  :: B.RowSpan
  -> B.ColSpan
  -> [(B.RowSpan, B.ColSpec)]
  -> (B.ColSpan, [(B.RowSpan, B.ColSpec)], [(B.RowSpan, B.ColSpec)])
splitCellHang h n = go 0
 where
  go acc ((1, spec) : ls) | acc < n =
    let (acc', hang, ls') = go (acc + 1) ls in (acc', (h, spec) : hang, ls')
  go acc l = (acc, [], l)

-- | Convert an annotated 'Table' to a Pandoc
-- 'B.Table'. This is the inverse of 'toTable' on
-- well-formed tables (i.e. tables satisfying the guarantees of
-- 'B.table').
fromTable
  :: Table
  -> ( B.Attr
     , B.Caption
     , [B.ColSpec]
     , B.TableHead
     , [B.TableBody]
     , B.TableFoot
     )
fromTable (Table attr cap cs th tbs tf) = (attr, cap, cs, th', tbs', tf')
 where
  th'  = fromTableHead th
  tbs' = map fromTableBody tbs
  tf'  = fromTableFoot tf

fromTableHead :: TableHead -> B.TableHead
fromTableHead (TableHead attr rows) = B.TableHead attr $ fromHeaderRow <$> rows

fromTableBody :: TableBody -> B.TableBody
fromTableBody (TableBody attr rhc th tb) =
  B.TableBody attr rhc (fromHeaderRow <$> th) (fromBodyRow <$> tb)

fromTableFoot :: TableFoot -> B.TableFoot
fromTableFoot (TableFoot attr rows) = B.TableFoot attr $ fromHeaderRow <$> rows

fromHeaderRow :: HeaderRow -> B.Row
fromHeaderRow (HeaderRow attr _ cells) = B.Row attr $ fromCell <$> cells

fromBodyRow :: BodyRow -> B.Row
fromBodyRow (BodyRow attr _ rh rb) =
  B.Row attr ((fromCell <$> rh) <> (fromCell <$> rb))

fromCell :: Cell -> B.Cell
fromCell (Cell _ _ c) = c

--
-- Instances
--
instance Walkable a B.Cell => Walkable a Cell where
  walkM f (Cell colspecs colnum cell) =
    Cell colspecs colnum <$> walkM f cell
  query f (Cell _colspecs _colnum cell) = query f cell

instance Walkable a B.Cell => Walkable a HeaderRow where
  walkM f (HeaderRow attr rownum cells) =
    HeaderRow attr rownum <$> walkM f cells
  query f (HeaderRow _attr _rownum cells) = query f cells

instance Walkable a B.Cell => Walkable a TableHead where
  walkM f (TableHead attr rows) =
    TableHead attr <$> walkM f rows
  query f (TableHead _attr rows) = query f rows
