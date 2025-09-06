{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{- |
   Module      : Text.Pandoc.Parsing.GridTable
   Copyright   : Copyright (C) 2006-2024 John MacFarlane
   License     : GPL-2.0-or-later
   Maintainer  : John MacFarlane <jgm@berkeley.edu>

Shared parsers for plaintext tables.
-}
module Text.Pandoc.Parsing.GridTable
  ( gridTableWith
  , gridTableWith'
  , tableWith
  , tableWith'
  , tableWithSpans
  , tableWithSpans'
  , widthsFromIndices
    -- * Components of a plain-text table
  , TableComponents (..)
  , TableNormalization (..)
  , toTableComponents
  , toTableComponents'
  , toTableComponentsWithSpans
  , toTableComponentsWithSpans'
  , singleRowSpans
  , singleColumnSpans
  )
where

import Data.Array (elems)
import Data.Text (Text)
import Safe (lastDef)
import Text.Pandoc.Options (ReaderOptions (readerColumns))
import Text.Pandoc.Builder (Blocks)
import Text.Pandoc.Definition
import Text.Pandoc.Parsing.Capabilities
import Text.Pandoc.Parsing.General
import Text.Pandoc.Sources
import Text.Parsec (Stream (..), ParsecT, optional, sepEndBy1, try)

import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Text.GridTable as GT
import qualified Text.Pandoc.Builder as B

-- | Collection of components making up a Table block.
data TableComponents = TableComponents
  { tableAttr     :: Attr
  , tableCaption  :: Caption
  , tableColSpecs :: [ColSpec]
  , tableHead     :: TableHead
  , tableBodies   :: [TableBody]
  , tableFoot     :: TableFoot
  }

-- | Creates a table block from the collection of table parts.
tableFromComponents :: TableComponents -> Blocks
tableFromComponents (TableComponents attr capt colspecs th tb tf) =
  B.tableWith attr capt colspecs th tb tf

-- | Bundles basic table components into a single value.
toTableComponents :: [Alignment] -> [Double] -> [[Blocks]] -> [[Blocks]]
                  -> TableComponents
toTableComponents = toTableComponents' NoNormalization

-- | Bundles basic table components into a single value, performing
-- normalizations as necessary.
toTableComponents' :: TableNormalization
                   -> [Alignment] -> [Double] -> [[Blocks]] -> [[Blocks]]
                   -> TableComponents
toTableComponents' normalization aligns widths heads rows =
  toTableComponentsWithSpans' normalization aligns widths (singleSpansBlocks heads) (singleSpansBlocks rows)

-- | Bundles basic table components with span information into a single value.
toTableComponentsWithSpans :: [Alignment] -> [Double] -> [[(Blocks, RowSpan, ColSpan)]] -> [[(Blocks, RowSpan, ColSpan)]]
                           -> TableComponents
toTableComponentsWithSpans = toTableComponentsWithSpans' NoNormalization

-- | Bundles basic table components with span information into a single value,
-- performing normalizations as necessary.
toTableComponentsWithSpans' :: TableNormalization
                            -> [Alignment] -> [Double] -> [[(Blocks, RowSpan, ColSpan)]] -> [[(Blocks, RowSpan, ColSpan)]]
                            -> TableComponents
toTableComponentsWithSpans' normalization aligns widths heads rows =
  let th = TableHead nullAttr (mapMaybe (toHeaderRow normalization) heads)
      tb = TableBody nullAttr 0 [] (map toRow rows)
      tf = TableFoot nullAttr []
      colspecs = toColSpecs aligns widths
  in TableComponents nullAttr B.emptyCaption colspecs th [tb] tf

-- | Combine a list of column alignments and column widths into a list
-- of column specifiers. Both input lists should have the same length.
toColSpecs :: [Alignment]   -- ^ column alignments
           -> [Double]      -- ^ column widths
           -> [ColSpec]
toColSpecs aligns widths = zip aligns (map fromWidth widths')
  where
    fromWidth n
      | n > 0     = ColWidth n
      | otherwise = ColWidthDefault

    -- renormalize widths if greater than 100%:
    totalWidth = sum widths
    widths' = if totalWidth < 1
              then widths
              else map (/ totalWidth) widths

-- | Whether the table header should be normalized, i.e., whether an header row
-- with only empty cells should be omitted.
data TableNormalization
  = NoNormalization
  | NormalizeHeader

--
-- Grid Tables
--

-- | Parse a grid table: starts with row of '-' on top, then header
-- (which may be grid), then the rows, which may be grid, separated by
-- blank lines, and ending with a footer (dashed line followed by blank
-- line).
gridTableWith :: (Monad m, Monad mf, HasLastStrPosition st, HasReaderOptions st)
              => ParsecT Sources st m (mf Blocks)  -- ^ Block list parser
              -> ParsecT Sources st m (mf Blocks)
gridTableWith blocks = fmap tableFromComponents <$>
  gridTableWith' NoNormalization blocks

-- | Like @'gridTableWith'@, but returns 'TableComponents' instead of a
-- Table.
gridTableWith' :: (Monad m, Monad mf,
                   HasReaderOptions st, HasLastStrPosition st)
               => TableNormalization
               -> ParsecT Sources st m (mf Blocks) -- ^ Block list parser
               -> ParsecT Sources st m (mf TableComponents)
gridTableWith' normalization blocks = do
  tbl <- GT.gridTable <* optional blanklines
  let blkTbl = GT.mapCells
               (\lns -> parseFromString' blocks
                        . flip T.snoc '\n'  -- ensure proper block parsing
                        . T.unlines
                        . removeOneLeadingSpace
                        $ map T.stripEnd lns)
               tbl
  let rows = GT.rows blkTbl
  let toPandocCell (GT.Cell c (GT.RowSpan rs) (GT.ColSpan cs)) =
        fmap (B.cell AlignDefault (B.RowSpan rs) (B.ColSpan cs) . plainify) <$> c
  rows' <- mapM (mapM toPandocCell) rows
  columns <- getOption readerColumns
  let colspecs = zipWith (\cs w -> (convAlign $ fst cs, B.ColWidth w))
                         (elems $ GT.arrayTableColSpecs tbl)
                         (fractionalColumnWidths tbl columns)
  let caption = B.emptyCaption
  return $ do
    rows'' <- mapM sequence rows'
    let headLen = maybe 0 GT.fromRowIndex $ GT.arrayTableHead tbl
    let (hRows, bRows') =
          splitAt headLen (map (B.Row B.nullAttr) rows'')
    let (bRows, fRows) =
          case GT.arrayTableFoot tbl of
            Just fIdx -> splitAt (GT.fromRowIndex fIdx - headLen - 1) bRows'
            Nothing   -> (bRows', [])
    let thead = B.TableHead B.nullAttr $ case (hRows, normalization) of
          -- normalize header if necessary: remove header if it contains
          -- only a single row in which all cells are empty.
          ([hrow], NormalizeHeader) ->
            let Row _attr cells = hrow
                simple = \case
                  Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1) [] ->
                    True
                  _ ->
                    False
            in [B.Row nullAttr cells | not (null cells) &&
                                       not (all simple cells)]
          _ -> hRows
    let tfoot = B.TableFoot B.nullAttr fRows
    let tbody = B.TableBody B.nullAttr 0 [] bRows
    return $ TableComponents nullAttr caption colspecs thead [tbody] tfoot

removeOneLeadingSpace :: [Text] -> [Text]
removeOneLeadingSpace xs =
  if all startsWithSpace xs
     then map (T.drop 1) xs
     else xs
   where startsWithSpace t = case T.uncons t of
           Nothing     -> True
           Just (c, _) -> c == ' '

plainify :: B.Blocks -> B.Blocks
plainify blks = case B.toList blks of
  [Para x] -> B.fromList [Plain x]
  _        -> blks

convAlign :: GT.Alignment -> B.Alignment
convAlign GT.AlignLeft    = B.AlignLeft
convAlign GT.AlignRight   = B.AlignRight
convAlign GT.AlignCenter  = B.AlignCenter
convAlign GT.AlignDefault = B.AlignDefault

fractionalColumnWidths :: GT.ArrayTable a -> Int -> [Double]
fractionalColumnWidths gt charColumns =
  let widths = map ((+1) . snd) -- include width of separator
               (elems $ GT.arrayTableColSpecs gt)
      norm = fromIntegral $ max (sum widths + length widths - 2) charColumns
  in map (\w -> fromIntegral w / norm) widths

---

-- | Parse a table using 'headerParser', 'rowParser',
-- 'lineParser', and 'footerParser'.
tableWith :: (Stream s m Char, UpdateSourcePos s Char,
              HasReaderOptions st, Monad mf)
          => ParsecT s st m (mf [[Blocks]], [Alignment], [Int]) -- ^ header parser
          -> ([Int] -> ParsecT s st m (mf [Blocks]))  -- ^ row parser
          -> ParsecT s st m sep                       -- ^ line parser
          -> ParsecT s st m end                       -- ^ footer parser
          -> ParsecT s st m (mf Blocks)
tableWith hp rp lp fp = fmap tableFromComponents <$>
  tableWith' NoNormalization hp rp lp fp

tableWith' :: (Stream s m Char, UpdateSourcePos s Char,
               HasReaderOptions st, Monad mf)
           => TableNormalization
           -> ParsecT s st m (mf [[Blocks]], [Alignment], [Int]) -- ^ header parser
           -> ([Int] -> ParsecT s st m (mf [Blocks]))  -- ^ row parser
           -> ParsecT s st m sep                       -- ^ line parser
           -> ParsecT s st m end                       -- ^ footer parser
           -> ParsecT s st m (mf TableComponents)
tableWith' n11n headerParser rowParser lineParser footerParser =
  tableWithSpans' n11n headerParser' rowParser' lineParser footerParser
    where
      headerParser' = fmap (\(heads, aligns, indices) -> (fmap singleSpansBlocks heads, aligns, indices)) headerParser
      rowParser' indices = fmap (\row -> zip3 row (repeat 1) (repeat 1)) <$> rowParser indices

tableWithSpans :: (Stream s m Char, UpdateSourcePos s Char,
              HasReaderOptions st, Monad mf)
               => ParsecT s st m (mf [[(Blocks, RowSpan, ColSpan)]], [Alignment], [Int]) -- ^ header parser
               -> ([Int] -> ParsecT s st m (mf [(Blocks, RowSpan, ColSpan)]))  -- ^ row parser
               -> ParsecT s st m sep                       -- ^ line parser
               -> ParsecT s st m end                       -- ^ footer parser
               -> ParsecT s st m (mf Blocks)
tableWithSpans hp rp lp fp = fmap tableFromComponents <$>
  tableWithSpans' NoNormalization hp rp lp fp

tableWithSpans' :: (Stream s m Char, UpdateSourcePos s Char,
               HasReaderOptions st, Monad mf)
                => TableNormalization
                -> ParsecT s st m (mf [[(Blocks, RowSpan, ColSpan)]], [Alignment], [Int]) -- ^ header parser
                -> ([Int] -> ParsecT s st m (mf [(Blocks, RowSpan, ColSpan)]))  -- ^ row parser
                -> ParsecT s st m sep                       -- ^ line parser
                -> ParsecT s st m end                       -- ^ footer parser
                -> ParsecT s st m (mf TableComponents)
tableWithSpans' n11n headerParser rowParser lineParser footerParser = try $ do
  (heads, aligns, indices) <- headerParser
  lines' <- sequence <$> rowParser indices `sepEndBy1` lineParser
  footerParser
  numColumns <- getOption readerColumns
  let widths = if null indices
               then replicate (length aligns) 0.0
               else widthsFromIndices numColumns indices
  return $ toTableComponentsWithSpans' n11n aligns widths <$> heads <*> lines'

toRow :: [(Blocks, RowSpan, ColSpan)] -> Row
toRow =  Row nullAttr . map (\(blocks, rowSpan, columnSpan) -> B.cell AlignDefault rowSpan columnSpan blocks)

toHeaderRow :: TableNormalization -> [(Blocks, RowSpan, ColSpan)] -> Maybe Row
toHeaderRow = \case
  NoNormalization -> \l -> if not (null l) then Just (toRow l) else Nothing
  NormalizeHeader -> \l -> if not (all nullHeaderRow l) then Just (toRow l) else Nothing
  where
    nullHeaderRow (l, _, _) = null l

-- | Calculate relative widths of table columns, based on indices
widthsFromIndices :: Int      -- Number of columns on terminal
                  -> [Int]    -- Indices
                  -> [Double] -- Fractional relative sizes of columns
widthsFromIndices _ [] = []
widthsFromIndices numColumns' indices =
  let numColumns = max numColumns' (lastDef 0 indices)
      lengths' = zipWith (-) indices (0:indices)
      lengths  = reverse $
                 case reverse lengths' of
                      []       -> []
                      [x]      -> [x]
                      -- compensate for the fact that intercolumn
                      -- spaces are counted in widths of all columns
                      -- but the last...
                      (x:y:zs) -> if x < y && y - x <= 2
                                     then y:y:zs
                                     else x:y:zs
      totLength = sum lengths
      quotient = if totLength > numColumns
                   then fromIntegral totLength
                   else fromIntegral numColumns
      fracs = map (\l -> fromIntegral l / quotient) lengths in
  drop 1 fracs

-- | List of lists of `RowSpan` of 1.
singleRowSpans :: [[RowSpan]]
singleRowSpans = repeat $ repeat 1

-- | List of lists of `ColsSpan` of 1.
singleColumnSpans :: [[ColSpan]]
singleColumnSpans = repeat $ repeat 1

singleSpansBlocks :: [[Blocks]] -> [[(Blocks, RowSpan, ColSpan)]]
singleSpansBlocks blocks = zipWith3 zip3 blocks singleRowSpans singleColumnSpans
