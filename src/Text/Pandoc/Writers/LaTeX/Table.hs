{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.LaTeX.Table
   Copyright   : Copyright (C) 2006-2022 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Output LaTeX formatted tables.
-}
module Text.Pandoc.Writers.LaTeX.Table
  ( tableToLaTeX
  ) where
import Control.Monad.State.Strict
import Data.List (intersperse)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.DocLayout
  ( Doc, braces, cr, empty, hcat, hsep, isEmpty, literal, nest
  , text, vcat, ($$) )
import Text.Pandoc.Shared (blocksToInlines, splitBy, tshow)
import Text.Pandoc.Walk (walk, query)
import Data.Monoid (Any(..))
import Text.Pandoc.Writers.LaTeX.Caption (getCaption)
import Text.Pandoc.Writers.LaTeX.Notes (notesToLaTeX)
import Text.Pandoc.Writers.LaTeX.Types
  ( LW, WriterState (stBeamer, stExternalNotes, stInMinipage, stMultiRow
                    , stNotes, stTable) )
import Text.Pandoc.Writers.LaTeX.Util (labelFor)
import Text.Printf (printf)
import qualified Text.Pandoc.Builder as B
import qualified Text.Pandoc.Writers.AnnotatedTable as Ann

tableToLaTeX :: PandocMonad m
             => ([Inline] -> LW m (Doc Text))
             -> ([Block]  -> LW m (Doc Text))
             -> Ann.Table
             -> LW m (Doc Text)
tableToLaTeX inlnsToLaTeX blksToLaTeX tbl = do
  let (Ann.Table (ident, _, _) caption specs thead tbodies tfoot) = tbl
  CaptionDocs capt captNotes <- captionToLaTeX inlnsToLaTeX caption ident
  let removeNote (Note _) = Span ("", [], []) []
      removeNote x        = x
  let colCount = ColumnCount $ length specs
  -- The first head is not repeated on the following pages. If we were to just
  -- use a single head, without a separate first head, then the caption would be
  -- repeated on all pages that contain a part of the table. We avoid this by
  -- making the caption part of the first head. The downside is that we must
  -- duplicate the header rows for this.
  head' <- do
    let mkHead = headToLaTeX blksToLaTeX colCount
    case (not $ isEmpty capt, not $ isEmptyHead thead) of
      (False, False) -> return "\\toprule()"
      (False, True)  -> mkHead thead
      (True, False)  -> return (capt $$ "\\toprule()" $$ "\\endfirsthead")
      (True, True)   -> do
        -- avoid duplicate notes in head and firsthead:
        firsthead <- mkHead thead
        repeated  <- mkHead (walk removeNote thead)
        return $ capt $$ firsthead $$ "\\endfirsthead" $$ repeated
  rows' <- mapM (rowToLaTeX blksToLaTeX colCount BodyCell) $
                mconcat (map bodyRows tbodies) <> footRows tfoot
  modify $ \s -> s{ stTable = True }
  notes <- notesToLaTeX <$> gets stNotes
  return
    $  "\\begin{longtable}[]" <>
          braces ("@{}" <> colDescriptors tbl <> "@{}")
          -- the @{} removes extra space at beginning and end
    $$ head'
    $$ "\\endhead"
    $$ vcat rows'
    $$ "\\bottomrule()"
    $$ "\\end{longtable}"
    $$ captNotes
    $$ notes

-- | Total number of columns in a table.
newtype ColumnCount = ColumnCount Int

-- | Creates column descriptors for the table.
colDescriptors :: Ann.Table -> Doc Text
colDescriptors (Ann.Table _attr _caption specs thead tbodies tfoot) =
  let (aligns, widths) = unzip specs

      defaultWidthsOnly = all (== ColWidthDefault) widths
      isSimpleTable = all (all isSimpleCell) $ mconcat
                      [ headRows thead
                      , concatMap bodyRows tbodies
                      , footRows tfoot
                      ]

      relativeWidths = if defaultWidthsOnly
                       then replicate (length specs)
                            (1 / fromIntegral (length specs))
                       else map toRelWidth widths
  in if defaultWidthsOnly && isSimpleTable
     then hcat $ map (literal . colAlign) aligns
     else (cr <>) . nest 2 . vcat . map literal $
          zipWith (toColDescriptor (length specs))
                  aligns
                  relativeWidths
  where
    toColDescriptor :: Int -> Alignment -> Double -> Text
    toColDescriptor numcols align width =
      T.pack $ printf
      ">{%s\\arraybackslash}p{(\\columnwidth - %d\\tabcolsep) * \\real{%0.4f}}"
      (T.unpack (alignCommand align))
      ((numcols - 1) * 2)
      width

    isSimpleCell (Ann.Cell _ _ (Cell _attr _align _rowspan _colspan blocks)) =
      case blocks of
        [Para _]  -> True
        [Plain _] -> True
        []        -> True
        _         -> False

    toRelWidth ColWidthDefault = 0
    toRelWidth (ColWidth w)    = w

alignCommand :: Alignment -> Text
alignCommand = \case
  AlignLeft    -> "\\raggedright"
  AlignRight   -> "\\raggedleft"
  AlignCenter  -> "\\centering"
  AlignDefault -> "\\raggedright"

colAlign :: Alignment -> Text
colAlign = \case
  AlignLeft    -> "l"
  AlignRight   -> "r"
  AlignCenter  -> "c"
  AlignDefault -> "l"

data CaptionDocs =
  CaptionDocs
  { captionCommand :: Doc Text
  , captionNotes :: Doc Text
  }

captionToLaTeX :: PandocMonad m
               => ([Inline] -> LW m (Doc Text))
               -> Caption
               -> Text     -- ^ table identifier (label)
               -> LW m CaptionDocs
captionToLaTeX inlnsToLaTeX (Caption _maybeShort longCaption) ident = do
  let caption = blocksToInlines longCaption
  (captionText, captForLot, captNotes) <- getCaption inlnsToLaTeX False caption
  label <- labelFor ident
  return $ CaptionDocs
    { captionNotes = captNotes
    , captionCommand = if isEmpty captionText && isEmpty label
                       then empty
                       else "\\caption" <> captForLot <>
                            braces captionText
                            <> label
                            <> "\\tabularnewline"
    }

type BlocksWriter m = [Block] -> LW m (Doc Text)

headToLaTeX :: PandocMonad m
            => BlocksWriter m
            -> ColumnCount
            -> Ann.TableHead
            -> LW m (Doc Text)
headToLaTeX blocksWriter colCount (Ann.TableHead _attr headerRows) = do
  rowsContents <-
    mapM (rowToLaTeX blocksWriter colCount HeaderCell . headerRowCells)
         headerRows
  return ("\\toprule()" $$ vcat rowsContents $$ "\\midrule()")

-- | Converts a row of table cells into a LaTeX row.
rowToLaTeX :: PandocMonad m
           => BlocksWriter m
           -> ColumnCount
           -> CellType
           -> [Ann.Cell]
           -> LW m (Doc Text)
rowToLaTeX blocksWriter colCount celltype row = do
  cellsDocs <- mapM (cellToLaTeX blocksWriter colCount celltype) (fillRow row)
  return $ hsep (intersperse "&" cellsDocs) <> " \\\\"

-- | Pads row with empty cells to adjust for rowspans above this row.
fillRow :: [Ann.Cell] -> [Ann.Cell]
fillRow = go 0
  where
    go _ [] = []
    go n (acell@(Ann.Cell _spec (Ann.ColNumber colnum) cell):cells) =
      let (Cell _ _ _ (ColSpan colspan) _) = cell
      in map mkEmptyCell [n .. colnum - 1] ++
         acell : go (colnum + colspan) cells

    mkEmptyCell :: Int -> Ann.Cell
    mkEmptyCell colnum =
      Ann.Cell ((AlignDefault, ColWidthDefault):|[])
               (Ann.ColNumber colnum)
               B.emptyCell

isEmptyHead :: Ann.TableHead -> Bool
isEmptyHead (Ann.TableHead _attr []) = True
isEmptyHead (Ann.TableHead _attr rows) = all (null . headerRowCells) rows

-- | Gets all cells in a header row.
headerRowCells :: Ann.HeaderRow -> [Ann.Cell]
headerRowCells (Ann.HeaderRow _attr _rownum cells) = cells

-- | Gets all cells in a body row.
bodyRowCells :: Ann.BodyRow -> [Ann.Cell]
bodyRowCells (Ann.BodyRow _attr _rownum rowhead cells) = rowhead <> cells

-- | Gets a list of rows of the table body, where a row is a simple
-- list of cells.
bodyRows :: Ann.TableBody -> [[Ann.Cell]]
bodyRows (Ann.TableBody _attr _rowheads headerRows rows) =
  map headerRowCells headerRows <> map bodyRowCells rows

-- | Gets a list of rows of the table head, where a row is a simple
-- list of cells.
headRows :: Ann.TableHead -> [[Ann.Cell]]
headRows (Ann.TableHead _attr rows) = map headerRowCells rows

-- | Gets a list of rows from the foot, where a row is a simple list
-- of cells.
footRows :: Ann.TableFoot -> [[Ann.Cell]]
footRows (Ann.TableFoot _attr rows) = map headerRowCells rows

-- For simple latex tables (without minipages or parboxes),
-- we need to go to some lengths to get line breaks working:
-- as LineBreak bs = \vtop{\hbox{\strut as}\hbox{\strut bs}}.
fixLineBreaks :: Block -> Block
fixLineBreaks = walk fixLineBreaks'

fixLineBreaks' :: [Inline] -> [Inline]
fixLineBreaks' ils = case splitBy (== LineBreak) ils of
                       []     -> []
                       [xs]   -> xs
                       chunks -> RawInline "tex" "\\vtop{" :
                                 concatMap tohbox chunks <>
                                 [RawInline "tex" "}"]
  where tohbox ys = RawInline "tex" "\\hbox{\\strut " : ys <>
                    [RawInline "tex" "}"]

-- We also change display math to inline math, since display
-- math breaks in simple tables.
displayMathToInline :: Inline -> Inline
displayMathToInline (Math DisplayMath x) = Math InlineMath x
displayMathToInline x                    = x

cellToLaTeX :: PandocMonad m
            => BlocksWriter m
            -> ColumnCount
            -> CellType
            -> Ann.Cell
            -> LW m (Doc Text)
cellToLaTeX blockListToLaTeX colCount celltype annotatedCell = do
  let (Ann.Cell specs colnum cell) = annotatedCell
  let colWidths = NonEmpty.map snd specs
  let hasWidths = NonEmpty.head colWidths /= ColWidthDefault
  let specAlign = fst (NonEmpty.head specs)
  let (Cell _attr align' rowspan colspan blocks) = cell
  let align = case align' of
                AlignDefault -> specAlign
                _            -> align'
  beamer <- gets stBeamer
  externalNotes <- gets stExternalNotes
  -- See #5367 -- footnotehyper/footnote don't work in beamer,
  -- so we need to produce the notes outside the table...
  modify $ \st -> st{ stExternalNotes = beamer }
  let isPlainOrPara = \case
        Para{}  -> True
        Plain{} -> True
        _       -> False
  let hasLineBreak LineBreak = Any True
      hasLineBreak _ = Any False
  let hasLineBreaks = getAny $ query hasLineBreak blocks
  result <-
    if not hasWidths || (celltype /= HeaderCell
                           && all isPlainOrPara blocks
                           && not hasLineBreaks)
       then
         blockListToLaTeX $ walk fixLineBreaks $ walk displayMathToInline blocks
       else do
         cellContents <- inMinipage $ blockListToLaTeX blocks
         let valign = text $ case celltype of
                               HeaderCell -> "[b]"
                               BodyCell   -> "[t]"
         let halign = literal $ alignCommand align
         return $ "\\begin{minipage}" <> valign <>
                  braces "\\linewidth" <> halign <> cr <>
                  cellContents <>
                  (if hasLineBreaks then "\\strut" else mempty)
                  <> cr <>
                  "\\end{minipage}"
  modify $ \st -> st{ stExternalNotes = externalNotes }
  when (rowspan /= RowSpan 1) $
    modify (\st -> st{ stMultiRow = True })
  let inMultiColumn x = case colspan of
                          (ColSpan 1) -> x
                          (ColSpan n) ->
                            let colDescr = multicolumnDescriptor align
                                                                 colWidths
                                                                 colCount
                                                                 colnum
                            in "\\multicolumn"
                               <> braces (literal (tshow n))
                               <> braces (literal colDescr)
                               <> braces ("%\n" <> x)
                                  -- linebreak for readability
  let inMultiRow x = case rowspan of
                       (RowSpan 1) -> x
                       (RowSpan n) -> let nrows = literal (tshow n)
                                      in "\\multirow" <> braces nrows
                                         <> braces "*" <> braces x
  return . inMultiColumn . inMultiRow $ result

-- | Returns the width of a cell spanning @n@ columns.
multicolumnDescriptor :: Alignment
                      -> NonEmpty ColWidth
                      -> ColumnCount
                      -> Ann.ColNumber
                      -> Text
multicolumnDescriptor align
  colWidths
  (ColumnCount numcols)
  (Ann.ColNumber colnum) =
  let toWidth = \case
        ColWidthDefault -> 0
        ColWidth x      -> x
      colspan = length colWidths
      width = sum $ NonEmpty.map toWidth colWidths

      -- no column separators at beginning of first and end of last column.
      skipColSep = "@{}" :: String
  in T.pack $
     printf "%s>{%s\\arraybackslash}p{(\\columnwidth - %d\\tabcolsep) * \\real{%0.4f} + %d\\tabcolsep}%s"
            (if colnum == 0 then skipColSep else "")
            (T.unpack (alignCommand align))
            (2 * (numcols - 1))
            width
            (2 * (colspan - 1))
            (if colnum + colspan >= numcols then skipColSep else "")

-- | Perform a conversion, assuming that the context is a minipage.
inMinipage :: Monad m => LW m a -> LW m a
inMinipage action = do
  isInMinipage <- gets stInMinipage
  modify $ \st -> st{ stInMinipage = True }
  result <- action
  modify $ \st -> st{ stInMinipage = isInMinipage }
  return result

data CellType
  = HeaderCell
  | BodyCell
  deriving Eq
