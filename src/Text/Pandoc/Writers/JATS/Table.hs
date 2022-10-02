{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{- |
   Module      : Text.Pandoc.Writers.JATS.Table
   Copyright   : © 2020-2022 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' tables to JATS XML.
-}
module Text.Pandoc.Writers.JATS.Table
  ( tableToJATS
  ) where
import Control.Monad.Reader (asks)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Text.DocLayout (Doc, empty, vcat, ($$))
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options (WriterOptions)
import Text.Pandoc.Shared (tshow)
import Text.Pandoc.Writers.JATS.Types
import Text.Pandoc.XML (escapeNCName, inTags, inTagsIndented, selfClosingTag)
import qualified Data.Text as T
import qualified Text.Pandoc.Writers.AnnotatedTable as Ann

tableToJATS :: PandocMonad m
            => WriterOptions
            -> Ann.Table
            -> JATS m (Doc Text)
tableToJATS opts (Ann.Table attr caption colspecs thead tbodies tfoot) = do
  let (Caption _maybeShortCaption captionBlocks) = caption
  -- Only paragraphs are allowed in captions, all other blocks must be
  -- wrapped in @<p>@ elements.
  let needsWrapping = \case
        Plain{} -> False
        Para{}  -> False
        _       -> True
  tbl <- captionlessTable opts attr colspecs thead tbodies tfoot
  captionDoc <- if null captionBlocks
                then return empty
                else do
                  blockToJATS <- asks jatsBlockWriter
                  inTagsIndented "caption" <$>
                    blockToJATS needsWrapping opts captionBlocks
  return $ inTags True "table-wrap" [] $ captionDoc $$ tbl

captionlessTable :: PandocMonad m
                 => WriterOptions
                 -> Attr
                 -> [ColSpec]
                 -> Ann.TableHead
                 -> [Ann.TableBody]
                 -> Ann.TableFoot
                 -> JATS m (Doc Text)
captionlessTable opts attr colspecs thead tbodies tfoot = do
  head' <- tableHeadToJats opts thead
  bodies <- mapM (tableBodyToJats opts) tbodies
  foot' <- tableFootToJats opts tfoot
  let validAttribs = [ "border", "cellpadding", "cellspacing", "content-type"
                     , "frame", "rules", "specific-use", "style", "summary"
                     , "width"
                     ]
  let attribs = toAttribs attr validAttribs
  return $ inTags True "table" attribs $ vcat
    [ colSpecListToJATS colspecs
    , head'
    , foot'
    , vcat bodies
    ]

validTablePartAttribs :: [Text]
validTablePartAttribs =
  [ "align", "char", "charoff", "content-type", "style", "valign" ]

tableBodyToJats :: PandocMonad m
                => WriterOptions
                -> Ann.TableBody
                -> JATS m (Doc Text)
tableBodyToJats opts (Ann.TableBody attr _rowHeadCols inthead rows) = do
  let attribs = toAttribs attr validTablePartAttribs
  intermediateHead <- if null inthead
                      then return mempty
                      else headerRowsToJats opts Thead inthead
  bodyRows <- bodyRowsToJats opts rows
  return $ inTags True "tbody" attribs $ intermediateHead $$ bodyRows

tableHeadToJats :: PandocMonad m
                => WriterOptions
                -> Ann.TableHead
                -> JATS m (Doc Text)
tableHeadToJats opts (Ann.TableHead attr rows) =
  tablePartToJats opts Thead attr rows

tableFootToJats :: PandocMonad m
                => WriterOptions
                -> Ann.TableFoot
                -> JATS m (Doc Text)
tableFootToJats opts (Ann.TableFoot attr rows) =
  tablePartToJats opts Tfoot attr rows

tablePartToJats :: PandocMonad m
                => WriterOptions
                -> TablePart
                -> Attr
                -> [Ann.HeaderRow]
                -> JATS m (Doc Text)
tablePartToJats opts tblpart attr rows =
  if null rows || all isEmptyRow rows
  then return mempty
  else do
    let tag' = case tblpart of
                 Thead -> "thead"
                 Tfoot -> "tfoot"
                 Tbody -> "tbody" -- this would be unexpected
    let attribs = toAttribs attr validTablePartAttribs
    inTags True tag' attribs <$> headerRowsToJats opts tblpart rows
  where
    isEmptyRow (Ann.HeaderRow _attr _rownum cells) = all isEmptyCell cells
    isEmptyCell (Ann.Cell _colspecs _colnum cell) =
      cell == Cell nullAttr AlignDefault (RowSpan 1) (ColSpan 1) []

-- | The part of a table; header, footer, or body.
data TablePart = Thead | Tfoot | Tbody
  deriving (Eq)

data CellType = HeaderCell | BodyCell

data TableRow = TableRow TablePart Attr Ann.RowNumber Ann.RowHead Ann.RowBody

headerRowsToJats :: PandocMonad m
                 => WriterOptions
                 -> TablePart
                 -> [Ann.HeaderRow]
                 -> JATS m (Doc Text)
headerRowsToJats opts tablepart =
  rowListToJats opts . map toTableRow
  where
    toTableRow (Ann.HeaderRow attr rownum rowbody) =
      TableRow tablepart attr rownum [] rowbody

bodyRowsToJats :: PandocMonad m
               => WriterOptions
               -> [Ann.BodyRow]
               -> JATS m (Doc Text)
bodyRowsToJats opts =
  rowListToJats opts . zipWith toTableRow [1..]
  where
    toTableRow rownum (Ann.BodyRow attr _rownum rowhead rowbody) =
      TableRow Tbody attr rownum rowhead rowbody

rowListToJats :: PandocMonad m
              => WriterOptions
              -> [TableRow]
              -> JATS m (Doc Text)
rowListToJats opts = fmap vcat . mapM (tableRowToJats opts)

colSpecListToJATS :: [ColSpec] -> Doc Text
colSpecListToJATS colspecs =
  let hasDefaultWidth (_, ColWidthDefault) = True
      hasDefaultWidth _                    = False

      percent w = tshow (round (100*w) :: Integer) <> "%"

      col :: ColWidth -> Doc Text
      col = selfClosingTag "col" . \case
        ColWidthDefault -> mempty
        ColWidth w -> [("width", percent w)]

   in if all hasDefaultWidth colspecs
      then mempty
      else inTags True "colgroup" [] $ vcat $ map (col . snd) colspecs

tableRowToJats :: PandocMonad m
               => WriterOptions
               -> TableRow
               -> JATS m (Doc Text)
tableRowToJats opts (TableRow tblpart attr _rownum rowhead rowbody) = do
  let validAttribs = [ "align", "char", "charoff", "content-type"
                     , "style", "valign"
                     ]
  let attr' = toAttribs attr validAttribs
  let celltype = case tblpart of
                   Thead -> HeaderCell
                   _     -> BodyCell
  headcells <- mapM (cellToJats opts HeaderCell) rowhead
  bodycells <- mapM (cellToJats opts celltype) rowbody
  return $ inTags True "tr" attr' $ mconcat
    [ vcat headcells
    , vcat bodycells
    ]

alignmentAttrib :: Alignment -> Maybe (Text, Text)
alignmentAttrib = fmap ("align",) . \case
  AlignLeft    -> Just "left"
  AlignRight   -> Just "right"
  AlignCenter  -> Just "center"
  AlignDefault -> Nothing

colspanAttrib :: ColSpan -> Maybe (Text, Text)
colspanAttrib = \case
  ColSpan 1 -> Nothing
  ColSpan n -> Just ("colspan", tshow n)

rowspanAttrib :: RowSpan -> Maybe (Text, Text)
rowspanAttrib = \case
  RowSpan 1 -> Nothing
  RowSpan n -> Just ("rowspan", tshow n)

cellToJats :: PandocMonad m
           => WriterOptions
           -> CellType
           -> Ann.Cell
           -> JATS m (Doc Text)
cellToJats opts celltype (Ann.Cell (colspec :| _) _colNum cell) =
  let align = fst colspec
  in tableCellToJats opts celltype align cell

toAttribs :: Attr -> [Text] -> [(Text, Text)]
toAttribs (ident, _classes, kvs) knownAttribs =
  (if T.null ident then id else (("id", escapeNCName ident) :)) $
  filter ((`elem` knownAttribs) . fst) kvs

tableCellToJats :: PandocMonad m
                => WriterOptions
                -> CellType
                -> Alignment
                -> Cell
                -> JATS m (Doc Text)
tableCellToJats opts ctype colAlign (Cell attr align rowspan colspan item) = do
  blockToJats   <- asks jatsBlockWriter
  inlinesToJats <- asks jatsInlinesWriter
  let fixBreak LineBreak = RawInline (Format "jats") "<break/>"
      fixBreak x         = x
  let cellContents = \case
        [Plain inlines] -> inlinesToJats opts
                             (map fixBreak inlines)
                             -- Note: <break/> is allowed only as a direct
                             -- child of <td>, so we don't use walk.
        blocks          -> blockToJats needsWrapInCell opts blocks
  let tag' = case ctype of
        BodyCell   -> "td"
        HeaderCell -> "th"
  let align' = case align of
        AlignDefault -> colAlign
        _            -> align
  let maybeCons = maybe id (:)
  let validAttribs = [ "abbr", "align", "axis", "char", "charoff"
                     , "content-type", "headers", "scope", "style", "valign"
                     ]
  let attribs = maybeCons (alignmentAttrib align')
              . maybeCons (rowspanAttrib rowspan)
              . maybeCons (colspanAttrib colspan)
              $ toAttribs attr validAttribs
  inTags False tag' attribs <$> cellContents item

-- | Whether the JATS produced from this block should be wrapped in a
-- @<p>@ element when put directly below a @<td>@ element.
needsWrapInCell :: Block -> Bool
needsWrapInCell = \case
  Plain{}          -> False  -- should be unwrapped anyway
  Para{}           -> False
  BulletList{}     -> False
  OrderedList{}    -> False
  DefinitionList{} -> False
  HorizontalRule   -> False
  CodeBlock{}      -> False
  RawBlock{}       -> False  -- responsibility of the user
  _                -> True
