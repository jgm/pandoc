{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{- |
   Module      : Text.Pandoc.Readers.HTML.Table
   Copyright   : © 2006-2021 John MacFarlane,
                   2020-2021 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
   Stability   : alpha
   Portability : portable

HTML table parser.
-}
module Text.Pandoc.Readers.HTML.Table (pTable) where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Either (lefts, rights)
import Data.List.NonEmpty (nonEmpty)
import Data.Text (Text)
import Text.HTML.TagSoup
import Text.Pandoc.Builder (Blocks)
import Text.Pandoc.CSS (cssAttributes)
import Text.Pandoc.Definition
import Text.Pandoc.Class.PandocMonad (PandocMonad (..))
import Text.Pandoc.Parsing
  ( eof, lookAhead, many, many1, manyTill, option, optional
  , optionMaybe, skipMany, try)
import Text.Pandoc.Readers.HTML.Parsing
import Text.Pandoc.Readers.HTML.Types (TagParser)
import Text.Pandoc.Shared (onlySimpleTableCells, safeRead)
import qualified Data.Text as T
import qualified Text.Pandoc.Builder as B

-- | Parses a @<col>@ element, returning the column's width.
-- An Either value is used:  Left i means a "relative length" with
-- integral value i (see https://www.w3.org/TR/html4/types.html#h-6.6);
-- Right w means a regular width.  Defaults to @'Right ColWidthDefault'@
-- if the width is not set or cannot be determined.
pCol :: PandocMonad m => TagParser m (Either Int ColWidth)
pCol = try $ do
  TagOpen _ attribs' <- pSatisfy (matchTagOpen "col" [])
  let attribs = toStringAttr attribs'
  skipMany pBlank
  optional $ pSatisfy (matchTagClose "col")
  skipMany pBlank
  return $ case lookup "width" attribs of
                Nothing -> case lookup "style" attribs of
                  Just (T.stripPrefix "width:" -> Just xs) | T.any (== '%') xs ->
                    maybe (Right ColWidthDefault) (Right . ColWidth . (/ 100.0))
                      $ safeRead (T.filter
                                   (`notElem` (" \t\r\n%'\";" :: [Char])) xs)
                  _ -> Right ColWidthDefault
                Just (T.unsnoc -> Just (xs, '*')) ->
                  maybe (Left 1) Left $ safeRead xs
                Just (T.unsnoc -> Just (xs, '%')) ->
                  maybe (Right ColWidthDefault)
                        (Right . ColWidth . (/ 100.0)) $ safeRead xs
                _ -> Right ColWidthDefault

pColgroup :: PandocMonad m => TagParser m [Either Int ColWidth]
pColgroup = try $ do
  pSatisfy (matchTagOpen "colgroup" [])
  skipMany pBlank
  manyTill pCol (pCloses "colgroup" <|> eof) <* skipMany pBlank

resolveRelativeLengths :: [Either Int ColWidth] -> [ColWidth]
resolveRelativeLengths ws =
  let remaining = 1 - sum (map getColWidth $ rights ws)
      relatives = sum $ lefts ws
      relUnit = remaining / fromIntegral relatives
      toColWidth (Right x) = x
      toColWidth (Left i) = ColWidth (fromIntegral i * relUnit)
  in  map toColWidth ws

getColWidth :: ColWidth -> Double
getColWidth ColWidthDefault = 0
getColWidth (ColWidth w) = w

data CellType
  = HeaderCell
  | BodyCell
  deriving Eq

pCell :: PandocMonad m
      => TagParser m Blocks
      -> CellType
      -> TagParser m (CellType, Cell)
pCell block celltype = try $ do
  let celltype' = case celltype of
        HeaderCell -> "th"
        BodyCell   -> "td"
  skipMany pBlank
  TagOpen _ attribs <- lookAhead $ pSatisfy (matchTagOpen celltype' [])
  let cssAttribs = maybe [] cssAttributes $ lookup "style" attribs
  let align = case lookup "align" attribs <|>
                   lookup "text-align" cssAttribs of
                Just "left"   -> AlignLeft
                Just "right"  -> AlignRight
                Just "center" -> AlignCenter
                _             -> AlignDefault
  let rowspan = RowSpan . fromMaybe 1 $
                safeRead =<< lookup "rowspan" attribs
  let colspan = ColSpan . fromMaybe 1 $
                safeRead =<< lookup "colspan" attribs
  res <- pInTags celltype' block
  skipMany pBlank
  let handledAttribs = ["align", "colspan", "rowspan", "text-align"]
      attribs' = foldr go [] attribs
      go kv@(k, _) acc = case k of
        "style" -> case filter ((/= "text-align") . fst) cssAttribs of
                     [] -> acc
                     cs -> ("style", toStyleString cs) : acc
        -- drop attrib if it's already handled
        _ | k `elem` handledAttribs -> acc
        _ -> kv : acc
  return (celltype, B.cellWith (toAttr attribs') align rowspan colspan res)

-- | Create a style attribute string from a list of CSS attributes
toStyleString :: [(Text, Text)] -> Text
toStyleString = T.intercalate "; " . map (\(k, v) -> k <> ": " <> v)

-- | Parses a normal table row; returns the row together with the number
-- of header cells at the beginning of the row.
pRow :: PandocMonad m
     => TagParser m Blocks
     -> TagParser m (RowHeadColumns, B.Row)
pRow block = try $ do
  skipMany pBlank
  TagOpen _ attribs <- pSatisfy (matchTagOpen "tr" []) <* skipMany pBlank
  cells <- many (pCell block BodyCell <|> pCell block HeaderCell)
  TagClose _ <- pSatisfy (matchTagClose "tr")
  return ( RowHeadColumns $ length (takeWhile ((== HeaderCell) . fst) cells)
         , Row (toAttr attribs) $ map snd cells
         )

-- | Parses a header row, i.e., a row which containing nothing but
-- @<th>@ elements.
pHeaderRow :: PandocMonad m
           => TagParser m Blocks
           -> TagParser m B.Row
pHeaderRow block = try $ do
  skipMany pBlank
  let pThs = map snd <$> many (pCell block HeaderCell)
  let mkRow (attribs, cells) = Row (toAttr attribs) cells
  mkRow <$> pInTagWithAttribs TagsRequired "tr" pThs

-- | Parses a table head. If there is no @thead@ element, this looks for
-- a row of @<th>@-only elements as the first line of the table.
pTableHead :: PandocMonad m
           => TagParser m Blocks
           -> TagParser m TableHead
pTableHead block = try $ do
  skipMany pBlank
  let pRows = many (pRow block)
  let pThead = pInTagWithAttribs ClosingTagOptional "thead" pRows
  optionMaybe pThead >>= \case
    Just (attribs, rows) ->
      return $ TableHead (toAttr attribs) $ map snd rows
    Nothing -> mkTableHead <$> optionMaybe (pHeaderRow block)
               where
                 mkTableHead = TableHead nullAttr . \case
                   -- Use row as header only if it's non-empty
                   Just row@(Row _ (_:_)) -> [row]
                   _                      -> []

-- | Parses a table foot
pTableFoot :: PandocMonad m
           => TagParser m Blocks
           -> TagParser m TableFoot
pTableFoot block = try $ do
  skipMany pBlank
  TagOpen _ attribs <- pSatisfy (matchTagOpen "tfoot" []) <* skipMany pBlank
  rows <- many (fmap snd $ pRow block <* skipMany pBlank)
  optional $ pSatisfy (matchTagClose "tfoot")
  return $ TableFoot (toAttr attribs) rows

-- | Parses a table body
pTableBody :: PandocMonad m
           => TagParser m Blocks
           -> TagParser m TableBody
pTableBody block = try $ do
  skipMany pBlank
  attribs <- option [] $ getAttribs <$> pSatisfy (matchTagOpen "tbody" [])
             <* skipMany pBlank
  bodyheads <- many (pHeaderRow block)
  (rowheads, rows) <- unzip <$> many1 (pRow block <* skipMany pBlank)
  optional $ pSatisfy (matchTagClose "tbody")
  return $ TableBody (toAttr attribs) (foldr max 0 rowheads) bodyheads rows
  where
    getAttribs (TagOpen _ attribs) = attribs
    getAttribs _ = []

-- | Parses a simple HTML table
pTable :: PandocMonad m
       => TagParser m Blocks -- ^ Caption and cell contents parser
       -> TagParser m Blocks
pTable block = try $ do
  TagOpen _ attribs <- pSatisfy (matchTagOpen "table" [])  <* skipMany pBlank
  caption <- option mempty $ pInTags "caption" block       <* skipMany pBlank
  widths <- resolveRelativeLengths <$>
              ((mconcat <$> many1 pColgroup) <|> many pCol) <* skipMany pBlank
  thead   <- pTableHead block               <* skipMany pBlank
  topfoot <- optionMaybe (pTableFoot block) <* skipMany pBlank
  tbodies <- many (pTableBody block)        <* skipMany pBlank
  botfoot <- optionMaybe (pTableFoot block) <* skipMany pBlank
  TagClose _ <- pSatisfy (matchTagClose "table")
  let tfoot = fromMaybe (TableFoot nullAttr []) $ topfoot <|> botfoot
  case normalize widths thead tbodies tfoot of
    Left err -> fail err
    Right (colspecs, thead', tbodies', tfoot') -> return $
      B.tableWith (toAttr attribs)
                  (B.simpleCaption caption)
                  colspecs
                  thead'
                  tbodies'
                  tfoot'
data TableType
  = SimpleTable
  | NormalTable

tableType :: [[Cell]] -> TableType
tableType cells =
  if onlySimpleTableCells $ map (map cellContents) cells
  then SimpleTable
  else NormalTable
  where
    cellContents :: Cell -> [Block]
    cellContents (Cell _ _ _ _ bs) = bs

normalize :: [ColWidth] -> TableHead -> [TableBody] -> TableFoot
          -> Either String ([ColSpec], TableHead, [TableBody], TableFoot)
normalize widths head' bodies foot = do
  let rows = headRows head' <> concatMap bodyRows bodies <> footRows foot
  let cellWidth (Cell _ _ _ (ColSpan cs) _) = cs
  let rowLength = foldr (\cell acc -> cellWidth cell + acc) 0 . rowCells
  let ncols = maybe 0 maximum $ nonEmpty $ map rowLength rows
  let tblType = tableType (map rowCells rows)
  -- fail on empty table
  if null rows
    then Left "empty table"
    else Right
         ( zip (calculateAlignments ncols bodies)
               (normalizeColWidths ncols tblType widths)
         , head'
         , bodies
         , foot
         )

normalizeColWidths :: Int -> TableType -> [ColWidth] -> [ColWidth]
normalizeColWidths ncols tblType = \case
  [] -> case tblType of
          SimpleTable -> replicate ncols ColWidthDefault
          NormalTable -> replicate ncols (ColWidth $ 1 / fromIntegral ncols)
  widths -> widths

calculateAlignments :: Int -> [TableBody] -> [Alignment]
calculateAlignments cols tbodies =
  case cells of
    cs:_ -> take cols $ concatMap cellAligns cs ++ repeat AlignDefault
    _    -> replicate cols AlignDefault
  where
    cells :: [[Cell]]
    cells = concatMap bodyRowCells tbodies
    cellAligns :: Cell -> [Alignment]
    cellAligns (Cell _ align _ (ColSpan cs) _) = replicate cs align

bodyRowCells :: TableBody -> [[Cell]]
bodyRowCells = map rowCells . bodyRows

headRows :: TableHead -> [B.Row]
headRows (TableHead _ rows) = rows

bodyRows :: TableBody -> [B.Row]
bodyRows (TableBody _ _ headerRows bodyRows') = headerRows <> bodyRows'

footRows :: TableFoot -> [B.Row]
footRows (TableFoot _ rows) = rows

rowCells :: B.Row -> [Cell]
rowCells (Row _ cells) = cells
