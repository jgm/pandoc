{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{- |
   Module      : Text.Pandoc.Parsing.GridTable
   Copyright   : Copyright (C) 2006-2022 John MacFarlane
   License     : GPL-2.0-or-later
   Maintainer  : John MacFarlane <jgm@berkeley.edu>

Shared parsers for plaintext tables.
-}
module Text.Pandoc.Parsing.GridTable
  ( gridTableWith
  , gridTableWith'
  , tableWith
  , tableWith'
  , widthsFromIndices
    -- * Components of a plain-text table
  , TableComponents (..)
  , TableNormalization (..)
  , toTableComponents
  , toTableComponents'
  )
where

import Control.Monad (guard)
import Data.List (transpose)
import Data.Text (Text)
import Text.Pandoc.Options (ReaderOptions (readerColumns))
import Text.Pandoc.Builder (Blocks)
import Text.Pandoc.Definition
import Text.Pandoc.Parsing.Capabilities
import Text.Pandoc.Parsing.General
import Text.Pandoc.Parsing.Types
import Text.Pandoc.Shared (compactify, splitTextByIndices, trim, trimr)
import Text.Pandoc.Sources
import Text.Parsec
  ( Stream (..), many1, notFollowedBy, option, optional, sepEndBy1, try )

import qualified Data.Text as T
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
toTableComponents :: [Alignment] -> [Double] -> [Blocks] -> [[Blocks]]
                  -> TableComponents
toTableComponents = toTableComponents' NoNormalization

-- | Bundles basic table components into a single value, performing
-- normalizations as necessary.
toTableComponents' :: TableNormalization
                   -> [Alignment] -> [Double] -> [Blocks] -> [[Blocks]]
                   -> TableComponents
toTableComponents' normalization aligns widths heads rows =
  let th = TableHead nullAttr (toHeaderRow normalization heads)
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
              => ParserT Sources st m (mf Blocks)  -- ^ Block list parser
              -> Bool                              -- ^ Headerless table
              -> ParserT Sources st m (mf Blocks)
gridTableWith blocks headless =
  tableWith (gridTableHeader headless blocks) (gridTableRow blocks)
            (gridTableSep '-') gridTableFooter

-- | Like @'gridTableWith'@, but returns 'TableComponents' instead of a
-- Table.
gridTableWith' :: (Monad m, Monad mf,
                   HasReaderOptions st, HasLastStrPosition st)
               => TableNormalization
               -> ParserT Sources st m (mf Blocks) -- ^ Block list parser
               -> Bool                             -- ^ Headerless table
               -> ParserT Sources st m (mf TableComponents)
gridTableWith' normalization blocks headless =
  tableWith' normalization
             (gridTableHeader headless blocks) (gridTableRow blocks)
             (gridTableSep '-') gridTableFooter

gridTableSplitLine :: [Int] -> Text -> [Text]
gridTableSplitLine indices line = map removeFinalBar $ tail $
  splitTextByIndices (init indices) $ trimr line

-- | Parses a grid segment, where the grid line is made up from the
-- given char and terminated with a plus (@+@). The grid line may begin
-- and/or end with a colon, signaling column alignment. Returns the size
-- of the grid part and column alignment
gridPart :: Monad m => Char -> ParserT Sources st m (Int, Alignment)
gridPart ch = do
  leftColon <- option False (True <$ char ':')
  dashes <- many1 (char ch)
  rightColon <- option False (True <$ char ':')
  char '+'
  let lengthDashes = length dashes + (if leftColon then 1 else 0) +
                       (if rightColon then 1 else 0)
  let alignment = case (leftColon, rightColon) of
                       (True, True)   -> AlignCenter
                       (True, False)  -> AlignLeft
                       (False, True)  -> AlignRight
                       (False, False) -> AlignDefault
  return (lengthDashes + 1, alignment)

gridDashedLines :: Monad m
                => Char -> ParserT Sources st m [(Int, Alignment)]
gridDashedLines ch = try $ char '+' >> many1 (gridPart ch) <* blankline

removeFinalBar :: Text -> Text
removeFinalBar = T.dropWhileEnd go . T.dropWhileEnd (=='|')
  where
    go c = T.any (== c) " \t"

-- | Separator between rows of grid table.
gridTableSep :: Monad m => Char -> ParserT Sources st m Char
gridTableSep ch = try $ gridDashedLines ch >> return '\n'

-- | Parse header for a grid table.
gridTableHeader :: (Monad m, Monad mf, HasLastStrPosition st)
                => Bool -- ^ Headerless table
                -> ParserT Sources st m (mf Blocks)
                -> ParserT Sources st m (mf [Blocks], [Alignment], [Int])
gridTableHeader True _ = do
  optional blanklines
  dashes <- gridDashedLines '-'
  let aligns = map snd dashes
  let lines'   = map fst dashes
  let indices  = scanl (+) 0 lines'
  return (return [], aligns, indices)
gridTableHeader False blocks = try $ do
  optional blanklines
  dashes <- gridDashedLines '-'
  rawContent  <- many1 (notFollowedBy (gridTableSep '=') >> char '|' >>
                           T.pack <$> many1Till anyChar newline)
  underDashes <- gridDashedLines '='
  guard $ length dashes == length underDashes
  let lines'   = map fst underDashes
  let indices  = scanl (+) 0 lines'
  let aligns   = map snd underDashes
  let rawHeads = map (T.unlines . map trim) $ transpose
                       $ map (gridTableSplitLine indices) rawContent
  heads <- sequence <$> mapM (parseFromString' blocks . trim) rawHeads
  return (heads, aligns, indices)

gridTableRawLine :: (Stream s m Char, UpdateSourcePos s Char)
                 => [Int] -> ParserT s st m [Text]
gridTableRawLine indices = do
  char '|'
  line <- many1Till anyChar newline
  return (gridTableSplitLine indices $ T.pack line)

-- | Parse row of grid table.
gridTableRow :: (Monad m, Monad mf, HasLastStrPosition st)
             => ParserT Sources st m (mf Blocks)
             -> [Int]
             -> ParserT Sources st m (mf [Blocks])
gridTableRow blocks indices = do
  colLines <- many1 (gridTableRawLine indices)
  let cols = map ((<> "\n") . T.unlines . removeOneLeadingSpace) $
               transpose colLines
      compactifyCell bs = case compactify [bs] of
                            []  -> mempty
                            x:_ -> x
  cells <- sequence <$> mapM (parseFromString' blocks) cols
  return $ fmap (map compactifyCell) cells

removeOneLeadingSpace :: [Text] -> [Text]
removeOneLeadingSpace xs =
  if all startsWithSpace xs
     then map (T.drop 1) xs
     else xs
   where startsWithSpace t = case T.uncons t of
           Nothing     -> True
           Just (c, _) -> c == ' '

-- | Parse footer for a grid table.
gridTableFooter :: (Stream s m Char, UpdateSourcePos s Char)
                => ParserT s st m ()
gridTableFooter = optional blanklines

---

-- | Parse a table using 'headerParser', 'rowParser',
-- 'lineParser', and 'footerParser'.
tableWith :: (Stream s m Char, UpdateSourcePos s Char,
              HasReaderOptions st, Monad mf)
          => ParserT s st m (mf [Blocks], [Alignment], [Int]) -- ^ header parser
          -> ([Int] -> ParserT s st m (mf [Blocks]))  -- ^ row parser
          -> ParserT s st m sep                       -- ^ line parser
          -> ParserT s st m end                       -- ^ footer parser
          -> ParserT s st m (mf Blocks)
tableWith hp rp lp fp = fmap tableFromComponents <$>
  tableWith' NoNormalization hp rp lp fp

tableWith' :: (Stream s m Char, UpdateSourcePos s Char,
               HasReaderOptions st, Monad mf)
           => TableNormalization
           -> ParserT s st m (mf [Blocks], [Alignment], [Int]) -- ^ header parser
           -> ([Int] -> ParserT s st m (mf [Blocks]))  -- ^ row parser
           -> ParserT s st m sep                       -- ^ line parser
           -> ParserT s st m end                       -- ^ footer parser
           -> ParserT s st m (mf TableComponents)
tableWith' n11n headerParser rowParser lineParser footerParser = try $ do
  (heads, aligns, indices) <- headerParser
  lines' <- sequence <$> rowParser indices `sepEndBy1` lineParser
  footerParser
  numColumns <- getOption readerColumns
  let widths = if null indices
               then replicate (length aligns) 0.0
               else widthsFromIndices numColumns indices
  return $ toTableComponents' n11n aligns widths <$> heads <*> lines'

toRow :: [Blocks] -> Row
toRow =  Row nullAttr . map B.simpleCell

toHeaderRow :: TableNormalization -> [Blocks] -> [Row]
toHeaderRow = \case
  NoNormalization -> \l -> [toRow l | not (null l)]
  NormalizeHeader -> \l -> [toRow l | not (null l) && not (all null l)]

-- Calculate relative widths of table columns, based on indices
widthsFromIndices :: Int      -- Number of columns on terminal
                  -> [Int]    -- Indices
                  -> [Double] -- Fractional relative sizes of columns
widthsFromIndices _ [] = []
widthsFromIndices numColumns' indices =
  let numColumns = max numColumns' (if null indices then 0 else last indices)
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
  tail fracs
