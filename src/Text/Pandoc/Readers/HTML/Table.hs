{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{- |
   Module      : Text.Pandoc.Readers.HTML.Table
   Copyright   : © 2006-2020 John MacFarlane,
                   2020 Albert Krewinkel
   License     : GNU GPL, version 2 or above

   Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
   Stability   : alpha
   Portability : portable

HTML table parser.
-}
module Text.Pandoc.Readers.HTML.Table (pTable') where

import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Text.HTML.TagSoup
import Text.Pandoc.Builder (Blocks)
import Text.Pandoc.Definition
import Text.Pandoc.Class.PandocMonad (PandocMonad (..))
import Text.Pandoc.Parsing
  ( (<|>), eof, many, many1, manyTill, option, optional, skipMany, try)
import Text.Pandoc.Readers.HTML.Parsing
import Text.Pandoc.Readers.HTML.Types (TagParser)
import Text.Pandoc.Shared (onlySimpleTableCells, safeRead)
import qualified Data.Text as T
import qualified Text.Pandoc.Builder as B

-- | Parses a @<col>@ element, returning the column's width. Defaults to
-- @'ColWidthDefault'@ if the width is not set or cannot be determined.
pCol :: PandocMonad m => TagParser m ColWidth
pCol = try $ do
  TagOpen _ attribs' <- pSatisfy (matchTagOpen "col" [])
  let attribs = toStringAttr attribs'
  skipMany pBlank
  optional $ pSatisfy (matchTagClose "col")
  skipMany pBlank
  let width = case lookup "width" attribs of
                Nothing -> case lookup "style" attribs of
                  Just (T.stripPrefix "width:" -> Just xs) | T.any (== '%') xs ->
                    fromMaybe 0.0 $ safeRead (T.filter
                      (`notElem` (" \t\r\n%'\";" :: [Char])) xs)
                  _ -> 0.0
                Just (T.unsnoc -> Just (xs, '%')) ->
                  fromMaybe 0.0 $ safeRead xs
                _ -> 0.0
  if width > 0.0
    then return $ ColWidth $ width / 100.0
    else return ColWidthDefault

pColgroup :: PandocMonad m => TagParser m [ColWidth]
pColgroup = try $ do
  pSatisfy (matchTagOpen "colgroup" [])
  skipMany pBlank
  manyTill pCol (pCloses "colgroup" <|> eof) <* skipMany pBlank

-- | Parses a simple HTML table
pTable' :: PandocMonad m
        => TagParser m Blocks           -- ^ Caption parser
        -> (Text -> TagParser m [Cell]) -- ^ Table cell parser
        -> TagParser m Blocks
pTable' block pCell = try $ do
  TagOpen _ attribs' <- pSatisfy (matchTagOpen "table" [])
  let attribs = toAttr attribs'
  skipMany pBlank
  caption <- option mempty $ pInTags "caption" block <* skipMany pBlank
  widths' <- (mconcat <$> many1 pColgroup) <|> many pCol
  let pTh = option [] $ pInTags "tr" (pCell "th")
      pTr = try $ skipMany pBlank
               *> pInTags "tr" (pCell "td" <|> pCell "th")
      pTBody = pInTag TagsOmittable "tbody" $ many1 pTr
  head'' <- pInTag ClosingTagOptional "thead" (option [] pTr)
        <|> pInTag TagsOmittable "thead" pTh
  head'  <- pInTag TagsOmittable "tbody"
               (if null head'' then pTh else return head'')
  topfoot <- option [] $ pInTag TagsRequired "tfoot" $ many pTr
  rowsLs <- many pTBody
  bottomfoot <- option [] $ pInTag ClosingTagOptional "tfoot" $ many pTr
  TagClose _ <- pSatisfy (matchTagClose "table")
  let rows = concat rowsLs <> topfoot <> bottomfoot
      rows''' = map (map cellContents) rows
  -- fail on empty table
  guard $ not $ null head' && null rows'''
  let isSimple = onlySimpleTableCells $
                 map cellContents head' : rows'''
  let cols = if null head'
                then maximum (map length rows''')
                else length head'
  let aligns = case rows of
                 (cs:_) -> take cols $
                           concatMap cellAligns cs ++ repeat AlignDefault
                 _      -> replicate cols AlignDefault
  let widths = if null widths'
                  then if isSimple
                       then replicate cols ColWidthDefault
                       else replicate cols (ColWidth (1.0 / fromIntegral cols))
                  else widths'
  let toRow = Row nullAttr
      toHeaderRow l = [toRow l | not (null l)]
  return $ B.tableWith attribs
                   (B.simpleCaption caption)
                   (zip aligns widths)
                   (TableHead nullAttr $ toHeaderRow head')
                   [TableBody nullAttr 0 [] $ map toRow rows]
                   (TableFoot nullAttr [])

cellContents :: Cell -> [Block]
cellContents (Cell _ _ _ _ bs) = bs

cellAligns :: Cell -> [Alignment]
cellAligns (Cell _ align _ (ColSpan cs) _) = replicate cs align
