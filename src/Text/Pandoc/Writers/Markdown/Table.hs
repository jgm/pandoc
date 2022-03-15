{-# LANGUAGE OverloadedStrings   #-}
{- |
   Module      : Text.Pandoc.Writers.Markdown
   Copyright   : © 2006-2022 John MacFarlane
   License     : GPL-2.0-or-later
   Maintainer  : John MacFarlane <jgm@berkeley.edu>

Create Markdown pipe-tables and pandoc-style tables.
-}
module Text.Pandoc.Writers.Markdown.Table
  ( pipeTable
  , pandocTable
  ) where

import Control.Monad.Reader (asks)
import Data.List (intersperse, transpose)
import Data.List.NonEmpty (nonEmpty)
import Data.Text (Text)
import qualified Data.Text as T
import Text.DocLayout
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition (Alignment (..))
import Text.Pandoc.Options (WriterOptions (writerColumns, writerWrapText),
                            WrapOption(WrapAuto))
import Text.Pandoc.Writers.Markdown.Types (MarkdownVariant(Markdown),
                                           WriterEnv(..), MD)

-- | Creates a Markdown pipe table.
pipeTable :: PandocMonad m
          => WriterOptions
          -> Bool            -- ^ headless?
          -> [Alignment]     -- ^ column alignments
          -> [Double]        -- ^ column widhts
          -> [Doc Text]      -- ^ table header cells
          -> [[Doc Text]]    -- ^ table body rows
          -> MD m (Doc Text)
pipeTable opts headless aligns widths rawHeaders rawRows = do
  let sp = literal " "
  let blockFor AlignLeft   x y = lblock (x + 2) (sp <> y) <> lblock 0 empty
      blockFor AlignCenter x y = cblock (x + 2) (sp <> y <> sp) <> lblock 0 empty
      blockFor AlignRight  x y = rblock (x + 2) (y <> sp) <> lblock 0 empty
      blockFor _           x y = lblock (x + 2) (sp <> y) <> lblock 0 empty
  let contentWidths = map (max 3 . maybe 3 maximum . nonEmpty . map offset) $
                       transpose (rawHeaders : rawRows)
  let colwidth = writerColumns opts
  let numcols = length contentWidths
  let maxwidth = sum contentWidths
  variant <- asks envVariant
  let pipeWidths = if variant == Markdown &&
                      not (all (== 0) widths) &&
                      maxwidth + (numcols + 1) > colwidth
                      then map
                            (floor . (* fromIntegral (colwidth - (numcols +1))))
                            widths
                      else contentWidths
  let torow cs = nowrap $ literal "|" <>
                    hcat (intersperse (literal "|") $
                          zipWith3 blockFor aligns contentWidths (map chomp cs))
                    <> literal "|"
  let toborder a w = literal $ case a of
                          AlignLeft    -> ":" <> T.replicate (w + 1) "-"
                          AlignCenter  -> ":" <> T.replicate w "-" <> ":"
                          AlignRight   -> T.replicate (w + 1) "-" <> ":"
                          AlignDefault -> T.replicate (w + 2) "-"
  -- note:  pipe tables can't completely lack a
  -- header; for a headerless table, we need a header of empty cells.
  -- see jgm/pandoc#1996.
  let header = if headless
                  then torow (replicate (length aligns) empty)
                  else torow rawHeaders
  let border = nowrap $ literal "|" <> hcat (intersperse (literal "|") $
                        zipWith toborder aligns pipeWidths) <> literal "|"
  let body   = vcat $ map torow rawRows
  return $ header $$ border $$ body

-- | Write a pandoc-style Markdown table.
pandocTable :: PandocMonad m
            => WriterOptions
            -> Bool            -- ^ whether this is a multiline table
            -> Bool            -- ^ whether the table has a header
            -> [Alignment]     -- ^ column alignments
            -> [Double]        -- ^ column widths
            -> [Doc Text]      -- ^ table header cells
            -> [[Doc Text]]    -- ^ table body rows
            -> MD m (Doc Text)
pandocTable opts multiline headless aligns widths rawHeaders rawRows = do
  let isSimple = all (==0) widths
  let alignHeader alignment = case alignment of
                                AlignLeft    -> lblock
                                AlignCenter  -> cblock
                                AlignRight   -> rblock
                                AlignDefault -> lblock
  -- Number of characters per column necessary to output every cell
  -- without requiring a line break.
  -- The @+2@ is needed for specifying the alignment.
  let numChars    = (+ 2) . maybe 0 maximum . nonEmpty . map offset
  -- Number of characters per column necessary to output every cell
  -- without requiring a line break *inside a word*.
  -- The @+2@ is needed for specifying the alignment.
  let minNumChars = (+ 2) . maybe 0 maximum . nonEmpty . map minOffset
  let columns = transpose (rawHeaders : rawRows)
  -- minimal column width without wrapping a single word
  let relWidth w col =
         max (floor $ fromIntegral (writerColumns opts - 1) * w)
             (if writerWrapText opts == WrapAuto
                 then minNumChars col
                 else numChars col)
  let widthsInChars
        | isSimple  = map numChars columns
        | otherwise = zipWith relWidth widths columns
  let makeRow = hcat . intersperse (lblock 1 (literal " ")) .
                   zipWith3 alignHeader aligns widthsInChars
  let rows' = map makeRow rawRows
  let head' = makeRow rawHeaders
  let underline = mconcat $ intersperse (literal " ") $
                  map (\width -> literal (T.replicate width "-")) widthsInChars
  let border
        | multiline = literal (T.replicate (sum widthsInChars +
                        length widthsInChars - 1) "-")
        | headless  = underline
        | otherwise = empty
  let head'' = if headless
                  then empty
                  else border <> cr <> head'
  let body = if multiline
                then vsep rows' $$
                     if length rows' < 2
                        then blankline -- #4578
                        else empty
                else vcat rows'
  let bottom = if headless
                  then underline
                  else border
  return $ head'' $$ underline $$ body $$ bottom
