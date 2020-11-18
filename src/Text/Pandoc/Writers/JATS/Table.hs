{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.JATS.Table
   Copyright   : © 2020 Albert Krewinkel
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
import Data.Text (Text)
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options (WriterOptions)
import Text.DocLayout (Doc, empty, vcat, ($$))
import Text.Pandoc.Shared (tshow)
import Text.Pandoc.Writers.JATS.Types
import Text.Pandoc.Writers.Shared (toLegacyTable)
import Text.Pandoc.XML (inTags, inTagsIndented, selfClosingTag)


tableToJATS :: PandocMonad m
            => WriterOptions
            -> Attr -> Caption -> [ColSpec] -> TableHead
            -> [TableBody] -> TableFoot
            -> JATS m (Doc Text)
tableToJATS opts _attr blkCapt specs th tb tf = do
  blockToJATS <- asks jatsBlockWriter
  let (caption, aligns, widths, headers, rows) =
        toLegacyTable blkCapt specs th tb tf
  captionDoc <- if null caption
                then return mempty
                else inTagsIndented "caption" <$> blockToJATS opts (Para caption)
  tbl <- captionlessTable aligns widths headers rows
  return $ inTags True "table-wrap" [] $ captionDoc $$ tbl
  where
    captionlessTable aligns widths headers rows = do
      let percent w = tshow (truncate (100*w) :: Integer) <> "*"
      let coltags = vcat $ zipWith (\w al -> selfClosingTag "col"
                           ([("width", percent w) | w > 0] ++
                            [("align", alignmentToText al)])) widths aligns
      thead <- if all null headers
                  then return empty
                  else inTagsIndented "thead" <$> tableRowToJATS opts True headers
      tbody <- inTagsIndented "tbody" . vcat <$>
                    mapM (tableRowToJATS opts False) rows
      return $ inTags True "table" [] $ coltags $$ thead $$ tbody

alignmentToText :: Alignment -> Text
alignmentToText alignment = case alignment of
                                 AlignLeft    -> "left"
                                 AlignRight   -> "right"
                                 AlignCenter  -> "center"
                                 AlignDefault -> "left"

tableRowToJATS :: PandocMonad m
               => WriterOptions
               -> Bool
               -> [[Block]]
               -> JATS m (Doc Text)
tableRowToJATS opts isHeader cols =
  inTagsIndented "tr" . vcat <$> mapM (tableItemToJATS opts isHeader) cols

tableItemToJATS :: PandocMonad m
                => WriterOptions
                -> Bool
                -> [Block]
                -> JATS m (Doc Text)
tableItemToJATS opts isHeader [Plain item] = do
  inlinesToJATS <- asks jatsInlinesWriter
  inTags False (if isHeader then "th" else "td") [] <$>
    inlinesToJATS opts item
tableItemToJATS opts isHeader item = do
  blockToJATS <- asks jatsBlockWriter
  inTags False (if isHeader then "th" else "td") [] . vcat <$>
    mapM (blockToJATS opts) item
