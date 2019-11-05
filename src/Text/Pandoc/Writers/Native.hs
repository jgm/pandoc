{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.Native
   Copyright   : Copyright (C) 2006-2019 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of a 'Pandoc' document to a string representation.
-}
module Text.Pandoc.Writers.Native ( writeNative )
where
import Prelude
import Data.List (intersperse)
import Data.Text (Text)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition
-- import Text.Pandoc.Options (WrapOption (..), WriterOptions (..)) TODO text: restore
import Text.DocLayout

-- TODO text: remove
import Text.Pandoc.Legacy.Options
--

prettyList :: [Doc Text] -> Doc Text
prettyList ds =
  "[" <>
  mconcat (intersperse (cr <> ",") $ map (nest 1) ds) <> "]"

-- | Prettyprint Pandoc block element.
prettyBlock :: Block -> Doc Text
prettyBlock (LineBlock lines') =
  "LineBlock" $$ prettyList (map (text . show) lines')
prettyBlock (BlockQuote blocks) =
  "BlockQuote" $$ prettyList (map prettyBlock blocks)
prettyBlock (OrderedList attribs blockLists) =
  "OrderedList" <> space <> text (show attribs) $$
  prettyList (map (prettyList . map prettyBlock) blockLists)
prettyBlock (BulletList blockLists) =
  "BulletList" $$
  prettyList (map (prettyList . map prettyBlock) blockLists)
prettyBlock (DefinitionList items) = "DefinitionList" $$
  prettyList (map deflistitem items)
    where deflistitem (term, defs) = "(" <> text (show term) <> "," <> cr <>
           nest 1 (prettyList $ map (prettyList . map prettyBlock) defs) <> ")"
prettyBlock (Table caption aligns widths header rows) =
  "Table " <> text (show caption) <> " " <> text (show aligns) <> " " <>
  text (show widths) $$
  prettyRow header $$
  prettyList (map prettyRow rows)
    where prettyRow cols = prettyList (map (prettyList . map prettyBlock) cols)
prettyBlock (Div attr blocks) =
  text ("Div " <> show attr) $$ prettyList (map prettyBlock blocks)
prettyBlock block = text $ show block

-- | Prettyprint Pandoc document.
writeNative :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeNative opts (Pandoc meta blocks) = return $
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
      withHead = case writerTemplate opts of
                      Just _  -> \bs -> text ("Pandoc (" ++ show meta ++ ")") $$
                                  bs $$ cr
                      Nothing -> id
  in  render colwidth $ withHead $ prettyList $ map prettyBlock blocks
