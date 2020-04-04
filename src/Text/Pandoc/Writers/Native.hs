{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Text.Pandoc.Writers.Native
   Copyright   : Copyright (C) 2006-2020 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of a 'Pandoc' document to a string representation.
-}
module Text.Pandoc.Writers.Native ( writeNative )
where
import Data.List (intersperse)
import Data.Text (Text)
import Text.Pandoc.Class.PandocMonad (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options (WrapOption (..), WriterOptions (..))
import Text.DocLayout

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
prettyBlock (Table attr blkCapt specs thead tbody tfoot) =
  mconcat [ "Table "
          , text (show attr)
          , " "
          , prettyCaption blkCapt ] $$
  prettyList (map (text . show) specs) $$
  prettyHead thead $$
  prettyBodies tbody $$
  prettyFoot tfoot
  where prettyRows = prettyList . map prettyRow
        prettyRow (Row a body) =
          text ("Row " <> show a) $$ prettyList (map prettyCell body)
        prettyCell (Cell a ma h w b) =
          mconcat [ "Cell "
                  , text (show a)
                  , " "
                  , text (show ma)
                  , " ("
                  , text (show h)
                  , ") ("
                  , text (show w)
                  , ")" ] $$
          prettyList (map prettyBlock b)
        prettyCaption (Caption mshort body) =
          "(Caption " <> text (showsPrec 11 mshort "") $$ prettyList (map prettyBlock body) <> ")"
        prettyHead (TableHead thattr body)
          = "(TableHead " <> text (show thattr) $$ prettyRows body <> ")"
        prettyBody (TableBody tbattr rhc hd bd)
          = mconcat [ "(TableBody "
                    , text (show tbattr)
                    , " ("
                    , text (show rhc)
                    , ")" ] $$ prettyRows hd $$ prettyRows bd <> ")"
        prettyBodies = prettyList . map prettyBody
        prettyFoot (TableFoot tfattr body)
          = "(TableFoot " <> text (show tfattr) $$ prettyRows body <> ")"
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
