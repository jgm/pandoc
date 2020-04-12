{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2008-2017 John MacFarlane <jgm@berkeley.edu>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

{- |
   Module      : Text.Pandoc.Writers.XWiki
   Copyright   : Copyright (C) 2008-2017 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : Derek Chen-Becker <dchenbecker@gmail.com>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to XWiki markup.

XWiki:  <http://www.xwiki.org/>
XWiki Syntax:  <http://www.xwiki.org/xwiki/bin/view/Documentation/UserGuide/Features/XWikiSyntax/>
-}

module Text.Pandoc.Writers.XWiki ( writeXWiki ) where
import Control.Monad.Reader (ReaderT, asks, local, runReaderT)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text, intercalate, replace, split)
import Text.Pandoc.Class.PandocMonad (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Shared
import Text.Pandoc.Writers.MediaWiki (highlightingLangs)
import Text.Pandoc.Writers.Shared (toLegacyTable)

data WriterState = WriterState {
  listLevel :: Text -- String at the beginning of items
}

type XWikiReader m = ReaderT WriterState m

-- | Convert Pandoc to XWiki.
writeXWiki :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeXWiki _ (Pandoc _ blocks) =
  let env = WriterState { listLevel = "" }
  in runReaderT (blockListToXWiki blocks) env

-- | Concatenates strings with line breaks between them.
vcat :: [Text] -> Text
vcat = intercalate "\n"

-- If an id is provided, we can generate an anchor using the id macro
-- https://extensions.xwiki.org/xwiki/bin/view/Extension/Id%20Macro
genAnchor :: Text -> Text
genAnchor id' = if Text.null id'
  then ""
  else "{{id name=\"" <> id' <> "\" /}}"

blockListToXWiki :: PandocMonad m => [Block] -> XWikiReader m Text
blockListToXWiki blocks =
  vcat <$> mapM blockToXWiki blocks

blockToXWiki :: PandocMonad m => Block -> XWikiReader m Text

blockToXWiki Null = return ""

blockToXWiki (Div (id', _, _) blocks) = do
  content <- blockListToXWiki blocks
  return $ genAnchor id' <> content

blockToXWiki (Plain inlines) =
  inlineListToXWiki inlines

blockToXWiki (Para inlines) = do
  contents <- inlineListToXWiki inlines
  return $ contents <> "\n"

blockToXWiki (LineBlock lns) =
  blockToXWiki $ linesToPara lns

blockToXWiki b@(RawBlock f str)
  | f == Format "xwiki" = return str
  | otherwise           = "" <$ report (BlockNotRendered b)

blockToXWiki HorizontalRule = return "\n----\n"

blockToXWiki (Header level (id', _, _) inlines) = do
  contents <- inlineListToXWiki inlines
  let eqs = Text.replicate level "="
  return $ eqs <> " " <> contents <> " " <> genAnchor id' <> eqs <> "\n"

-- XWiki doesn't appear to differentiate between inline and block-form code, so we delegate
-- We do amend the text to ensure that the code markers are on their own lines, since this is a block
blockToXWiki (CodeBlock attrs str) = do
  contents <- inlineToXWiki (Code attrs ("\n" <> str <> "\n"))
  return $ "\n" <> contents <> "\n"

blockToXWiki (BlockQuote blocks) = do
  blockText <- blockListToXWiki blocks
  let quoteLines = split (== '\n') blockText
  let prefixed = map (">" <>) quoteLines
  return $ vcat prefixed

blockToXWiki (BulletList contents) = blockToXWikiList "*" contents

blockToXWiki (OrderedList _ contents) = blockToXWikiList "1" contents

blockToXWiki (DefinitionList items) = do
  lev <- asks listLevel
  contents <- local (\s -> s { listLevel = listLevel s <> ";" }) $ mapM definitionListItemToMediaWiki items
  return $ vcat contents <> if Text.null lev then "\n" else ""

-- TODO: support more features
blockToXWiki (Table _ blkCapt specs thead tbody tfoot) = do
  let (_, _, _, headers, rows') = toLegacyTable blkCapt specs thead tbody tfoot
  headers' <- mapM (tableCellXWiki True) $ take (length specs) $ headers ++ repeat []
  otherRows <- mapM formRow rows'
  return $ Text.unlines (Text.unwords headers':otherRows)

formRow :: PandocMonad m => [[Block]] -> XWikiReader m Text
formRow row = do
  cellStrings <- mapM (tableCellXWiki False) row
  return $ Text.unwords cellStrings


tableCellXWiki :: PandocMonad m => Bool -> [Block] -> XWikiReader m Text
tableCellXWiki isHeader cell = do
  contents <- blockListToXWiki cell
  let isMultiline = (length . split (== '\n')) contents > 1
  let contents' = intercalate contents $ if isMultiline then ["(((", ")))"] else [mempty, mempty]
  let cellBorder = if isHeader then "|=" else "|"
  return $ cellBorder <> contents'


inlineListToXWiki :: PandocMonad m => [Inline] -> XWikiReader m Text
inlineListToXWiki lst =
  mconcat <$> mapM inlineToXWiki lst

inlineToXWiki :: PandocMonad m => Inline -> XWikiReader m Text

inlineToXWiki (Str str) = return $ escapeXWikiString str

inlineToXWiki Space = return " "

-- Special syntax for XWiki 2.0. This won't break table cells
inlineToXWiki LineBreak = return "\\\\"

inlineToXWiki SoftBreak = return " "

inlineToXWiki (Emph lst) = do
  contents <- inlineListToXWiki lst
  return $ "//" <> contents <> "//"

inlineToXWiki (Underline lst) = do
  contents <- inlineListToXWiki lst
  return $ "__" <> contents <> "__"

inlineToXWiki (Strong lst) = do
  contents <- inlineListToXWiki lst
  return $ "**" <> contents <> "**"

inlineToXWiki (Strikeout lst) = do
  contents <- inlineListToXWiki lst
  return $ "--" <> contents <> "--"

inlineToXWiki (Superscript lst) = do
  contents <- inlineListToXWiki lst
  return $ "^^" <> contents <> "^^"

inlineToXWiki (Subscript lst) = do
  contents <- inlineListToXWiki lst
  return $ ",," <> contents <> ",,"

-- TODO: Not supported. Maybe escape to HTML?
inlineToXWiki (SmallCaps lst) =
  inlineListToXWiki lst

inlineToXWiki (Quoted SingleQuote lst) = do
  contents <- inlineListToXWiki lst
  return $ "‘" <> contents <> "’"

inlineToXWiki (Quoted DoubleQuote lst) = do
  contents <- inlineListToXWiki lst
  return $ "“" <> contents <> "”"

inlineToXWiki (Code (_,classes,_) contents) = do
  let at  = Set.fromList classes `Set.intersection` highlightingLangs
  return $
    case Set.toList at of
      [] -> "{{code}}" <> contents <> "{{/code}}"
      (l:_) -> "{{code language=\"" <> l <> "\"}}" <> contents <> "{{/code}}"

inlineToXWiki (Cite _ lst) = inlineListToXWiki lst

-- FIXME: optionally support this (plugin?)
inlineToXWiki (Math _ str) = return $ "{{formula}}" <> str <> "{{/formula}}"

inlineToXWiki il@(RawInline frmt str)
  | frmt == Format "xwiki" = return str
  | otherwise              = "" <$ report (InlineNotRendered il)

-- TODO: Handle anchors
inlineToXWiki (Link (id', _, _) txt (src, _)) = do
  label <- inlineListToXWiki txt
  case txt of
     [Str s] | isURI src && escapeURI s == src -> return $ src <> genAnchor id'
     _  -> return $ "[[" <> label <> ">>" <> src <> "]]" <> genAnchor id'

inlineToXWiki (Image _ alt (source, tit)) = do
  alt' <- inlineListToXWiki alt
  let
    params = Text.unwords $ filter (not . Text.null) [
        if Text.null alt' then "" else "alt=\"" <> alt' <> "\"",
          if Text.null tit then "" else "title=\"" <> tit <> "\""
        ]
  return $ "[[image:" <> source <> (if Text.null params then "" else "||" <> params) <> "]]"

inlineToXWiki (Note contents) = do
  contents' <- blockListToXWiki contents
  return $ "{{footnote}}" <> Text.strip contents' <> "{{/footnote}}"

-- TODO: support attrs other than id (anchor)
inlineToXWiki (Span (id', _, _) contents) = do
  contents' <- inlineListToXWiki contents
  return $ genAnchor id' <> contents'

-- Utility method since (for now) all lists are handled the same way
blockToXWikiList :: PandocMonad m => Text -> [[Block]] -> XWikiReader m Text
blockToXWikiList marker contents = do
  lev <- asks listLevel
  contents' <- local (\s -> s { listLevel = listLevel s <> marker } ) $ mapM listItemToXWiki contents
  return $ vcat contents' <> if Text.null lev then "\n" else ""


listItemToXWiki :: PandocMonad m => [Block] -> XWikiReader m Text
listItemToXWiki contents = do
  marker <- asks listLevel
  contents' <- blockListToXWiki contents
  return $ marker <> ". " <> Text.strip contents'


-- | Convert definition list item (label, list of blocks) to MediaWiki.
definitionListItemToMediaWiki :: PandocMonad m
                              => ([Inline],[[Block]])
                              -> XWikiReader m Text
definitionListItemToMediaWiki (label, items) = do
  labelText <- inlineListToXWiki label
  contents <- mapM blockListToXWiki items
  marker <- asks listLevel
  return $ marker <> " " <> labelText <> "\n" <>
    intercalate "\n" (map (\d -> Text.init marker <> ": " <> d) contents)

-- Escape the escape character, as well as formatting pairs
escapeXWikiString :: Text -> Text
escapeXWikiString s = foldr (uncurry replace) s $ zip ["--", "**", "//", "^^", ",,", "~"] ["~-~-", "~*~*", "~/~/", "~^~^", "~,~,", "~~"]
