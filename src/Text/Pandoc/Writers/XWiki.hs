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
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Text (Text, concat, init, intercalate, pack, replace, unlines, unwords)
import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Shared (escapeURI, isURI, linesToPara)
import Text.Pandoc.Writers.MediaWiki (highlightingLangs)

data WriterState = WriterState {
  listLevel :: Text -- String at the beginning of items
}

type XWikiReader m = ReaderT WriterState m

-- | Convert Pandoc to XWiki.
writeXWiki :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeXWiki _ (Pandoc _ blocks) =
  let env = WriterState { listLevel = "" }
  in do
  body <- runReaderT (blockListToXWiki blocks) env
  return $ body

-- | Concatenates strings with line breaks between them.
vcat :: [Text] -> Text
vcat = intercalate "\n"

blockListToXWiki :: PandocMonad m => [Block] -> XWikiReader m Text
blockListToXWiki blocks =
  fmap vcat $ mapM blockToXWiki blocks

blockToXWiki :: PandocMonad m => Block -> XWikiReader m Text

blockToXWiki Null = return ""

-- TODO: handle this
blockToXWiki (Div _ blocks) =
  blockListToXWiki blocks

blockToXWiki (Plain inlines) =
  inlineListToXWiki inlines

blockToXWiki (Para inlines) = do
  contents <- inlineListToXWiki inlines
  return $ contents <> "\n\n"

blockToXWiki (LineBlock lns) =
  blockToXWiki $ linesToPara lns

blockToXWiki b@(RawBlock f str)
  | f == Format "xwiki" = return $ pack str
  | otherwise           = "" <$ report (BlockNotRendered b)

blockToXWiki HorizontalRule = return "\n----\n"

blockToXWiki (Header level _ inlines) = do
  contents <- inlineListToXWiki inlines
  let eqs = Text.replicate level "="
  return $ eqs <> " " <> contents <> " " <> eqs <> "\n"

-- XWiki doesn't appear to differentiate between inline and block-form code, so we delegate
blockToXWiki (CodeBlock attrs str) = inlineToXWiki (Code attrs str)

-- TODO: Figure out how to handle this better
blockToXWiki (BlockQuote blocks) =
  blockListToXWiki blocks

blockToXWiki (BulletList contents) = blockToXWikiList "*" $ contents

blockToXWiki (OrderedList _ contents) = blockToXWikiList "1" $ contents

blockToXWiki (DefinitionList items) = do
  lev <- asks listLevel
  contents <- local (\s -> s { listLevel = listLevel s <> ";" }) $ mapM definitionListItemToMediaWiki items
  return $ vcat contents <> if Text.null lev then "\n" else ""

-- TODO: support more features
blockToXWiki (Table _ _ _ headers rows') = do
  headers' <- mapM (tableCellXWiki True) headers
  otherRows <- mapM formRow rows'
  return $ Data.Text.unlines (Data.Text.unwords headers':otherRows)

formRow :: PandocMonad m => [[Block]] -> XWikiReader m Text
formRow row = do
  cellStrings <- mapM (tableCellXWiki False) row
  return $ Data.Text.unwords cellStrings


tableCellXWiki :: PandocMonad m => Bool -> [Block] -> XWikiReader m Text
tableCellXWiki isHeader cell = do
  contents <- blockListToXWiki cell
  let cellBorder = if isHeader then "|=" else "|"
  return $ cellBorder <> contents
  

inlineListToXWiki :: PandocMonad m => [Inline] -> XWikiReader m Text
inlineListToXWiki lst =
  fmap Data.Text.concat $ mapM inlineToXWiki lst


inlineToXWiki :: PandocMonad m => Inline -> XWikiReader m Text

inlineToXWiki (Str str) = return $ escapeXWikiString $ pack str

inlineToXWiki Space = return " "

inlineToXWiki LineBreak = return "\n"

-- FIXME: better understand this
inlineToXWiki SoftBreak = return " "

inlineToXWiki (Emph lst) = do
  contents <- inlineListToXWiki lst
  return $ "//" <> contents <> "//"

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
inlineToXWiki (SmallCaps lst) = do
  contents <- inlineListToXWiki lst
  return contents

inlineToXWiki (Quoted SingleQuote lst) = do
  contents <- inlineListToXWiki lst
  return $ "'" <> contents <> "'"

inlineToXWiki (Quoted DoubleQuote lst) = do
  contents <- inlineListToXWiki lst
  return $ "\"" <> contents <> "\""

inlineToXWiki (Code (_,classes,_) contents') = do
  let at  = Set.fromList classes `Set.intersection` highlightingLangs
  let contents = pack contents'
  return $
    case Set.toList at of
      [] -> "{{" <> contents <> "}}"
      (l:_) -> "{{code language=\"" <> (pack l) <> "\"}}" <> contents <> "{{/code}}"

inlineToXWiki (Cite _ lst) = inlineListToXWiki lst

-- FIXME: optionally support this (plugin?) 
inlineToXWiki (Math _ str) = return $ pack str

inlineToXWiki il@(RawInline frmt str)
  | frmt == Format "xwiki" = return $ pack str
  | otherwise              = "" <$ report (InlineNotRendered il)

-- TODO: Handle anchors
inlineToXWiki (Link _ txt (src, _)) = do
  label <- inlineListToXWiki txt
  case txt of
     [Str s] | isURI src && escapeURI s == src -> return $ pack src
     _  -> return $ "[[" <> label <> ">>" <> (pack src) <> "]]"

inlineToXWiki (Image _ alt (source, tit)) = do
  alt' <- inlineListToXWiki alt
  let
    titText = pack tit
    params = intercalate " " [
        if Text.null alt' then "" else "alt=\"" <> alt' <> "\"",
          if Text.null titText then "" else "title=\"" <> titText <> "\""
        ]
  return $ "[[image:" <> (pack source) <> (if Text.null params then "" else "|| " <> params) <> "]]"

inlineToXWiki (Note contents) = blockListToXWiki contents

-- FIXME: support attrs
inlineToXWiki (Span _ contents) = inlineListToXWiki contents
  
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
  return $ marker <> ". " <> contents'


-- | Convert definition list item (label, list of blocks) to MediaWiki.
definitionListItemToMediaWiki :: PandocMonad m
                              => ([Inline],[[Block]])
                              -> XWikiReader m Text
definitionListItemToMediaWiki (label, items) = do
  labelText <- inlineListToXWiki label
  contents <- mapM blockListToXWiki items
  marker <- asks listLevel
  return $ marker <> " " <> labelText <> "\n" <>
    intercalate "\n" (map (\d -> (Data.Text.init marker) <> ": " <> d) contents)

escapeXWikiString :: Text -> Text
escapeXWikiString = replace "~" "~~"

