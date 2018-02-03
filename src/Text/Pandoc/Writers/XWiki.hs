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
import Data.List (intercalate)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Shared (escapeURI, isURI, linesToPara)
import Text.Pandoc.Writers.MediaWiki (highlightingLangs)

data WriterState = WriterState {
  listLevel :: String -- String at the beginning of items
}

type XWikiReader m = ReaderT WriterState m

-- | Convert Pandoc to XWiki.
writeXWiki :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeXWiki _ (Pandoc _ blocks) =
  let env = WriterState { listLevel = [] }
  in do
  body <- runReaderT (blockListToXWiki blocks) env
  return $ pack body

-- | Concatenates strings with line breaks between them.
vcat :: [String] -> String
vcat = intercalate "\n"

blockListToXWiki :: PandocMonad m => [Block] -> XWikiReader m String
blockListToXWiki blocks =
  fmap vcat $ mapM blockToXWiki blocks

blockToXWiki :: PandocMonad m => Block -> XWikiReader m String

blockToXWiki Null = return ""

-- TODO: handle this
blockToXWiki (Div _ blocks) =
  blockListToXWiki blocks

blockToXWiki (Plain inlines) =
  inlineListToXWiki inlines

blockToXWiki (Para inlines) = do
  contents <- inlineListToXWiki inlines
  return $ contents ++ "\n\n"

blockToXWiki (LineBlock lns) =
  blockToXWiki $ linesToPara lns

blockToXWiki b@(RawBlock f str)
  | f == Format "xwiki" = return str
  | otherwise           = "" <$ report (BlockNotRendered b)

blockToXWiki HorizontalRule = return "\n----\n"

blockToXWiki (Header level _ inlines) = do
  contents <- inlineListToXWiki inlines
  let eqs = replicate level '='
  return $ eqs ++ " " ++ contents ++ " " ++ eqs ++ "\n"

-- XWiki doesn't appear to differentiate between inline and block-form code, so we delegate
blockToXWiki (CodeBlock attrs str) = inlineToXWiki (Code attrs str)

-- TODO: Figure out how to handle this better
blockToXWiki (BlockQuote blocks) =
  blockListToXWiki blocks

blockToXWiki (BulletList contents) = blockToXWikiList "*" contents

blockToXWiki (OrderedList _ contents) = blockToXWikiList "1" contents

blockToXWiki (DefinitionList items) = do
  lev <- asks listLevel
  contents <- local (\s -> s { listLevel = listLevel s ++ ";" }) $ mapM definitionListItemToMediaWiki items
  return $ vcat contents ++ if null lev then "\n" else ""

-- TODO: support more features
blockToXWiki (Table _ _ _ headers rows') = do
  headers' <- mapM (tableCellXWiki True) headers
  otherRows <- mapM formRow rows'
  return $ unlines (unwords headers':otherRows)

formRow :: PandocMonad m => [[Block]] -> XWikiReader m String
formRow row = do
  cellStrings <- mapM (tableCellXWiki False) row
  return $ unwords cellStrings


tableCellXWiki :: PandocMonad m => Bool -> [Block] -> XWikiReader m String
tableCellXWiki isHeader cell = do
  contents <- blockListToXWiki cell
  let cellBorder = if isHeader then "|=" else "|"
  return $ cellBorder ++ contents
  

inlineListToXWiki :: PandocMonad m => [Inline] -> XWikiReader m String
inlineListToXWiki lst =
  fmap concat $ mapM inlineToXWiki lst


inlineToXWiki :: PandocMonad m => Inline -> XWikiReader m String

inlineToXWiki (Str str) = return $ escapeXWikiString str

inlineToXWiki Space = return " "

inlineToXWiki LineBreak = return "\n"

-- FIXME: better understand this
inlineToXWiki SoftBreak = return " "

inlineToXWiki (Emph lst) = do
  contents <- inlineListToXWiki lst
  return $ "//" ++ contents ++ "//"

inlineToXWiki (Strong lst) = do
  contents <- inlineListToXWiki lst
  return $ "**" ++ contents ++ "**"

inlineToXWiki (Strikeout lst) = do
  contents <- inlineListToXWiki lst
  return $ "--" ++ contents ++ "--"

inlineToXWiki (Superscript lst) = do
  contents <- inlineListToXWiki lst
  return $ "^^" ++ contents ++ "^^"

inlineToXWiki (Subscript lst) = do
  contents <- inlineListToXWiki lst
  return $ ",," ++ contents ++ ",,"

-- TODO: Not supported. Maybe escape to HTML?
inlineToXWiki (SmallCaps lst) = do
  contents <- inlineListToXWiki lst
  return contents

inlineToXWiki (Quoted SingleQuote lst) = do
  contents <- inlineListToXWiki lst
  return $ "'" ++ contents ++ "'"

inlineToXWiki (Quoted DoubleQuote lst) = do
  contents <- inlineListToXWiki lst
  return $ "\"" ++ contents ++ "\""

inlineToXWiki (Code (_,classes,_) contents) = do
  let at  = Set.fromList classes `Set.intersection` highlightingLangs
  return $
    case Set.toList at of
      [] -> "{{" ++ contents ++ "}}"
      (l:_) -> "{{code language=\"" ++ l ++ "\"}}" ++ contents ++ "{{/code}}"

inlineToXWiki (Cite _ lst) = inlineListToXWiki lst

-- FIXME: optionally support this (plugin?) 
inlineToXWiki (Math _ str) = return str

inlineToXWiki il@(RawInline frmt str)
  | frmt == Format "xwiki" = return str
  | otherwise              = "" <$ report (InlineNotRendered il)

-- TODO: Handle anchors
inlineToXWiki (Link _ txt (src, _)) = do
  label <- inlineListToXWiki txt
  case txt of
     [Str s] | isURI src && escapeURI s == src -> return src
     _  -> return $ "[[" ++ label ++ ">>" ++ src ++ "]]"

inlineToXWiki (Image _ alt (source, tit)) = do
  alt' <- inlineListToXWiki alt
  let params = intercalate " " [
        if null alt' then [] else "alt=\"" ++ alt' ++ "\"",
          if null tit  then [] else "title=\"" ++ tit ++ "\""
        ]
  return $ "[[image:" ++ source ++ (if null params then "" else "|| " ++ params) ++ "]]"

inlineToXWiki (Note contents) = blockListToXWiki contents

-- FIXME: support attrs
inlineToXWiki (Span _ contents) = inlineListToXWiki contents
  
-- Utility method since (for now) all lists are handled the same way
blockToXWikiList :: PandocMonad m => String -> [[Block]] -> XWikiReader m String
blockToXWikiList marker contents = do
  lev <- asks listLevel
  contents' <- local (\s -> s { listLevel = listLevel s ++ marker } ) $ mapM listItemToXWiki contents
  return $ vcat contents' ++ if null lev then "\n" else ""
  

listItemToXWiki :: PandocMonad m => [Block] -> XWikiReader m String
listItemToXWiki contents = do
  marker <- asks listLevel
  contents' <- blockListToXWiki contents
  return $ marker ++ ". " ++ contents'


-- | Convert definition list item (label, list of blocks) to MediaWiki.
definitionListItemToMediaWiki :: PandocMonad m
                              => ([Inline],[[Block]])
                              -> XWikiReader m String
definitionListItemToMediaWiki (label, items) = do
  labelText <- inlineListToXWiki label
  contents <- mapM blockListToXWiki items
  marker <- asks listLevel
  return $ marker ++ " " ++ labelText ++ "\n" ++
           intercalate "\n" (map (\d -> init marker ++ ": " ++ d) contents)

escapeXWikiString :: String -> String
escapeXWikiString (x:xs)
  | x == '~' = "~~" ++ escapeXWikiString xs
  | otherwise = x : escapeXWikiString xs
escapeXWikiString [] = []

