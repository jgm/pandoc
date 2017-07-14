{-
Copyright (C) 2015 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.CommonMark
   Copyright   : Copyright (C) 2015 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to CommonMark.

CommonMark:  <http://commonmark.org>
-}
module Text.Pandoc.Writers.CommonMark (writeCommonMark) where

import CMark
import Control.Monad.State.Strict (State, get, modify, runState)
import Data.Foldable (foldrM)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Shared (isTightList, linesToPara)
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Walk (walkM)
import Text.Pandoc.Writers.HTML (writeHtml5String)
import Text.Pandoc.Writers.Shared

-- | Convert Pandoc to CommonMark.
writeCommonMark :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeCommonMark opts (Pandoc meta blocks) = do
  let (blocks', notes) = runState (walkM processNotes blocks) []
      notes' = if null notes
               then []
               else [OrderedList (1, Decimal, Period) $ reverse notes]
  main <-  blocksToCommonMark opts (blocks' ++ notes')
  metadata <- metaToJSON opts
              (blocksToCommonMark opts)
              (inlinesToCommonMark opts)
              meta
  let context = defField "body" main $ metadata
  case writerTemplate opts of
       Nothing  -> return main
       Just tpl -> renderTemplate' tpl context

processNotes :: Inline -> State [[Block]] Inline
processNotes (Note bs) = do
  modify (bs :)
  notes <- get
  return $ Str $ "[" ++ show (length notes) ++ "]"
processNotes x = return x

node :: NodeType -> [Node] -> Node
node = Node Nothing

blocksToCommonMark :: PandocMonad m => WriterOptions -> [Block] -> m Text
blocksToCommonMark opts bs = do
  let cmarkOpts = [optHardBreaks | isEnabled Ext_hard_line_breaks opts]
      colwidth = if writerWrapText opts == WrapAuto
                 then Just $ writerColumns opts
                 else Nothing
  nodes <- blocksToNodes bs
  return $
    nodeToCommonmark cmarkOpts colwidth $
    node DOCUMENT nodes

inlinesToCommonMark :: PandocMonad m => WriterOptions -> [Inline] -> m Text
inlinesToCommonMark opts ils = return $
  nodeToCommonmark cmarkOpts colwidth $ node PARAGRAPH (inlinesToNodes ils)
   where cmarkOpts = [optHardBreaks | isEnabled Ext_hard_line_breaks opts]
         colwidth = if writerWrapText opts == WrapAuto
                       then Just $ writerColumns opts
                       else Nothing

blocksToNodes :: PandocMonad m => [Block] -> m [Node]
blocksToNodes = foldrM blockToNodes []

blockToNodes :: PandocMonad m => Block -> [Node] -> m [Node]
blockToNodes (Plain xs) ns = return (node PARAGRAPH (inlinesToNodes xs) : ns)
blockToNodes (Para xs) ns = return (node PARAGRAPH (inlinesToNodes xs) : ns)
blockToNodes (LineBlock lns) ns = blockToNodes (linesToPara lns) ns
blockToNodes (CodeBlock (_,classes,_) xs) ns = return $
  (node (CODE_BLOCK (T.pack (unwords classes)) (T.pack xs)) [] : ns)
blockToNodes (RawBlock fmt xs) ns
  | fmt == Format "html" = return (node (HTML_BLOCK (T.pack xs)) [] : ns)
  | otherwise = return (node (CUSTOM_BLOCK (T.pack xs) (T.empty)) [] : ns)
blockToNodes (BlockQuote bs) ns = do
  nodes <- blocksToNodes bs
  return (node BLOCK_QUOTE nodes : ns)
blockToNodes (BulletList items) ns = do
  nodes <- mapM blocksToNodes items
  return (node (LIST ListAttributes{
                   listType = BULLET_LIST,
                   listDelim = PERIOD_DELIM,
                   listTight = isTightList items,
                   listStart = 1 }) (map (node ITEM) nodes) : ns)
blockToNodes (OrderedList (start, _sty, delim) items) ns = do
  nodes <- mapM blocksToNodes items
  return (node (LIST ListAttributes{
                   listType = ORDERED_LIST,
                   listDelim = case delim of
                                 OneParen  -> PAREN_DELIM
                                 TwoParens -> PAREN_DELIM
                                 _         -> PERIOD_DELIM,
                   listTight = isTightList items,
                   listStart = start }) (map (node ITEM) nodes) : ns)
blockToNodes HorizontalRule ns = return (node THEMATIC_BREAK [] : ns)
blockToNodes (Header lev _ ils) ns = return (node (HEADING lev) (inlinesToNodes ils) : ns)
blockToNodes (Div _ bs) ns = do
  nodes <- blocksToNodes bs
  return (nodes ++ ns)
blockToNodes (DefinitionList items) ns = blockToNodes (BulletList items') ns
  where items' = map dlToBullet items
        dlToBullet (term, ((Para xs : ys) : zs))  =
          Para (term ++ [LineBreak] ++ xs) : ys ++ concat zs
        dlToBullet (term, ((Plain xs : ys) : zs)) =
          Plain (term ++ [LineBreak] ++ xs) : ys ++ concat zs
        dlToBullet (term, xs) =
          Para term : concat xs
blockToNodes t@(Table _ _ _ _ _) ns = do
  s <- writeHtml5String def $! Pandoc nullMeta [t]
  return (node (HTML_BLOCK s) [] : ns)
blockToNodes Null ns = return ns

inlinesToNodes :: [Inline] -> [Node]
inlinesToNodes  = foldr inlineToNodes []

inlineToNodes :: Inline -> [Node] -> [Node]
inlineToNodes (Str s) = (node (TEXT (T.pack s)) [] :)
inlineToNodes Space   = (node (TEXT (T.pack " ")) [] :)
inlineToNodes LineBreak = (node LINEBREAK [] :)
inlineToNodes SoftBreak = (node SOFTBREAK [] :)
inlineToNodes (Emph xs) = (node EMPH (inlinesToNodes xs) :)
inlineToNodes (Strong xs) = (node STRONG (inlinesToNodes xs) :)
inlineToNodes (Strikeout xs) =
  ((node (HTML_INLINE (T.pack "<s>")) [] : inlinesToNodes xs ++
   [node (HTML_INLINE (T.pack "</s>")) []]) ++ )
inlineToNodes (Superscript xs) =
  ((node (HTML_INLINE (T.pack "<sup>")) [] : inlinesToNodes xs ++
   [node (HTML_INLINE (T.pack "</sup>")) []]) ++ )
inlineToNodes (Subscript xs) =
  ((node (HTML_INLINE (T.pack "<sub>")) [] : inlinesToNodes xs ++
   [node (HTML_INLINE (T.pack "</sub>")) []]) ++ )
inlineToNodes (SmallCaps xs) =
  ((node (HTML_INLINE (T.pack "<span class=\"smallcaps\">")) []
    : inlinesToNodes xs ++
    [node (HTML_INLINE (T.pack "</span>")) []]) ++ )
inlineToNodes (Link _ ils (url,tit)) =
  (node (LINK (T.pack url) (T.pack tit)) (inlinesToNodes ils) :)
inlineToNodes (Image _ ils (url,tit)) =
  (node (IMAGE (T.pack url) (T.pack tit)) (inlinesToNodes ils) :)
inlineToNodes (RawInline fmt xs)
  | fmt == Format "html" = (node (HTML_INLINE (T.pack xs)) [] :)
  | otherwise = (node (CUSTOM_INLINE (T.pack xs) (T.empty)) [] :)
inlineToNodes (Quoted qt ils) =
  ((node (TEXT start) [] : inlinesToNodes ils ++ [node (TEXT end) []]) ++)
  where (start, end) = case qt of
                          SingleQuote -> (T.pack "‘", T.pack "’")
                          DoubleQuote -> (T.pack "“", T.pack "”")
inlineToNodes (Code _ str) = (node (CODE (T.pack str)) [] :)
inlineToNodes (Math mt str) =
  case mt of
    InlineMath  ->
      (node (HTML_INLINE (T.pack ("\\(" ++ str ++ "\\)"))) [] :)
    DisplayMath ->
      (node (HTML_INLINE (T.pack ("\\[" ++ str ++ "\\]"))) [] :)
inlineToNodes (Span _ ils) = (inlinesToNodes ils ++)
inlineToNodes (Cite _ ils) = (inlinesToNodes ils ++)
inlineToNodes (Note _) = id -- should not occur
-- we remove Note elements in preprocessing
