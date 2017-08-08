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

import CMarkGFM
import Control.Monad.State.Strict (State, get, modify, runState)
import Data.Foldable (foldrM)
import Data.Monoid (Any (..))
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Shared (isTightList, linesToPara)
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Walk (walkM, walk, query)
import Text.Pandoc.Writers.HTML (writeHtml5String)
import Text.Pandoc.Writers.Shared

-- | Convert Pandoc to CommonMark.
writeCommonMark :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeCommonMark opts (Pandoc meta blocks) = do
  let (blocks', notes) = runState (walkM processNotes blocks) []
      notes' = if null notes
               then []
               else [OrderedList (1, Decimal, Period) $ reverse notes]
  let softBreakToSpace SoftBreak = Space
      softBreakToSpace x = x
  let blocks'' = if writerWrapText opts == WrapNone
                    then walk softBreakToSpace blocks'
                    else blocks'
  main <-  blocksToCommonMark opts (blocks'' ++ notes')
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
  nodes <- blocksToNodes opts bs
  return $
    nodeToCommonmark cmarkOpts colwidth $
    node DOCUMENT nodes

inlinesToCommonMark :: PandocMonad m => WriterOptions -> [Inline] -> m Text
inlinesToCommonMark opts ils = return $
  nodeToCommonmark cmarkOpts colwidth $
    node PARAGRAPH (inlinesToNodes opts ils)
   where cmarkOpts = [optHardBreaks | isEnabled Ext_hard_line_breaks opts]
         colwidth = if writerWrapText opts == WrapAuto
                       then Just $ writerColumns opts
                       else Nothing

blocksToNodes :: PandocMonad m => WriterOptions -> [Block] -> m [Node]
blocksToNodes opts = foldrM (blockToNodes opts) []

blockToNodes :: PandocMonad m => WriterOptions -> Block -> [Node] -> m [Node]
blockToNodes opts (Plain xs) ns =
  return (node PARAGRAPH (inlinesToNodes opts xs) : ns)
blockToNodes opts (Para xs) ns =
  return (node PARAGRAPH (inlinesToNodes opts xs) : ns)
blockToNodes opts (LineBlock lns) ns = blockToNodes opts (linesToPara lns) ns
blockToNodes _ (CodeBlock (_,classes,_) xs) ns = return $
  (node (CODE_BLOCK (T.pack (unwords classes)) (T.pack xs)) [] : ns)
blockToNodes _ (RawBlock fmt xs) ns
  | fmt == Format "html" = return (node (HTML_BLOCK (T.pack xs)) [] : ns)
  | otherwise = return (node (CUSTOM_BLOCK (T.pack xs) (T.empty)) [] : ns)
blockToNodes opts (BlockQuote bs) ns = do
  nodes <- blocksToNodes opts bs
  return (node BLOCK_QUOTE nodes : ns)
blockToNodes opts (BulletList items) ns = do
  nodes <- mapM (blocksToNodes opts) items
  return (node (LIST ListAttributes{
                   listType = BULLET_LIST,
                   listDelim = PERIOD_DELIM,
                   listTight = isTightList items,
                   listStart = 1 }) (map (node ITEM) nodes) : ns)
blockToNodes opts (OrderedList (start, _sty, delim) items) ns = do
  nodes <- mapM (blocksToNodes opts) items
  return (node (LIST ListAttributes{
                   listType = ORDERED_LIST,
                   listDelim = case delim of
                                 OneParen  -> PAREN_DELIM
                                 TwoParens -> PAREN_DELIM
                                 _         -> PERIOD_DELIM,
                   listTight = isTightList items,
                   listStart = start }) (map (node ITEM) nodes) : ns)
blockToNodes _ HorizontalRule ns = return (node THEMATIC_BREAK [] : ns)
blockToNodes opts (Header lev _ ils) ns =
  return (node (HEADING lev) (inlinesToNodes opts ils) : ns)
blockToNodes opts (Div _ bs) ns = do
  nodes <- blocksToNodes opts bs
  return (nodes ++ ns)
blockToNodes opts (DefinitionList items) ns =
  blockToNodes opts (BulletList items') ns
  where items' = map dlToBullet items
        dlToBullet (term, ((Para xs : ys) : zs))  =
          Para (term ++ [LineBreak] ++ xs) : ys ++ concat zs
        dlToBullet (term, ((Plain xs : ys) : zs)) =
          Plain (term ++ [LineBreak] ++ xs) : ys ++ concat zs
        dlToBullet (term, xs) =
          Para term : concat xs
blockToNodes opts t@(Table _capt aligns widths headers rows) ns = do
  let allrows = headers:rows
  let isLineBreak LineBreak = Any True
      isLineBreak _         = Any False
  let isSimple = all (==0) widths &&
                 not ( getAny (query isLineBreak allrows) )
  if isEnabled Ext_pipe_tables opts && isSimple
     then do
       let toAlign AlignDefault = NoAlignment
           toAlign AlignLeft    = LeftAligned
           toAlign AlignCenter  = CenterAligned
           toAlign AlignRight   = RightAligned
       let aligns' = map toAlign aligns
       let toCell bs   = node TABLE_CELL <$> blocksToNodes opts bs
       let toRow cells = node TABLE_ROW <$> mapM toCell cells
       cmrows <- mapM toRow allrows
       return (node (TABLE aligns') cmrows : ns)
     else do -- fall back to raw HTML
       s <- writeHtml5String def $! Pandoc nullMeta [t]
       return (node (HTML_BLOCK s) [] : ns)
blockToNodes _ Null ns = return ns

inlinesToNodes :: WriterOptions -> [Inline] -> [Node]
inlinesToNodes opts  = foldr (inlineToNodes opts) []

inlineToNodes :: WriterOptions -> Inline -> [Node] -> [Node]
inlineToNodes _ (Str s) = (node (TEXT (T.pack s)) [] :)
inlineToNodes _ Space   = (node (TEXT (T.pack " ")) [] :)
inlineToNodes _ LineBreak = (node LINEBREAK [] :)
inlineToNodes _ SoftBreak = (node SOFTBREAK [] :)
inlineToNodes opts (Emph xs) = (node EMPH (inlinesToNodes opts xs) :)
inlineToNodes opts (Strong xs) = (node STRONG (inlinesToNodes opts xs) :)
inlineToNodes opts (Strikeout xs) =
  if isEnabled Ext_strikeout opts
     then (node STRIKETHROUGH (inlinesToNodes opts xs) :)
     else ((node (HTML_INLINE (T.pack "<s>")) [] : inlinesToNodes opts xs ++
           [node (HTML_INLINE (T.pack "</s>")) []]) ++ )
inlineToNodes opts (Superscript xs) =
  ((node (HTML_INLINE (T.pack "<sup>")) [] : inlinesToNodes opts xs ++
   [node (HTML_INLINE (T.pack "</sup>")) []]) ++ )
inlineToNodes opts (Subscript xs) =
  ((node (HTML_INLINE (T.pack "<sub>")) [] : inlinesToNodes opts xs ++
   [node (HTML_INLINE (T.pack "</sub>")) []]) ++ )
inlineToNodes opts (SmallCaps xs) =
  ((node (HTML_INLINE (T.pack "<span class=\"smallcaps\">")) []
    : inlinesToNodes opts xs ++
    [node (HTML_INLINE (T.pack "</span>")) []]) ++ )
inlineToNodes opts (Link _ ils (url,tit)) =
  (node (LINK (T.pack url) (T.pack tit)) (inlinesToNodes opts ils) :)
inlineToNodes opts (Image _ ils (url,tit)) =
  (node (IMAGE (T.pack url) (T.pack tit)) (inlinesToNodes opts ils) :)
inlineToNodes _ (RawInline fmt xs)
  | fmt == Format "html" = (node (HTML_INLINE (T.pack xs)) [] :)
  | otherwise = (node (CUSTOM_INLINE (T.pack xs) (T.empty)) [] :)
inlineToNodes opts (Quoted qt ils) =
  ((node (TEXT start) [] :
   inlinesToNodes opts ils ++ [node (TEXT end) []]) ++)
  where (start, end) = case qt of
                          SingleQuote -> (T.pack "‘", T.pack "’")
                          DoubleQuote -> (T.pack "“", T.pack "”")
inlineToNodes _ (Code _ str) = (node (CODE (T.pack str)) [] :)
inlineToNodes _ (Math mt str) =
  case mt of
    InlineMath  ->
      (node (HTML_INLINE (T.pack ("\\(" ++ str ++ "\\)"))) [] :)
    DisplayMath ->
      (node (HTML_INLINE (T.pack ("\\[" ++ str ++ "\\]"))) [] :)
inlineToNodes opts (Span _ ils) = (inlinesToNodes opts ils ++)
inlineToNodes opts (Cite _ ils) = (inlinesToNodes opts ils ++)
inlineToNodes _ (Note _) = id -- should not occur
-- we remove Note elements in preprocessing
