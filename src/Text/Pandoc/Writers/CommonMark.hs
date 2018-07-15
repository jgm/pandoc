{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2015-2018 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2015-2018 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to CommonMark.

CommonMark:  <http://commonmark.org>
-}
module Text.Pandoc.Writers.CommonMark (writeCommonMark) where

import Prelude
import CMarkGFM
import Control.Monad.State.Strict (State, get, modify, runState)
import Data.Foldable (foldrM)
import Data.List (transpose)
import Data.Monoid (Any (..))
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP (urlEncode)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Shared (isTightList, linesToPara, substitute)
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Walk (query, walk, walkM)
import Text.Pandoc.Writers.HTML (writeHtml5String, tagWithAttributes)
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
  let context = defField "body" main metadata
  case writerTemplate opts of
       Nothing  -> return main
       Just tpl -> renderTemplate' tpl context

softBreakToSpace :: Inline -> Inline
softBreakToSpace SoftBreak = Space
softBreakToSpace x         = x

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
  return $ T.stripEnd $
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
blockToNodes _ (CodeBlock (_,classes,_) xs) ns = return
  (node (CODE_BLOCK (T.pack (unwords classes)) (T.pack xs)) [] : ns)
blockToNodes opts (RawBlock fmt xs) ns
  | fmt == Format "html" && isEnabled Ext_raw_html opts
              = return (node (HTML_BLOCK (T.pack xs)) [] : ns)
  | (fmt == Format "latex" || fmt == Format "tex") && isEnabled Ext_raw_tex opts
              = return (node (CUSTOM_BLOCK (T.pack xs) T.empty) [] : ns)
  | otherwise = return ns
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
blockToNodes opts (Div attr bs) ns = do
  nodes <- blocksToNodes opts bs
  let op = tagWithAttributes opts True False "div" attr
  if isEnabled Ext_raw_html opts
     then return (node (HTML_BLOCK op) [] : nodes ++
                  [node (HTML_BLOCK (T.pack "</div>")) []] ++ ns)
     else return (nodes ++ ns)
blockToNodes opts (DefinitionList items) ns =
  blockToNodes opts (BulletList items') ns
  where items' = map dlToBullet items
        dlToBullet (term, (Para xs : ys) : zs)  =
          Para (term ++ [LineBreak] ++ xs) : ys ++ concat zs
        dlToBullet (term, (Plain xs : ys) : zs) =
          Plain (term ++ [LineBreak] ++ xs) : ys ++ concat zs
        dlToBullet (term, xs) =
          Para term : concat xs
blockToNodes opts t@(Table capt aligns _widths headers rows) ns = do
  let allcells = concat (headers:rows)
  let isLineBreak LineBreak = Any True
      isLineBreak _         = Any False
  let isPlainOrPara [Para _]  = True
      isPlainOrPara [Plain _] = True
      isPlainOrPara []        = True
      isPlainOrPara _         = False
  let isSimple = all isPlainOrPara allcells &&
                 not ( getAny (query isLineBreak allcells) )
  if isEnabled Ext_pipe_tables opts && isSimple
     then do
       -- We construct a table manually as a CUSTOM_BLOCK, for
       -- two reasons:  (1) cmark-gfm currently doesn't support
       -- rendering TABLE nodes; (2) we can align the column sides;
       -- (3) we can render the caption as a regular paragraph.
       let capt' = node PARAGRAPH (inlinesToNodes opts capt)
       -- backslash | in code and raw:
       let fixPipe (Code attr xs) =
             Code attr (substitute "|" "\\|" xs)
           fixPipe (RawInline format xs) =
             RawInline format (substitute "|" "\\|" xs)
           fixPipe x = x
       let toCell [Plain ils] = T.strip
                                $ nodeToCommonmark [] Nothing
                                $ node (CUSTOM_INLINE mempty mempty)
                                $ inlinesToNodes opts
                                $ walk (fixPipe . softBreakToSpace) ils
           toCell [Para  ils] = T.strip
                                $ nodeToCommonmark [] Nothing
                                $ node (CUSTOM_INLINE mempty mempty)
                                $ inlinesToNodes opts
                                $ walk (fixPipe . softBreakToSpace) ils
           toCell []          = ""
           toCell xs          = error $ "toCell encountered " ++ show xs
       let separator = " | "
       let starter = "| "
       let ender   = " |"
       let rawheaders = map toCell headers
       let rawrows = map (map toCell) rows
       let maximum' [] = 0
           maximum' xs = maximum xs
       let colwidths = map (maximum' . map T.length) $
                        transpose (rawheaders:rawrows)
       let toHeaderLine len AlignDefault = T.replicate len "-"
           toHeaderLine len AlignLeft    = ":" <>
                  T.replicate (max (len - 1) 1) "-"
           toHeaderLine len AlignRight   =
                  T.replicate (max (len - 1) 1) "-" <> ":"
           toHeaderLine len AlignCenter  = ":" <>
                  T.replicate (max (len - 2) 1) (T.pack "-") <> ":"
       let rawheaderlines = zipWith toHeaderLine colwidths aligns
       let headerlines = starter <> T.intercalate separator rawheaderlines <>
                          ender
       let padContent (align, w) t' =
             let padding = w - T.length t'
                 halfpadding = padding `div` 2
             in  case align of
                      AlignRight -> T.replicate padding " " <> t'
                      AlignCenter -> T.replicate halfpadding " " <> t' <>
                                     T.replicate (padding - halfpadding) " "
                      _ -> t' <> T.replicate padding " "
       let toRow xs = starter <> T.intercalate separator
                      (zipWith padContent (zip aligns colwidths) xs) <>
                      ender
       let table' = toRow rawheaders <> "\n" <> headerlines <> "\n" <>
                     T.intercalate "\n" (map toRow rawrows)
       return (node (CUSTOM_BLOCK table' mempty) [] :
               if null capt
                  then ns
                  else capt' : ns)
     else do -- fall back to raw HTML
       s <- writeHtml5String def $! Pandoc nullMeta [t]
       return (node (HTML_BLOCK s) [] : ns)
blockToNodes _ Null ns = return ns

inlinesToNodes :: WriterOptions -> [Inline] -> [Node]
inlinesToNodes opts  = foldr (inlineToNodes opts) []

inlineToNodes :: WriterOptions -> Inline -> [Node] -> [Node]
inlineToNodes opts (Str s) = (node (TEXT (T.pack s')) [] :)
  where s' = if isEnabled Ext_smart opts
                then unsmartify opts s
                else s
inlineToNodes _ Space   = (node (TEXT (T.pack " ")) [] :)
inlineToNodes _ LineBreak = (node LINEBREAK [] :)
inlineToNodes opts SoftBreak
  | isEnabled Ext_hard_line_breaks opts = (node LINEBREAK [] :)
  | writerWrapText opts == WrapNone     = (node (TEXT " ") [] :)
  | otherwise                           = (node SOFTBREAK [] :)
inlineToNodes opts (Emph xs) = (node EMPH (inlinesToNodes opts xs) :)
inlineToNodes opts (Strong xs) = (node STRONG (inlinesToNodes opts xs) :)
inlineToNodes opts (Strikeout xs) =
  if isEnabled Ext_strikeout opts
     then (node (CUSTOM_INLINE "~~" "~~") (inlinesToNodes opts xs) :)
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
-- title beginning with fig: indicates implicit figure
inlineToNodes opts (Image alt ils (url,'f':'i':'g':':':tit)) =
  inlineToNodes opts (Image alt ils (url,tit))
inlineToNodes opts (Image _ ils (url,tit)) =
  (node (IMAGE (T.pack url) (T.pack tit)) (inlinesToNodes opts ils) :)
inlineToNodes opts (RawInline fmt xs)
  | fmt == Format "html" && isEnabled Ext_raw_html opts
              = (node (HTML_INLINE (T.pack xs)) [] :)
  | (fmt == Format "latex" || fmt == Format "tex") && isEnabled Ext_raw_tex opts
              = (node (CUSTOM_INLINE (T.pack xs) T.empty) [] :)
  | otherwise = id
inlineToNodes opts (Quoted qt ils) =
  ((node (TEXT start) [] :
   inlinesToNodes opts ils ++ [node (TEXT end) []]) ++)
  where (start, end) = case qt of
                          SingleQuote
                            | isEnabled Ext_smart opts -> ("'","'")
                            | otherwise -> ("‘", "’")
                          DoubleQuote
                            | isEnabled Ext_smart opts -> ("\"", "\"")
                            | otherwise -> ("“", "”")
inlineToNodes _ (Code _ str) = (node (CODE (T.pack str)) [] :)
inlineToNodes opts (Math mt str) =
  case writerHTMLMathMethod opts of
       WebTeX url ->
           let core = inlineToNodes opts
                        (Image nullAttr [Str str] (url ++ urlEncode str, str))
               sep = if mt == DisplayMath
                        then (node LINEBREAK [] :)
                        else id
           in  (sep . core . sep)
       _  ->
           case mt of
            InlineMath  ->
              (node (HTML_INLINE (T.pack ("\\(" ++ str ++ "\\)"))) [] :)
            DisplayMath ->
              (node (HTML_INLINE (T.pack ("\\[" ++ str ++ "\\]"))) [] :)
inlineToNodes opts (Span ("",["emoji"],kvs) [Str s]) = do
  case lookup "data-emoji" kvs of
       Just emojiname | isEnabled Ext_emoji opts ->
            (node (TEXT (":" <> T.pack emojiname <> ":")) [] :)
       _ -> (node (TEXT (T.pack s)) [] :)
inlineToNodes opts (Span attr ils) =
  let nodes = inlinesToNodes opts ils
      op = tagWithAttributes opts True False "span" attr
  in  if isEnabled Ext_raw_html opts
         then ((node (HTML_INLINE op) [] : nodes ++
                [node (HTML_INLINE (T.pack "</span>")) []]) ++)
         else (nodes ++)
inlineToNodes opts (Cite _ ils) = (inlinesToNodes opts ils ++)
inlineToNodes _ (Note _) = id -- should not occur
-- we remove Note elements in preprocessing
