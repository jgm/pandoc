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
   Module      : Text.Pandoc.Readers.CommonMark
   Copyright   : Copyright (C) 2015 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of CommonMark-formatted plain text to 'Pandoc' document.

CommonMark is a strongly specified variant of Markdown: http://commonmark.org.
-}
module Text.Pandoc.Readers.CommonMark (readCommonMark)
where

import CMarkGFM
import Control.Monad.State
import Data.Char (isLetter, isAlphaNum, isSpace, toLower)
import Data.List (groupBy)
import Data.Text (Text, unpack)
import qualified Data.Map as Map
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Emoji (emojis)
import Text.Pandoc.Options
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Walk (walkM, walk)

-- | Parse a CommonMark formatted string into a 'Pandoc' structure.
readCommonMark :: PandocMonad m => ReaderOptions -> Text -> m Pandoc
readCommonMark opts s = return $
  (if enabled Ext_gfm_auto_identifiers
      then addHeaderIdentifiers
      else id) $
  (if enabled Ext_emoji
      then addEmojis
      else id) $
  (if enabled Ext_hard_line_breaks
      then walk softToHardBreaks
      else id) $
  nodeToPandoc $ commonmarkToNode opts' exts s
  where opts' = [ optSmart | enabled Ext_smart ]
        exts = [ extStrikethrough | enabled Ext_strikeout ] ++
               [ extTable | enabled Ext_pipe_tables ] ++
               [ extAutolink | enabled Ext_autolink_bare_uris ]
        enabled x = extensionEnabled x (readerExtensions opts)

softToHardBreaks :: Inline -> Inline
softToHardBreaks SoftBreak = LineBreak
softToHardBreaks x = x

addEmojis :: Pandoc -> Pandoc
addEmojis = walk go
  where go (Str xs) = Str (convertEmojis xs)
        go x = x
        convertEmojis (':':xs) =
           case break (==':') xs of
                (ys,':':zs) ->
                   case Map.lookup ys emojis of
                        Just s  -> s ++ convertEmojis zs
                        Nothing -> ':' : ys ++ convertEmojis (':':zs)
                _ -> ':':xs
        convertEmojis (x:xs) = x : convertEmojis xs
        convertEmojis [] = []

addHeaderIdentifiers :: Pandoc -> Pandoc
addHeaderIdentifiers doc = evalState (walkM addHeaderId doc) mempty

addHeaderId :: Block -> State (Map.Map String Int) Block
addHeaderId (Header lev (_,classes,kvs) ils) = do
  idmap <- get
  let ident = toIdent ils
  ident' <- case Map.lookup ident idmap of
                 Nothing -> do
                   put (Map.insert ident 1 idmap)
                   return ident
                 Just i -> do
                   put (Map.adjust (+ 1) ident idmap)
                   return (ident ++ "-" ++ show i)
  return $ Header lev (ident',classes,kvs) ils
addHeaderId x = return x

toIdent :: [Inline] -> String
toIdent =   map (\c -> if isSpace c then '-' else c)
          . filter (\c -> isLetter c || isAlphaNum c || isSpace c ||
                           c == '_' || c == '-')
          . map toLower . stringify

nodeToPandoc :: Node -> Pandoc
nodeToPandoc (Node _ DOCUMENT nodes) =
  Pandoc nullMeta $ foldr addBlock [] nodes
nodeToPandoc n =  -- shouldn't happen
  Pandoc nullMeta $ foldr addBlock [] [n]

addBlocks :: [Node] -> [Block]
addBlocks = foldr addBlock []

addBlock :: Node -> [Block] -> [Block]
addBlock (Node _ PARAGRAPH nodes) =
  (Para (addInlines nodes) :)
addBlock (Node _ THEMATIC_BREAK _) =
  (HorizontalRule :)
addBlock (Node _ BLOCK_QUOTE nodes) =
  (BlockQuote (addBlocks nodes) :)
addBlock (Node _ (HTML_BLOCK t) _) =
  (RawBlock (Format "html") (unpack t) :)
-- Note:  the cmark parser will never generate CUSTOM_BLOCK,
-- so we don't need to handle it:
addBlock (Node _ (CUSTOM_BLOCK _onEnter _onExit) _nodes) =
  id
addBlock (Node _ (CODE_BLOCK info t) _) =
  (CodeBlock ("", take 1 (words (unpack info)), []) (unpack t) :)
addBlock (Node _ (HEADING lev) nodes) =
  (Header lev ("",[],[]) (addInlines nodes) :)
addBlock (Node _ (LIST listAttrs) nodes) =
  (constructor (map (setTightness . addBlocks . children) nodes) :)
  where constructor = case listType listAttrs of
                       BULLET_LIST  -> BulletList
                       ORDERED_LIST -> OrderedList
                                         (start, DefaultStyle, delim)
        start = listStart listAttrs
        setTightness = if listTight listAttrs
                           then map paraToPlain
                           else id
        paraToPlain (Para xs) = Plain (xs)
        paraToPlain x         = x
        delim = case listDelim listAttrs of
                     PERIOD_DELIM -> Period
                     PAREN_DELIM  -> OneParen
addBlock (Node _ (TABLE alignments) nodes) = do
  (Table [] aligns widths headers rows :)
  where aligns = map fromTableCellAlignment alignments
        fromTableCellAlignment NoAlignment   = AlignDefault
        fromTableCellAlignment LeftAligned   = AlignLeft
        fromTableCellAlignment RightAligned  = AlignRight
        fromTableCellAlignment CenterAligned = AlignCenter
        widths = replicate numcols 0.0
        numcols = if null rows'
                     then 0
                     else maximum $ map length rows'
        rows' = map toRow $ filter isRow nodes
        (headers, rows) = case rows' of
                               (h:rs) -> (h, rs)
                               []     -> ([], [])
        isRow (Node _ TABLE_ROW _) = True
        isRow _ = False
        isCell (Node _ TABLE_CELL _) = True
        isCell _ = False
        toRow (Node _ TABLE_ROW ns) = map toCell $ filter isCell ns
        toRow (Node _ t _) = error $ "toRow encountered non-row " ++ show t
        toCell (Node _ TABLE_CELL []) = []
        toCell (Node _ TABLE_CELL (n:ns))
          | isBlockNode n = addBlocks (n:ns)
          | otherwise     = [Plain (addInlines (n:ns))]
        toCell (Node _ t _) = error $ "toCell encountered non-cell " ++ show t
addBlock (Node _ TABLE_ROW _) = id -- handled in TABLE
addBlock (Node _ TABLE_CELL _) = id -- handled in TABLE
addBlock _ = id

isBlockNode :: Node -> Bool
isBlockNode (Node _ nodetype _) =
  case nodetype of
       DOCUMENT -> True
       THEMATIC_BREAK -> True
       PARAGRAPH -> True
       BLOCK_QUOTE -> True
       HTML_BLOCK _ -> True
       CUSTOM_BLOCK _ _ -> True
       CODE_BLOCK _ _ -> True
       HEADING _ -> True
       LIST _ -> True
       ITEM -> True
       TEXT _ -> False
       SOFTBREAK -> False
       LINEBREAK -> False
       HTML_INLINE _ -> False
       CUSTOM_INLINE _ _ -> False
       CODE _ -> False
       EMPH -> False
       STRONG -> False
       LINK _ _ -> False
       IMAGE _ _ -> False
       STRIKETHROUGH -> False
       TABLE _ -> False
       TABLE_ROW -> False
       TABLE_CELL -> False

children :: Node -> [Node]
children (Node _ _ ns) = ns

addInlines :: [Node] -> [Inline]
addInlines = foldr addInline []

addInline :: Node -> [Inline] -> [Inline]
addInline (Node _ (TEXT t) _) = (map toinl clumps ++)
  where raw = unpack t
        clumps = groupBy samekind raw
        samekind ' ' ' ' = True
        samekind ' ' _   = False
        samekind _   ' ' = False
        samekind _  _    = True
        toinl (' ':_) = Space
        toinl xs      = Str xs
addInline (Node _ LINEBREAK _) = (LineBreak :)
addInline (Node _ SOFTBREAK _) = (SoftBreak :)
addInline (Node _ (HTML_INLINE t) _) =
  (RawInline (Format "html") (unpack t) :)
-- Note:  the cmark parser will never generate CUSTOM_BLOCK,
-- so we don't need to handle it:
addInline (Node _ (CUSTOM_INLINE _onEnter _onExit) _nodes) =
  id
addInline (Node _ (CODE t) _) =
  (Code ("",[],[]) (unpack t) :)
addInline (Node _ EMPH nodes) =
  (Emph (addInlines nodes) :)
addInline (Node _ STRONG nodes) =
  (Strong (addInlines nodes) :)
addInline (Node _ STRIKETHROUGH nodes) =
  (Strikeout (addInlines nodes) :)
addInline (Node _ (LINK url title) nodes) =
  (Link nullAttr (addInlines nodes) (unpack url, unpack title) :)
addInline (Node _ (IMAGE url title) nodes) =
  (Image nullAttr (addInlines nodes) (unpack url, unpack title) :)
addInline _ = id
