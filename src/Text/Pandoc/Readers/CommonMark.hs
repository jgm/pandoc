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

import CMark
import Data.Text (unpack, pack)
import Data.List (groupBy)
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Error

-- | Parse a CommonMark formatted string into a 'Pandoc' structure.
readCommonMark :: ReaderOptions -> String -> Either PandocError Pandoc
readCommonMark opts = Right . nodeToPandoc . commonmarkToNode opts' . pack
  where opts' = if readerSmart opts
                   then [optNormalize, optSmart]
                   else [optNormalize]

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
                     PERIOD_DELIM  -> Period
                     PAREN_DELIM   -> OneParen
addBlock (Node _ ITEM _) = id -- handled in LIST
addBlock _ = id

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
        toinl (' ':_)    = Space
        toinl xs         = Str xs
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
addInline (Node _ (LINK url title) nodes) =
  (Link nullAttr (addInlines nodes) (unpack url, unpack title) :)
addInline (Node _ (IMAGE url title) nodes) =
  (Image nullAttr (addInlines nodes) (unpack url, unpack title) :)
addInline _ = id
