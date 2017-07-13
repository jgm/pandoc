{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2017 Alexander Krotov <ilabdsf@gmail.com>

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
   Module      : Text.Pandoc.Writers.Muse
   Copyright   : Copyright (C) 2017 Alexander Krotov
   License     : GNU GPL, version 2 or above

   Maintainer  : Alexander Krotov <ilabdsf@gmail.com>
   Stability   : stable
   Portability : portable

Conversion of 'Pandoc' documents to Muse.

This module is mostly intended for <https://amusewiki.org/ Amusewiki> markup support,
as described by <https://amusewiki.org/library/manual Text::Amuse markup manual>.
Original <https://www.gnu.org/software/emacs-muse/ Emacs Muse> markup support
is a secondary goal.

Where Text::Amuse markup
<https://metacpan.org/pod/Text::Amuse#DIFFERENCES-WITH-THE-ORIGINAL-EMACS-MUSE-MARKUP differs>
from <https://www.gnu.org/software/emacs-muse/manual/ Emacs Muse markup>,
Text::Amuse markup is supported.
For example, native tables are always used instead of Org Mode tables.
However, @\<literal style="html">@ tag is used for HTML raw blocks
even though it is supported only in Emacs Muse.
-}
module Text.Pandoc.Writers.Muse (writeMuse) where
import Control.Monad.State.Strict
import Data.Text (Text)
import Data.List (intersperse, transpose, isInfixOf)
import System.FilePath (takeExtension)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Pretty
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Writers.Math
import Text.Pandoc.Writers.Shared
import qualified Data.Set as Set

type Notes = [[Block]]
data WriterState =
  WriterState { stNotes       :: Notes
              , stOptions     :: WriterOptions
              , stTopLevel    :: Bool
              , stInsideBlock :: Bool
              , stIds         :: Set.Set String
              }

-- | Convert Pandoc to Muse.
writeMuse :: PandocMonad m
          => WriterOptions
          -> Pandoc
          -> m Text
writeMuse opts document =
  let st = WriterState { stNotes = []
                       , stOptions = opts
                       , stTopLevel = True
                       , stInsideBlock = False
                       , stIds = Set.empty
                       }
  in evalStateT (pandocToMuse document) st

-- | Return Muse representation of document.
pandocToMuse :: PandocMonad m
             => Pandoc
             -> StateT WriterState m Text
pandocToMuse (Pandoc meta blocks) = do
  opts <- gets stOptions
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  let render' :: Doc -> Text
      render' = render Nothing
  metadata <- metaToJSON opts
               (fmap render' . blockListToMuse)
               (fmap render' . inlineListToMuse)
               meta
  body <- blockListToMuse blocks
  notes <- liftM (reverse . stNotes) get >>= notesToMuse
  let main = render colwidth $ body $+$ notes
  let context = defField "body" main metadata
  case writerTemplate opts of
       Nothing  -> return main
       Just tpl -> renderTemplate' tpl context

-- | Convert list of Pandoc block elements to Muse
-- | without setting stTopLevel.
flatBlockListToMuse :: PandocMonad m
                => [Block]       -- ^ List of block elements
                -> StateT WriterState m Doc
flatBlockListToMuse blocks = cat <$> mapM blockToMuse blocks

-- | Convert list of Pandoc block elements to Muse.
blockListToMuse :: PandocMonad m
                => [Block]       -- ^ List of block elements
                -> StateT WriterState m Doc
blockListToMuse blocks = do
  oldState <- get
  modify $ \s -> s { stTopLevel = not $ stInsideBlock s
                   , stInsideBlock = True
                   }
  result <- flatBlockListToMuse blocks
  modify $ \s -> s { stTopLevel = stTopLevel oldState
                   , stInsideBlock = stInsideBlock oldState
                   }
  return result

-- | Convert Pandoc block element to Muse.
blockToMuse :: PandocMonad m
            => Block         -- ^ Block element
            -> StateT WriterState m Doc
blockToMuse (Plain inlines) = inlineListToMuse inlines
blockToMuse (Para inlines) = do
  contents <- inlineListToMuse inlines
  return $ contents <> blankline
blockToMuse (LineBlock lns) = do
  let splitStanza [] = []
      splitStanza xs = case break (== mempty) xs of
        (l, [])  -> [l]
        (l, _:r) -> l : splitStanza r
  let joinWithLinefeeds  = nowrap . mconcat . intersperse cr
  let joinWithBlankLines = mconcat . intersperse blankline
  let prettyfyStanza ls  = joinWithLinefeeds <$> mapM inlineListToMuse ls
  contents <- joinWithBlankLines <$> mapM prettyfyStanza (splitStanza lns)
  return $ blankline $$ "<verse>" $$ contents $$ "</verse>" <> blankline
blockToMuse (CodeBlock (_,_,_) str) =
  return $ "<example>" $$ text str $$ "</example>" $$ blankline
blockToMuse (RawBlock (Format format) str) =
  return $ blankline $$ "<literal style=\"" <> text format <> "\">" $$
           text str $$ "</literal>" $$ blankline
blockToMuse (BlockQuote blocks) = do
  contents <- flatBlockListToMuse blocks
  return $ blankline
        <> "<quote>"
        $$ nest 0 contents -- nest 0 to remove trailing blank lines
        $$ "</quote>"
        <> blankline
blockToMuse (OrderedList (start, style, _) items) = do
  let markers = take (length items) $ orderedListMarkers
                                      (start, style, Period)
  let maxMarkerLength = maximum $ map length markers
  let markers' = map (\m -> let s = maxMarkerLength - length m
                            in  m ++ replicate s ' ') markers
  contents <- zipWithM orderedListItemToMuse markers' items
  -- ensure that sublists have preceding blank line
  topLevel <- gets stTopLevel
  return $ cr $$ (if topLevel then nest 1 else id) (vcat contents) $$ blankline
  where orderedListItemToMuse :: PandocMonad m
                              => String   -- ^ marker for list item
                              -> [Block]  -- ^ list item (list of blocks)
                              -> StateT WriterState m Doc
        orderedListItemToMuse marker item = do
        contents <- blockListToMuse item
        return $ hang (length marker + 1) (text marker <> space) contents
blockToMuse (BulletList items) = do
  contents <- mapM bulletListItemToMuse items
  -- ensure that sublists have preceding blank line
  topLevel <- gets stTopLevel
  return $ cr $$ (if topLevel then nest 1 else id) (vcat contents) $$ blankline
  where bulletListItemToMuse :: PandocMonad m
                             => [Block]
                             -> StateT WriterState m Doc
        bulletListItemToMuse item = do
          contents <- blockListToMuse item
          return $ hang 2 "- " contents
blockToMuse (DefinitionList items) = do
  contents <- mapM definitionListItemToMuse items
  return $ cr $$ nest 1 (vcat contents) $$ blankline
  where definitionListItemToMuse :: PandocMonad m
                                 => ([Inline], [[Block]])
                                 -> StateT WriterState m Doc
        definitionListItemToMuse (label, defs) = do
          label' <- inlineListToMuse label
          contents <- liftM vcat $ mapM blockListToMuse defs
          let label'' = label' <> " :: "
          let ind = offset label''
          return $ hang ind label'' contents
blockToMuse (Header level (ident,_,_) inlines) = do
  opts <- gets stOptions
  contents <- inlineListToMuse inlines

  ids <- gets stIds
  let autoId = uniqueIdent inlines ids
  modify $ \st -> st{ stIds = Set.insert autoId ids }

  let attr' = if null ident || (isEnabled Ext_auto_identifiers opts && ident == autoId)
                 then empty
                 else "#" <> text ident <> cr
  let header' = text $ replicate level '*'
  return $ blankline <> nowrap (header' <> space <> contents)
                 <> blankline <> attr'
-- https://www.gnu.org/software/emacs-muse/manual/muse.html#Horizontal-Rules-and-Anchors
blockToMuse HorizontalRule = return $ blankline $$ "----" $$ blankline
blockToMuse (Table caption _ _ headers rows) =  do
  caption' <- inlineListToMuse caption
  headers' <- mapM blockListToMuse headers
  rows' <- mapM (mapM blockListToMuse) rows
  let noHeaders = all null headers

  let numChars = maximum . map offset
  -- FIXME: width is not being used.
  let widthsInChars =
       map numChars $ transpose (headers' : rows')
  -- FIXME: Muse doesn't allow blocks with height more than 1.
  let hpipeBlocks sep blocks = hcat $ intersperse sep' blocks
        where h      = maximum (1 : map height blocks)
              sep'   = lblock (length sep) $ vcat (replicate h (text sep))
  let makeRow sep = (" " <>) . hpipeBlocks sep . zipWith lblock widthsInChars
  let head' = makeRow " || " headers'
  let rowSeparator = if noHeaders then " | " else " |  "
  rows'' <- mapM (\row -> do cols <- mapM blockListToMuse row
                             return $ makeRow rowSeparator cols) rows
  let body = vcat rows''
  return $  (if noHeaders then empty else head')
         $$ body
         $$ (if null caption then empty else " |+ " <> caption' <> " +|")
         $$ blankline
blockToMuse (Div _ bs) = blockListToMuse bs
blockToMuse Null = return empty

-- | Return Muse representation of notes.
notesToMuse :: PandocMonad m
            => Notes
            -> StateT WriterState m Doc
notesToMuse notes = liftM vsep (zipWithM noteToMuse [1 ..] notes)

-- | Return Muse representation of a note.
noteToMuse :: PandocMonad m
           => Int
           -> [Block]
           -> StateT WriterState m Doc
noteToMuse num note = do
  contents <- blockListToMuse note
  let marker = "[" ++ show num ++ "] "
  return $ hang (length marker) (text marker) contents

-- | Escape special characters for Muse.
escapeString :: String -> String
escapeString s =
  "<verbatim>" ++
  substitute "</verbatim>" "<</verbatim><verbatim>/verbatim>" s ++
  "</verbatim>"

-- | Escape special characters for Muse if needed.
conditionalEscapeString :: String -> String
conditionalEscapeString s
  | any (`elem` ("*<=>[]|" :: String)) s ||
    "::" `isInfixOf` s = escapeString s
  | otherwise = s

-- | Convert list of Pandoc inline elements to Muse.
inlineListToMuse :: PandocMonad m
                 => [Inline]
                 -> StateT WriterState m Doc
inlineListToMuse lst = liftM hcat (mapM inlineToMuse lst)

-- | Convert Pandoc inline element to Muse.
inlineToMuse :: PandocMonad m
             => Inline
             -> StateT WriterState m Doc
inlineToMuse (Str str) = return $ text $ conditionalEscapeString str
inlineToMuse (Emph lst) = do
  contents <- inlineListToMuse lst
  return $ "<em>" <> contents <> "</em>"
inlineToMuse (Strong lst) = do
  contents <- inlineListToMuse lst
  return $ "<strong>" <> contents <> "</strong>"
inlineToMuse (Strikeout lst) = do
  contents <- inlineListToMuse lst
  return $ "<del>" <> contents <> "</del>"
inlineToMuse (Superscript lst) = do
  contents <- inlineListToMuse lst
  return $ "<sup>" <> contents <> "</sup>"
inlineToMuse (Subscript lst) = do
  contents <- inlineListToMuse lst
  return $ "<sub>" <> contents <> "</sub>"
inlineToMuse (SmallCaps lst) = inlineListToMuse lst
inlineToMuse (Quoted SingleQuote lst) = do
  contents <- inlineListToMuse lst
  return $ "'" <> contents <> "'"
inlineToMuse (Quoted DoubleQuote lst) = do
  contents <- inlineListToMuse lst
  return $ "\"" <> contents <> "\""
-- Amusewiki does not support <cite> tag,
-- and Emacs Muse citation support is limited
-- (https://www.gnu.org/software/emacs-muse/manual/html_node/Citations.html#Citation)
-- so just fallback to expanding inlines.
inlineToMuse (Cite _  lst) = inlineListToMuse lst
inlineToMuse (Code _ str) = return $
  "<code>" <> text (conditionalEscapeString str) <> "</code>"
inlineToMuse (Math InlineMath str) =
  lift (texMathToInlines InlineMath str) >>= inlineListToMuse
inlineToMuse (Math DisplayMath str) = do
  contents <- lift (texMathToInlines DisplayMath str) >>= inlineListToMuse
  return $ "<verse>" <> contents <> "</verse>" <> blankline
inlineToMuse (RawInline (Format f) str) =
  return $ "<literal style=\"" <> text f <> "\">" <> text str <> "</literal>"
inlineToMuse LineBreak = return $ "<br>" <> cr
inlineToMuse Space = return space
inlineToMuse SoftBreak = do
  wrapText <- gets $ writerWrapText . stOptions
  return $ if wrapText == WrapPreserve then cr else space
inlineToMuse (Link _ txt (src, _)) =
  case txt of
        [Str x] | escapeURI x == src ->
             return $ "[[" <> text (escapeLink x) <> "]]"
        _ -> do contents <- inlineListToMuse txt
                return $ "[[" <> text (escapeLink src) <> "][" <> contents <> "]]"
  where escapeLink lnk = escapeURI (if isImageUrl lnk then "URL:" ++ lnk else lnk)
        -- Taken from muse-image-regexp defined in Emacs Muse file lisp/muse-regexps.el
        imageExtensions = [".eps", ".gif", ".jpg", ".jpeg", ".pbm", ".png", ".tiff", ".xbm", ".xpm"]
        isImageUrl = (`elem` imageExtensions) . takeExtension
inlineToMuse (Image attr alt (source,'f':'i':'g':':':title)) =
  inlineToMuse (Image attr alt (source,title))
inlineToMuse (Image _ inlines (source, title)) = do
  alt <- inlineListToMuse inlines
  let title' = if null title
                  then if null inlines
                          then ""
                          else "[" <> alt <> "]"
                  else "[" <> text title <> "]"
  return $ "[[" <> text source <> "]" <> title' <> "]"
inlineToMuse (Note contents) = do
  -- add to notes in state
  notes <- gets stNotes
  modify $ \st -> st { stNotes = contents:notes }
  let ref = show $ length notes + 1
  return $ "[" <> text ref <> "]"
inlineToMuse (Span (_,name:_,_) inlines) = do
  contents <- inlineListToMuse inlines
  return $ "<class name=\"" <> text name <> "\">" <> contents <> "</class>"
inlineToMuse (Span _ lst) = inlineListToMuse lst
