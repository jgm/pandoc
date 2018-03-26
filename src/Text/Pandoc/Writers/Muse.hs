{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2017-2018 Alexander Krotov <ilabdsf@gmail.com>

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
   Copyright   : Copyright (C) 2017-2018 Alexander Krotov
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
import Prelude
import Control.Monad.State.Strict
import Data.Char (isSpace, isDigit, isAsciiUpper, isAsciiLower)
import Data.Text (Text)
import Data.List (intersperse, transpose, isInfixOf)
import System.FilePath (takeExtension)
import Text.Pandoc.Class (PandocMonad)
import Text.Pandoc.Definition
import Text.Pandoc.ImageSize
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

-- | Helper function for flatBlockListToMuse
-- | Render all blocks and insert blank lines between the first two
catWithBlankLines :: PandocMonad m
                  => [Block]       -- ^ List of block elements
                  -> Int           -- ^ Number of blank lines
                  -> StateT WriterState m Doc
catWithBlankLines (b : bs) n = do
  b' <- blockToMuse b
  bs' <- flatBlockListToMuse bs
  return $ b' <> blanklines n <> bs'
catWithBlankLines _ _ = error "Expected at least one block"

-- | Convert list of Pandoc block elements to Muse
-- | without setting stTopLevel.
flatBlockListToMuse :: PandocMonad m
                => [Block]       -- ^ List of block elements
                -> StateT WriterState m Doc
flatBlockListToMuse bs@(BulletList _ : BulletList _ : _) = catWithBlankLines bs 2
flatBlockListToMuse bs@(OrderedList (_, style1, _) _ : OrderedList (_, style2, _) _ : _) =
  catWithBlankLines bs (if style1' == style2' then 2 else 0)
    where
      style1' = normalizeStyle style1
      style2' = normalizeStyle style2
      normalizeStyle DefaultStyle = Decimal
      normalizeStyle s = s
flatBlockListToMuse bs@(DefinitionList _ : DefinitionList _ : _) = catWithBlankLines bs 2
flatBlockListToMuse bs@(_ : _) = catWithBlankLines bs 0
flatBlockListToMuse [] = return mempty

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
blockToMuse (Plain inlines) = inlineListToMuse' inlines
blockToMuse (Para inlines) = do
  contents <- inlineListToMuse' inlines
  return $ contents <> blankline
blockToMuse (LineBlock lns) = do
  lns' <- mapM inlineListToMuse lns
  return $ nowrap $ vcat (map (text "> " <>) lns') <> blankline
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
  contents <- zipWithM orderedListItemToMuse markers items
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
  -- ensure that sublists have preceding blank line
  topLevel <- gets stTopLevel
  return $ cr $$ (if topLevel then nest 1 else id) (vcat contents) $$ blankline
  where definitionListItemToMuse :: PandocMonad m
                                 => ([Inline], [[Block]])
                                 -> StateT WriterState m Doc
        definitionListItemToMuse (label, defs) = do
          label' <- inlineListToMuse' label
          contents <- liftM vcat $ mapM descriptionToMuse defs
          let ind = offset label'
          return $ hang ind label' contents
        descriptionToMuse :: PandocMonad m
                          => [Block]
                          -> StateT WriterState m Doc
        descriptionToMuse desc = hang 4 " :: " <$> blockListToMuse desc
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
                 $$ attr' <> blankline
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
blockToMuse (Div _ bs) = flatBlockListToMuse bs
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

startsWithMarker :: (Char -> Bool) -> String -> Bool
startsWithMarker f (' ':xs) = startsWithMarker f xs
startsWithMarker f (x:xs) =
  f x && (startsWithMarker f xs || startsWithDot xs)
  where
    startsWithDot ['.'] = True
    startsWithDot ('.':c:_) = isSpace c
    startsWithDot _ = False
startsWithMarker _ [] = False

-- | Escape special characters for Muse if needed.
conditionalEscapeString :: String -> String
conditionalEscapeString s =
  if any (`elem` ("#*<=>[]|" :: String)) s ||
     "::" `isInfixOf` s ||
     "----" `isInfixOf` s ||
     "~~" `isInfixOf` s
    then escapeString s
    else s

-- Expand Math and Cite before normalizing inline list
preprocessInlineList :: PandocMonad m
                     => [Inline]
                     -> m [Inline]
preprocessInlineList (Math t str:xs) = (++) <$> texMathToInlines t str <*> preprocessInlineList xs
-- Amusewiki does not support <cite> tag,
-- and Emacs Muse citation support is limited
-- (https://www.gnu.org/software/emacs-muse/manual/html_node/Citations.html#Citation)
-- so just fallback to expanding inlines.
preprocessInlineList (Cite _  lst:xs) = (lst ++) <$> preprocessInlineList xs
preprocessInlineList (x:xs) = (x:) <$> preprocessInlineList xs
preprocessInlineList [] = return []

replaceSmallCaps :: Inline -> Inline
replaceSmallCaps (SmallCaps lst) = Emph lst
replaceSmallCaps x = x

removeKeyValues :: Inline -> Inline
removeKeyValues (Code (i, cls, _) xs) = Code (i, cls, []) xs
-- Do not remove attributes from Link
-- Do not remove attributes, such as "width", from Image
removeKeyValues (Span (i, cls, _) xs) = Span (i, cls, []) xs
removeKeyValues x = x

normalizeInlineList :: [Inline] -> [Inline]
normalizeInlineList (Str "" : xs)
  = normalizeInlineList xs
normalizeInlineList (x : Str "" : xs)
  = normalizeInlineList (x:xs)
normalizeInlineList (Str x1 : Str x2 : xs)
  = normalizeInlineList $ Str (x1 ++ x2) : xs
normalizeInlineList (Emph x1 : Emph x2 : ils)
  = normalizeInlineList $ Emph (x1 ++ x2) : ils
normalizeInlineList (Strong x1 : Strong x2 : ils)
  = normalizeInlineList $ Strong (x1 ++ x2) : ils
normalizeInlineList (Strikeout x1 : Strikeout x2 : ils)
  = normalizeInlineList $ Strikeout (x1 ++ x2) : ils
normalizeInlineList (Superscript x1 : Superscript x2 : ils)
  = normalizeInlineList $ Superscript (x1 ++ x2) : ils
normalizeInlineList (Subscript x1 : Subscript x2 : ils)
  = normalizeInlineList $ Subscript (x1 ++ x2) : ils
normalizeInlineList (SmallCaps x1 : SmallCaps x2 : ils)
  = normalizeInlineList $ SmallCaps (x1 ++ x2) : ils
normalizeInlineList (Code _ x1 : Code _ x2 : ils)
  = normalizeInlineList $ Code nullAttr (x1 ++ x2) : ils
normalizeInlineList (RawInline f1 x1 : RawInline f2 x2 : ils) | f1 == f2
  = normalizeInlineList $ RawInline f1 (x1 ++ x2) : ils
-- Do not join Span's during normalization
normalizeInlineList (x:xs) = x : normalizeInlineList xs
normalizeInlineList [] = []

fixNotes :: [Inline] -> [Inline]
fixNotes [] = []
fixNotes (Space : n@Note{} : rest) = Str " " : n : fixNotes rest
fixNotes (SoftBreak : n@Note{} : rest) = Str " " : n : fixNotes rest
fixNotes (x:xs) = x : fixNotes xs

urlEscapeBrackets :: String -> String
urlEscapeBrackets (']':xs) = '%':'5':'D':urlEscapeBrackets xs
urlEscapeBrackets (x:xs) = x:urlEscapeBrackets xs
urlEscapeBrackets [] = []

fixOrEscape :: Inline -> Bool
fixOrEscape (Str "-") = True -- TODO: "  - " should be escaped too
fixOrEscape (Str ";") = True
fixOrEscape (Str s) = startsWithMarker isDigit s ||
                      startsWithMarker isAsciiLower s ||
                      startsWithMarker isAsciiUpper s
fixOrEscape Space = True
fixOrEscape SoftBreak = True
fixOrEscape _ = False

-- | Convert list of Pandoc inline elements to Muse
renderInlineList :: PandocMonad m
                 => Bool
                 -> [Inline]
                 -> StateT WriterState m Doc
renderInlineList True [] = pure "<verbatim></verbatim>"
renderInlineList False [] = pure ""
renderInlineList start (x:xs) = do r <- inlineToMuse x
                                   opts <- gets stOptions
                                   lst' <- renderInlineList (x == SoftBreak && writerWrapText opts == WrapPreserve) xs
                                   if start && fixOrEscape x
                                     then pure (text "<verbatim></verbatim>" <> r <> lst')
                                     else pure (r <> lst')

-- | Normalize and convert list of Pandoc inline elements to Muse.
inlineListToMuse'' :: PandocMonad m
                  => Bool
                  -> [Inline]
                  -> StateT WriterState m Doc
inlineListToMuse'' start lst = do
  lst' <- (normalizeInlineList . fixNotes) <$> preprocessInlineList (map (removeKeyValues . replaceSmallCaps) lst)
  renderInlineList start lst'

inlineListToMuse' :: PandocMonad m => [Inline] -> StateT WriterState m Doc
inlineListToMuse' = inlineListToMuse'' True

inlineListToMuse :: PandocMonad m => [Inline] -> StateT WriterState m Doc
inlineListToMuse = inlineListToMuse'' False

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
inlineToMuse SmallCaps {} =
  fail "SmallCaps should be expanded before normalization"
inlineToMuse (Quoted SingleQuote lst) = do
  contents <- inlineListToMuse lst
  return $ "‘" <> contents <> "’"
inlineToMuse (Quoted DoubleQuote lst) = do
  contents <- inlineListToMuse lst
  return $ "“" <> contents <> "”"
inlineToMuse Cite {} =
  fail "Citations should be expanded before normalization"
inlineToMuse (Code _ str) = return $
  "<code>" <> text (substitute "</code>" "<</code><code>/code>" str) <> "</code>"
inlineToMuse Math{} =
  fail "Math should be expanded before normalization"
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
  where escapeLink lnk = if isImageUrl lnk then "URL:" ++ urlEscapeBrackets lnk else urlEscapeBrackets lnk
        -- Taken from muse-image-regexp defined in Emacs Muse file lisp/muse-regexps.el
        imageExtensions = [".eps", ".gif", ".jpg", ".jpeg", ".pbm", ".png", ".tiff", ".xbm", ".xpm"]
        isImageUrl = (`elem` imageExtensions) . takeExtension
inlineToMuse (Image attr alt (source,'f':'i':'g':':':title)) =
  inlineToMuse (Image attr alt (source,title))
inlineToMuse (Image attr inlines (source, title)) = do
  opts <- gets stOptions
  alt <- inlineListToMuse inlines
  let title' = if null title
                  then if null inlines
                          then ""
                          else "[" <> alt <> "]"
                  else "[" <> text (escape title) <> "]"
  let width = case dimension Width attr of
                Just (Percent x) | isEnabled Ext_amuse opts -> " " ++ show (round x :: Integer)
                _ -> ""
  return $ "[[" <> text (urlEscapeBrackets source ++ width) <> "]" <> title' <> "]"
  where escape s = if "]" `isInfixOf` s then escapeString s else conditionalEscapeString s
inlineToMuse (Note contents) = do
  -- add to notes in state
  notes <- gets stNotes
  modify $ \st -> st { stNotes = contents:notes }
  let ref = show $ length notes + 1
  return $ "[" <> text ref <> "]"
inlineToMuse (Span (anchor,names,_) inlines) = do
  contents <- inlineListToMuse inlines
  let anchorDoc = if null anchor
                     then mempty
                     else text ('#':anchor) <> space
  return $ anchorDoc <> (if null inlines && not (null anchor)
                         then mempty
                         else (if null names
                               then "<class>"
                               else "<class name=\"" <> text (head names) <> "\">") <> contents <> "</class>")
