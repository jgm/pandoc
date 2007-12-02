{-
Copyright (C) 2006-7 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.RST 
   Copyright   : Copyright (C) 2006-7 John MacFarlane
   License     : GNU GPL, version 2 or above 

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to reStructuredText.

reStructuredText:  <http://docutils.sourceforge.net/rst.html>
-}
module Text.Pandoc.Writers.RST ( writeRST) where
import Text.Pandoc.Definition
import Text.Pandoc.Shared 
import Text.Pandoc.Readers.TeXMath
import Text.Pandoc.Blocks
import Data.List ( isPrefixOf, isSuffixOf, drop, intersperse )
import Text.PrettyPrint.HughesPJ hiding ( Str )
import Control.Monad.State

type Notes = [[Block]]
type Refs = KeyTable
type WriterState = (Notes, Refs, Refs) -- first Refs is links, second pictures

-- | Convert Pandoc to RST.
writeRST :: WriterOptions -> Pandoc -> String
writeRST opts document = 
  render $ evalState (pandocToRST opts document) ([],[],[]) 

-- | Return RST representation of document.
pandocToRST :: WriterOptions -> Pandoc -> State WriterState Doc
pandocToRST opts (Pandoc meta blocks) = do
  let before  = writerIncludeBefore opts
  let after   = writerIncludeAfter opts
      before' = if null before then empty else text before
      after'  = if null after then empty else text after
  metaBlock <- metaToRST opts meta
  let head = if (writerStandalone opts)
                then metaBlock $+$ text (writerHeader opts)
                else empty
  body <- blockListToRST opts blocks
  (notes, _, _) <- get
  notes' <- notesToRST opts (reverse notes)
  (_, refs, pics) <- get  -- note that the notes may contain refs
  refs' <- keyTableToRST opts (reverse refs)
  pics' <- pictTableToRST opts (reverse pics)
  return $ head $+$ before' $+$ body $+$ notes' $+$ text "" $+$ refs' $+$ 
           pics' $+$ after'

-- | Return RST representation of reference key table.
keyTableToRST :: WriterOptions -> KeyTable -> State WriterState Doc
keyTableToRST opts refs = mapM (keyToRST opts) refs >>= return . vcat
 
-- | Return RST representation of a reference key. 
keyToRST :: WriterOptions 
         -> ([Inline], (String, String)) 
         -> State WriterState Doc
keyToRST opts (label, (src, tit)) = do
  label' <- inlineListToRST opts label
  let label'' = if ':' `elem` (render label')
                   then char '`' <> label' <> char '`'
                   else label'
  return $ text ".. _" <> label'' <> text ": " <> text src

-- | Return RST representation of notes.
notesToRST :: WriterOptions -> [[Block]] -> State WriterState Doc
notesToRST opts notes = 
  mapM (\(num, note) -> noteToRST opts num note) (zip [1..] notes) >>= 
  return . vcat

-- | Return RST representation of a note.
noteToRST :: WriterOptions -> Int -> [Block] -> State WriterState Doc
noteToRST opts num note = do
  contents <- blockListToRST opts note
  let marker = text ".. [" <> text (show num) <> text "] "
  return $ hang marker 3 contents 

-- | Return RST representation of picture reference table.
pictTableToRST :: WriterOptions -> KeyTable -> State WriterState Doc
pictTableToRST opts refs = mapM (pictToRST opts) refs >>= return . vcat
 
-- | Return RST representation of a picture substitution reference. 
pictToRST :: WriterOptions 
         -> ([Inline], (String, String)) 
         -> State WriterState Doc
pictToRST opts (label, (src, _)) = do
  label' <- inlineListToRST opts label
  return $ text ".. " <> char '|' <> label' <> char '|' <> text " image:: " <>
           text src

-- | Take list of inline elements and return wrapped doc.
wrappedRST :: WriterOptions -> [Inline] -> State WriterState Doc
wrappedRST opts inlines = mapM (wrapIfNeeded opts (inlineListToRST opts))
                          (splitBy LineBreak inlines) >>= return . vcat

-- | Escape special characters for RST.
escapeString :: String -> String
escapeString = escapeStringUsing (backslashEscapes "`\\|*_")

-- | Convert bibliographic information into RST header.
metaToRST :: WriterOptions -> Meta -> State WriterState Doc
metaToRST opts (Meta title authors date) = do
  title'   <- titleToRST opts title
  authors' <- authorsToRST authors
  date'    <- dateToRST date
  let toc  =  if writerTableOfContents opts
                 then text "" $+$ text ".. contents::"
                 else empty
  return $ title' $+$ authors' $+$ date' $+$ toc

titleToRST :: WriterOptions -> [Inline] -> State WriterState Doc
titleToRST opts [] = return empty
titleToRST opts lst = do
  contents <- inlineListToRST opts lst
  let titleLength = length $ render contents
  let border = text (replicate titleLength '=')
  return $ border $+$ contents $+$ border <> text "\n"

authorsToRST :: [String] -> State WriterState Doc
authorsToRST [] = return empty
authorsToRST (first:rest) = do
  rest' <- authorsToRST rest
  return $ (text ":Author: " <> text first) $+$ rest'

dateToRST :: String -> State WriterState Doc
dateToRST [] = return empty
dateToRST str = return $ text ":Date: " <> text (escapeString str)

-- | Convert Pandoc block element to RST. 
blockToRST :: WriterOptions -- ^ Options
                -> Block         -- ^ Block element
                -> State WriterState Doc 
blockToRST opts Null = return empty
blockToRST opts (Plain inlines) = wrappedRST opts inlines
blockToRST opts (Para inlines) = do
  contents <- wrappedRST opts inlines
  return $ contents <> text "\n"
blockToRST opts (RawHtml str) = 
  let str' = if "\n" `isSuffixOf` str then str ++ "\n" else str ++ "\n\n" in
  return $ hang (text "\n.. raw:: html\n") 3 $ vcat $ map text (lines str')
blockToRST opts HorizontalRule = return $ text "--------------\n"
blockToRST opts (Header level inlines) = do
  contents <- inlineListToRST opts inlines
  let headerLength = length $ render contents
  let headerChar = if level > 5 then ' ' else "=-~^'" !! (level - 1)
  let border = text $ replicate headerLength headerChar
  return $ contents $+$ border <> text "\n"
blockToRST opts (CodeBlock str) = return $ (text "::\n") $+$ 
  (nest (writerTabStop opts) $ vcat $ map text (lines str)) <> text "\n"
blockToRST opts (BlockQuote blocks) = do
  contents <- blockListToRST opts blocks 
  return $ (nest (writerTabStop opts) contents) <> text "\n"
blockToRST opts (Table caption aligns widths headers rows) =  do
  caption' <- inlineListToRST opts caption
  let caption'' = if null caption
                     then empty
                     else text "" $+$ (text "Table: " <> caption')
  headers' <- mapM (blockListToRST opts) headers
  let widthsInChars = map (floor . (78 *)) widths
  let alignHeader alignment = case alignment of
                                AlignLeft    -> leftAlignBlock
                                AlignCenter  -> centerAlignBlock
                                AlignRight   -> rightAlignBlock
                                AlignDefault -> leftAlignBlock  
  let hpipeBlocks blocks = hcatBlocks [beg, middle, end] 
        where height = maximum (map heightOfBlock blocks)
              sep    = TextBlock 3 height (replicate height " | ")
              beg    = TextBlock 2 height (replicate height "| ")
              end    = TextBlock 2 height (replicate height " |")
              middle = hcatBlocks $ intersperse sep blocks
  let makeRow = hpipeBlocks . zipWith docToBlock widthsInChars
  let head = makeRow headers'
  rows' <- mapM (\row -> do cols <- mapM (blockListToRST opts) row
                            return $ makeRow cols) rows
  let tableWidth = sum widthsInChars
  let maxRowHeight = maximum $ map heightOfBlock (head:rows')
  let border ch = char '+' <> char ch <>
                  (hcat $ intersperse (char ch <> char '+' <> char ch) $ 
                          map (\l -> text $ replicate l ch) widthsInChars) <>
                  char ch <> char '+'
  let body = vcat $ intersperse (border '-') $ map blockToDoc rows'
  return $ border '-' $+$ blockToDoc head $+$ border '=' $+$ body $+$ 
           border '-' $$ caption'' $$ text ""
blockToRST opts (BulletList items) = do
  contents <- mapM (bulletListItemToRST opts) items
  -- ensure that sublists have preceding blank line
  return $ text "" $+$ vcat contents <> text "\n"
blockToRST opts (OrderedList (start, style, delim) items) = do
  let markers = if start == 1 && style == DefaultStyle && delim == DefaultDelim 
                   then take (length items) $ repeat "#."
                   else take (length items) $ orderedListMarkers 
                                              (start, style, delim)
  let maxMarkerLength = maximum $ map length markers
  let markers' = map (\m -> let s = maxMarkerLength - length m
                            in  m ++ replicate s ' ') markers
  contents <- mapM (\(item, num) -> orderedListItemToRST opts item num) $
              zip markers' items  
  -- ensure that sublists have preceding blank line
  return $ text "" $+$ vcat contents <> text "\n"
blockToRST opts (DefinitionList items) = do
  contents <- mapM (definitionListItemToRST opts) items
  return $ (vcat contents) <> text "\n"

-- | Convert bullet list item (list of blocks) to RST.
bulletListItemToRST :: WriterOptions -> [Block] -> State WriterState Doc
bulletListItemToRST opts items = do
  contents <- blockListToRST opts items
  return $ hang (text "- ") 3 contents

-- | Convert ordered list item (a list of blocks) to RST.
orderedListItemToRST :: WriterOptions -- ^ options
                          -> String   -- ^ marker for list item
                          -> [Block]  -- ^ list item (list of blocks)
                          -> State WriterState Doc
orderedListItemToRST opts marker items = do
  contents <- blockListToRST opts items
  return $ hang (text marker) (length marker + 1) contents 

-- | Convert defintion list item (label, list of blocks) to RST.
definitionListItemToRST :: WriterOptions -> ([Inline], [Block]) -> State WriterState Doc
definitionListItemToRST opts (label, items) = do
  label <- inlineListToRST opts label
  contents <- blockListToRST opts items
  return $ label $+$ nest (writerTabStop opts) contents

-- | Convert list of Pandoc block elements to RST.
blockListToRST :: WriterOptions -- ^ Options
                    -> [Block]       -- ^ List of block elements
                    -> State WriterState Doc 
blockListToRST opts blocks =
  mapM (blockToRST opts) blocks >>= return . vcat

-- | Convert list of Pandoc inline elements to RST.
inlineListToRST :: WriterOptions -> [Inline] -> State WriterState Doc
inlineListToRST opts lst = mapM (inlineToRST opts) lst >>= return . hcat

-- | Convert Pandoc inline element to RST.
inlineToRST :: WriterOptions -> Inline -> State WriterState Doc
inlineToRST opts (Emph lst) = do 
  contents <- inlineListToRST opts lst
  return $ char '*' <> contents <> char '*'
inlineToRST opts (Strong lst) = do
  contents <- inlineListToRST opts lst
  return $ text "**" <> contents <> text "**"
inlineToRST opts (Strikeout lst) = do 
  contents <- inlineListToRST opts lst
  return $ text "[STRIKEOUT:" <> contents <> char ']'
inlineToRST opts (Superscript lst) = do 
  contents <- inlineListToRST opts lst
  return $ text "\\ :sup:`" <> contents <> text "`\\ "
inlineToRST opts (Subscript lst) = do 
  contents <- inlineListToRST opts lst
  return $ text "\\ :sub:`" <> contents <> text "`\\ "
inlineToRST opts (Quoted SingleQuote lst) = do
  contents <- inlineListToRST opts lst
  return $ char '\'' <> contents <> char '\''
inlineToRST opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToRST opts lst
  return $ char '"' <> contents <> char '"'
inlineToRST opts EmDash = return $ text "--"
inlineToRST opts EnDash = return $ char '-'
inlineToRST opts Apostrophe = return $ char '\''
inlineToRST opts Ellipses = return $ text "..."
inlineToRST opts (Code str) = return $ text $ "``" ++ str ++ "``"
inlineToRST opts (Str str) = return $ text $ escapeString str
inlineToRST opts (Math str) = return $ text $ "$" ++ str ++ "$"
inlineToRST opts (TeX str) = return empty
inlineToRST opts (HtmlInline str) = return empty
inlineToRST opts (LineBreak) = return $ char ' ' -- RST doesn't have linebreaks 
inlineToRST opts Space = return $ char ' '
inlineToRST opts (Link [Code str] (src, tit)) | src == str ||
                                                src == "mailto:" ++ str = do
  let srcSuffix = if isPrefixOf "mailto:" src then drop 7 src else src
  return $ text srcSuffix
inlineToRST opts (Link txt (src, tit)) = do
  let useReferenceLinks = writerReferenceLinks opts
  linktext <- inlineListToRST opts $ normalizeSpaces txt
  if useReferenceLinks
    then do (notes, refs, pics) <- get
            let refs' = if (txt, (src, tit)) `elem` refs
                           then refs
                           else (txt, (src, tit)):refs
            put (notes, refs', pics)
            return $ char '`' <> linktext <> text "`_"
    else return $ char '`' <> linktext <> text " <" <> text src <> text ">`_"
inlineToRST opts (Image alternate (source, tit)) = do
  (notes, refs, pics) <- get
  let labelsUsed = map fst pics 
  let txt = if null alternate || alternate == [Str ""] || 
               alternate `elem` labelsUsed
               then [Str $ "image" ++ show (length refs)]
               else alternate
  let pics' = if (txt, (source, tit)) `elem` pics
                 then pics
                 else (txt, (source, tit)):pics
  put (notes, refs, pics')
  label <- inlineListToRST opts txt
  return $ char '|' <> label <> char '|'
inlineToRST opts (Note contents) = do 
  -- add to notes in state
  modify (\(notes, refs, pics) -> (contents:notes, refs, pics))
  (notes, _, _) <- get
  let ref = show $ (length notes)
  return $ text " [" <> text ref <> text "]_"
