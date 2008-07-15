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
import Text.Pandoc.Blocks
import Data.List ( isPrefixOf, isSuffixOf, drop, intersperse )
import Text.PrettyPrint.HughesPJ hiding ( Str )
import Control.Monad.State

data WriterState = 
  WriterState { stNotes     :: [[Block]]
              , stLinks     :: KeyTable
              , stImages    :: KeyTable
              , stIncludes  :: [String]
              , stOptions   :: WriterOptions
              }

-- | Convert Pandoc to RST.
writeRST :: WriterOptions -> Pandoc -> String
writeRST opts document = 
  let st = WriterState { stNotes = [], stLinks = [],
                         stImages = [], stIncludes = [],
                         stOptions = opts }
  in render $ evalState (pandocToRST document) st

-- | Return RST representation of document.
pandocToRST :: Pandoc -> State WriterState Doc
pandocToRST (Pandoc meta blocks) = do
  opts <- get >>= (return . stOptions)
  let before  = writerIncludeBefore opts
  let after   = writerIncludeAfter opts
      before' = if null before then empty else text before
      after'  = if null after then empty else text after
  metaBlock <- metaToRST opts meta
  let head' = if (writerStandalone opts)
                 then metaBlock $+$ text (writerHeader opts)
                 else empty
  body <- blockListToRST blocks
  includes <- get >>= (return . concat . stIncludes)
  let includes' = if null includes then empty else text includes
  notes <- get >>= (notesToRST . reverse . stNotes)
  -- note that the notes may contain refs, so we do them first
  refs <- get >>= (keyTableToRST . reverse . stLinks)
  pics <- get >>= (pictTableToRST . reverse . stImages)
  return $ head' $+$ before' $+$ includes' $+$ body $+$ notes $+$ text "" $+$
           refs $+$ pics $+$ after'

-- | Return RST representation of reference key table.
keyTableToRST :: KeyTable -> State WriterState Doc
keyTableToRST refs = mapM keyToRST refs >>= return . vcat
 
-- | Return RST representation of a reference key. 
keyToRST :: ([Inline], (String, String)) 
         -> State WriterState Doc
keyToRST (label, (src, _)) = do
  label' <- inlineListToRST label
  let label'' = if ':' `elem` (render label')
                   then char '`' <> label' <> char '`'
                   else label'
  return $ text ".. _" <> label'' <> text ": " <> text src

-- | Return RST representation of notes.
notesToRST :: [[Block]] -> State WriterState Doc
notesToRST notes = 
  mapM (\(num, note) -> noteToRST num note) (zip [1..] notes) >>= 
  return . vcat

-- | Return RST representation of a note.
noteToRST :: Int -> [Block] -> State WriterState Doc
noteToRST num note = do
  contents <- blockListToRST note
  let marker = text ".. [" <> text (show num) <> text "] "
  return $ hang marker 3 contents 

-- | Return RST representation of picture reference table.
pictTableToRST :: KeyTable -> State WriterState Doc
pictTableToRST refs = mapM pictToRST refs >>= return . vcat
 
-- | Return RST representation of a picture substitution reference. 
pictToRST :: ([Inline], (String, String)) 
          -> State WriterState Doc
pictToRST (label, (src, _)) = do
  label' <- inlineListToRST label
  return $ text ".. " <> char '|' <> label' <> char '|' <> text " image:: " <>
           text src

-- | Take list of inline elements and return wrapped doc.
wrappedRST :: WriterOptions -> [Inline] -> State WriterState Doc
wrappedRST opts inlines = do
  lineBreakDoc <- inlineToRST LineBreak  
  chunks <- mapM (wrapIfNeeded opts inlineListToRST)
                 (splitBy LineBreak inlines)
  return $ vcat $ intersperse lineBreakDoc chunks

-- | Escape special characters for RST.
escapeString :: String -> String
escapeString = escapeStringUsing (backslashEscapes "`\\|*_")

-- | Convert bibliographic information into RST header.
metaToRST :: WriterOptions -> Meta -> State WriterState Doc
metaToRST opts (Meta title authors date) = do
  title'   <- titleToRST title
  authors' <- authorsToRST authors
  date'    <- dateToRST date
  let toc  =  if writerTableOfContents opts
                 then text "" $+$ text ".. contents::"
                 else empty
  return $ title' $+$ authors' $+$ date' $+$ toc

titleToRST :: [Inline] -> State WriterState Doc
titleToRST [] = return empty
titleToRST lst = do
  contents <- inlineListToRST lst
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
blockToRST :: Block         -- ^ Block element
           -> State WriterState Doc 
blockToRST Null = return empty
blockToRST (Plain inlines) = do
  opts <- get >>= (return . stOptions)
  wrappedRST opts inlines
blockToRST (Para inlines) = do
  opts <- get >>= (return . stOptions)
  contents <- wrappedRST opts inlines
  return $ contents <> text "\n"
blockToRST (RawHtml str) = 
  let str' = if "\n" `isSuffixOf` str then str ++ "\n" else str ++ "\n\n" in
  return $ hang (text "\n.. raw:: html\n") 3 $ vcat $ map text (lines str')
blockToRST HorizontalRule = return $ text "--------------\n"
blockToRST (Header level inlines) = do
  contents <- inlineListToRST inlines
  let headerLength = length $ render contents
  let headerChar = if level > 5 then ' ' else "=-~^'" !! (level - 1)
  let border = text $ replicate headerLength headerChar
  return $ contents $+$ border <> text "\n"
blockToRST (CodeBlock _ str) = do
  tabstop <- get >>= (return . writerTabStop . stOptions)
  return $ (text "::\n") $+$ 
            (nest tabstop $ vcat $ map text (lines str)) <> text "\n"
blockToRST (BlockQuote blocks) = do
  tabstop <- get >>= (return . writerTabStop . stOptions)
  contents <- blockListToRST blocks 
  return $ (nest tabstop contents) <> text "\n"
blockToRST (Table caption _ widths headers rows) =  do
  caption' <- inlineListToRST caption
  let caption'' = if null caption
                     then empty
                     else text "" $+$ (text "Table: " <> caption')
  headers' <- mapM blockListToRST headers
  let widthsInChars = map (floor . (78 *)) widths
  let hpipeBlocks blocks = hcatBlocks [beg, middle, end] 
        where height = maximum (map heightOfBlock blocks)
              sep'   = TextBlock 3 height (replicate height " | ")
              beg    = TextBlock 2 height (replicate height "| ")
              end    = TextBlock 2 height (replicate height " |")
              middle = hcatBlocks $ intersperse sep' blocks
  let makeRow = hpipeBlocks . zipWith docToBlock widthsInChars
  let head' = makeRow headers'
  rows' <- mapM (\row -> do cols <- mapM blockListToRST row
                            return $ makeRow cols) rows
  let border ch = char '+' <> char ch <>
                  (hcat $ intersperse (char ch <> char '+' <> char ch) $ 
                          map (\l -> text $ replicate l ch) widthsInChars) <>
                  char ch <> char '+'
  let body = vcat $ intersperse (border '-') $ map blockToDoc rows'
  return $ border '-' $+$ blockToDoc head' $+$ border '=' $+$ body $+$ 
           border '-' $$ caption'' $$ text ""
blockToRST (BulletList items) = do
  contents <- mapM bulletListItemToRST items
  -- ensure that sublists have preceding blank line
  return $ text "" $+$ vcat contents <> text "\n"
blockToRST (OrderedList (start, style', delim) items) = do
  let markers = if start == 1 && style' == DefaultStyle && delim == DefaultDelim 
                   then take (length items) $ repeat "#."
                   else take (length items) $ orderedListMarkers 
                                              (start, style', delim)
  let maxMarkerLength = maximum $ map length markers
  let markers' = map (\m -> let s = maxMarkerLength - length m
                            in  m ++ replicate s ' ') markers
  contents <- mapM (\(item, num) -> orderedListItemToRST item num) $
              zip markers' items  
  -- ensure that sublists have preceding blank line
  return $ text "" $+$ vcat contents <> text "\n"
blockToRST (DefinitionList items) = do
  contents <- mapM definitionListItemToRST items
  return $ (vcat contents) <> text "\n"

-- | Convert bullet list item (list of blocks) to RST.
bulletListItemToRST :: [Block] -> State WriterState Doc
bulletListItemToRST items = do
  contents <- blockListToRST items
  return $ hang (text "- ") 3 contents

-- | Convert ordered list item (a list of blocks) to RST.
orderedListItemToRST :: String   -- ^ marker for list item
                     -> [Block]  -- ^ list item (list of blocks)
                     -> State WriterState Doc
orderedListItemToRST marker items = do
  contents <- blockListToRST items
  return $ hang (text marker) (length marker + 1) contents 

-- | Convert defintion list item (label, list of blocks) to RST.
definitionListItemToRST :: ([Inline], [Block]) -> State WriterState Doc
definitionListItemToRST (label, items) = do
  label' <- inlineListToRST label
  contents <- blockListToRST items
  tabstop <- get >>= (return . writerTabStop . stOptions)
  return $ label' $+$ nest tabstop contents

-- | Convert list of Pandoc block elements to RST.
blockListToRST :: [Block]       -- ^ List of block elements
               -> State WriterState Doc 
blockListToRST blocks = mapM blockToRST blocks >>= return . vcat

-- | Convert list of Pandoc inline elements to RST.
inlineListToRST :: [Inline] -> State WriterState Doc
inlineListToRST lst = mapM inlineToRST lst >>= return . hcat

-- | Convert Pandoc inline element to RST.
inlineToRST :: Inline -> State WriterState Doc
inlineToRST (Emph lst) = do 
  contents <- inlineListToRST lst
  return $ char '*' <> contents <> char '*'
inlineToRST (Strong lst) = do
  contents <- inlineListToRST lst
  return $ text "**" <> contents <> text "**"
inlineToRST (Strikeout lst) = do 
  contents <- inlineListToRST lst
  return $ text "[STRIKEOUT:" <> contents <> char ']'
inlineToRST (Superscript lst) = do 
  contents <- inlineListToRST lst
  return $ text "\\ :sup:`" <> contents <> text "`\\ "
inlineToRST (Subscript lst) = do 
  contents <- inlineListToRST lst
  return $ text "\\ :sub:`" <> contents <> text "`\\ "
inlineToRST (SmallCaps lst) = inlineListToRST lst
inlineToRST (Quoted SingleQuote lst) = do
  contents <- inlineListToRST lst
  return $ char '\'' <> contents <> char '\''
inlineToRST (Quoted DoubleQuote lst) = do
  contents <- inlineListToRST lst
  return $ char '"' <> contents <> char '"'
inlineToRST EmDash = return $ text "--"
inlineToRST EnDash = return $ char '-'
inlineToRST Apostrophe = return $ char '\''
inlineToRST Ellipses = return $ text "..."
inlineToRST (Code str) = return $ text $ "``" ++ str ++ "``"
inlineToRST (Str str) = return $ text $ escapeString str
inlineToRST (Math str) = do
  includes <- get >>= (return . stIncludes)
  let rawMathRole = ".. role:: math(raw)\n\
                    \   :format: html latex\n"
  if not (rawMathRole `elem` includes)
     then modify $ \st -> st { stIncludes = rawMathRole : includes }
     else return ()
  return $ text $ ":math:`$" ++ str ++ "$`"
inlineToRST (TeX _) = return empty
inlineToRST (HtmlInline _) = return empty
inlineToRST (LineBreak) = do
  return $ empty -- there's no line break in RST
inlineToRST Space = return $ char ' '
inlineToRST (Link [Code str] (src, _)) | src == str ||
                                         src == "mailto:" ++ str = do
  let srcSuffix = if isPrefixOf "mailto:" src then drop 7 src else src
  return $ text srcSuffix
inlineToRST (Link txt (src, tit)) = do
  useReferenceLinks <- get >>= (return . writerReferenceLinks . stOptions)
  linktext <- inlineListToRST $ normalizeSpaces txt
  if useReferenceLinks
    then do refs <- get >>= (return . stLinks)
            let refs' = if (txt, (src, tit)) `elem` refs
                           then refs
                           else (txt, (src, tit)):refs
            modify $ \st -> st { stLinks = refs' }
            return $ char '`' <> linktext <> text "`_"
    else return $ char '`' <> linktext <> text " <" <> text src <> text ">`_"
inlineToRST (Image alternate (source, tit)) = do
  pics <- get >>= (return . stImages)
  let labelsUsed = map fst pics 
  let txt = if null alternate || alternate == [Str ""] || 
               alternate `elem` labelsUsed
               then [Str $ "image" ++ show (length pics)]
               else alternate
  let pics' = if (txt, (source, tit)) `elem` pics
                 then pics
                 else (txt, (source, tit)):pics
  modify $ \st -> st { stImages = pics' }
  label <- inlineListToRST txt
  return $ char '|' <> label <> char '|'
inlineToRST (Note contents) = do 
  -- add to notes in state
  notes <- get >>= (return . stNotes)
  modify $ \st -> st { stNotes = contents:notes }
  let ref = show $ (length notes) + 1
  return $ text " [" <> text ref <> text "]_"
