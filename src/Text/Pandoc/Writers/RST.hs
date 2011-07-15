{-# LANGUAGE OverloadedStrings #-}
{-
Copyright (C) 2006-2010 John MacFarlane <jgm@berkeley.edu>

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
   Copyright   : Copyright (C) 2006-2010 John MacFarlane
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
import Text.Pandoc.Templates (renderTemplate)
import Data.List ( isPrefixOf, intersperse, transpose )
import Text.Pandoc.Pretty
import Control.Monad.State
import Control.Applicative ( (<$>) )

type Refs = [([Inline], Target)]

data WriterState = 
  WriterState { stNotes     :: [[Block]]
              , stLinks     :: Refs
              , stImages    :: Refs
              , stHasMath   :: Bool
              , stOptions   :: WriterOptions
              }

-- | Convert Pandoc to RST.
writeRST :: WriterOptions -> Pandoc -> String
writeRST opts document = 
  let st = WriterState { stNotes = [], stLinks = [],
                         stImages = [], stHasMath = False,
                         stOptions = opts }
  in evalState (pandocToRST document) st

-- | Return RST representation of document.
pandocToRST :: Pandoc -> State WriterState String
pandocToRST (Pandoc (Meta tit auth dat) blocks) = do
  opts <- liftM stOptions get
  title <- titleToRST tit
  authors <- mapM inlineListToRST auth
  date <- inlineListToRST dat
  body <- blockListToRST blocks
  notes <- liftM (reverse . stNotes) get >>= notesToRST
  -- note that the notes may contain refs, so we do them first
  refs <- liftM (reverse . stLinks) get >>= refsToRST
  pics <- liftM (reverse . stImages) get >>= pictRefsToRST
  hasMath <- liftM stHasMath get
  let colwidth = if writerWrapText opts
                    then Just $ writerColumns opts
                    else Nothing
  let main = render colwidth $ foldl ($+$) empty $ [body, notes, refs, pics]
  let context = writerVariables opts ++
                [ ("body", main)
                , ("title", render Nothing title)
                , ("date", render colwidth date) ] ++
                [ ("math", "yes") | hasMath ] ++
                [ ("author", render colwidth a) | a <- authors ]
  if writerStandalone opts
     then return $ renderTemplate context $ writerTemplate opts
     else return main

-- | Return RST representation of reference key table.
refsToRST :: Refs -> State WriterState Doc
refsToRST refs = mapM keyToRST refs >>= return . vcat

-- | Return RST representation of a reference key. 
keyToRST :: ([Inline], (String, String)) 
         -> State WriterState Doc
keyToRST (label, (src, _)) = do
  label' <- inlineListToRST label
  let label'' = if ':' `elem` (render Nothing label')
                   then char '`' <> label' <> char '`'
                   else label'
  return $ ".. _" <> label'' <> ": " <> text src

-- | Return RST representation of notes.
notesToRST :: [[Block]] -> State WriterState Doc
notesToRST notes = 
  mapM (\(num, note) -> noteToRST num note) (zip [1..] notes) >>=
  return . vsep

-- | Return RST representation of a note.
noteToRST :: Int -> [Block] -> State WriterState Doc
noteToRST num note = do
  contents <- blockListToRST note
  let marker = ".. [" <> text (show num) <> "]"
  return $ marker $$ nest 3 contents

-- | Return RST representation of picture reference table.
pictRefsToRST :: Refs -> State WriterState Doc
pictRefsToRST refs = mapM pictToRST refs >>= return . vcat

-- | Return RST representation of a picture substitution reference. 
pictToRST :: ([Inline], (String, String)) 
          -> State WriterState Doc
pictToRST (label, (src, _)) = do
  label' <- inlineListToRST label
  return $ ".. |" <> label' <> "| image:: " <> text src

-- | Escape special characters for RST.
escapeString :: String -> String
escapeString = escapeStringUsing (backslashEscapes "`\\|*_")

titleToRST :: [Inline] -> State WriterState Doc
titleToRST [] = return empty
titleToRST lst = do
  contents <- inlineListToRST lst
  let titleLength = length $ (render Nothing contents :: String)
  let border = text (replicate titleLength '=')
  return $ border $$ contents $$ border

-- | Convert Pandoc block element to RST. 
blockToRST :: Block         -- ^ Block element
           -> State WriterState Doc 
blockToRST Null = return empty
blockToRST (Plain inlines) = inlineListToRST inlines
blockToRST (Para [Image txt (src,tit)]) = do
  capt <- inlineListToRST txt
  let fig = "figure:: " <> text src
  let align = ":align: center"
  let alt = ":alt: " <> if null tit then capt else text tit
  return $ hang 3 ".. " $ fig $$ align $$ alt $+$ capt $$ blankline
blockToRST (Para inlines) = do
  contents <- inlineListToRST inlines
  return $ contents <> blankline
blockToRST (RawBlock f str) =
  return $ blankline <> ".. raw:: " <> text f $+$
           (nest 3 $ text str) $$ blankline
blockToRST HorizontalRule =
  return $ blankline $$ "--------------" $$ blankline
blockToRST (Header level inlines) = do
  contents <- inlineListToRST inlines
  let headerChar = if level > 5 then ' ' else "=-~^'" !! (level - 1)
  let border = text $ replicate (offset contents) headerChar
  return $ contents $$ border $$ blankline
blockToRST (CodeBlock (_,classes,_) str) = do
  opts <- stOptions <$> get
  let tabstop = writerTabStop opts
  if "haskell" `elem` classes && "literate" `elem` classes &&
                  writerLiterateHaskell opts
     then return $ prefixed "> " (text str) $$ blankline
     else return $ "::" $+$ nest tabstop (text str) $$ blankline
blockToRST (BlockQuote blocks) = do
  tabstop <- get >>= (return . writerTabStop . stOptions)
  contents <- blockListToRST blocks 
  return $ (nest tabstop contents) <> blankline
blockToRST (Table caption _ widths headers rows) =  do
  caption' <- inlineListToRST caption
  let caption'' = if null caption
                     then empty
                     else blankline <> text "Table: " <> caption'
  headers' <- mapM blockListToRST headers
  rawRows <- mapM (mapM blockListToRST) rows
  let isSimple = all (==0) widths && all (all (\bs -> length bs == 1)) rows
  let numChars = maximum . map offset
  opts <- get >>= return . stOptions
  let widthsInChars =
       if isSimple
          then map ((+2) . numChars) $ transpose (headers' : rawRows)
          else map (floor . (fromIntegral (writerColumns opts) *)) widths
  let hpipeBlocks blocks = hcat [beg, middle, end] 
        where h      = maximum (map height blocks)
              sep'   = lblock 3 $ vcat (map text $ replicate h " | ")
              beg    = lblock 2 $ vcat (map text $ replicate h "| ")
              end    = lblock 2 $ vcat (map text $ replicate h " |")
              middle = hcat $ intersperse sep' blocks
  let makeRow = hpipeBlocks . zipWith lblock widthsInChars
  let head' = makeRow headers'
  rows' <- mapM (\row -> do cols <- mapM blockListToRST row
                            return $ makeRow cols) rows
  let border ch = char '+' <> char ch <>
                  (hcat $ intersperse (char ch <> char '+' <> char ch) $ 
                          map (\l -> text $ replicate l ch) widthsInChars) <>
                  char ch <> char '+'
  let body = vcat $ intersperse (border '-') rows'
  let head'' = if all null headers
                  then empty
                  else head' $$ border '='
  return $ border '-' $$ head'' $$ body $$ border '-' $$ caption'' $$ blankline
blockToRST (BulletList items) = do
  contents <- mapM bulletListItemToRST items
  -- ensure that sublists have preceding blank line
  return $ blankline $$ vcat contents $$ blankline
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
  return $ blankline $$ vcat contents $$ blankline
blockToRST (DefinitionList items) = do
  contents <- mapM definitionListItemToRST items
  -- ensure that sublists have preceding blank line
  return $ blankline $$ vcat contents $$ blankline

-- | Convert bullet list item (list of blocks) to RST.
bulletListItemToRST :: [Block] -> State WriterState Doc
bulletListItemToRST items = do
  contents <- blockListToRST items
  return $ hang 3 "-  " $ contents <> cr

-- | Convert ordered list item (a list of blocks) to RST.
orderedListItemToRST :: String   -- ^ marker for list item
                     -> [Block]  -- ^ list item (list of blocks)
                     -> State WriterState Doc
orderedListItemToRST marker items = do
  contents <- blockListToRST items
  let marker' = marker ++ " "
  return $ hang (length marker') (text marker') $ contents <> cr

-- | Convert defintion list item (label, list of blocks) to RST.
definitionListItemToRST :: ([Inline], [[Block]]) -> State WriterState Doc
definitionListItemToRST (label, defs) = do
  label' <- inlineListToRST label
  contents <- liftM vcat $ mapM blockListToRST defs
  tabstop <- get >>= (return . writerTabStop . stOptions)
  return $ label' $$ nest tabstop (contents <> cr)

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
  return $ "*" <> contents <> "*"
inlineToRST (Strong lst) = do
  contents <- inlineListToRST lst
  return $ "**" <> contents <> "**"
inlineToRST (Strikeout lst) = do 
  contents <- inlineListToRST lst
  return $ "[STRIKEOUT:" <> contents <> "]"
inlineToRST (Superscript lst) = do 
  contents <- inlineListToRST lst
  return $ "\\ :sup:`" <> contents <> "`\\ "
inlineToRST (Subscript lst) = do 
  contents <- inlineListToRST lst
  return $ "\\ :sub:`" <> contents <> "`\\ "
inlineToRST (SmallCaps lst) = inlineListToRST lst
inlineToRST (Quoted SingleQuote lst) = do
  contents <- inlineListToRST lst
  return $ "‘" <> contents <> "’"
inlineToRST (Quoted DoubleQuote lst) = do
  contents <- inlineListToRST lst
  return $ "“" <> contents <> "”"
inlineToRST (Cite _  lst) =
  inlineListToRST lst
inlineToRST EmDash = return $ char '\8212'
inlineToRST EnDash = return $ char '\8211'
inlineToRST Apostrophe = return $ char '\8217'
inlineToRST Ellipses = return $ char '\8230'
inlineToRST (Code _ str) = return $ "``" <> text str <> "``"
inlineToRST (Str str) = return $ text $ escapeString str
inlineToRST (Math t str) = do
  modify $ \st -> st{ stHasMath = True }
  return $ if t == InlineMath
              then ":math:`$" <> text str <> "$`"
              else ":math:`$$" <> text str <> "$$`"
inlineToRST (RawInline _ _) = return empty
inlineToRST (LineBreak) = return cr -- there's no line break in RST
inlineToRST Space = return space
inlineToRST (Link [Code _ str] (src, _)) | src == str ||
                                           src == "mailto:" ++ str = do
  let srcSuffix = if isPrefixOf "mailto:" src then drop 7 src else src
  return $ text $ unescapeURI srcSuffix
inlineToRST (Link txt (src', tit)) = do
  let src = unescapeURI src'
  useReferenceLinks <- get >>= return . writerReferenceLinks . stOptions
  linktext <- inlineListToRST $ normalizeSpaces txt
  if useReferenceLinks
    then do refs <- get >>= return . stLinks
            let refs' = if (txt, (src, tit)) `elem` refs
                           then refs
                           else (txt, (src, tit)):refs
            modify $ \st -> st { stLinks = refs' }
            return $ "`" <> linktext <> "`_"
    else return $ "`" <> linktext <> " <" <> text src <> ">`_"
inlineToRST (Image alternate (source', tit)) = do
  let source = unescapeURI source'
  pics <- get >>= return . stImages
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
  return $ "|" <> label <> "|"
inlineToRST (Note contents) = do 
  -- add to notes in state
  notes <- get >>= return . stNotes
  modify $ \st -> st { stNotes = contents:notes }
  let ref = show $ (length notes) + 1
  return $ " [" <> text ref <> "]_"
