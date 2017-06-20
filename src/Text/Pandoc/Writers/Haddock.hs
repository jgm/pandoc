{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-
Copyright (C) 2014-2015, 2017 John MacFarlane <jgm@berkeley.edu>

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
   Module      : Text.Pandoc.Writers.Haddock
   Copyright   : Copyright (C) 2014-2015,2017 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

Conversion of 'Pandoc' documents to haddock markup.

Haddock:  <http://www.haskell.org/haddock/doc/html/>
-}
module Text.Pandoc.Writers.Haddock (writeHaddock) where
import Control.Monad.State.Strict
import Data.Default
import Data.Text (Text)
import Data.List (intersperse, transpose)
import Text.Pandoc.Class (PandocMonad, report)
import Text.Pandoc.Definition
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Pretty
import Text.Pandoc.Shared
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Writers.Math (texMathToInlines)
import Text.Pandoc.Writers.Shared

type Notes = [[Block]]
data WriterState = WriterState { stNotes :: Notes }
instance Default WriterState
  where def = WriterState{ stNotes = [] }

-- | Convert Pandoc to Haddock.
writeHaddock :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeHaddock opts document =
  evalStateT (pandocToHaddock opts{
                  writerWrapText = writerWrapText opts } document) def

-- | Return haddock representation of document.
pandocToHaddock :: PandocMonad m
                => WriterOptions -> Pandoc -> StateT WriterState m Text
pandocToHaddock opts (Pandoc meta blocks) = do
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
  body <- blockListToHaddock opts blocks
  st <- get
  notes' <- notesToHaddock opts (reverse $ stNotes st)
  let render' :: Doc -> Text
      render' = render colwidth
  let main = render' $ body <>
               (if isEmpty notes' then empty else blankline <> notes')
  metadata <- metaToJSON opts
               (fmap render' . blockListToHaddock opts)
               (fmap render' . inlineListToHaddock opts)
               meta
  let context  = defField "body" main
               $ metadata
  case writerTemplate opts of
          Nothing  -> return main
          Just tpl -> renderTemplate' tpl context

-- | Return haddock representation of notes.
notesToHaddock :: PandocMonad m
               => WriterOptions -> [[Block]] -> StateT WriterState m Doc
notesToHaddock opts notes =
  if null notes
     then return empty
     else do
       contents <- blockToHaddock opts $ OrderedList (1,DefaultStyle,DefaultDelim) notes
       return $ text "#notes#" <> blankline <> contents

-- | Escape special characters for Haddock.
escapeString :: String -> String
escapeString = escapeStringUsing haddockEscapes
  where haddockEscapes = backslashEscapes "\\/'`\"@<"

-- | Convert Pandoc block element to haddock.
blockToHaddock :: PandocMonad m
               => WriterOptions -- ^ Options
               -> Block         -- ^ Block element
               -> StateT WriterState m Doc
blockToHaddock _ Null = return empty
blockToHaddock opts (Div _ ils) = do
  contents <- blockListToHaddock opts ils
  return $ contents <> blankline
blockToHaddock opts (Plain inlines) = do
  contents <- inlineListToHaddock opts inlines
  return $ contents <> cr
-- title beginning with fig: indicates figure
blockToHaddock opts (Para [Image attr alt (src,'f':'i':'g':':':tit)]) =
  blockToHaddock opts (Para [Image attr alt (src,tit)])
blockToHaddock opts (Para inlines) =
  -- TODO:  if it contains linebreaks, we need to use a @...@ block
  (<> blankline) `fmap` blockToHaddock opts (Plain inlines)
blockToHaddock opts (LineBlock lns) =
  blockToHaddock opts $ linesToPara lns
blockToHaddock _ b@(RawBlock f str)
  | f == "haddock" = do
      return $ text str <> text "\n"
  | otherwise = do
    report $ BlockNotRendered b
    return empty
blockToHaddock opts HorizontalRule =
  return $ blankline <> text (replicate (writerColumns opts) '_') <> blankline
blockToHaddock opts (Header level (ident,_,_) inlines) = do
  contents <- inlineListToHaddock opts inlines
  let attr' = if null ident
                 then empty
                 else cr <> text "#" <> text ident <> text "#"
  return $ nowrap (text (replicate level '=') <> space <> contents)
                 <> attr' <> blankline
blockToHaddock _ (CodeBlock (_,_,_) str) =
  return $ prefixed "> " (text str) <> blankline
-- Nothing in haddock corresponds to block quotes:
blockToHaddock opts (BlockQuote blocks) =
  blockListToHaddock opts blocks
-- Haddock doesn't have tables.  Use haddock tables in code.
blockToHaddock opts (Table caption aligns widths headers rows) = do
  caption' <- inlineListToHaddock opts caption
  let caption'' = if null caption
                     then empty
                     else blankline <> caption' <> blankline
  rawHeaders <- mapM (blockListToHaddock opts) headers
  rawRows <- mapM (mapM (blockListToHaddock opts)) rows
  let isSimple = all (==0) widths
  let isPlainBlock (Plain _) = True
      isPlainBlock _         = False
  let hasBlocks = not (all isPlainBlock $ concat . concat $ headers:rows)
  (nst,tbl) <- case True of
                _ | isSimple -> fmap (nest 2,) $
                         pandocTable opts (all null headers) aligns widths
                             rawHeaders rawRows
                  | not hasBlocks -> fmap (nest 2,) $
                         pandocTable opts (all null headers) aligns widths
                             rawHeaders rawRows
                  | otherwise -> fmap (id,) $
                         gridTable opts blockListToHaddock
                          (all null headers) aligns widths headers rows
  return $ (prefixed "> " $ nst $ tbl $$ blankline $$ caption'') $$ blankline
blockToHaddock opts (BulletList items) = do
  contents <- mapM (bulletListItemToHaddock opts) items
  return $ cat contents <> blankline
blockToHaddock opts (OrderedList (start,_,delim) items) = do
  let attribs = (start, Decimal, delim)
  let markers  = orderedListMarkers attribs
  let markers' = map (\m -> if length m < 3
                               then m ++ replicate (3 - length m) ' '
                               else m) markers
  contents <- mapM (\(item, num) -> orderedListItemToHaddock opts item num) $
              zip markers' items
  return $ cat contents <> blankline
blockToHaddock opts (DefinitionList items) = do
  contents <- mapM (definitionListItemToHaddock opts) items
  return $ cat contents <> blankline

pandocTable :: PandocMonad m
            => WriterOptions -> Bool -> [Alignment] -> [Double]
            -> [Doc] -> [[Doc]] -> StateT WriterState m Doc
pandocTable opts headless aligns widths rawHeaders rawRows =  do
  let isSimple = all (==0) widths
  let alignHeader alignment = case alignment of
                                AlignLeft    -> lblock
                                AlignCenter  -> cblock
                                AlignRight   -> rblock
                                AlignDefault -> lblock
  let numChars = maximum . map offset
  let widthsInChars = if isSimple
                         then map ((+2) . numChars)
                              $ transpose (rawHeaders : rawRows)
                         else map
                              (floor . (fromIntegral (writerColumns opts) *))
                              widths
  let makeRow = hcat . intersperse (lblock 1 (text " ")) .
                   (zipWith3 alignHeader aligns widthsInChars)
  let rows' = map makeRow rawRows
  let head' = makeRow rawHeaders
  let maxRowHeight = maximum $ map height (head':rows')
  let underline = cat $ intersperse (text " ") $
                  map (\width -> text (replicate width '-')) widthsInChars
  let border = if maxRowHeight > 1
                  then text (replicate (sum widthsInChars +
                          length widthsInChars - 1) '-')
                  else if headless
                          then underline
                          else empty
  let head'' = if headless
                  then empty
                  else border <> cr <> head'
  let body = if maxRowHeight > 1
                then vsep rows'
                else vcat rows'
  let bottom = if headless
                  then underline
                  else border
  return $ head'' $$ underline $$ body $$ bottom

-- | Convert bullet list item (list of blocks) to haddock
bulletListItemToHaddock :: PandocMonad m
                        => WriterOptions -> [Block] -> StateT WriterState m Doc
bulletListItemToHaddock opts items = do
  contents <- blockListToHaddock opts items
  let sps = replicate (writerTabStop opts - 2) ' '
  let start = text ('-' : ' ' : sps)
  -- remove trailing blank line if it is a tight list
  let contents' = case reverse items of
                       (BulletList xs:_) | isTightList xs ->
                            chomp contents <> cr
                       (OrderedList _ xs:_) | isTightList xs ->
                            chomp contents <> cr
                       _ -> contents
  return $ hang (writerTabStop opts) start $ contents' <> cr

-- | Convert ordered list item (a list of blocks) to haddock
orderedListItemToHaddock :: PandocMonad m
                         => WriterOptions -- ^ options
                         -> String        -- ^ list item marker
                         -> [Block]       -- ^ list item (list of blocks)
                         -> StateT WriterState m Doc
orderedListItemToHaddock opts marker items = do
  contents <- blockListToHaddock opts items
  let sps = case length marker - writerTabStop opts of
                   n | n > 0 -> text $ replicate n ' '
                   _ -> text " "
  let start = text marker <> sps
  return $ hang (writerTabStop opts) start $ contents <> cr

-- | Convert definition list item (label, list of blocks) to haddock
definitionListItemToHaddock :: PandocMonad m
                            => WriterOptions
                            -> ([Inline],[[Block]])
                            -> StateT WriterState m Doc
definitionListItemToHaddock opts (label, defs) = do
  labelText <- inlineListToHaddock opts label
  defs' <- mapM (mapM (blockToHaddock opts)) defs
  let contents = vcat $ map (\d -> hang 4 empty $ vcat d <> cr) defs'
  return $ nowrap (brackets labelText) <> cr <> contents <> cr

-- | Convert list of Pandoc block elements to haddock
blockListToHaddock :: PandocMonad m
                   => WriterOptions -- ^ Options
                   -> [Block]       -- ^ List of block elements
                   -> StateT WriterState m Doc
blockListToHaddock opts blocks =
  mapM (blockToHaddock opts) blocks >>= return . cat

-- | Convert list of Pandoc inline elements to haddock.
inlineListToHaddock :: PandocMonad m
                    => WriterOptions -> [Inline] -> StateT WriterState m Doc
inlineListToHaddock opts lst =
  mapM (inlineToHaddock opts) lst >>= return . cat

-- | Convert Pandoc inline element to haddock.
inlineToHaddock :: PandocMonad m
                => WriterOptions -> Inline -> StateT WriterState m Doc
inlineToHaddock opts (Span (ident,_,_) ils) = do
  contents <- inlineListToHaddock opts ils
  if not (null ident) && null ils
     then return $ "#" <> text ident <> "#"
     else return contents
inlineToHaddock opts (Emph lst) = do
  contents <- inlineListToHaddock opts lst
  return $ "/" <> contents <> "/"
inlineToHaddock opts (Strong lst) = do
  contents <- inlineListToHaddock opts lst
  return $ "__" <> contents <> "__"
inlineToHaddock opts (Strikeout lst) = do
  contents <- inlineListToHaddock opts lst
  -- not supported in haddock, but we fake it:
  return $ "~~" <> contents <> "~~"
-- not supported in haddock:
inlineToHaddock opts (Superscript lst) = inlineListToHaddock opts lst
-- not supported in haddock:
inlineToHaddock opts (Subscript lst) = inlineListToHaddock opts lst
-- not supported in haddock:
inlineToHaddock opts (SmallCaps lst) = inlineListToHaddock opts lst
inlineToHaddock opts (Quoted SingleQuote lst) = do
  contents <- inlineListToHaddock opts lst
  return $ "‘" <> contents <> "’"
inlineToHaddock opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToHaddock opts lst
  return $ "“" <> contents <> "”"
inlineToHaddock _ (Code _ str) =
  return $ "@" <> text (escapeString str) <> "@"
inlineToHaddock _ (Str str) = do
  return $ text $ escapeString str
inlineToHaddock opts (Math mt str) = do
  let adjust x = case mt of
                      DisplayMath -> cr <> x <> cr
                      InlineMath  -> x
  adjust <$> (lift (texMathToInlines mt str) >>= inlineListToHaddock opts)
inlineToHaddock _ il@(RawInline f str)
  | f == "haddock" = return $ text str
  | otherwise = do
    report $ InlineNotRendered il
    return empty
-- no line break in haddock (see above on CodeBlock)
inlineToHaddock _ LineBreak = return cr
inlineToHaddock opts SoftBreak =
  case writerWrapText opts of
       WrapAuto     -> return space
       WrapNone     -> return space
       WrapPreserve -> return cr
inlineToHaddock _ Space = return space
inlineToHaddock opts (Cite _ lst) = inlineListToHaddock opts lst
inlineToHaddock _ (Link _ txt (src, _)) = do
  let linktext = text $ escapeString $ stringify txt
  let useAuto = isURI src &&
                case txt of
                      [Str s] | escapeURI s == src -> True
                      _       -> False
  return $ nowrap $ "<" <> text src <>
           (if useAuto then empty else space <> linktext) <> ">"
inlineToHaddock opts (Image attr alternate (source, tit)) = do
  linkhaddock <- inlineToHaddock opts (Link attr alternate (source, tit))
  return $ "<" <> linkhaddock <> ">"
-- haddock doesn't have notes, but we can fake it:
inlineToHaddock opts (Note contents) = do
  modify (\st -> st{ stNotes = contents : stNotes st })
  st <- get
  let ref = text $ writerIdentifierPrefix opts ++ show (length $ stNotes st)
  return $ "<#notes [" <> ref <> "]>"
